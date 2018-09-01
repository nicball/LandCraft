module Model where

import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map, (!))
import           GHC.Generics (Generic)
import qualified Data.Binary as Bin
import           Data.Binary (Binary)

data Dir = UpD | DownD | RightD | LeftD
    deriving (Generic, Show)
instance Binary Dir

data CoordSys = CoordSys Int

type Coord = (Int, Int)

validCoord :: CoordSys -> Coord -> Bool
validCoord (CoordSys size) (x, y)
    = x >= 0 && x < size && y >= 0 && y < size

moveCoord :: CoordSys -> Dir -> Coord -> Coord
moveCoord cs dir (x, y)
    = if validCoord cs coord then coord else (x, y)
    where coord = case dir of
              UpD -> (x, y + 1)
              DownD -> (x, y - 1)
              LeftD -> (x - 1, y)
              RightD -> (x + 1, y)

distance :: Coord -> Coord -> Int
distance (x1, y1) (x2, y2)
    = ceiling . sqrt . fromIntegral $ sq (x1 - x2) + sq (y1 - y2)
    where sq x = x * x

type Uid = Int

data Command :: * -> * where
    Spawn :: Coord -> Command Uid
    Move :: Uid -> Dir -> Command ()
    Quit :: Uid -> Command ()

instance Show (Command a) where
    show (Spawn c) = "Spawn " ++ show c
    show (Move uid dir) = "Move " ++ show uid ++ " " ++ show dir
    show (Quit uid) = "Quit " ++ show uid

data WrappedCommand where
    WrappedCommand :: Command a -> WrappedCommand

instance Show WrappedCommand where
    show (WrappedCommand cmd) = show cmd

instance Binary WrappedCommand where
    put (WrappedCommand (Spawn coord)) = do
        Bin.putWord8 0
        Bin.put coord
    put (WrappedCommand (Move uid dir)) = do
        Bin.putWord8 1
        Bin.put uid
        Bin.put dir
    put (WrappedCommand (Quit uid)) = do
        Bin.putWord8 2
        Bin.put uid
    get = do
        alt <- Bin.getWord8
        case alt of
            0 -> WrappedCommand . Spawn <$> Bin.get
            1 -> WrappedCommand <$> (Move <$> Bin.get <*> Bin.get)
            2 -> WrappedCommand . Quit <$> Bin.get

data Program :: * -> * where
    PureP :: a -> Program a
    BindP :: (Command a) -> (a -> Program b) -> Program b

instance Functor Program where
    fmap f (PureP a) = PureP (f a)
    fmap f (BindP m c) = BindP m (fmap (fmap f) c)

instance Applicative Program where
    pure = PureP
    (PureP f) <*> m = f <$> m
    (BindP a c) <*> m = BindP a (fmap (<*> m) c)

instance Monad Program where
    return = PureP
    (PureP a) >>= f = f a
    (BindP m c) >>= f = BindP m (fmap (>>= f) c)

liftCommand :: Command a -> Program a
liftCommand cmd = BindP cmd return

spawn :: Coord -> Program Uid
spawn = liftCommand . Spawn

move :: Uid -> Dir -> Program ()
move uid dir = BindP (Move uid dir) return

quit :: Uid -> Program ()
quit = liftCommand . Quit

data Unit
    = Unit { unitSight :: Int
           , unitHp :: Int
           , unitPos :: Coord
           }

unitIsAlive :: Unit -> Bool
unitIsAlive = (> 0) . unitHp

defaultUnit :: Coord -> Unit
defaultUnit pos = Unit 4 5 pos

data GameState
    = GameState { gsUnits :: Map Uid Unit
                , gsMyUid :: Maybe Uid
                , gsCoordSys :: CoordSys
                }

findUnitByPos :: GameState -> Coord -> Maybe Unit
findUnitByPos gs pos
    = let u = Map.elems . Map.filter ((== pos) . unitPos) . gsUnits $ gs
      in case u of
          [x] -> Just x
          [] -> Nothing
          _ -> error "Many units at same position."

findUnitById :: GameState -> Uid -> Unit
findUnitById gs uid
    = (gsUnits gs) ! uid

runProgram :: GameState -> Program a -> (a, GameState)
runProgram gs (PureP a) = (a, gs)
runProgram gs (BindP cmd cont)
    = let (a, gs') = runCommand gs cmd
      in runProgram gs' (cont a)

runCommand :: GameState -> Command a -> (a, GameState)
runCommand gs (Spawn pos)
    = let uid = (+ 1) . Map.size . gsUnits $ gs
          gs' = gs { gsUnits = Map.insert uid (defaultUnit pos) (gsUnits gs) }
      in (uid, gs')
runCommand gs (Move uid dir)
    = let unit = gsUnits gs ! uid
          unit' = unit { unitPos = moveCoord (gsCoordSys gs) dir (unitPos unit) }
          gs' = gs { gsUnits = Map.insert uid unit' (gsUnits gs) }
      in ((), gs')
runCommand gs (Quit uid)
    = let gs' = gs { gsUnits = Map.delete uid (gsUnits gs) }
      in ((), gs')
