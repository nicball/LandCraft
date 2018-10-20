module Model where

import qualified Data.Binary as Bin
import           Data.Binary (Binary)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map, (!))
import Data.Maybe
import GHC.Generics (Generic)
import System.Random

data Dir = UpD | DownD | RightD | LeftD
    deriving (Generic, Show)
instance Binary Dir

data CoordSys = CoordSys Int

type Coord = (Int, Int)

data Unit
    = Unit { unitSight :: Int
           , unitHp :: Int
           , unitPos :: Coord
           }
    deriving Eq

data Command :: * -> * where
    Spawn :: Coord -> Command Uid
    Move :: Uid -> Dir -> Command ()
    Quit :: Uid -> Command ()

data WrappedCommand where
    WrappedCommand :: Command a -> WrappedCommand

data Program :: * -> * where
    PureP :: a -> Program a
    BindP :: (Command a) -> (a -> Program b) -> Program b

data GameState
    = GameState { gsUnits :: Map Uid Unit
                , gsMyUid :: Maybe Uid
                , gsCoordSys :: CoordSys
                }

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

isQuit :: Command a -> Bool
isQuit (Quit _) = True
isQuit _ = False

instance Show (Command a) where
    show (Spawn c) = "Spawn " ++ show c
    show (Move uid dir) = "Move " ++ show uid ++ " " ++ show dir
    show (Quit uid) = "Quit " ++ show uid

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

defaultUnit :: Coord -> Unit
defaultUnit = Unit 4 5

emptyGameState :: CoordSys -> GameState
emptyGameState = GameState Map.empty Nothing 

findUnitsByPos :: GameState -> Coord -> [Unit]
findUnitsByPos gs pos
    = Map.elems . Map.filter ((== pos) . unitPos) . gsUnits $ gs

findAliveUnitByPos :: GameState -> Coord -> Maybe Unit
findAliveUnitByPos gs = listToMaybe . filter ((> 0) . unitHp) . findUnitsByPos gs

findUidsByPos :: GameState -> Coord -> [Uid]
findUidsByPos gs pos
    = Map.keys . Map.filter ((== pos) . unitPos) . gsUnits $ gs

findAliveUidByPos :: GameState -> Coord -> Maybe Uid
findAliveUidByPos gs = listToMaybe . filter (isUnitAlive gs) . findUidsByPos gs

findUnitById :: GameState -> Uid -> Unit
findUnitById gs uid
    = (gsUnits gs) ! uid

modifyUnitById :: GameState -> Uid -> (Unit -> Unit) -> GameState
modifyUnitById gs uid f
    = gs { gsUnits = Map.update (Just . f) uid . gsUnits $ gs }

isUnitAlive :: GameState -> Uid -> Bool
isUnitAlive gs = (> 0) . unitHp . findUnitById gs

inSight :: GameState -> Uid -> Coord -> Bool
inSight gs uid
    = let unit = findUnitById gs uid
      in (< unitSight unit) . distance (unitPos unit)

amIAlive :: GameState -> Bool
amIAlive gs
    = case gsMyUid gs of
        Just uid -> isUnitAlive gs uid
        Nothing -> False

amIWatcher :: GameState -> Bool
amIWatcher gs
    = case gsMyUid gs of
        Just uid -> not . isUnitAlive gs $ uid
        Nothing -> False

allDead :: GameState -> Bool
allDead = all ((<= 0) . unitHp) . (Map.elems . gsUnits)

genLocation :: GameState -> IO Coord
genLocation gs = do
    x <- getStdRandom (randomR coordRange)
    y <- getStdRandom (randomR coordRange)
    case findAliveUnitByPos gs (x, y) of
        Nothing -> return (x, y)
        Just _ -> genLocation gs
    where coordRange = let CoordSys size = gsCoordSys gs
                       in (0, size - 1)

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
          newpos = moveCoord (gsCoordSys gs) dir (unitPos unit)
      in if newpos == unitPos unit
         then ((), gs)
         else case findAliveUidByPos gs newpos of
             Just tuid ->
                 let tunit = findUnitById gs tuid
                     newhp = unitHp tunit - 1
                 in if newhp <= 0
                    then ((), gs { gsUnits = Map.insert uid unit { unitPos = newpos }
                                           . Map.insert tuid tunit { unitHp = newhp }
                                           . gsUnits
                                           $ gs })
                    else ((), gs { gsUnits = Map.insert tuid tunit { unitHp = newhp } . gsUnits $ gs })
             Nothing ->
                 let unit' = unit { unitPos = newpos }
                     gs' = gs { gsUnits = Map.insert uid unit' (gsUnits gs) }
                 in ((), gs')
runCommand gs (Quit uid)
    = ((), modifyUnitById gs uid \u -> u { unitHp = 0 })

execCommand :: GameState -> Command a -> GameState
execCommand gs = snd . runCommand gs
