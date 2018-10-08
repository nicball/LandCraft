module Config where

import Model

serverPort :: (Num a, Show a) => a
serverPort = 40052

serverName :: String
serverName = "landcraft.nicball.me"

roomSize :: Num a => a
roomSize = 2

mapSize :: CoordSys
mapSize = CoordSys 10
