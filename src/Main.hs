module Main where

import System.Environment

import Client
import Server
import Config

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["server"] -> startServer serverPort
        ["server", port] -> startServer (read port)
        ["client", name] -> startClient name serverName (show serverPort)
        ["client", name, serverName] -> startClient name serverName (show serverPort)
        ["client", name, serverName, serverPort] -> startClient name serverName serverPort
        _ -> putStrLn "Unknown command line arguments."
