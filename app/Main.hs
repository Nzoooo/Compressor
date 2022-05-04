module Main where

import Lib
import ImageCompressor

import System.Environment
import System.Exit (exitWith, ExitCode (ExitSuccess, ExitFailure), exitSuccess)
import Text.Read
import Control.Monad

usage :: IO()
usage = putStrLn "USAGE: ./imageCompressor -n N -l L -f F\n" >>
        putStrLn "      N\tnumber of colors in the final image" >>
        putStrLn "      L\tconvergence limit" >>
        putStr "      F\tpath to the file containing the colors" >>
        putStrLn "of the pixels"

main :: IO ()
main = do
    args <- getArgs
    let maConfig = join $ toConfig <$> (manageArgs args)
    case maConfig of
        Nothing -> usage >>
            exitWith (ExitFailure 84)
        -- Just newConfig -> fct newConfig
    return()
