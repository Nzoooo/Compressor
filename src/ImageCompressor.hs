module ImageCompressor where

import System.Environment
import System.Exit (exitWith, ExitCode (ExitSuccess, ExitFailure), exitSuccess)
import Text.Read
import Control.Monad

data Params =
    Params { colors :: Maybe Int
            , converge :: Maybe Float
            , filepath :: Maybe String
            }

data Rule = Rule30 | Rule90 | Rule110

data Config =
    Config { configColors :: Int
            , configConverge :: Float
            , configFilepath :: String
            }

initConf :: Params
initConf = Params { colors = Nothing
                    , converge = Nothing
                    , filepath = Nothing
                    }

toConfig :: Params -> Maybe Config
toConfig (Params Nothing _ _) = Nothing
toConfig (Params _ Nothing _) = Nothing
toConfig (Params _ _ Nothing) = Nothing
toConfig (Params (Just colors) (Just converge) (Just filepath)) = do
    return $ Config { configColors = colors
                    , configConverge = converge
                    , configFilepath = filepath
                    }

manageArgs :: [String] -> Maybe Params
manageArgs args = go initConf args
    where
        go :: Params -> [String] -> Maybe Params
        go config [] = Just config
        go config [_] = Nothing
        go config ("-n":valueColors:params) =
                            go config{colors = readMaybe valueColors} params
        go config ("-l":valueConverge:params) =
                            go config{converge = readMaybe valueConverge} params
        go config ("-f":valueFilepath:params) =
                            go config{filepath = Just valueFilepath} params
        go _ _ = Nothing

distance :: (Float, Float, Float) -> (Float, Float, Float) -> Float
distance (x, xs, xt) (y, ys, yt) =
    sqrt ((x - y)**2 + (xs - ys)**2 + (xt - yt)**2)