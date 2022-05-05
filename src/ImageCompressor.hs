module ImageCompressor where

import System.Environment
import System.Exit (exitWith, ExitCode (ExitSuccess, ExitFailure), exitSuccess)
import Text.Read
import Control.Monad
import Control.Exception
import Data.Char

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

type Point = (Int, Int)

type Color = (Int, Int, Int)

data Pixel =
    Pixel { point :: Point
            , color :: Color
            }
instance Show Pixel where
    show (Pixel (x, y) (r, g, b)) = "(" ++ show x ++ "," ++ show y ++ ") (" 
                    ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")" ++ "\n"

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
        go config ("-l":valueConv:params) =
                            go config{converge = readMaybe valueConv} params
        go config ("-f":valueFilepath:params) =
                            go config{filepath = Just valueFilepath} params
        go _ _ = Nothing

distance :: (Float, Float, Float) -> (Float, Float, Float) -> Float
distance (x, xs, xt) (y, ys, yt) =
    sqrt ((x - y)**2 + (xs - ys)**2 + (xt - yt)**2)

manageCompressor :: Config -> IO()
manageCompressor conf@(Config n l f) = openFile n f

printException :: SomeException -> IO String
printException _ = putStrLn "File not found." >>
                   exitWith (ExitFailure 84)

openFile :: Int -> String -> IO ()
openFile n f = do
    file <- catch (readFile f) printException
    let pixels = fct [] (lines file)
    print pixels
    -- let cluster =  (pixelsToColors(getCluster pixel [] n g))
    -- let index = (getClustersIndexs pixel (cluster))
    -- clusterLoop n pixel index cluster

myGetNbr :: String -> Int -> Int
myGetNbr [] nb = nb
myGetNbr (x:xs) nb | isDigit x = myGetNbr xs (nb * 10 + digitToInt x)
                   | otherwise = nb

initPixel :: Pixel
initPixel = Pixel { point = (0, 0)
                    , color = (0, 0, 0)
                    }

countOccurences :: String -> Char -> Int -> Int
countOccurences [] _ nb = nb
countOccurences (x:y) c nb | x == c = countOccurences y c (nb + 1)
                           | otherwise = countOccurences y c nb

parseLine :: Pixel -> String -> Pixel
parseLine pxl [] = pxl
parseLine pxl@(Pixel p@(x, y) c@(r, g, b)) (l:ls) | l == '(' &&
    (countOccurences ls '(' 0) == 1 = parseLine (Pixel (myGetNbr ls 0, y) c) ls
                                                  | l == ',' &&
    (countOccurences ls '(' 0) == 1 = parseLine (Pixel (x, myGetNbr ls 0) c) ls
                                                  | l == '(' &&
    (countOccurences ls '(' 0) == 0 =
        parseLine (Pixel p (myGetNbr ls 0, g, b)) ls
                                                  | l == ',' &&
    (countOccurences ls '(' 0) == 0 && (countOccurences ls ',' 0) == 1 =
        parseLine (Pixel p (r, myGetNbr ls 0, b)) ls
                                                  | l == ',' &&
    (countOccurences ls '(' 0) == 0 && (countOccurences ls ',' 0) == 0 =
        parseLine (Pixel p (r, g, myGetNbr ls 0)) ls
                                                  | otherwise = parseLine pxl ls

fct :: [Pixel] -> [String] -> [Pixel]
fct pixels [] = pixels
fct pixels (x:y) = fct (reverse ((parseLine initPixel x):reverse pixels)) y