module ImageCompressor where

import System.Environment
import System.Exit (exitWith, ExitCode (ExitSuccess, ExitFailure), exitSuccess)
import Text.Read
import Data.List
import Control.Monad
import Control.Exception
import Data.Char
import GHC.Float

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
                    ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")"

data Cluster =
    Cluster { new :: Color
             , old :: Color
             , pixels :: [Pixel]
            }
instance Show Cluster where
    show (Cluster (r, g, b) _ _) = "--\n(" ++ show r ++ "," ++ show g ++
                                        "," ++ show b ++ ")\n-"

initConf :: Params
initConf = Params { colors = Nothing
                    , converge = Nothing
                    , filepath = Nothing
                    }

toConfig :: Params -> Maybe Config
toConfig (Params Nothing _ _) = Nothing
toConfig (Params _ Nothing _) = Nothing
toConfig (Params _ _ Nothing) = Nothing
toConfig (Params (Just colors) (Just converge) (Just filepath))
        | colors <= 0 || converge <= 0 = Nothing
        | otherwise = do
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

distance :: Color -> Color -> Float
distance (x, xs, xt) (y, ys, yt) =
    sqrt ((int2Float x - int2Float y)**2 + (int2Float xs - int2Float ys)**2 +
        (int2Float xt - int2Float yt)**2)

manageCompressor :: Config -> IO()
manageCompressor conf@(Config n l f) = openFile n l f

printException :: SomeException -> IO String
printException _ = putStrLn "File not found." >>
                   exitWith (ExitFailure 84)

checkFile :: String -> IO ()
checkFile [] = return ()
checkFile (x:xs) | isDigit x == False && x /= ')' && x /= '(' && x /= ' ' &&
                            x /= ',' && x /= '\n' = exitWith (ExitFailure 84)
                 | otherwise = checkFile xs

checkPixels :: [Pixel] -> IO ()
checkPixels [] = return ()
checkPixels ((Pixel (x, y) (r, g, b)):pxls) 
                | r > 255 || g > 255 || b > 255 = exitWith (ExitFailure 84)
                | otherwise = checkPixels pxls

openFile :: Int -> Float -> String -> IO ()
openFile n l f = do
    file <- catch (readFile f) printException
    checkFile file
    let pixels = getPixels [] (lines file)
    checkPixels pixels
    let clusters = createCluster [] pixels n
    let compressed = compress (changeCluster pixels clusters) [] pixels l
    displayClusters compressed

displayClusters :: [Cluster] -> IO ()
displayClusters [] = return ()
displayClusters (x@(Cluster _ _ pxls):xs) = print x >> displayPixels pxls >>
                                            displayClusters xs

displayPixels :: [Pixel] -> IO ()
displayPixels [] = return ()
displayPixels (x:xs) = print x >> displayPixels xs

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

getPixels :: [Pixel] -> [String] -> [Pixel]
getPixels pixels [] = pixels
getPixels pixels (x:y) = getPixels
                (reverse ((parseLine initPixel x):reverse pixels)) y

createCluster :: [Cluster] -> [Pixel] -> Int -> [Cluster]
createCluster list [] _ = list
createCluster list _ 0 = list
createCluster list (pixel@(Pixel _ col):xs) l | checkColor col list =
                                                createCluster list xs l
    | otherwise = createCluster (Cluster col col [pixel]:list) xs (l - 1)

checkColor :: Color -> [Cluster] -> Bool
checkColor _ [] = False
checkColor color (Cluster c _ _:x) | color == c = True
    | otherwise = checkColor color x

compress :: [Cluster] -> [Cluster] -> [Pixel] -> Float -> [Cluster]
compress oclusters [] pxls l = compress oclusters
                            (changeCluster pxls oclusters) pxls l
compress oclusters nclusters pxls l | checkConvergence l nclusters = oclusters
                          | otherwise = compress nclusters [] pxls l

changeCluster :: [Pixel] -> [Cluster] -> [Cluster]
changeCluster pixels clusters = foldl
                checkPixelsForCluster (map changeClusterColor clusters) pixels

checkPixelsForCluster :: [Cluster] -> Pixel -> [Cluster]
checkPixelsForCluster cluster pixel = putPixelInCluster pixel
                    (sortOn (getPixelAndClusterDistance pixel) cluster)

putPixelInCluster :: Pixel -> [Cluster] -> [Cluster]
putPixelInCluster _ [] = []
putPixelInCluster pixel ((Cluster old new clpxls):rest) =
                    ((Cluster old new (pixel:clpxls)):rest)

getPixelAndClusterDistance :: Pixel -> Cluster -> Float
getPixelAndClusterDistance (Pixel _ pixelColor) (Cluster oldClColor _ _) =
                                            distance pixelColor oldClColor

changeClusterColor :: Cluster -> Cluster
changeClusterColor (Cluster old _ pixels) = (Cluster (getAverageColor
        (foldl mergeColor (0, 0, 0) pixels) (length pixels)) old [])

mergeColor :: Color -> Pixel -> Color
mergeColor (r, g, b) (Pixel _ (rs, gs, bs)) = (r + rs, g + gs, b + bs)

getAverageColor :: Color -> Int -> Color
getAverageColor (r, g, b) l = ((r `div` l), (g `div` l), (b `div` l))

checkConvergence :: Float -> [Cluster] -> Bool
checkConvergence _ [] = True
checkConvergence l (Cluster ocol ncol _:xs) | distance ocol ncol > l = False
                                        | otherwise = checkConvergence l xs
