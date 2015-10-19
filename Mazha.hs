{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as B
import qualified Data.Conduit.Binary as CB
import System.IO
import System.Environment
import System.Process
import Control.Monad
import Control.Concurrent.Async (mapConcurrently)
import Data.Conduit
import Control.Monad.Trans.Resource (runResourceT)

splitSize = 20 * 1024 * 1024 -- 20 Mega Bytes

download :: String -> String -> IO ()
download url fname = do
  system $ "curl -sI " ++ url ++
             " | awk '/Content-Length/ { print $2 }' > /tmp/hacurl"
  fileSize <- fmap (read) $ readFile $ "/tmp/hacurl" :: IO Int
  putStrLn $ show fileSize
  let splits = ((fromIntegral fileSize) / (fromIntegral splitSize))
      rs = ceiling splits
  putStrLn $ "Number of splits: " ++ (show rs)
  mapConcurrently (\x -> download' url x fileSize fname) [1..rs]
  putStrLn "Download completed! Currently merging all the parts."
  combine rs fname

download' :: String -> Int -> Int -> String -> IO ()
download' url n tot fname = do
  system $ "curl -s -o \"" ++ fname ++ "." ++ (show n) ++
             "\" --range " ++ start ++  "-" ++ end ++ " " ++ url ++ " -C -"
  return ()
    where
      start = show $ (n - 1) * splitSize
      end = let size = (n * splitSize - 1)
            in show $ if size > tot then tot else size

combine :: Int -> String -> IO ()
combine n fname = runResourceT $ sequence_ inp $$ CB.sinkFile fname
    where inp = map (\x -> CB.sourceFile $ fname ++ "." ++ show x) [1..n]

main = do
  ar <- fmap head $ getArgs
  let fileName = reverse $ takeWhile (/= '/') $ reverse ar
  download ar fileName

