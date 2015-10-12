{-# LANGUAGE OverloadedStrings #-}
import System.IO
import System.Environment
import System.Process
import Control.Monad
import Control.Concurrent.Async
import qualified Data.ByteString as B

splitSize = 128 * 1024 * 1024 -- 128 Mega Bytes

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
  combine rs fname

download' :: String -> Int -> Int -> String -> IO ()
download' url n tot fname = do
  system $ "curl -o \"" ++ fname ++ "." ++ (show n) ++
             "\" --range " ++ start ++  "-" ++ end ++ " " ++ url
  return ()
    where
      start = show $ (n - 1) * splitSize
      end = let size = (n * splitSize - 1)
            in show $ if size > tot then tot else size

combine :: Int -> String -> IO ()
combine n fname = do
  let xs = map (\x -> aux x) [1..n]
  all <- foldr (aux') (return "") xs
  B.writeFile fname all  
      where aux x = B.readFile $ fname ++  "." ++ (show x)
            aux' :: IO B.ByteString -> IO B.ByteString -> IO B.ByteString
            aux' s1 s2 = liftM2 B.append s1 s2

main = do
  ar <- fmap head $ getArgs
  let fileName = reverse $ takeWhile (/= '/') $ reverse ar
  download ar fileName

