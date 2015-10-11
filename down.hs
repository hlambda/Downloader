{-# LANGUAGE OverloadedStrings #-}
import System.IO
import System.Environment
import System.Process
import qualified Data.ByteString as B

-- Problems
-- -------
--
-- The files that are to be combined are binary files.
-- Might require the use of Bytestring package or something similar.

splitSize = 128 * 1024 * 1024 -- 128 Mega Bytes

download :: String -> IO ()
download url = do
  system $ "curl -sI " ++ url ++
             " | awk '/Content-Length/ { print $2 }' > /tmp/hacurl"
  fileSize <- fmap (read) $ readFile $ "/tmp/hacurl" :: IO Int
  putStrLn $ show fileSize
  let splits = ((fromIntegral fileSize) / (fromIntegral splitSize))
      rs = ceiling splits
      -- Room for parallel download'?
      -- Note that there is a parallel map function
      xss = map (\x -> download' url x fileSize)  [1..rs]
  putStrLn $ "Number of splits: " ++ (show rs)
  foldr (>>) (return ()) xss
  combine rs

download' :: String -> Int -> Int -> IO ()
download' url n tot = do
  -- will continue work properly?
  -- Corner case? What if we have reached the end?
  system $ "curl -o \"file." ++ (show n) ++ "\" --range " ++ start ++  "-" ++ end ++ " " ++ url
  return ()
    where
      start = show $ (n - 1) * splitSize
      end = let size = (n * splitSize - 1)
            in show $ if size > tot then tot else size

combine :: Int -> IO ()
combine n = do
  let xs = map (\x -> aux x) [1..n]
  all <- foldr (aux') (return "") xs
  B.writeFile "file" all  
      where aux x = B.readFile $ "file." ++ (show x)
            aux' s1 s2 = do
              s1' <- s1
              s2' <- s2
              let s3 = s1' `B.append` s2'
              return s3  

main = do
  ar <- fmap head $ getArgs
  putStrLn $ ar
  download ar
  return ()
