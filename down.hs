import System.IO
import System.Environment
import System.Process

-- Problems
-- -------
--
-- The files that are to be combined are binary files.
-- Might require the use of Bytestring package or something similar.

splitSize = 1024 * 1024

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
      xss = map (\x -> download' url x)  [1..rs]
  foldr (>>) (return ()) xss
  combine rs
  putStrLn $ show rs


download' :: String -> Int -> IO ()
download' url n = do
  -- will continue work properly?
  system $ "curl -o \"file." ++ (show n) ++ "\" --range " ++ start ++  "-" ++ end ++ " " ++ url
  return ()
    where
      start = show $ n * splitSize
      end = show $ ((n + 1) * splitSize - 1)

combine :: Int -> IO ()
combine n = do
  let xs = map (\x -> aux x) [1..n]
  all <- foldr (aux') (return "") xs
  writeFile "file" all  
      where aux x = readFile $ "file." ++ (show x)
            aux':: (IO String) -> (IO String) -> (IO String)
            aux' s1 s2 = do
              s1' <- s1
              s2' <- s2
              let s3 = s1' ++ s2'
              return s3  

main = do
  ar <- getArgs
  putStrLn $ head ar
  download $ head ar
  return ()
