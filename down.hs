import System.IO
import System.Environment
import System.Process

splitSize = 128 * 1024 * 1024

download :: String -> IO ()
download url = do
  system $ "curl -sI " ++ url ++
             " | awk '/Content-Length/ { print $2 }' > /tmp/hacurl"
  fileSize <- fmap (read) $ readFile $ "/tmp/hacurl" :: IO Int
  putStrLn $ show fileSize
  let splits = ((fromIntegral fileSize) / (fromIntegral splitSize))
      rs = ceiling splits
      xss = map (\x -> download' url x)  [1..rs]
  foldr (>>) (return ()) xss
  combine rs
  putStrLn $ show rs


download' :: String -> Int -> IO ()
download' url n = do
  system $ "curl -o \"file." ++ (show n) ++ "\" --range " ++ start ++  "-" ++ end ++ " " ++ url
  return ()
    where
      start = show $ n * splitSize
      end = show $ (n + 1) * splitSize

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
