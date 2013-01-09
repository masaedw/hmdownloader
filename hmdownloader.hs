import Control.Concurrent.Chan (Chan, readChan, writeChan)
import Control.Monad
import Control.ThreadPool (threadPoolIO)
import Data.List
import System.Directory
import System.Environment
import Text.HandsomeSoup
import Text.Regex.Posix
import Text.XML.HXT.Core

main = do
  galleries <- parseTopPage
  runPool_ 5 $ map downloadFiles galleries

type Url = String

parseTopPage :: IO [Url]
parseTopPage = do
  let url = "http://www.himemix.com/"
  doc <- fromUrl url
  candidate <- runX $ doc >>> css ".con02 a" ! "href"
  return $ map (url ++) $ filter ("girl/" `isPrefixOf`) candidate

downloadFiles :: Url -> IO ()
downloadFiles url = do
  let base = baseurl url
      (_, _, _, name:_) = url =~ "girl/(.+)/" :: (String,String,String,[String])
  doc <- fromUrl url
  hrefs <- runX $ doc >>> css "a" ! "href"
  exist <- doesDirectoryExist name
  if not exist
    then putStrLn $ "mkdir " ++ name
    else return ()
  let images = map (base ++) $ filter ("img/" `isPrefixOf`) hrefs
      movies = filter (".mp4" `isSuffixOf`) hrefs
  mapM_ (uncurry download) $ map (\x -> (x, imageFilename name x)) $ images ++ movies

download :: Url -> FilePath -> IO ()
download url filepath = do
  exist <- doesFileExist filepath
  if not exist
    then putStrLn $ "curl -# '" ++ url ++ "' -o " ++ filepath
    else return ()

imageFilename :: String -> Url -> FilePath
imageFilename name url = name ++ "/" ++ basename url

baseurl :: Url -> Url
baseurl = reverse . dropWhile (/= '/') . reverse

basename :: String -> String
basename = reverse . takeWhile (/= '/') . reverse

-- http://stackoverflow.com/questions/9193349/how-do-i-create-a-thread-pool
runPool_ :: Int -> [IO a] -> IO ()
runPool_ n as = do
  (input, output) <- threadPoolIO n id
  forM_ as $ writeChan input
  sequence_ (take (length as) . repeat $ readChan output)
