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
  doc <- fromUrl url
  hrefs <- runX $ doc >>> css "a" ! "href"
  exist <- doesDirectoryExist name
  unless exist .
    putStrLn $ "mkdir " ++ name
  let images = map (baseurl url ++) $ filter ("img/" `isPrefixOf`) hrefs
      movies = filter (=~ "(mp4|zip)$") hrefs
  forM_ (images ++ movies) $ \x ->
    download x $ imageFilename name x
    where (_, _, _, name:_) = url =~ "girl/(.+)/" :: (String,String,String,[String])

download :: Url -> FilePath -> IO ()
download url filepath = do
  exist <- doesFileExist filepath
  unless exist .
    putStrLn $ "curl -# '" ++ url ++ "' -o " ++ filepath

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
  forM_ as . const $ readChan output
