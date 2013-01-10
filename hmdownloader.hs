import Control.Concurrent.Chan (Chan, readChan, writeChan)
import Control.Monad
import Control.ThreadPool (threadPoolIO)
import Data.List
import System.Directory
import System.Environment
import System.Process (rawSystem)
import Text.HandsomeSoup
import Text.Regex.Posix
import Text.XML.HXT.Core

main :: IO ()
main = do
  galleries <- parseTopPage
  requests <- liftM concat . runPool 5 $ map parseGallery galleries
  sequence_ . map download $ requests

type Url = String

data DownloadRequest = DownloadRequest Url FilePath

parseTopPage :: IO [Url]
parseTopPage = do
  let url = "http://www.himemix.com/"
  doc <- fromUrl url
  candidate <- runX $ doc >>> css ".con02 a" ! "href"
  return . map (url ++) $ filter ("girl/" `isPrefixOf`) candidate

parseGallery :: Url -> IO [DownloadRequest]
parseGallery url = do
  doc <- fromUrl url
  hrefs <- runX $ doc >>> css "a" ! "href"
  let images = map (baseurl url ++) $ filter ("img/" `isPrefixOf`) hrefs
      movies = filter (=~ "(mp4|zip)$") hrefs
  forM (images ++ movies) $ \x ->
    return . DownloadRequest x $ imageFilename name x
    where (_, _, _, name:_) = url =~ "girl/(.+)/" :: (String,String,String,[String])

download :: DownloadRequest -> IO ()
download (DownloadRequest url filepath) = do
  createDirectoryIfMissing True $ dirname filepath
  exist <- doesFileExist filepath
  unless exist $ do
    putStrLn filepath
    void $ rawSystem "curl" ["-#", url, "-o", filepath]

imageFilename :: String -> Url -> FilePath
imageFilename name url = name ++ "/" ++ basename url

dirname :: FilePath -> FilePath
dirname = baseurl

baseurl :: Url -> Url
baseurl = reverse . dropWhile (/= '/') . reverse

basename :: String -> String
basename = reverse . takeWhile (/= '/') . reverse

-- http://stackoverflow.com/questions/9193349/how-do-i-create-a-thread-pool
runPool :: Int -> [IO a] -> IO [a]
runPool n as = do
  (input, output) <- threadPoolIO n id
  forM_ as $ writeChan input
  forM as . const $ readChan output
