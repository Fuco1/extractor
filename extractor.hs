import Control.Applicative
import Control.Monad

import Data.String.Utils as S
import qualified Data.ByteString.Lazy.Char8 as C
import Network.HTTP.Conduit
import System.Environment
import System.Process
import Text.HTML.TagSoup

getYoutubeUrls :: String -> IO [String]
getYoutubeUrls url = do
  src <- parseTags <$> simpleHttp url
  let iframes = sections (~== "<iframe>") src
      youtubeUrls = map (C.unpack . fromAttrib (C.pack "src") . head) iframes
  return $ map (S.replace "embed/" "watch?v=") youtubeUrls

main :: IO ()
main = do
  args <- getArgs
  youtube <- concat <$> mapM
             (\x -> do
                 putStrLn $ "Processing: " ++ x
                 getYoutubeUrls x)
             args
  void $ spawnProcess "vlc" youtube
