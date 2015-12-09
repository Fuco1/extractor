import Control.Applicative
import Control.Monad

import qualified Data.ByteString.Lazy.Char8 as C
import Network.HTTP.Conduit
import System.Environment
import System.Process
import Text.HTML.TagSoup

process url = do
  src <- parseTags <$> simpleHttp url
  let iframes = sections (~== "<iframe>") src
      youtubeUrls = map (C.unpack . fromAttrib (C.pack "src") . head) iframes
  videoData <- lines <$> readProcess "youtube-dl" (["-q", "-g"] ++ youtubeUrls) ""
  void $ spawnProcess "vlc" videoData

main :: IO ()
main = getArgs >>= process . head
