{-# LANGUAGE BangPatterns #-}

import Control.Applicative
import Control.Concurrent.Async
import Control.Monad

import Control.DeepSeq

import Data.String.Utils as S
import Data.List (intercalate)
import qualified Data.ByteString.Lazy.Char8 as C
import Network.HTTP.Conduit
import System.Environment
import System.Console.GetOpt
import System.Process
import System.IO
import System.Posix.Terminal (queryTerminal)
import System.Posix.IO (stdOutput)
import Text.HTML.TagSoup

getYoutubeUrls :: String -> IO [String]
getYoutubeUrls url = do
  src <- parseTags <$> simpleHttp url
  let iframes = sections (~== "<iframe>") src
      youtubeUrls = map (C.unpack . fromAttrib (C.pack "src") . head) iframes
  let r = map (S.replace "embed/" "watch?v=") youtubeUrls in deepseq r (return r)

data Flag = PrintOnly | Download
  deriving (Eq, Ord, Show)

options :: [OptDescr Flag]
options =
  [ Option "p" ["print"] (NoArg PrintOnly) "Print the extracted links to stdout"
  , Option "d" ["download"] (NoArg Download) "Download extracted links to with youtube-dl"
  ]

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  args <- getArgs
  let (flags, links, _) = getOpt RequireOrder options args
  youtube <- concat <$> mapConcurrently
             (\x -> do
                 hPutStrLn stderr $ "Processing: " ++ x
                 getYoutubeUrls x)
             links
  if PrintOnly `elem` flags
    then do
      istty <- queryTerminal stdOutput
      putStrLn
        (if istty
          then intercalate "\n" youtube
          else unwords youtube)
    else if Download `elem` flags
         then void $ spawnProcess "youtube-dl" youtube
         else void $ spawnProcess "vlc" youtube
