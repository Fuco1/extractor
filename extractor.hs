import Control.Applicative
import Control.Monad

import Data.String.Utils as S
import Data.List (intercalate)
import qualified Data.ByteString.Lazy.Char8 as C
import Network.HTTP.Conduit
import System.Environment
import System.Console.GetOpt
import System.Process
import Text.HTML.TagSoup

getYoutubeUrls :: String -> IO [String]
getYoutubeUrls url = do
  src <- parseTags <$> simpleHttp url
  let iframes = sections (~== "<iframe>") src
      youtubeUrls = map (C.unpack . fromAttrib (C.pack "src") . head) iframes
  return $ map (S.replace "embed/" "watch?v=") youtubeUrls

data Flag = PrintOnly
  deriving (Eq, Ord, Show)

options :: [OptDescr Flag]
options =
  [ Option "p" ["print"] (NoArg PrintOnly) "Print the extracted links to stdout"
  ]

main :: IO ()
main = do
  args <- getArgs
  let (flags, links, _) = getOpt RequireOrder options args
  youtube <- concat <$> mapM
             (\x -> do
                 putStrLn $ "Processing: " ++ x
                 getYoutubeUrls x)
             links
  if PrintOnly `elem` flags
    then putStrLn $ intercalate "\n" youtube
    else void $ spawnProcess "vlc" youtube
