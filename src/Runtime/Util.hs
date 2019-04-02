module Runtime.Util where

import System.IO
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Text.Prettyprint.Doc
import Data.Text (Text)


--------------------------------------------------------------------------------
-- | IO control

-- special mode
controlStdin :: IO ()
controlStdin = do
  hSetBuffering stdin NoBuffering
  hSetEcho      stdin False

-- normal mode
restoreStdin :: IO ()
restoreStdin = do
  hSetBuffering stdin LineBuffering
  hSetEcho      stdin True

-- to capture keys that is longer than 1 character
interceptStdin :: String -> IO String
interceptStdin input = reverse <$> interceptStdin' input
  where
    interceptStdin' buffer = do
      char <- getChar
      more <- hReady stdin
      if more
        then interceptStdin' (char:buffer)
        else return          (char:buffer)


--------------------------------------------------------------------------------
-- | Colouring stuff

yellow :: Pretty a => a -> IO ()
yellow = putDoc . annotate (color Yellow) . pretty

blue :: Pretty a => a -> IO ()
blue = putDoc . annotate (colorDull Blue) . pretty

red :: Pretty a => a -> IO ()
red = putDoc . annotate (color Red) . pretty

green :: Pretty a => a -> IO ()
green = putDoc . annotate (color Green) . pretty

text :: Text -> Text
text = id
