module Parser
  ( parse
  , Error(..)
  , ErrClass(..)
  )
  where

import Data.ByteString.Lazy (ByteString)
import Data.List (isPrefixOf)
import Parser.Base (runAlex)
import Parser.Parser (happyParser)
import Syntax.Abstract (Pi)

data ErrClass
    = Syntactical (Maybe String)
    | Lexical
    | Message String
    deriving (Show, Eq)

data Error = Error
    { errLine  :: Int
    , errPos   :: Int
    , errClass :: ErrClass
    } deriving (Show, Eq)

parse :: ByteString -> Either Error Pi
parse s =
    -- Alex's error type is a String, that we have to parse here,
    -- otherwise we cannot get type-safe information out of 'parse'.
    let showErrPrefix       = "show-error: " :: String
        lexicalErrorPrefix  = "lexical error at line " :: String
    in case runAlex s $ happyParser of
        Right x -> Right x
        Left str | showErrPrefix `isPrefixOf` str ->
                      let (line, column, m) =
                              (read (drop (length showErrPrefix) str) :: (Int, Int, Maybe String))
                      in Left (Error line column (Syntactical m))
                 | lexicalErrorPrefix `isPrefixOf` str ->
                      let info = drop (length lexicalErrorPrefix) str
                          lineStr = takeWhile (/= ',') info
                          columnStr = drop (9 + length lineStr) info
                      in Left (Error (read lineStr) (read columnStr) Lexical)
                 | otherwise  -> Left (Error 0 0 (Message str))
