module Syntax.Parser
  ( parseByteString
  , parseSyntaxTree
  , SyntaxTree
  , Point(..), Range(..)
  , ParseError(..)
  )
  where

import Data.ByteString.Lazy (ByteString)
import Data.List (isPrefixOf)
import Syntax.Parser.AlexHappy.Base (runAlex)
import Syntax.Parser.AlexHappy.Parser (happyParser)
import Syntax.Abstract (Prog, fromConcrete)
import Syntax.Parser.TreeSitter.SyntaxTree (parse)
import Syntax.Parser.Type

-- | Haskell parser
parseByteString :: ByteString -> Either ParseError Prog
parseByteString s =
  -- Alex's error type is a String, that we have to parse here,
  -- otherwise we cannot get type-safe information out of 'parse'.
  case runAlex s happyParser of
    Right x -> Right (fromConcrete x)
    Left str | showErrPrefix `isPrefixOf` str ->
                  let (line, col, m) =
                          (read (drop (length showErrPrefix) str) :: (Int, Int, Maybe String))
                  in Left (HaskellParseError line col (Syntactical m))
             | lexicalErrorPrefix `isPrefixOf` str ->
                  let info = drop (length lexicalErrorPrefix) str
                      lineStr = takeWhile (/= ',') info
                      columnStr = drop (9 + length lineStr) info
                  in Left (HaskellParseError (read lineStr) (read columnStr) Lexical)
             | otherwise  -> Left (HaskellParseError 0 0 (Message str))
  where
    showErrPrefix       = "show-error: " :: String
    lexicalErrorPrefix  = "lexical error at line " :: String

-- | Tree-sitter parser
parseSyntaxTree :: SyntaxTree -> Either ParseError Prog
parseSyntaxTree tree =
  case parse tree of
    Right program -> Right $ fromConcrete program
    Left  err     -> Left $ err
