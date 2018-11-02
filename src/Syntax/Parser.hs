module Syntax.Parser
  ( parseByteString
  , parseSyntaxTree
  , SyntaxTree
  )
  where

import Data.ByteString.Lazy (ByteString)
import Data.List (isPrefixOf)
import Syntax.Parser.Base (runAlex)
import Syntax.Parser.Parser (happyParser)
import Syntax.Primitive (SyntaxTree)
import Syntax.Concrete (parsePrim)
import Syntax.Abstract (Prog, fromConcrete)
import Syntax.Parser.Error

-- | Haskell parser
parseByteString :: ByteString -> Either ParseError Prog
parseByteString s =
    -- Alex's error type is a String, that we have to parse here,
    -- otherwise we cannot get type-safe information out of 'parse'.
    let showErrPrefix       = "show-error: " :: String
        lexicalErrorPrefix  = "lexical error at line " :: String
    in case runAlex s $ happyParser of
        Right x -> Right x
        Left str | showErrPrefix `isPrefixOf` str ->
                      let (line, column, m) =
                              (read (drop (length showErrPrefix) str) :: (Int, Int, Maybe String))
                      in Left (HaskellParseError line column (Syntactical m))
                 | lexicalErrorPrefix `isPrefixOf` str ->
                      let info = drop (length lexicalErrorPrefix) str
                          lineStr = takeWhile (/= ',') info
                          columnStr = drop (9 + length lineStr) info
                      in Left (HaskellParseError (read lineStr) (read columnStr) Lexical)
                 | otherwise  -> Left (HaskellParseError 0 0 (Message str))

-- | Tree-sitter parser
parseSyntaxTree :: SyntaxTree -> Either ParseError Prog
parseSyntaxTree tree =
    case parsePrim tree of
      Left  err     -> Left $ err
      Right program -> Right $ fromConcrete program
