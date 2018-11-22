module Syntax.Parser
  ( parseByteString
  , parseByteString2
  , ParseError(..)
  , printParseError
  )
  where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (unpack)
import Syntax.Abstract (Prog, fromConcrete)
import Syntax.Concrete (Program)

import Syntax.Parser.Parser (piParser)
import Syntax.Parser.Lexer (lexer)
import Syntax.Parser.Type
import Language.Lexer.Applicative
import Control.Monad.Except
import Control.Monad.State
import Data.Loc

parseByteString :: FilePath -> ByteString -> Either ParseError Prog
parseByteString filePath src = fromConcrete <$> runExcept (evalStateT piParser initState)
  where initState = ParserState startingLoc startingLoc (runLexer lexer filePath (unpack src))
        startingLoc = Loc (startPos filePath) (startPos filePath)

parseByteString2 :: FilePath -> ByteString -> Either ParseError (Program Loc)
parseByteString2 filePath src = runExcept (evalStateT piParser initState)
  where initState = ParserState startingLoc startingLoc (runLexer lexer filePath (unpack src))
        startingLoc = Loc (startPos filePath) (startPos filePath)

printParseError :: ParseError -> Maybe ByteString -> IO ()
printParseError _ Nothing = error "panic: no source file to print parse errors"
printParseError (Lexical pos)        (Just source) = do
  putStrLn (show pos)
printParseError (Syntatical loc tok) (Just source) = putStrLn (show loc)
