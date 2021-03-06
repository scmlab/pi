module Syntax.Parser
  ( parseProgram
  , parseProc
  , parseByteString2
  , ParseError(..)
  )
  where

import Syntax.Concrete (Program, Proc)
import Syntax.Parser.Parser (programParser, processParser)
import Syntax.Parser.Lexer (lexer)
import Syntax.Parser.Type

import Control.Monad.Except
import Control.Monad.State
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Loc
import Language.Lexer.Applicative

parseProgram :: FilePath -> ByteString -> Either ParseError Program
parseProgram filePath src = runExcept (evalStateT programParser initState)
  where initState = ParserState startingLoc startingLoc (runLexer lexer filePath (BS.unpack src))
        startingLoc = Loc (startPos filePath) (startPos filePath)

parseProc :: ByteString -> Either ParseError Proc
parseProc src = runExcept (evalStateT processParser initState)
  where filePath = ""
        initState = ParserState startingLoc startingLoc (runLexer lexer filePath (BS.unpack src))
        startingLoc = Loc (startPos filePath) (startPos filePath)

parseByteString2 :: FilePath -> ByteString -> Either ParseError Program
parseByteString2 filePath src = runExcept (evalStateT programParser initState)
  where initState = ParserState startingLoc startingLoc (runLexer lexer filePath (BS.unpack src))
        startingLoc = Loc (startPos filePath) (startPos filePath)
--
-- printParseError :: ParseError -> Maybe ByteString -> IO ()
-- printParseError _ Nothing = error "panic: no source file to print parse errors"
-- printParseError (Lexical pos)        (Just source) = do
--   setSGR [SetColor Foreground Vivid Red]
--   putStr "\n  Lexical parse error\n  "
--   setSGR [SetColor Foreground Dull Blue]
--   putStrLn $ displayPos pos
--   setSGR []
--   printSourceCode $ SourceCode (BS.unpack source) (Loc pos pos) 2
--
-- printParseError (Syntatical loc _) (Just source) = do
--   setSGR [SetColor Foreground Vivid Red]
--   putStr "\n  Syntatical parse error\n  "
--   setSGR [SetColor Foreground Dull Blue]
--   putStrLn $ displayLoc loc
--   setSGR []
--   printSourceCode $ SourceCode (BS.unpack source) loc 2
