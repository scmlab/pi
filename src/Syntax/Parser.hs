module Syntax.Parser
  ( parseProgram
  , parseProcess
  , parseByteString2
  , ParseError(..)
  , printParseError
  )
  where

import qualified Syntax.Abstract as A
import Syntax.Abstract (Pi, fromConcrete)
import qualified Syntax.Concrete as C
import Syntax.Parser.Parser (programParser, processParser)
import Syntax.Parser.Lexer (lexer)
import Syntax.Parser.Type

import Control.Monad.Except
import Control.Monad.State
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Loc
import PPrint
import Language.Lexer.Applicative
import System.Console.ANSI

parseProgram :: FilePath -> ByteString -> Either ParseError A.Program
parseProgram filePath src = fromConcrete <$> runExcept (evalStateT programParser initState)
  where initState = ParserState startingLoc startingLoc (runLexer lexer filePath (BS.unpack src))
        startingLoc = Loc (startPos filePath) (startPos filePath)

parseProcess :: ByteString -> Either ParseError Pi
parseProcess src = fromConcrete <$> runExcept (evalStateT processParser initState)
  where filePath = ""
        initState = ParserState startingLoc startingLoc (runLexer lexer filePath (BS.unpack src))
        startingLoc = Loc (startPos filePath) (startPos filePath)

parseByteString2 :: FilePath -> ByteString -> Either ParseError (C.Program Loc)
parseByteString2 filePath src = runExcept (evalStateT programParser initState)
  where initState = ParserState startingLoc startingLoc (runLexer lexer filePath (BS.unpack src))
        startingLoc = Loc (startPos filePath) (startPos filePath)

printParseError :: ParseError -> Maybe ByteString -> IO ()
printParseError _ Nothing = error "panic: no source file to print parse errors"
printParseError (Lexical pos)        (Just source) = do
  setSGR [SetColor Foreground Vivid Red]
  putStr "\n  Lexical parse error\n  "
  setSGR [SetColor Foreground Dull Blue]
  putStrLn $ displayPos pos
  setSGR []
  printSourceCode $ SourceCode (BS.unpack source) (Loc pos pos) 2

printParseError (Syntatical loc _) (Just source) = do
  setSGR [SetColor Foreground Vivid Red]
  putStr "\n  Syntatical parse error\n  "
  setSGR [SetColor Foreground Dull Blue]
  putStrLn $ displayLoc loc
  setSGR []
  printSourceCode $ SourceCode (BS.unpack source) loc 2
