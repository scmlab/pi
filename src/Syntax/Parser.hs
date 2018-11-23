module Syntax.Parser
  ( parseByteString
  , parseByteString2
  , ParseError(..)
  , printParseError
  )
  where

import Syntax.Abstract (Prog, fromConcrete)
import Syntax.Concrete (Program)
import Syntax.Parser.Parser (piParser)
import Syntax.Parser.Lexer (lexer)
import Syntax.Parser.Type

import Control.Monad.Except
import Control.Monad.State
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Loc
import Data.Text.Prettyprint.Doc
import PPrint
import Language.Lexer.Applicative
import System.Console.ANSI


parseByteString :: FilePath -> ByteString -> Either ParseError Prog
parseByteString filePath src = fromConcrete <$> runExcept (evalStateT piParser initState)
  where initState = ParserState startingLoc startingLoc (runLexer lexer filePath (BS.unpack src))
        startingLoc = Loc (startPos filePath) (startPos filePath)

parseByteString2 :: FilePath -> ByteString -> Either ParseError (Program Loc)
parseByteString2 filePath src = runExcept (evalStateT piParser initState)
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

printParseError (Syntatical loc tok) (Just source) = do
  setSGR [SetColor Foreground Vivid Red]
  putStr "\n  Syntatical parse error\n  "
  setSGR [SetColor Foreground Dull Blue]
  putStrLn $ displayLoc loc
  setSGR []
  printSourceCode $ SourceCode (BS.unpack source) loc 2
