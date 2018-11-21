module Syntax.Parser
  ( parseByteString
  -- , parseSyntaxTree
  -- , SyntaxTree
  -- , Point(..), Range(..)
  , ParseError(..)
  )
  where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (unpack)
import Syntax.Abstract (Prog, fromConcrete)

import Syntax.Parser.AlexHappy.Parser (piParser)
import Syntax.Parser.AlexHappy.Lexer (lexer)
import Syntax.Parser.AlexHappy.Type
import Language.Lexer.Applicative
import Control.Monad.Except
import Control.Monad.State
import Data.Loc

parseByteString :: FilePath -> ByteString -> Either ParseError Prog
parseByteString filePath src = fromConcrete <$> runExcept (evalStateT piParser initState)
  where initState = ParserState startingLoc startingLoc (runLexer lexer filePath (unpack src))
        startingLoc = Loc (startPos filePath) (startPos filePath)
