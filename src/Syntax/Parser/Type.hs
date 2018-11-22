module Syntax.Parser.Type where


import Control.Monad.State
import Control.Monad.Except
import Data.Loc
import Data.Text (Text)

import Language.Lexer.Applicative

--------------------------------------------------------------------------------
-- | Tokens

data Token
  = TokenDefn
  | TokenEnd
  | TokenSend
  | TokenRecv
  | TokenNu
  | TokenSeq
  | TokenPar
  | TokenStdIn
  | TokenStdOut
  | TokenLabel Text
  | TokenNamePos Text
  | TokenNameNeg Text
  | TokenInt Int
  | TokenSortInt
  | TokenSortBool
  | TokenParenStart | TokenParenEnd
  | TokenPlus | TokenMinus
  -- | TokenTrue | TokenFalse
  -- | TokenIf | TokenThen | TokenElse
  | TokenAngleStart | TokenAngleEnd | TokenComma
  | TokenBraceStart | TokenBraceEnd | TokenSemi | TokenArrow
  | TokenTypeOf
  -- whitespace, comments amd EOF
  | TokenComment Text
  | TokenWhitespace
  | TokenEOF
  deriving (Eq, Show)


--------------------------------------------------------------------------------
-- | ParseError and stuff

data ParseError = Lexical Pos | Syntatical Loc Token
  deriving (Show)

data ParserState = ParserState
  { currentLoc :: Loc
  , lookaheadLoc :: Loc
  , tokenStream :: TokenStream (L Token)
  } deriving (Show)

type Parser = StateT ParserState (Except ParseError)

syntaticalError :: Token -> Parser a
syntaticalError tok = do
  loc <- gets lookaheadLoc
  throwError $ Syntatical loc tok

locate :: (Loc -> a) -> Parser a
locate f = f <$> gets currentLoc
