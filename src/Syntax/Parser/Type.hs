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
  | TokenChoice
  | TokenSelect
  | TokenNu
  | TokenSeq
  | TokenPar
  | TokenStar
  | TokenStdIn
  | TokenStdOut
  | TokenLabel Text
  | TokenString Text
  | TokenNamePos Text
  | TokenNameNeg Text
  | TokenInt Int
  | TokenSortInt
  | TokenSortBool
  | TokenDoubleQuote
  | TokenParenStart | TokenParenEnd
  | TokenBracketStart | TokenBracketEnd
  | TokenAdd | TokenSub | TokenDiv
  | TokenComma
  | TokenBraceStart | TokenBraceEnd | TokenSemi | TokenArrow

  -- typing stuff
  | TokenTypeOf
  | TokenType
  | TokenTypeSend
  | TokenTypeRecv
  | TokenTypeChoi
  | TokenTypeSele
  | TokenTypeEnd
  | TokenTypeUn
  | TokenTypeMu
  | TokenTypeName Text

  -- boolean stuff
  | TokenTrue
  | TokenFalse
  | TokenEQ
  | TokenNEQ
  | TokenGT
  | TokenGTE
  | TokenLT
  | TokenLTE
  | TokenIf
  | TokenThen
  | TokenElse
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
