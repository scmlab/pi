{-# LANGUAGE OverloadedStrings #-}

module Syntax.Parser.AlexHappy.Lexer2 where

import Language.Lexer.Applicative
import Text.Regex.Applicative
import Control.Monad.State
import Control.Monad.Except
import Data.Char
import Data.Loc
import Data.Text (Text, pack, cons)

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

matchWhen :: (s -> Bool) -> a -> RE s a
matchWhen p symbol = msym (\t -> if p t then Just symbol else Nothing)

--------------------------------------------------------------------------------

  -- $white+                               ;
  -- "--".*                                ;
  -- end                                   { tok          TokenEnd }
  -- StdOut                                { tok          TokenStdOut }
  -- StdIn                                 { tok          TokenStdIn }
  -- nu                                    { tok          TokenNu }
  -- True                                  { tok          TokenTrue }
  -- False                                 { tok          TokenFalse }
  -- if                                    { tok          TokenIf }
  -- then                                  { tok          TokenThen }
  -- else                                  { tok          TokenElse }
  -- Int                                   { tok          TokenSortInt }
  -- Bool                                  { tok          TokenSortBool }
  -- [\=]                                  { tok          TokenDefn }
  -- [\!]                                  { tok          TokenSend }
  -- [\?]                                  { tok          TokenRecv }
  -- [\.]                                  { tok          TokenSeq }
  -- [\|]                                  { tok          TokenPar }
  -- [\(]                                  { tok          TokenParenStart }
  -- [\)]                                  { tok          TokenParenEnd }
  -- [\+]                                  { tok          TokenPlus }
  -- [\-]                                  { tok          TokenMinus }
  -- [\<]                                  { tok          TokenAngleStart }
  -- [\>]                                  { tok          TokenAngleEnd }
  -- [\,]                                  { tok          TokenComma }
  -- [\{]                                  { tok          TokenBraceStart }
  -- [\}]                                  { tok          TokenBraceEnd }
  -- [\;]                                  { tok          TokenSemi }
  -- \-\>                                  { tok          TokenArrow }
  -- [\:]                                  { tok          TokenTypeOf }
  -- $digit+                               { tok_read     TokenInt }

tokenRE :: RE Char Token
tokenRE =
      TokenDefn         <$ "="
  <|> TokenEnd          <$ "end"
  <|> TokenSend         <$ "!"
  <|> TokenRecv         <$ "?"
  <|> TokenNu           <$ "nu"
  <|> TokenSeq          <$ "."
  <|> TokenPar          <$ "|"
  <|> TokenStdIn        <$ "stdin"
  <|> TokenStdOut       <$ "stdout"
  <|> TokenLabel        <$> labelRE
  <|> TokenNamePos      <$> namePosRE
  <|> TokenNameNeg      <$> nameNegRE
  <|> TokenInt          <$> intRE
  <|> TokenSortInt      <$ "Int"
  <|> TokenSortBool     <$ "Bool"
  <|> TokenParenStart   <$ "("
  <|> TokenParenEnd     <$ ")"
  <|> TokenPlus         <$ "+"
  <|> TokenMinus        <$ "-"
  <|> TokenAngleStart   <$ "<"
  <|> TokenAngleEnd     <$ "<"
  <|> TokenComma        <$ ","
  <|> TokenBraceStart   <$ "{"
  <|> TokenBraceEnd     <$ "}"
  <|> TokenSemi         <$ ";"
  <|> TokenArrow        <$ "->"
  <|> TokenTypeOf       <$ ":"

namePosRE :: RE Char Text
namePosRE = fmap pack $ (:) <$> psym isLower <*> many (psym (\c -> isAlphaNum c || c == '_' || c == '\''))

nameNegRE :: RE Char Text
nameNegRE = cons <$> psym (== '`') <*> namePosRE

labelRE :: RE Char Text
labelRE = fmap pack $ (:) <$> psym isUpper <*> many (psym (\c -> isUpper c || isDigit c || c == '_' || c == '\''))

intRE :: RE Char Int
intRE = read <$> some (psym isDigit)

whitespaceRE :: RE Char Token
whitespaceRE = matchWhen isSpace TokenWhitespace

commentStartRE :: RE Char String
commentStartRE = string "--"

commentEndRE :: String -> RE Char Token
commentEndRE pref = TokenComment <$> fmap pack ((++) <$> pure pref <*> ((++) <$> many anySym <*> string "\n"))

lexer :: Lexer Token
lexer = mconcat
  [ token       (longest tokenRE)
  , whitespace  (longest whitespaceRE)
  , whitespace  (longestShortest commentStartRE commentEndRE)
  ]

--------------------------------------------------------------------------------

data ParseError2 = Lexical2 LexicalError | Syntatical2 Token
  deriving (Show)

data ParserState = ParserState
  { currentLoc :: Loc
  , lookaheadLoc :: Loc
  , tokenStream :: TokenStream (L Token)
  } deriving (Show)

type Parser = StateT ParserState (Except ParseError2)

parseError :: Token -> Parser a
parseError = throwError . Syntatical2

scan :: (Token -> Parser a) -> Parser a
scan f = scanNext >>= f

scanNext :: Parser Token
scanNext = do
  result <- gets tokenStream
  oldLoc <- gets lookaheadLoc
  case result of
    TsToken (L newLoc tok) stream -> do
      put $ ParserState oldLoc newLoc stream
      return tok
    TsEof -> do
      modify $ \st -> st { currentLoc = oldLoc , lookaheadLoc = NoLoc}
      return TokenEOF
    TsError err -> throwError $ Lexical2 err

locate :: (Loc -> a) -> Parser a
locate f = f <$> gets currentLoc
