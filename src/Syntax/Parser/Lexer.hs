{-# LANGUAGE OverloadedStrings #-}

module Syntax.Parser.Lexer (lexer, scan) where

import Syntax.Parser.Type

import Control.Monad.State
import Control.Monad.Except
import Data.Char
import Data.Loc
import Data.Text (Text, pack, cons)
import Language.Lexer.Applicative
import Text.Regex.Applicative

matchWhen :: (s -> Bool) -> a -> RE s a
matchWhen p symbol = msym (\t -> if p t then Just symbol else Nothing)

--------------------------------------------------------------------------------

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
  <|> TokenAdd          <$ "+"
  <|> TokenSub          <$ "-"
  <|> TokenMul          <$ "*"
  <|> TokenDiv          <$ "/"
  <|> TokenComma        <$ ","
  <|> TokenBraceStart   <$ "{"
  <|> TokenBraceEnd     <$ "}"
  <|> TokenSemi         <$ ";"
  <|> TokenArrow        <$ "->"
  <|> TokenTypeOf       <$ ":"
  -- Boolean stuff
  <|> TokenTrue         <$ "True"
  <|> TokenFalse        <$ "False"
  <|> TokenEQ           <$ "=="
  <|> TokenNEQ          <$ "!="
  <|> TokenGT           <$ ">"
  <|> TokenGTE          <$ ">="
  <|> TokenLT           <$ "<"
  <|> TokenLTE          <$ "<="
  <|> TokenIf           <$ "if"
  <|> TokenThen         <$ "then"
  <|> TokenElse         <$ "else"


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
    TsError (LexicalError pos) -> do
      throwError $ Lexical pos

    -- -- boolean stuff
    -- 'True'          { TokenTrue }
    -- 'False'         { TokenFalse }
    -- '=='            { TokenEQ }
    -- '!='            { TokenNEQ }
    -- '>'             { TokenGT }
    -- '>='            { TokenGTE }
    -- '<'             { TokenLT }
    -- '<='            { TokenLTE }
    -- 'if'            { TokenIf }
    -- 'then'          { TokenThen }
    -- 'else'          { TokenElse }

  -- | Expr '==' Expr                      {% locate $ Mul $1 $3 }
  -- | Expr '!=' Expr                      {% locate $ Div $1 $3 }
  -- | Expr '>'  Expr                      {% locate $ Mul $1 $3 }
  -- | Expr '>=' Expr                      {% locate $ Div $1 $3 }
  -- | Expr '<'  Expr                      {% locate $ Mul $1 $3 }
  -- | Expr '<=' Expr                      {% locate $ Div $1 $3 }
