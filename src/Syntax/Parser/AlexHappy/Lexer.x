{
{-# LANGUAGE OverloadedStrings                  #-}
{-# LANGUAGE NoMonomorphismRestriction          #-}
{-# LANGUAGE CPP                                #-}
{-# OPTIONS_GHC -fno-warn-unused-binds          #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures    #-}
{-# OPTIONS_GHC -fno-warn-unused-matches        #-}
{-# OPTIONS_GHC -fno-warn-unused-imports        #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing        #-}
{-# OPTIONS_GHC -fno-warn-tabs                  #-}
{-# OPTIONS_GHC -funbox-strict-fields           #-}

module Syntax.Parser.AlexHappy.Lexer
    ( Alex(..)
    , AlexPosn(..)
    , AlexState(..)
    , Token(..)
    , TokenClass(..)
    , alexError
    , alexMonadScan
    , runAlex
    , tokenToPosN
    ) where

import System.Exit
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
}

%wrapper "monadUserState-bytestring"

$digit = 0-9                    -- digits
$alpha = [a-zA-Z]               -- alphabetic characters
$capital = [A-Z]               -- capital alphabetic characters

tokens :-

  $white+                               ;
  "--".*                                ;
  end                                   { tok          TokenEnd }
  StdOut                                { tok          TokenStdOut }
  StdIn                                 { tok          TokenStdIn }
  nu                                    { tok          TokenNu }
  True                                  { tok          TokenTrue }
  False                                 { tok          TokenFalse }
  if                                    { tok          TokenIf }
  then                                  { tok          TokenThen }
  else                                  { tok          TokenElse }
  [\=]                                  { tok          TokenDefn }
  [\!]                                  { tok          TokenSend }
  [\?]                                  { tok          TokenRecv }
  [\.]                                  { tok          TokenSeq }
  [\|]                                  { tok          TokenPar }
  [\(]                                  { tok          TokenParenStart }
  [\)]                                  { tok          TokenParenEnd }
  [\+]                                  { tok          TokenPlus }
  [\-]                                  { tok          TokenMinus }
  [\<]                                  { tok          TokenAngleStart }
  [\>]                                  { tok          TokenAngleEnd }
  [\,]                                  { tok          TokenComma }
  [\{]                                  { tok          TokenBraceStart }
  [\}]                                  { tok          TokenBraceEnd }
  [\;]                                  { tok          TokenSemi }
  \-\>                                  { tok          TokenArrow }
  $digit+                               { tok_read     TokenInt }
  $alpha [$alpha $digit \_ \']*         { tok_text     TokenName }
  $capital [$capital $digit \_ \']*     { tok_text     TokenLabel }
{
-- Some action helpers:
tok' f (p, _, input, _) len = return $ Token p (f (B.take (fromIntegral len) input))
tok x = tok' (\s -> x)
tok_string x = tok' (\s -> x (B.unpack s))
tok_text x = tok' (\s -> x (decodeUtf8 $ B.toStrict s))
tok_read x = tok' (\s -> x (read (B.unpack s)))

-- The token type:
data Token = Token AlexPosn TokenClass
  deriving (Show)

tokenToPosN :: Token -> AlexPosn
tokenToPosN (Token p _) = p

data TokenClass
  = TokenDefn
  | TokenEnd
  | TokenSend
  | TokenRecv
  | TokenStdOut
  | TokenStdIn
  | TokenNu
  | TokenLabel Text
  | TokenName Text
  | TokenInt Int
  | TokenSeq
  | TokenPar
  | TokenParenStart | TokenParenEnd
  | TokenPlus | TokenMinus
  | TokenTrue | TokenFalse
  | TokenIf | TokenThen | TokenElse
  | TokenAngleStart | TokenAngleEnd | TokenComma
  | TokenBraceStart | TokenBraceEnd | TokenSemi | TokenArrow
  | TokenEOF
  deriving (Eq, Show)

alexEOF :: Alex Token
alexEOF = do
  (p, _, _, _) <- alexGetInput
  return $ Token p TokenEOF

type AlexUserState = ()
alexInitUserState = ()
}
