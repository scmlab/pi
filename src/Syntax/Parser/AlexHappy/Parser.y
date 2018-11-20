{
{-# LANGUAGE OverloadedStrings                  #-}

module Syntax.Parser.AlexHappy.Parser where

import Syntax.Parser.AlexHappy.Base
import Syntax.Concrete

import Data.Text (Text)
}

%name happyParser
%tokentype { Token }

%monad { Parser } { thenP } { returnP }
%lexer { lexer } { Token _ TokenEOF }

%token
        label           { Token _ (TokenLabel $$) }
        namePos         { Token _ (TokenNamePos $$) }
        nameNeg         { Token _ (TokenNameNeg $$) }
        int             { Token _ (TokenInt $$) }
        'stdout'        { Token _ TokenStdOut }
        'stdin'         { Token _ TokenStdIn }
        'end'           { Token _ TokenEnd }
        'nu'            { Token _ TokenNu }
        '='             { Token _ TokenDefn      }
        '!'             { Token _ TokenSend      }
        '?'             { Token _ TokenRecv      }
        '.'             { Token _ TokenSeq       }
        '|'             { Token _ TokenPar       }
        '('             { Token _ TokenParenStart }
        ')'             { Token _ TokenParenEnd }
        '+'             { Token _ TokenPlus }
        '-'             { Token _ TokenMinus }
        '{'             { Token _ TokenBraceStart }
        '}'             { Token _ TokenBraceEnd }
        '<'             { Token _ TokenAngleStart }
        '>'             { Token _ TokenAngleEnd }
        ','             { Token _ TokenComma }
        ';'             { Token _ TokenSemi }
        '->'            { Token _ TokenArrow }
        ':'             { Token _ TokenTypeOf }
        'Int'           { Token _ TokenSortInt }
        'Bool'          { Token _ TokenSortBool }

%right '|'
%right '.'
%left '+' '-'

%%

Program :: {Program Token}
    : ProcDecls                             {%^ return . Program (reverse $1) }

-- left recursive
ProcDecls :: {[ProcDecl Token]}
    : ProcDecl                              { [$1] }
    | ProcDecls ProcDecl                    { $2:$1 }

ProcDecl :: {ProcDecl Token}
    : SimpName '=' ProcessPar               {%^ return . ProcDecl $1 $3 }

-- left recursive
ProcessPar :: {Process Token}
    : ProcessPar '|' Process                {%^ return . Par $1 $3 }
    | Process                               { $1 }

Process :: {Process Token}
    : Name '!' Expr '.' Process               {%^ return . Send $1 $3 $5  }
    | Name '?' '{' Clauses '}'                {%^ return . Recv $1 (reverse $4)  }
    | Name '?' ClauseDot                      {%^ return . Recv $1 [$3]  }
    | 'end'                                   {%^ return . End }
    | '(' 'nu' SimpName ')' Process           {%^ return . Nu $3 Nothing $5 }
    | '(' 'nu' SimpName ':' Type ')' Process  {%^ return . Nu $3 (Just $5) $7 }
    | SimpName                                {%^ return . Call $1 }
    | '(' ProcessPar ')'                      { $2 }

Pattern :: {Pattern Token}
         : SimpName                         {%^ return . PtrnName $1 }
         | '<' Patterns '>'                 {%^ return . PtrnTuple (reverse $2) }
         | Label                            {%^ return . PtrnLabel $1 }
Patterns :: {[Pattern Token]}
    : Patterns ',' Pattern                  { $3 : $1 }
    | Pattern                               { [ $1 ]  }

ClauseDot :: {Clause Token}
        : Pattern '.' Process               {%^ return .  Clause $1 $3 }
ClauseArr :: {Clause Token}
        : Pattern '->' Process              {%^ return .  Clause $1 $3 }
Clauses :: {[Clause Token]}
        : Clauses ';' ClauseArr             { $3 : $1 }
        | ClauseArr                         { [ $1 ]  }

SimpName :: {SimpName Token}
      : namePos                             {%^ return . SimpName $1 }

Name :: {Name Token}
      : namePos                             {%^ return . Positive $1 }
      | nameNeg                             {%^ return . Negative $1 }
      | ReservedName                        {%^ return . Reserved $1 }

ReservedName :: {Text}
     : 'stdout'                             { "StdOut" }
     | 'stdin'                              { "StdIn" }

Expr :: {Expr Token}
      : Expr '+' Expr                       {%^ return . Add $1 $3 }
      | Expr '-' Expr                       {%^ return . Sub $1 $3 }
      | '(' Expr ')'                        { $2 }
      | '<' Exprs '>'                       {%^ return . ExprTuple (reverse $2) }
      | Name                                {%^ return . ExprName $1 }
      | int                                 {%^ return . ExprDigit $1 }
      | Label                               {%^ return . ExprLabel $1 }
Exprs :: {[Expr Token]}
    : Exprs ',' Expr                        { $3 : $1 }
    | Expr                                  { [ $1 ]  }

Label :: {Label Token}
    : label                                 {%^ return . Label $1 }

Sort :: {Sort Token}
    : 'Int'                                 {%^ return . SortInt }
    | 'Bool'                                {%^ return . SortBool }
Type :: {Type Token}
    : '!' Sort '.' Type                     {%^ return . TypeSend (Left  $2) $4 }
    | '!' Type '.' Type                     {%^ return . TypeSend (Right $2) $4 }
    | '?' Sort '.' Type                     {%^ return . TypeRecv (Left  $2) $4 }
    | '?' Type '.' Type                     {%^ return . TypeRecv (Right $2) $4 }
    | '!' '{' TypeOfLabels '}'              {%^ return . TypeSele (reverse $3)  }
    | '?' '{' TypeOfLabels '}'              {%^ return . TypeChoi (reverse $3)  }
    | SimpName                              {%^ return . TypeCall $1            }
    | 'end'                                 {%^ return . TypeEnd                }

TypeOfLabel :: {TypeOfLabel Token}
    : Label ':' Type                        {%^ return . TypeOfLabel $1 $3  }

TypeOfLabels :: {[TypeOfLabel Token]}
    : TypeOfLabels ';' TypeOfLabel          { $3 : $1 }
    | TypeOfLabel                           { [ $1 ]  }

{}
