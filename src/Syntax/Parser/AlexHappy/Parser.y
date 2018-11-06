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
        name            { Token _ (TokenName $$) }
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
        ';'             { Token _ TokenSemi }
        '->'            { Token _ TokenArrow }

%right '.'
%left '+' '-'

%%

Program :: {Program Token}
    : ProcDecls                             {%^ return . Program $1 }

ProcDecls :: {[ProcDecl Token]}
    : ProcDecl                              { [$1] }
    | ProcDecl ProcDecls                    { $1:$2 }

ProcDecl :: {ProcDecl Token}
    : Name '=' ParalleledProcess            {%^ return . ProcDecl $1 $3 }

ParalleledProcess :: {Process Token}
    : ParalleledProcess '|' Process         {%^ return . Par $1 $3 }
    | Process '|' Process                   {%^ return . Par $1 $3 }


Process :: {Process Token}
    : Name '!' Expr '.' Process             {%^ return . Send $1 $3 $5  }
    | Name '?' '{' Clauses '}'              {%^ return . Recv $1 (reverse $4)  }
    | Name '?' ClauseDot                    {%^ return . Recv $1 [$3]  }
    | 'end'                                 {%^ return . End }
    | '(' 'nu' Name ')' Process             {%^ return . Nu $3 $5 }
    | Name                                  {%^ return . Call $1 }

Pattern :: {Pattern Token}
         : Name                             {%^ return . PtrnName $1 }
         | Label                            {%^ return . PtrnLabel $1 }

ClauseDot :: {Clause Token}
        : Pattern '.' Process               {%^ return .  Clause $1 $3 }
ClauseArr :: {Clause Token}
        : Pattern '->' Process              {%^ return .  Clause $1 $3 }
Clauses :: {[Clause Token]}
        : Clauses ';' ClauseArr             { $3 : $1 }
        | ClauseArr                         { [ $1 ] }

Name :: {Name Token}
      : name                                {%^ return . Name $1 }
      | ReservedName                        {%^ return . Reserved $1 }

ReservedName :: {Text}
     : 'stdout'                             { "StdOut" }
     | 'stdin'                              { "StdIn" }

Expr :: {Expr Token}
      : Expr '+' Expr                       {%^ return . Add $1 $3 }
      | Expr '-' Expr                       {%^ return . Sub $1 $3 }
      | '(' Expr ')'                        { $2 }
      | Name                                {%^ return . ExprName $1 }
      | int                                 {%^ return . ExprDigit $1 }
      | Label                               {%^ return . ExprLabel $1 }

Label :: {Label Token}
    : label                                 {%^ return . Label $1 }

{}
