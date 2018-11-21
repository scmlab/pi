{
{-# LANGUAGE OverloadedStrings                  #-}

module Syntax.Parser.AlexHappy.Parser2 where

import Syntax.Parser.AlexHappy.Lexer2
import Syntax.Concrete
import Data.Loc

import Data.Text (Text)

}

%name piParser
%tokentype { Token }
%error { parseError }

%monad { Parser }
%lexer { scan } { TokenEOF }

%token
        label           { TokenLabel $$ }
        namePos         { TokenNamePos $$ }
        nameNeg         { TokenNameNeg $$ }
        int             { TokenInt $$ }
        'stdout'        { TokenStdOut }
        'stdin'         { TokenStdIn }
        'end'           { TokenEnd }
        'nu'            { TokenNu }
        '='             { TokenDefn      }
        '!'             { TokenSend      }
        '?'             { TokenRecv      }
        '.'             { TokenSeq       }
        '|'             { TokenPar       }
        '('             { TokenParenStart }
        ')'             { TokenParenEnd }
        '+'             { TokenPlus }
        '-'             { TokenMinus }
        '{'             { TokenBraceStart }
        '}'             { TokenBraceEnd }
        '<'             { TokenAngleStart }
        '>'             { TokenAngleEnd }
        ','             { TokenComma }
        ';'             { TokenSemi }
        '->'            { TokenArrow }
        ':'             { TokenTypeOf }
        'Int'           { TokenSortInt }
        'Bool'          { TokenSortBool }

%right '|'
%right '.'
%left '+' '-'

%%

Program :: {Program Loc}
    : ProcDecls                             {% locate $ Program (reverse $1) }

-- left recursive
ProcDecls :: {[ProcDecl Loc]}
    : ProcDecl                              { [$1] }
    | ProcDecls ProcDecl                    { $2:$1 }

ProcDecl :: {ProcDecl Loc}
    : SimpName '=' ProcessPar               {% locate $ ProcDecl $1 $3 }

-- left recursive
ProcessPar :: {Process Loc}
    : ProcessPar '|' Process                {% locate $ Par $1 $3 }
    | Process                               { $1 }

Process :: {Process Loc}
    : Name '!' Expr '.' Process               {% locate $ Send $1 $3 $5  }
    | Name '?' '{' Clauses '}'                {% locate $ Recv $1 (reverse $4)  }
    | Name '?' ClauseDot                      {% locate $ Recv $1 [$3]  }
    | 'end'                                   {% locate $ End }
    | '(' 'nu' SimpName ')' Process           {% locate $ Nu $3 Nothing $5 }
    | '(' 'nu' SimpName ':' Type ')' Process  {% locate $ Nu $3 (Just $5) $7 }
    | SimpName                                {% locate $ Call $1 }
    | '(' ProcessPar ')'                      { $2 }

Pattern :: {Pattern Loc}
         : SimpName                         {% locate $ PtrnName $1 }
         | '<' Patterns '>'                 {% locate $ PtrnTuple (reverse $2) }
         | Label                            {% locate $ PtrnLabel $1 }
Patterns :: {[Pattern Loc]}
    : Patterns ',' Pattern                  { $3 : $1 }
    | Pattern                               { [ $1 ]  }

ClauseDot :: {Clause Loc}
        : Pattern '.' Process               {% locate $  Clause $1 $3 }
ClauseArr :: {Clause Loc}
        : Pattern '->' Process              {% locate $  Clause $1 $3 }
Clauses :: {[Clause Loc]}
        : Clauses ';' ClauseArr             { $3 : $1 }
        | ClauseArr                         { [ $1 ]  }

SimpName :: {SimpName Loc}
      : namePos                             {% locate $ SimpName $1 }

Name :: {Name Loc}
      : namePos                             {% locate $ Positive $1 }
      | nameNeg                             {% locate $ Negative $1 }
      | ReservedName                        {% locate $ Reserved $1 }

ReservedName :: {Text}
     : 'stdout'                             { "StdOut" }
     | 'stdin'                              { "StdIn" }

Expr :: {Expr Loc}
      : Expr '+' Expr                       {% locate $ Add $1 $3 }
      | Expr '-' Expr                       {% locate $ Sub $1 $3 }
      | '(' Expr ')'                        { $2 }
      | '<' Exprs '>'                       {% locate $ ExprTuple (reverse $2) }
      | Name                                {% locate $ ExprName $1 }
      | int                                 {% locate $ ExprDigit $1 }
      | Label                               {% locate $ ExprLabel $1 }
Exprs :: {[Expr Loc]}
    : Exprs ',' Expr                        { $3 : $1 }
    | Expr                                  { [ $1 ]  }

Label :: {Label Loc}
    : label                                 {% locate $ Label $1 }

Sort :: {Sort Loc}
    : 'Int'                                 {% locate $ SortInt }
    | 'Bool'                                {% locate $ SortBool }
Type :: {Type Loc}
    : '!' Sort '.' Type                     {% locate $ TypeSend (Left  $2) $4 }
    | '!' Type '.' Type                     {% locate $ TypeSend (Right $2) $4 }
    | '?' Sort '.' Type                     {% locate $ TypeRecv (Left  $2) $4 }
    | '?' Type '.' Type                     {% locate $ TypeRecv (Right $2) $4 }
    | '!' '{' TypeOfLabels '}'              {% locate $ TypeSele (reverse $3)  }
    | '?' '{' TypeOfLabels '}'              {% locate $ TypeChoi (reverse $3)  }
    | SimpName                              {% locate $ TypeCall $1            }
    | 'end'                                 {% locate $ TypeEnd                }

TypeOfLabel :: {TypeOfLabel Loc}
    : Label ':' Type                        {% locate $ TypeOfLabel $1 $3  }

TypeOfLabels :: {[TypeOfLabel Loc]}
    : TypeOfLabels ';' TypeOfLabel          { $3 : $1 }
    | TypeOfLabel                           { [ $1 ]  }

{}
