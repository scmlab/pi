{
{-# LANGUAGE OverloadedStrings                  #-}

module Syntax.Parser.Parser where

import Syntax.Parser.Lexer
import Syntax.Parser.Type
import Syntax.Concrete
import Data.Loc
import Prelude hiding (GT, LT, EQ)

import Data.Text (Text)

}

%name programParser Program
%name processParser ProcessPar
%tokentype { Token }
%error { syntaticalError }

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
        '*'             { TokenStar      }
        '|'             { TokenPar       }
        '('             { TokenParenStart }
        ')'             { TokenParenEnd }
        '+'             { TokenAdd }
        '-'             { TokenSub }
        '/'             { TokenDiv }
        '{'             { TokenBraceStart }
        '}'             { TokenBraceEnd }
        '['             { TokenBracketStart }
        ']'             { TokenBracketEnd }
        ','             { TokenComma }
        ';'             { TokenSemi }
        '->'            { TokenArrow }
        ':'             { TokenTypeOf }
        'Int'           { TokenSortInt }
        string          { TokenString $$ }
        -- boolean stuff
        'Bool'          { TokenSortBool }
        'True'          { TokenTrue }
        'False'         { TokenFalse }
        '=='            { TokenEQ }
        '!='            { TokenNEQ }
        '>'             { TokenGT }
        '>='            { TokenGTE }
        '<'             { TokenLT }
        '<='            { TokenLTE }
        'if'            { TokenIf }
        'then'          { TokenThen }
        'else'          { TokenElse }


%right '|'
%right 'else'
%right '.'
%left '+' '-'
%left '*' '/'
%left '==' '!='
%left '>' '>='
%left '<' '<='

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
    : Name '[' Expr ']' '.' Process           {% locate $ Send $1 $3 $6  }
    | Name '?' '{' Clauses '}'                {% locate $ Recv $1 (reverse $4)  }
    | Name '?' ClauseDot                      {% locate $ Recv $1 [$3]  }
    | 'end'                                   {% locate $ End }
    | '(' 'nu' SimpName ')' Process           {% locate $ Nu $3 Nothing $5 }
    | '(' 'nu' SimpName ':' Type ')' Process  {% locate $ Nu $3 (Just $5) $7 }
    | '*' Process                             {% locate $ Repl $2 }
    | SimpName                                {% locate $ Call $1 }
    | '(' ProcessPar ')'                      { $2 }

Pattern :: {Pattern Loc}
         : SimpName                         {% locate $ PtrnName $1 }
         | '(' Patterns ')'                 {% locate $ PtrnTuple (reverse $2) }
         | Label                            {% locate $ PtrnLabel $1 }
Patterns :: {[Pattern Loc]}
    : Patterns ',' Pattern                  { $3 : $1 }
    | Pattern  ',' Pattern                  { [ $3, $1 ]  }

ClauseDot :: {Clause Loc}
        : Pattern '.' ProcessPar            {% locate $  Clause $1 $3 }
ClauseArr :: {Clause Loc}
        : Pattern '->' ProcessPar           {% locate $  Clause $1 $3 }
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
     : 'stdout'                             { "stdout" }
     | 'stdin'                              { "stdin" }

Expr :: {Expr Loc}
    : Expr '+' Expr                       {% locate $ Add $1 $3 }
    | Expr '-' Expr                       {% locate $ Sub $1 $3 }
    | Expr '*' Expr                       {% locate $ Mul $1 $3 }
    | Expr '/' Expr                       {% locate $ Div $1 $3 }
    | 'if' Expr 'then' Expr 'else' Expr   {% locate $ IfThenElse $2 $4 $6 }
    | '(' Exprs ')'                       {% locate $ ExprTuple (reverse $2) }
    | Term                                { $1 }
Exprs :: {[Expr Loc]}
    : Exprs ',' Expr                        { $3 : $1 }
    | Expr  ',' Expr                        { [ $3 , $1 ]  }

Term :: {Expr Loc}
    : '(' Expr ')'                        { $2 }
    | Name                                {% locate $ ExprName $1 }
    | int                                 {% locate $ ExprDigit $1 }
    | Label                               {% locate $ ExprLabel $1 }
    | string                              {% locate $ ExprString $1 }
    | Expr '==' Expr                      {% locate $ EQ  $1 $3 }
    | Expr '!=' Expr                      {% locate $ NEQ $1 $3 }
    | Expr '>'  Expr                      {% locate $ GT  $1 $3 }
    | Expr '>=' Expr                      {% locate $ GTE $1 $3 }
    | Expr '<'  Expr                      {% locate $ LT  $1 $3 }
    | Expr '<=' Expr                      {% locate $ LTE $1 $3 }
    | Boolean                             { $1 }

Boolean :: {Expr Loc}
    : 'True'                              {% locate $ ExprBool True }
    | 'False'                             {% locate $ ExprBool False }
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
