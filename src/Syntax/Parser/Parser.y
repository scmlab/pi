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
        '>>'            { TokenChoice      }
        '<<'            { TokenSelect      }
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
        -- typing stuff
        '!'             { TokenTypeSend      }
        '?'             { TokenTypeRecv      }
        '0'             { TokenTypeEnd      }
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
    : Definitions                              {% locate $ Program (reverse $1) }

-- left recursive
Definitions :: {[Definition Loc]}
    : Definition                                { [$1] }
    | Definitions Definition                    { $2:$1 }

Definition :: {Definition Loc}
    : ProcName '=' ProcessPar                   {% locate $ ProcDefn $1 $3 }
    | ProcName ':' Type                         {% locate $ TypeSign $1 $3 }

-- left recursive
ProcessPar :: {Process Loc}
    : ProcessPar '|' Process                    {% locate $ Par $1 $3 }
    | Process                                   { $1 }

Process :: {Process Loc}
    : Name '[' Expr ']' '.' Process           {% locate $ Send $1 $3 $6  }
    | Name RecvClause                         {% locate $ Recv $1 [$2]  }
    | Name '>>' '{' ChoiceClauses '}'         {% locate $ Recv $1 (reverse $4)  }
    | Name '<<' SelectLabel '.' Process       {% locate $ Send $1 $3 $5 }
    | 'end'                                   {% locate $ End }
    | '(' 'nu' ProcName ')' Process           {% locate $ Nu $3 Nothing $5 }
    | '(' 'nu' ProcName ':' Type ')' Process  {% locate $ Nu $3 (Just $5) $7 }
    | '*' Process                             {% locate $ Repl $2 }
    | ProcName                                {% locate $ Call $1 }
    | '(' ProcessPar ')'                      { $2 }

Pattern :: {Pattern Loc}
         : ProcName                         {% locate $ PtrnName $1 }
         | '(' Patterns ')'                 {% locate $ PtrnTuple (reverse $2) }
         | Label                            {% locate $ PtrnLabel $1 }
Patterns :: {[Pattern Loc]}
    : Patterns ',' Pattern                  { $3 : $1 }
    | Pattern  ',' Pattern                  { [ $3, $1 ]  }

RecvClause :: {Clause Loc}
        : '(' Pattern ')' '.' ProcessPar    {% locate $  Clause $2 $5 }
ChoiceClause :: {Clause Loc}
        : Pattern '->' ProcessPar           {% locate $  Clause $1 $3 }
ChoiceClauses :: {[Clause Loc]}
        : ChoiceClauses ';' ChoiceClause    { $3 : $1 }
        | ChoiceClause                      { [ $1 ]  }

ProcName :: {ProcName Loc}
      : namePos                             {% locate $ ProcName $1 }

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

SelectLabel :: {Expr Loc}
    : Label                               {% locate $ ExprLabel $1 }

Label :: {Label Loc}
    : label                                 {% locate $ Label $1 }

BaseType :: {BaseType Loc}
    : 'Int'                                 {% locate $ BaseInt }
    | 'Bool'                                {% locate $ BaseBool }

TypeBase :: {Type Loc}
    : BaseType                              {% locate $ TypeBase $1  }

Type :: {Type Loc}
    : '0'                                   {% locate $ TypeEnd }
    | '!' TypeBase '.' Type                 {% locate $ TypeSend $2 $4 }
    | '!' Type '.' Type                     {% locate $ TypeSend $2 $4 }
    | '?' TypeBase '.' Type                 {% locate $ TypeRecv $2 $4 }
    | '?' Type '.' Type                     {% locate $ TypeRecv $2 $4 }
    | '>>' '{' TypeOfLabels '}'             {% locate $ TypeSele (reverse $3)  }
    | '<<' '{' TypeOfLabels '}'             {% locate $ TypeChoi (reverse $3)  }
    | '(' Type ')'                          { $2  }

TypeOfLabel :: {TypeOfLabel Loc}
    : Label ':' Type                        {% locate $ TypeOfLabel $1 $3  }

TypeOfLabels :: {[TypeOfLabel Loc]}
    : TypeOfLabels ';' TypeOfLabel          { $3 : $1 }
    | TypeOfLabel                           { [ $1 ]  }

{}
