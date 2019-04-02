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
%name processParser ProcPar
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
        'type'          { TokenType      }
        '!'             { TokenTypeSend      }
        '?'             { TokenTypeRecv      }
        '&'             { TokenTypeChoi      }
        '#'             { TokenTypeSele      }
        '0'             { TokenTypeEnd      }
        'un'            { TokenTypeUn      }
        'mu'            { TokenTypeMu      }
        typeName        { TokenTypeName $$ }

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
%left ','
%left '+' '-'
%left '*' '/'
%left '==' '!='
%left '>' '>='
%left '<' '<='

%%

Program :: {Program}
    : Definitions                              {% locate $ Program (reverse $1) }

-- left recursive
Definitions :: {[Definition]}
    : Definition                                { [$1] }
    | Definitions Definition                    { $2:$1 }

Definition :: {Definition}
    : ProcName '=' ProcPar                      {% locate $ ProcDefn $1 $3 }
    | Name ':' Type                             {% locate $ ChanType $1 $3 }
    | 'type' TypeName '=' Type                  {% locate $ TypeDefn $2 $4 }

-- left recursive
ProcPar :: {Proc}
    : ProcPar '|' Proc                          {% locate $ Par $1 $3 }
    | Proc                                      { $1 }

Proc :: {Proc}
    : Name '[' Expr ']' '.' Proc                {% locate $ Send $1 $3 $6  }
    | Name RecvClause                           {% locate $ Recv $1 [$2]  }
    | Name '>>' '{' ChoiceClauses '}'           {% locate $ Recv $1 (reverse $4)  }
    | Name '<<' SelectLabel '.' Proc            {% locate $ Send $1 $3 $5 }
    | 'end'                                     {% locate $ End }
    | '(' 'nu' ProcName ')' Proc                {% locate $ Nu $3 Nothing $5 }
    | '(' 'nu' ProcName ':' Type ')' Proc       {% locate $ Nu $3 (Just $5) $7 }
    | '*' Proc                                  {% locate $ Repl $2 }
    | ProcName                                  {% locate $ Call $1 }
    | '(' ProcPar ')'                           { $2 }

Pattern :: {Pattern}
         : ProcName                         {% locate $ PtrnName $1 }
         | Patterns                         {% locate $ PtrnTuple (reverse $1) }
         | Label                            {% locate $ PtrnLabel $1 }
Patterns :: {[Pattern]}
    : Patterns ',' Pattern                  { $3 : $1 }
    | Pattern  ',' Pattern                  { [ $3, $1 ]  }

RecvClause :: {Clause}
        : '(' Pattern ')' '.' ProcPar    {% locate $  Clause $2 $5 }
ChoiceClause :: {Clause}
        : Pattern '->' ProcPar           {% locate $  Clause $1 $3 }
ChoiceClauses :: {[Clause]}
        : ChoiceClauses ';' ChoiceClause    { $3 : $1 }
        | ChoiceClause                      { [ $1 ]  }

TypeName :: {TypeName}
    : typeName                              {% locate $ TypeName $1 }
    | label                                 {% locate $ TypeName $1 }

ProcName :: {ProcName}
      : namePos                             {% locate $ ProcName $1 }

TypeVar :: {TypeVar}
    : TypeName                              {% locate $ TypeVarText $1 }

Name :: {Name}
      : namePos                             {% locate $ Positive $1 }
      | nameNeg                             {% locate $ Negative $1 }
      | ReservedName                        {% locate $ Reserved $1 }

ReservedName :: {Text}
     : 'stdout'                             { "stdout" }
     | 'stdin'                              { "stdin" }

Expr :: {Expr}
    : Expr '+' Expr                       {% locate $ Add $1 $3 }
    | Expr '-' Expr                       {% locate $ Sub $1 $3 }
    | Expr '*' Expr                       {% locate $ Mul $1 $3 }
    | Expr '/' Expr                       {% locate $ Div $1 $3 }
    | 'if' Expr 'then' Expr 'else' Expr   {% locate $ IfThenElse $2 $4 $6 }
    | Exprs                               {% locate $ ExprTuple (reverse $1) }
    | Term                                { $1 }
Exprs :: {[Expr]}
    : Exprs ',' Expr                        { $3 : $1 }
    | Expr  ',' Expr                        { [ $3 , $1 ]  }

Term :: {Expr}
    : '(' Expr ')'                        { $2 }
    | Name                                {% locate $ ExprName $1 }
    | '0'                                 {% locate $ ExprDigit 0 }
    | int                                 {% locate $ ExprDigit $1 }
    | string                              {% locate $ ExprString $1 }
    | Expr '==' Expr                      {% locate $ EQ  $1 $3 }
    | Expr '!=' Expr                      {% locate $ NEQ $1 $3 }
    | Expr '>'  Expr                      {% locate $ GT  $1 $3 }
    | Expr '>=' Expr                      {% locate $ GTE $1 $3 }
    | Expr '<'  Expr                      {% locate $ LT  $1 $3 }
    | Expr '<=' Expr                      {% locate $ LTE $1 $3 }
    | Boolean                             { $1 }

Boolean :: {Expr}
    : 'True'                              {% locate $ ExprBool True }
    | 'False'                             {% locate $ ExprBool False }

SelectLabel :: {Expr}
    : Label                               {% locate $ ExprLabel $1 }

Label :: {Label}
    : label                                 {% locate $ Label $1 }

BaseType :: {BaseType}
    : 'Int'                                 {% locate $ BaseInt }
    | 'Bool'                                {% locate $ BaseBool }

TypeBase :: {Type}
    : BaseType                              {% locate $ TypeBase $1  }

Type :: {Type}
    : '0'                                   {% locate $ TypeEnd }
    | TypeVar                               {% locate $ TypeVar $1 }
    | BaseType                              {% locate $ TypeBase $1 }
    | '!' Type '.' Type                     {% locate $ TypeSend $2 $4 }
    | '?' Type '.' Type                     {% locate $ TypeRecv $2 $4 }
    | '&' '{' TypeOfLabels '}'              {% locate $ TypeChoi (reverse $3)  }
    | '#' '{' TypeOfLabels '}'              {% locate $ TypeSele (reverse $3)  }
    | 'un' '(' Type ')'                     {% locate $ TypeUn $3 }
    | 'mu' '(' TypeVar ')' '(' Type ')'     {% locate $ TypeMu $6 }
    | '(' TypeOfTuples ')'                  {% locate $ TypeTuple (reverse $2) }
    | '(' Type ')'                          { $2 }

TypeOfTuples :: {[Type]}
    : TypeOfTuples ',' Type                 { $3 : $1 }
    | Type ',' Type                         { [$3, $1] }

TypeOfLabel :: {TypeOfLabel}
    : Label '->' Type                        {% locate $ TypeOfLabel $1 $3  }

TypeOfLabels :: {[TypeOfLabel]}
    : TypeOfLabels ';' TypeOfLabel          { $3 : $1 }
    | TypeOfLabel                           { [ $1 ]  }

{}
