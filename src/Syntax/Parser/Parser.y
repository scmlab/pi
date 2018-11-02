{
module Syntax.Parser.Parser where

import Syntax.Parser.Base
import Syntax.Abstract
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
        'true'          { Token _ TokenTrue }
        'false'         { Token _ TokenFalse }
        'if'            { Token _ TokenIf }
        'then'          { Token _ TokenThen }
        'else'          { Token _ TokenElse }
        '='             { Token _ TokenDefn      }
        '!'             { Token _ TokenSend      }
        '?'             { Token _ TokenRecv      }
        '.'             { Token _ TokenSeq       }
        '|'             { Token _ TokenPar       }
        '('             { Token _ TokenParenStart }
        ')'             { Token _ TokenParenEnd }
        '+'             { Token _ TokenPlus }
        '-'             { Token _ TokenMinus }
        '<'             { Token _ TokenAngleStart }
        '>'             { Token _ TokenAngleEnd }
        ','             { Token _ TokenComma }
        '{'             { Token _ TokenBraceStart }
        '}'             { Token _ TokenBraceEnd }
        ';'             { Token _ TokenSemi }
        '->'            { Token _ TokenArrow }

%%

Program :: {Prog}
    : PiDecls                   { Prog $1 }

PiDecls :: {[PiDecl]}
    : PiDecl                    { [$1] }
    | PiDecl PiDecls            { $1:$2 }

PiDecl :: {PiDecl}
    : Name '=' Pi               { PiDecl $1 $3 }

Pi :: {Pi}
    : Name '!' Expr '.' Pi      { Send $1 $3 $5  }
    | Name '?' '{' Clauses '}'  { Recv $1 (reverse $4)  }
    | Name '?' Pattern '.' Pi   { Recv $1 [Clause $3 $5]  }
    | 'end'                     { End }
    | Pi '|' Pi                 { Par $1 $3 }
    | '(' 'nu' Name ')' Pi      { Nu $3 $5 }
    | Name                      { Call $1 }

Pattern :: {Ptrn}
         : Name                 { PN $1 }
         | label                { PL $1 }

Clause :: {Clause}
        : Pattern '->' Pi          { Clause $1 $3 }
Clauses :: {[Clause]}
        : Clauses ';' Clause       { $3 : $1 }
        | Clause                   { [ $1 ] }

Name :: {Name}
      : name                { NS $1 }
      | ResName             { NR $1 }

ResName :: {ResName}
         : 'stdout'         { StdOut }
         | 'stdin'          { StdIn }

Expr :: {Expr}
      : Expr '+' Term       { EPlus $1 $3 }
      | Expr '-' Term       { EMinus $1 $3 }
      | Term                { $1 }
      | '(' Expr ')'        { $2 }

Term :: {Expr}
      : Val                 { EV $1 }
      | '(' Expr ')'        { $2 }

Val :: {Val}
     : Name                 { N $1 }
     | int                  { VI $1 }
     | label                { VL $1 }

{}
