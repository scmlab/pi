{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
{-# LANGUAGE OverloadedStrings                  #-}

module Syntax.Parser.Parser where

import Syntax.Parser.Lexer
import Syntax.Parser.Type
import Syntax.Concrete
import Data.Loc
import Prelude hiding (GT, LT, EQ)

import Data.Text (Text)
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.9

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn5 :: (Program Loc) -> (HappyAbsSyn )
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> (Program Loc)
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: ([ProcDecl Loc]) -> (HappyAbsSyn )
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> ([ProcDecl Loc])
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: (ProcDecl Loc) -> (HappyAbsSyn )
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> (ProcDecl Loc)
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: (Process Loc) -> (HappyAbsSyn )
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> (Process Loc)
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: (Process Loc) -> (HappyAbsSyn )
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> (Process Loc)
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: (Pattern Loc) -> (HappyAbsSyn )
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> (Pattern Loc)
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: ([Pattern Loc]) -> (HappyAbsSyn )
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> ([Pattern Loc])
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: (Clause Loc) -> (HappyAbsSyn )
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> (Clause Loc)
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: (Clause Loc) -> (HappyAbsSyn )
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> (Clause Loc)
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: ([Clause Loc]) -> (HappyAbsSyn )
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> ([Clause Loc])
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyIn15 :: (SimpName Loc) -> (HappyAbsSyn )
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> (SimpName Loc)
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: (Name Loc) -> (HappyAbsSyn )
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> (Name Loc)
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyIn17 :: (Text) -> (HappyAbsSyn )
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> (Text)
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyIn18 :: (Expr Loc) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> (Expr Loc)
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: ([Expr Loc]) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> ([Expr Loc])
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: (Expr Loc) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> (Expr Loc)
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyIn21 :: (Expr Loc) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> (Expr Loc)
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
happyIn22 :: (Label Loc) -> (HappyAbsSyn )
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> (Label Loc)
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
happyIn23 :: (Sort Loc) -> (HappyAbsSyn )
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> (Sort Loc)
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
happyIn24 :: (Type Loc) -> (HappyAbsSyn )
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> (Type Loc)
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
happyIn25 :: (TypeOfLabel Loc) -> (HappyAbsSyn )
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> (TypeOfLabel Loc)
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
happyIn26 :: ([TypeOfLabel Loc]) -> (HappyAbsSyn )
happyIn26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> ([TypeOfLabel Loc])
happyOut26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut26 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x60\x07\x05\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x07\x05\x00\x00\x00\x00\x00\x80\x3d\x14\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x02\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3f\x40\x00\x34\x10\x00\x00\x00\x0c\x00\x21\x00\x00\x00\x00\x00\x60\x07\x05\x00\x00\x00\x00\x00\x80\x1d\x14\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x40\x00\x00\x00\x00\x00\x00\x0c\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc6\x01\xf0\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfc\x00\x01\xd0\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfc\x00\x01\xd0\x40\x00\x00\x00\x00\x00\x08\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x76\x50\x00\x00\x00\x00\x00\x00\x08\x19\x00\x00\x00\x00\x00\x00\x00\x00\x71\x00\xfc\x02\x00\x00\x00\x00\xe4\x09\xf0\x03\x00\x00\x00\x00\x80\x20\x00\x00\x00\x00\x00\xd8\x41\x01\x00\x00\x00\x00\x00\xf0\x03\x04\x40\x03\x01\x00\x00\xc0\x0f\x10\x00\x0d\x04\x00\x00\x00\x3f\x40\x00\x34\x10\x00\x00\x00\xfc\x00\x01\xd0\x40\x00\x00\x00\xf0\x03\x04\x40\x03\x01\x00\x00\xc0\x0f\x10\x00\x0d\x04\x00\x00\x00\x3f\x40\x00\x34\x10\x00\x00\x00\xfc\x00\x01\xd0\x40\x00\x00\x00\xf0\x03\x04\x40\x03\x01\x00\x00\xc0\x0f\x10\x00\x0d\x04\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x80\x20\x00\x00\x00\x00\x00\xd8\x41\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x40\x00\x00\x00\x00\x00\x00\x0c\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x00\x10\x00\x00\x00\x00\x00\x00\x76\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x3c\x00\x00\x00\x00\x00\x00\x00\xf0\x00\x00\x00\x00\x00\x00\x00\xf0\x03\x00\x00\x00\x00\x10\x04\xc0\x0f\x00\x00\x00\x00\x40\x10\x00\x3f\x00\x00\x00\x00\x00\x00\x00\xfc\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfc\x00\x01\xd0\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x0f\x10\x00\x0d\x04\x00\x00\x00\x3f\x40\x00\x34\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x42\x06\x08\x0a\x00\x00\x00\x00\x08\x19\x20\x28\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x80\x1d\x14\x00\x00\x00\x00\x00\x00\x00\x10\x07\xc0\x4f\x00\x00\x00\x00\x40\x1c\x00\x3f\x00\x00\x00\x00\x00\x71\x00\xfc\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x0f\x10\x00\x0d\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x14\x00\x00\x00\x00\x00\x42\x06\x00\x00\x00\x00\x00\x00\x08\x19\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x00\x80\x90\x01\x00\x00\x00\x00\x00\x00\x42\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x42\x06\x00\x00\x00\x00\x00\x00\x00\x40\x1c\x00\x3f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_programParser","%start_processParser","Program","ProcDecls","ProcDecl","ProcessPar","Process","Pattern","Patterns","ClauseDot","ClauseArr","Clauses","SimpName","Name","ReservedName","Expr","Exprs","Term","Boolean","Label","Sort","Type","TypeOfLabel","TypeOfLabels","label","namePos","nameNeg","int","'stdout'","'stdin'","'end'","'nu'","'='","'!'","'?'","'.'","'*'","'|'","'('","')'","'+'","'-'","'/'","'{'","'}'","','","';'","'->'","':'","'Int'","string","'Bool'","'True'","'False'","'=='","'!='","'>'","'>='","'<'","'<='","'if'","'then'","'else'","%eof"]
        bit_start = st * 66
        bit_end = (st + 1) * 66
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..65]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x0d\x00\xa3\x00\x0d\x00\x0d\x00\x00\x00\x1a\x00\x00\x00\xfa\xff\x00\x00\x00\x00\x3e\x00\x00\x00\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa3\x00\x95\x00\xe9\xff\x93\x00\x2f\x00\x00\x00\x01\x00\x09\x00\xa3\x00\xa3\x00\x00\x00\x2d\x00\x00\x00\x35\x00\x00\x00\x00\x00\x00\x00\x00\x00\x11\x00\x11\x00\x00\x00\x4c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x01\x00\x8e\x00\x00\x00\xa3\x00\xce\x00\x31\x00\x53\x00\x89\x00\xa3\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x4a\x00\x00\x00\x96\x00\x37\x00\x6c\x01\xa3\x00\x00\x00\x00\x00\x11\x00\x11\x00\x00\x00\x11\x00\xa3\x00\x00\x00\x00\x00\x5d\x01\x5d\x01\xe0\x00\xe0\x00\x64\x01\x72\x00\x72\x00\x64\x01\x00\x00\x00\x00\x01\x00\x00\x00\x01\x00\x01\x00\x00\x00\x5a\x00\x00\x00\x2b\x00\x32\x00\x00\x00\x5b\x00\x65\x00\x7a\x00\x00\x00\x00\x00\x77\x00\x94\x00\x85\x00\xa3\x00\x08\x00\x6b\x00\x6b\x00\xa8\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\xa0\x00\x00\x00\xca\x00\xce\x00\xce\x00\xe1\x00\xce\x00\xce\x00\x00\x00\x00\x00\x00\x00\xc2\x00\x00\x00\x00\x00\x00\x00\xce\x00\x6b\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x0c\x00\x76\x00\x36\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe4\x00\xb1\x00\x00\x00\x00\x00\xc5\x00\x00\x00\x05\x01\x29\x00\xe7\x00\xbb\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x45\x00\x7f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfe\x00\x00\x00\x00\x00\x00\x00\x0c\x01\x00\x00\x00\x00\xf0\x00\xad\x00\x00\x00\x00\x00\x00\x00\xf3\x00\x13\x01\x1a\x01\x21\x01\x28\x01\x2f\x01\x36\x01\x3d\x01\x44\x01\x4b\x01\x52\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbe\x00\x00\x00\x00\x00\x57\x00\xa9\x00\x00\x00\xa7\x00\xc8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x59\x01\x00\x00\x60\x01\x67\x01\x00\x00\x00\x00\x00\x00\xc4\x00\xd0\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd8\x00\xfc\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6e\x01\x00\x00\x00\x00\x00\x00\x00\x00\xba\x00\xd1\x00\x00\x00\xd2\x00\xd3\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\xd4\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\x00\x00\xfd\xff\xfc\xff\x00\x00\xe5\xff\x00\x00\xf8\xff\xf0\xff\x00\x00\xe2\xff\xe5\xff\xe3\xff\xe1\xff\xe0\xff\xf4\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf1\xff\x00\x00\x00\x00\x00\x00\x00\x00\xfb\xff\xfa\xff\xf9\xff\x00\x00\xf5\xff\xee\xff\xec\xff\xc8\xff\x00\x00\x00\x00\xd5\xff\x00\x00\xd9\xff\xcb\xff\xd3\xff\xe4\xff\xd4\xff\x00\x00\xd2\xff\xca\xff\xc9\xff\x00\x00\x00\x00\xef\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe6\xff\x00\x00\x00\x00\x00\x00\x00\x00\xe9\xff\xed\xff\x00\x00\x00\x00\xf6\xff\x00\x00\x00\x00\xcc\xff\xcd\xff\xce\xff\xcf\xff\xd0\xff\xd1\xff\xdc\xff\xde\xff\xdf\xff\xdd\xff\xf7\xff\xda\xff\x00\x00\xd6\xff\x00\x00\x00\x00\xbf\xff\x00\x00\xbe\xff\x00\x00\x00\x00\xf3\xff\x00\x00\x00\x00\x00\x00\xc7\xff\xc6\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd7\xff\xd8\xff\xe8\xff\xe7\xff\xea\xff\xeb\xff\x00\x00\xf2\xff\x00\x00\xbb\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc3\xff\xc2\xff\xc0\xff\x00\x00\xc5\xff\xc4\xff\xc1\xff\x00\x00\xdb\xff\xbd\xff\xbc\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x02\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x0e\x00\x0a\x00\x01\x00\x02\x00\x00\x00\x01\x00\x02\x00\x02\x00\x0f\x00\x28\x00\x01\x00\x02\x00\x11\x00\x0d\x00\x0a\x00\x14\x00\x0f\x00\x11\x00\x12\x00\x13\x00\x1b\x00\x14\x00\x1d\x00\x1e\x00\x0f\x00\x11\x00\x28\x00\x09\x00\x14\x00\x15\x00\x25\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x02\x00\x05\x00\x27\x00\x07\x00\x02\x00\x07\x00\x0a\x00\x02\x00\x0a\x00\x0b\x00\x01\x00\x02\x00\x07\x00\x11\x00\x0e\x00\x0a\x00\x0b\x00\x0d\x00\x14\x00\x0a\x00\x0c\x00\x11\x00\x12\x00\x13\x00\x1a\x00\x14\x00\x1c\x00\x0a\x00\x0b\x00\x05\x00\x06\x00\x1a\x00\x16\x00\x1c\x00\x0a\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x11\x00\x26\x00\x0c\x00\x0d\x00\x0a\x00\x0b\x00\x05\x00\x11\x00\x12\x00\x13\x00\x0d\x00\x0a\x00\x18\x00\x10\x00\x11\x00\x12\x00\x13\x00\x0c\x00\x11\x00\x16\x00\x10\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x0c\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x0d\x00\x03\x00\x04\x00\x01\x00\x11\x00\x12\x00\x13\x00\x0d\x00\x0a\x00\x0b\x00\x0c\x00\x0c\x00\x05\x00\x13\x00\x01\x00\x08\x00\x09\x00\x0a\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x11\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x02\x00\x03\x00\x10\x00\x05\x00\x06\x00\x07\x00\x08\x00\x10\x00\x16\x00\x0c\x00\x0e\x00\x0d\x00\x10\x00\x0f\x00\x02\x00\x03\x00\x19\x00\x05\x00\x06\x00\x07\x00\x15\x00\x05\x00\x17\x00\x05\x00\x08\x00\x0d\x00\x0a\x00\x0f\x00\x0a\x00\x03\x00\x04\x00\x0e\x00\x0a\x00\x11\x00\x19\x00\x11\x00\x0a\x00\x0b\x00\x0c\x00\x03\x00\x04\x00\x13\x00\x03\x00\x04\x00\x01\x00\x0a\x00\x0a\x00\x0b\x00\x0c\x00\x0a\x00\x0b\x00\x0c\x00\x03\x00\x04\x00\x13\x00\x0a\x00\x0a\x00\x02\x00\xff\xff\x0a\x00\x0b\x00\x0c\x00\x07\x00\x12\x00\x13\x00\x0a\x00\x0b\x00\x0a\x00\x0a\x00\x0a\x00\x0a\x00\x0a\x00\x15\x00\xff\xff\x17\x00\x12\x00\x13\x00\x13\x00\x13\x00\x13\x00\x13\x00\x04\x00\x11\x00\xff\xff\x04\x00\x14\x00\x15\x00\x0a\x00\x0b\x00\x0c\x00\x0a\x00\x0b\x00\x0c\x00\x04\x00\xff\xff\x15\x00\x04\x00\x17\x00\xff\xff\x0a\x00\x0b\x00\x0c\x00\x0a\x00\x0b\x00\x0c\x00\x04\x00\x21\x00\x22\x00\x23\x00\x24\x00\xff\xff\x0a\x00\x0b\x00\x0c\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x0f\x00\x10\x00\x11\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x0f\x00\x10\x00\x11\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x0f\x00\x10\x00\x11\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x0f\x00\x10\x00\x11\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x0f\x00\x10\x00\x11\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x0f\x00\x10\x00\x11\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x0f\x00\x10\x00\x11\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x0f\x00\x10\x00\x11\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x0f\x00\x10\x00\x11\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x0f\x00\x10\x00\x11\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x0f\x00\x10\x00\x11\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x0f\x00\x10\x00\x11\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x0f\x00\x10\x00\x11\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x0f\x00\x10\x00\x11\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x0f\x00\x10\x00\x11\x00\x0b\x00\x0c\x00\x0d\x00\x10\x00\x0f\x00\x10\x00\x11\x00\x23\x00\x24\x00\x16\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x1b\x00\x23\x00\x2b\x00\x0e\x00\x2c\x00\x0f\x00\x10\x00\x1a\x00\x05\x00\x23\x00\x07\x00\x13\x00\x03\x00\x04\x00\x07\x00\x2d\x00\xff\xff\x23\x00\x07\x00\x78\x00\x3a\x00\x05\x00\x8a\x00\x24\x00\x3b\x00\x3c\x00\x3d\x00\x2e\x00\x25\x00\x2f\x00\x30\x00\x24\x00\x78\x00\xff\xff\x1b\x00\x79\x00\x7d\x00\x31\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x07\x00\x1e\x00\x77\x00\x1f\x00\x07\x00\x63\x00\x20\x00\x07\x00\x64\x00\x65\x00\x03\x00\x04\x00\x63\x00\x21\x00\x1a\x00\x64\x00\x65\x00\x3a\x00\x6e\x00\x05\x00\x49\x00\x3b\x00\x3c\x00\x3d\x00\x6a\x00\x69\x00\x6b\x00\x18\x00\x19\x00\x46\x00\x47\x00\x6a\x00\x4d\x00\x6b\x00\x20\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x21\x00\x60\x00\x39\x00\x3a\x00\xe4\xff\xe4\xff\x75\x00\x3b\x00\x3c\x00\x3d\x00\x3a\x00\x20\x00\x50\x00\x5e\x00\x3b\x00\x3c\x00\x3d\x00\x80\x00\x21\x00\x5f\x00\x6f\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x7f\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x3a\x00\x07\x00\x08\x00\x23\x00\x3b\x00\x3c\x00\x3d\x00\x3a\x00\x09\x00\x0a\x00\x0b\x00\x7d\x00\x43\x00\x3d\x00\x23\x00\x44\x00\x45\x00\x20\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x21\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x0d\x00\x0e\x00\x5c\x00\x0f\x00\x10\x00\x11\x00\x16\x00\x34\x00\x5d\x00\x7c\x00\x1a\x00\x12\x00\x33\x00\x13\x00\x0d\x00\x0e\x00\x35\x00\x0f\x00\x10\x00\x11\x00\x4e\x00\x43\x00\x4f\x00\x74\x00\x73\x00\x12\x00\x20\x00\x13\x00\x20\x00\x14\x00\x08\x00\x1a\x00\x60\x00\x21\x00\x88\x00\x21\x00\x09\x00\x0a\x00\x0b\x00\x1c\x00\x08\x00\x61\x00\x49\x00\x08\x00\x23\x00\x60\x00\x09\x00\x0a\x00\x0b\x00\x09\x00\x0a\x00\x0b\x00\x72\x00\x08\x00\x85\x00\x60\x00\x31\x00\x07\x00\x00\x00\x09\x00\x0a\x00\x0b\x00\x63\x00\x6b\x00\x6c\x00\x64\x00\x65\x00\x60\x00\x60\x00\x60\x00\x60\x00\x60\x00\x87\x00\x00\x00\x84\x00\x66\x00\x67\x00\x84\x00\x81\x00\x80\x00\x89\x00\x16\x00\x78\x00\x00\x00\x1d\x00\x79\x00\x7a\x00\x09\x00\x0a\x00\x0b\x00\x09\x00\x0a\x00\x0b\x00\x65\x00\x00\x00\x83\x00\x5a\x00\x84\x00\x00\x00\x09\x00\x0a\x00\x0b\x00\x09\x00\x0a\x00\x0b\x00\x77\x00\x40\x00\x41\x00\x42\x00\x43\x00\x00\x00\x09\x00\x0a\x00\x0b\x00\x25\x00\x0b\x00\x36\x00\x37\x00\x27\x00\x28\x00\x29\x00\x25\x00\x0b\x00\x26\x00\x00\x00\x27\x00\x28\x00\x29\x00\x25\x00\x0b\x00\x35\x00\x00\x00\x27\x00\x28\x00\x29\x00\x25\x00\x0b\x00\x59\x00\x00\x00\x27\x00\x28\x00\x29\x00\x25\x00\x0b\x00\x58\x00\x00\x00\x27\x00\x28\x00\x29\x00\x25\x00\x0b\x00\x57\x00\x00\x00\x27\x00\x28\x00\x29\x00\x25\x00\x0b\x00\x56\x00\x00\x00\x27\x00\x28\x00\x29\x00\x25\x00\x0b\x00\x55\x00\x00\x00\x27\x00\x28\x00\x29\x00\x25\x00\x0b\x00\x54\x00\x00\x00\x27\x00\x28\x00\x29\x00\x25\x00\x0b\x00\x53\x00\x00\x00\x27\x00\x28\x00\x29\x00\x25\x00\x0b\x00\x52\x00\x00\x00\x27\x00\x28\x00\x29\x00\x25\x00\x0b\x00\x51\x00\x00\x00\x27\x00\x28\x00\x29\x00\x25\x00\x0b\x00\x50\x00\x00\x00\x27\x00\x28\x00\x29\x00\x25\x00\x0b\x00\x71\x00\x00\x00\x27\x00\x28\x00\x29\x00\x25\x00\x0b\x00\x70\x00\x00\x00\x27\x00\x28\x00\x29\x00\x25\x00\x0b\x00\x6f\x00\x00\x00\x27\x00\x28\x00\x29\x00\x25\x00\x0b\x00\x88\x00\x4b\x00\x27\x00\x28\x00\x29\x00\x42\x00\x43\x00\x4c\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (2, 68) [
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68)
	]

happy_n_terms = 41 :: Int
happy_n_nonterms = 22 :: Int

happyReduce_2 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_2 = happyMonadReduce 1# 0# happyReduction_2
happyReduction_2 (happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOut6 happy_x_1 of { happy_var_1 -> 
	( locate $ Program (reverse happy_var_1))})
	) (\r -> happyReturn (happyIn5 r))

happyReduce_3 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_3 = happySpecReduce_1  1# happyReduction_3
happyReduction_3 happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn6
		 ([happy_var_1]
	)}

happyReduce_4 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_4 = happySpecReduce_2  1# happyReduction_4
happyReduction_4 happy_x_2
	happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	case happyOut7 happy_x_2 of { happy_var_2 -> 
	happyIn6
		 (happy_var_2:happy_var_1
	)}}

happyReduce_5 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_5 = happyMonadReduce 3# 2# happyReduction_5
happyReduction_5 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOut15 happy_x_1 of { happy_var_1 -> 
	case happyOut8 happy_x_3 of { happy_var_3 -> 
	( locate $ ProcDecl happy_var_1 happy_var_3)}})
	) (\r -> happyReturn (happyIn7 r))

happyReduce_6 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_6 = happyMonadReduce 3# 3# happyReduction_6
happyReduction_6 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOut8 happy_x_1 of { happy_var_1 -> 
	case happyOut9 happy_x_3 of { happy_var_3 -> 
	( locate $ Par happy_var_1 happy_var_3)}})
	) (\r -> happyReturn (happyIn8 r))

happyReduce_7 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_7 = happySpecReduce_1  3# happyReduction_7
happyReduction_7 happy_x_1
	 =  case happyOut9 happy_x_1 of { happy_var_1 -> 
	happyIn8
		 (happy_var_1
	)}

happyReduce_8 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_8 = happyMonadReduce 5# 4# happyReduction_8
happyReduction_8 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_3 of { happy_var_3 -> 
	case happyOut9 happy_x_5 of { happy_var_5 -> 
	( locate $ Send happy_var_1 happy_var_3 happy_var_5)}}})
	) (\r -> happyReturn (happyIn9 r))

happyReduce_9 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_9 = happyMonadReduce 5# 4# happyReduction_9
happyReduction_9 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut14 happy_x_4 of { happy_var_4 -> 
	( locate $ Recv happy_var_1 (reverse happy_var_4))}})
	) (\r -> happyReturn (happyIn9 r))

happyReduce_10 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_10 = happyMonadReduce 3# 4# happyReduction_10
happyReduction_10 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	( locate $ Recv happy_var_1 [happy_var_3])}})
	) (\r -> happyReturn (happyIn9 r))

happyReduce_11 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_11 = happyMonadReduce 1# 4# happyReduction_11
happyReduction_11 (happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((( locate $ End))
	) (\r -> happyReturn (happyIn9 r))

happyReduce_12 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_12 = happyMonadReduce 5# 4# happyReduction_12
happyReduction_12 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOut15 happy_x_3 of { happy_var_3 -> 
	case happyOut9 happy_x_5 of { happy_var_5 -> 
	( locate $ Nu happy_var_3 Nothing happy_var_5)}})
	) (\r -> happyReturn (happyIn9 r))

happyReduce_13 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_13 = happyMonadReduce 7# 4# happyReduction_13
happyReduction_13 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOut15 happy_x_3 of { happy_var_3 -> 
	case happyOut24 happy_x_5 of { happy_var_5 -> 
	case happyOut9 happy_x_7 of { happy_var_7 -> 
	( locate $ Nu happy_var_3 (Just happy_var_5) happy_var_7)}}})
	) (\r -> happyReturn (happyIn9 r))

happyReduce_14 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_14 = happyMonadReduce 2# 4# happyReduction_14
happyReduction_14 (happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOut9 happy_x_2 of { happy_var_2 -> 
	( locate $ Repl happy_var_2)})
	) (\r -> happyReturn (happyIn9 r))

happyReduce_15 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_15 = happyMonadReduce 1# 4# happyReduction_15
happyReduction_15 (happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOut15 happy_x_1 of { happy_var_1 -> 
	( locate $ Call happy_var_1)})
	) (\r -> happyReturn (happyIn9 r))

happyReduce_16 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_16 = happySpecReduce_3  4# happyReduction_16
happyReduction_16 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_2 of { happy_var_2 -> 
	happyIn9
		 (happy_var_2
	)}

happyReduce_17 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_17 = happyMonadReduce 1# 5# happyReduction_17
happyReduction_17 (happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOut15 happy_x_1 of { happy_var_1 -> 
	( locate $ PtrnName happy_var_1)})
	) (\r -> happyReturn (happyIn10 r))

happyReduce_18 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_18 = happyMonadReduce 3# 5# happyReduction_18
happyReduction_18 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOut11 happy_x_2 of { happy_var_2 -> 
	( locate $ PtrnTuple (reverse happy_var_2))})
	) (\r -> happyReturn (happyIn10 r))

happyReduce_19 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_19 = happyMonadReduce 1# 5# happyReduction_19
happyReduction_19 (happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOut22 happy_x_1 of { happy_var_1 -> 
	( locate $ PtrnLabel happy_var_1)})
	) (\r -> happyReturn (happyIn10 r))

happyReduce_20 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_20 = happySpecReduce_3  6# happyReduction_20
happyReduction_20 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	case happyOut10 happy_x_3 of { happy_var_3 -> 
	happyIn11
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_21 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_21 = happySpecReduce_3  6# happyReduction_21
happyReduction_21 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	case happyOut10 happy_x_3 of { happy_var_3 -> 
	happyIn11
		 ([ happy_var_3, happy_var_1 ]
	)}}

happyReduce_22 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_22 = happyMonadReduce 3# 7# happyReduction_22
happyReduction_22 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOut10 happy_x_1 of { happy_var_1 -> 
	case happyOut8 happy_x_3 of { happy_var_3 -> 
	( locate $  Clause happy_var_1 happy_var_3)}})
	) (\r -> happyReturn (happyIn12 r))

happyReduce_23 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_23 = happyMonadReduce 3# 8# happyReduction_23
happyReduction_23 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOut10 happy_x_1 of { happy_var_1 -> 
	case happyOut8 happy_x_3 of { happy_var_3 -> 
	( locate $  Clause happy_var_1 happy_var_3)}})
	) (\r -> happyReturn (happyIn13 r))

happyReduce_24 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_24 = happySpecReduce_3  9# happyReduction_24
happyReduction_24 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn14
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_25 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_25 = happySpecReduce_1  9# happyReduction_25
happyReduction_25 happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	happyIn14
		 ([ happy_var_1 ]
	)}

happyReduce_26 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_26 = happyMonadReduce 1# 10# happyReduction_26
happyReduction_26 (happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOutTok happy_x_1 of { (TokenNamePos happy_var_1) -> 
	( locate $ SimpName happy_var_1)})
	) (\r -> happyReturn (happyIn15 r))

happyReduce_27 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_27 = happyMonadReduce 1# 11# happyReduction_27
happyReduction_27 (happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOutTok happy_x_1 of { (TokenNamePos happy_var_1) -> 
	( locate $ Positive happy_var_1)})
	) (\r -> happyReturn (happyIn16 r))

happyReduce_28 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_28 = happyMonadReduce 1# 11# happyReduction_28
happyReduction_28 (happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOutTok happy_x_1 of { (TokenNameNeg happy_var_1) -> 
	( locate $ Negative happy_var_1)})
	) (\r -> happyReturn (happyIn16 r))

happyReduce_29 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_29 = happyMonadReduce 1# 11# happyReduction_29
happyReduction_29 (happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOut17 happy_x_1 of { happy_var_1 -> 
	( locate $ Reserved happy_var_1)})
	) (\r -> happyReturn (happyIn16 r))

happyReduce_30 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_30 = happySpecReduce_1  12# happyReduction_30
happyReduction_30 happy_x_1
	 =  happyIn17
		 ("stdout"
	)

happyReduce_31 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_31 = happySpecReduce_1  12# happyReduction_31
happyReduction_31 happy_x_1
	 =  happyIn17
		 ("stdin"
	)

happyReduce_32 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_32 = happyMonadReduce 3# 13# happyReduction_32
happyReduction_32 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_3 of { happy_var_3 -> 
	( locate $ Add happy_var_1 happy_var_3)}})
	) (\r -> happyReturn (happyIn18 r))

happyReduce_33 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_33 = happyMonadReduce 3# 13# happyReduction_33
happyReduction_33 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_3 of { happy_var_3 -> 
	( locate $ Sub happy_var_1 happy_var_3)}})
	) (\r -> happyReturn (happyIn18 r))

happyReduce_34 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_34 = happyMonadReduce 3# 13# happyReduction_34
happyReduction_34 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_3 of { happy_var_3 -> 
	( locate $ Mul happy_var_1 happy_var_3)}})
	) (\r -> happyReturn (happyIn18 r))

happyReduce_35 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_35 = happyMonadReduce 3# 13# happyReduction_35
happyReduction_35 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_3 of { happy_var_3 -> 
	( locate $ Div happy_var_1 happy_var_3)}})
	) (\r -> happyReturn (happyIn18 r))

happyReduce_36 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_36 = happyMonadReduce 6# 13# happyReduction_36
happyReduction_36 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOut18 happy_x_2 of { happy_var_2 -> 
	case happyOut18 happy_x_4 of { happy_var_4 -> 
	case happyOut18 happy_x_6 of { happy_var_6 -> 
	( locate $ IfThenElse happy_var_2 happy_var_4 happy_var_6)}}})
	) (\r -> happyReturn (happyIn18 r))

happyReduce_37 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_37 = happyMonadReduce 3# 13# happyReduction_37
happyReduction_37 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOut19 happy_x_2 of { happy_var_2 -> 
	( locate $ ExprTuple (reverse happy_var_2))})
	) (\r -> happyReturn (happyIn18 r))

happyReduce_38 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_38 = happySpecReduce_1  13# happyReduction_38
happyReduction_38 happy_x_1
	 =  case happyOut20 happy_x_1 of { happy_var_1 -> 
	happyIn18
		 (happy_var_1
	)}

happyReduce_39 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_39 = happySpecReduce_3  14# happyReduction_39
happyReduction_39 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut19 happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_3 of { happy_var_3 -> 
	happyIn19
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_40 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_40 = happySpecReduce_3  14# happyReduction_40
happyReduction_40 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_3 of { happy_var_3 -> 
	happyIn19
		 ([ happy_var_3 , happy_var_1 ]
	)}}

happyReduce_41 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_41 = happySpecReduce_3  15# happyReduction_41
happyReduction_41 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_2 of { happy_var_2 -> 
	happyIn20
		 (happy_var_2
	)}

happyReduce_42 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_42 = happyMonadReduce 1# 15# happyReduction_42
happyReduction_42 (happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOut16 happy_x_1 of { happy_var_1 -> 
	( locate $ ExprName happy_var_1)})
	) (\r -> happyReturn (happyIn20 r))

happyReduce_43 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_43 = happyMonadReduce 1# 15# happyReduction_43
happyReduction_43 (happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOutTok happy_x_1 of { (TokenInt happy_var_1) -> 
	( locate $ ExprDigit happy_var_1)})
	) (\r -> happyReturn (happyIn20 r))

happyReduce_44 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_44 = happyMonadReduce 1# 15# happyReduction_44
happyReduction_44 (happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOut22 happy_x_1 of { happy_var_1 -> 
	( locate $ ExprLabel happy_var_1)})
	) (\r -> happyReturn (happyIn20 r))

happyReduce_45 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_45 = happyMonadReduce 1# 15# happyReduction_45
happyReduction_45 (happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOutTok happy_x_1 of { (TokenString happy_var_1) -> 
	( locate $ ExprString happy_var_1)})
	) (\r -> happyReturn (happyIn20 r))

happyReduce_46 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_46 = happyMonadReduce 3# 15# happyReduction_46
happyReduction_46 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_3 of { happy_var_3 -> 
	( locate $ EQ  happy_var_1 happy_var_3)}})
	) (\r -> happyReturn (happyIn20 r))

happyReduce_47 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_47 = happyMonadReduce 3# 15# happyReduction_47
happyReduction_47 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_3 of { happy_var_3 -> 
	( locate $ NEQ happy_var_1 happy_var_3)}})
	) (\r -> happyReturn (happyIn20 r))

happyReduce_48 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_48 = happyMonadReduce 3# 15# happyReduction_48
happyReduction_48 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_3 of { happy_var_3 -> 
	( locate $ GT  happy_var_1 happy_var_3)}})
	) (\r -> happyReturn (happyIn20 r))

happyReduce_49 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_49 = happyMonadReduce 3# 15# happyReduction_49
happyReduction_49 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_3 of { happy_var_3 -> 
	( locate $ GTE happy_var_1 happy_var_3)}})
	) (\r -> happyReturn (happyIn20 r))

happyReduce_50 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_50 = happyMonadReduce 3# 15# happyReduction_50
happyReduction_50 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_3 of { happy_var_3 -> 
	( locate $ LT  happy_var_1 happy_var_3)}})
	) (\r -> happyReturn (happyIn20 r))

happyReduce_51 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_51 = happyMonadReduce 3# 15# happyReduction_51
happyReduction_51 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_3 of { happy_var_3 -> 
	( locate $ LTE happy_var_1 happy_var_3)}})
	) (\r -> happyReturn (happyIn20 r))

happyReduce_52 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_52 = happySpecReduce_1  15# happyReduction_52
happyReduction_52 happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	happyIn20
		 (happy_var_1
	)}

happyReduce_53 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_53 = happyMonadReduce 1# 16# happyReduction_53
happyReduction_53 (happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((( locate $ ExprBool True))
	) (\r -> happyReturn (happyIn21 r))

happyReduce_54 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_54 = happyMonadReduce 1# 16# happyReduction_54
happyReduction_54 (happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((( locate $ ExprBool False))
	) (\r -> happyReturn (happyIn21 r))

happyReduce_55 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_55 = happyMonadReduce 1# 17# happyReduction_55
happyReduction_55 (happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOutTok happy_x_1 of { (TokenLabel happy_var_1) -> 
	( locate $ Label happy_var_1)})
	) (\r -> happyReturn (happyIn22 r))

happyReduce_56 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_56 = happyMonadReduce 1# 18# happyReduction_56
happyReduction_56 (happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((( locate $ SortInt))
	) (\r -> happyReturn (happyIn23 r))

happyReduce_57 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_57 = happyMonadReduce 1# 18# happyReduction_57
happyReduction_57 (happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((( locate $ SortBool))
	) (\r -> happyReturn (happyIn23 r))

happyReduce_58 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_58 = happyMonadReduce 4# 19# happyReduction_58
happyReduction_58 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOut23 happy_x_2 of { happy_var_2 -> 
	case happyOut24 happy_x_4 of { happy_var_4 -> 
	( locate $ TypeSend (Left  happy_var_2) happy_var_4)}})
	) (\r -> happyReturn (happyIn24 r))

happyReduce_59 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_59 = happyMonadReduce 4# 19# happyReduction_59
happyReduction_59 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOut24 happy_x_2 of { happy_var_2 -> 
	case happyOut24 happy_x_4 of { happy_var_4 -> 
	( locate $ TypeSend (Right happy_var_2) happy_var_4)}})
	) (\r -> happyReturn (happyIn24 r))

happyReduce_60 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_60 = happyMonadReduce 4# 19# happyReduction_60
happyReduction_60 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOut23 happy_x_2 of { happy_var_2 -> 
	case happyOut24 happy_x_4 of { happy_var_4 -> 
	( locate $ TypeRecv (Left  happy_var_2) happy_var_4)}})
	) (\r -> happyReturn (happyIn24 r))

happyReduce_61 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_61 = happyMonadReduce 4# 19# happyReduction_61
happyReduction_61 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOut24 happy_x_2 of { happy_var_2 -> 
	case happyOut24 happy_x_4 of { happy_var_4 -> 
	( locate $ TypeRecv (Right happy_var_2) happy_var_4)}})
	) (\r -> happyReturn (happyIn24 r))

happyReduce_62 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_62 = happyMonadReduce 4# 19# happyReduction_62
happyReduction_62 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOut26 happy_x_3 of { happy_var_3 -> 
	( locate $ TypeSele (reverse happy_var_3))})
	) (\r -> happyReturn (happyIn24 r))

happyReduce_63 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_63 = happyMonadReduce 4# 19# happyReduction_63
happyReduction_63 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOut26 happy_x_3 of { happy_var_3 -> 
	( locate $ TypeChoi (reverse happy_var_3))})
	) (\r -> happyReturn (happyIn24 r))

happyReduce_64 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_64 = happyMonadReduce 1# 19# happyReduction_64
happyReduction_64 (happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOut15 happy_x_1 of { happy_var_1 -> 
	( locate $ TypeCall happy_var_1)})
	) (\r -> happyReturn (happyIn24 r))

happyReduce_65 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_65 = happyMonadReduce 1# 19# happyReduction_65
happyReduction_65 (happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((( locate $ TypeEnd))
	) (\r -> happyReturn (happyIn24 r))

happyReduce_66 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_66 = happyMonadReduce 3# 20# happyReduction_66
happyReduction_66 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOut22 happy_x_1 of { happy_var_1 -> 
	case happyOut24 happy_x_3 of { happy_var_3 -> 
	( locate $ TypeOfLabel happy_var_1 happy_var_3)}})
	) (\r -> happyReturn (happyIn25 r))

happyReduce_67 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_67 = happySpecReduce_3  21# happyReduction_67
happyReduction_67 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_3 of { happy_var_3 -> 
	happyIn26
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_68 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )
happyReduce_68 = happySpecReduce_1  21# happyReduction_68
happyReduction_68 happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	happyIn26
		 ([ happy_var_1 ]
	)}

happyNewToken action sts stk
	= scan(\tk -> 
	let cont i = happyDoAction i tk action sts stk in
	case tk of {
	TokenEOF -> happyDoAction 40# tk action sts stk;
	TokenLabel happy_dollar_dollar -> cont 1#;
	TokenNamePos happy_dollar_dollar -> cont 2#;
	TokenNameNeg happy_dollar_dollar -> cont 3#;
	TokenInt happy_dollar_dollar -> cont 4#;
	TokenStdOut -> cont 5#;
	TokenStdIn -> cont 6#;
	TokenEnd -> cont 7#;
	TokenNu -> cont 8#;
	TokenDefn -> cont 9#;
	TokenSend -> cont 10#;
	TokenRecv -> cont 11#;
	TokenSeq -> cont 12#;
	TokenStar -> cont 13#;
	TokenPar -> cont 14#;
	TokenParenStart -> cont 15#;
	TokenParenEnd -> cont 16#;
	TokenAdd -> cont 17#;
	TokenSub -> cont 18#;
	TokenDiv -> cont 19#;
	TokenBraceStart -> cont 20#;
	TokenBraceEnd -> cont 21#;
	TokenComma -> cont 22#;
	TokenSemi -> cont 23#;
	TokenArrow -> cont 24#;
	TokenTypeOf -> cont 25#;
	TokenSortInt -> cont 26#;
	TokenString happy_dollar_dollar -> cont 27#;
	TokenSortBool -> cont 28#;
	TokenTrue -> cont 29#;
	TokenFalse -> cont 30#;
	TokenEQ -> cont 31#;
	TokenNEQ -> cont 32#;
	TokenGT -> cont 33#;
	TokenGTE -> cont 34#;
	TokenLT -> cont 35#;
	TokenLTE -> cont 36#;
	TokenIf -> cont 37#;
	TokenThen -> cont 38#;
	TokenElse -> cont 39#;
	_ -> happyError' (tk, [])
	})

happyError_ explist 40# tk = happyError' (tk, explist)
happyError_ explist _ tk = happyError' (tk, explist)

happyThen :: () => Parser a -> (a -> Parser b) -> Parser b
happyThen = (>>=)
happyReturn :: () => a -> Parser a
happyReturn = (return)
happyParse :: () => Happy_GHC_Exts.Int# -> Parser (HappyAbsSyn )

happyNewToken :: () => Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )

happyDoAction :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn )

happyReduceArr :: () => Happy_Data_Array.Array Int (Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Parser (HappyAbsSyn ))

happyThen1 :: () => Parser a -> (a -> Parser b) -> Parser b
happyThen1 = happyThen
happyReturn1 :: () => a -> Parser a
happyReturn1 = happyReturn
happyError' :: () => ((Token), [String]) -> Parser a
happyError' tk = (\(tokens, _) -> syntaticalError tokens) tk
programParser = happySomeParser where
 happySomeParser = happyThen (happyParse 0#) (\x -> happyReturn (happyOut5 x))

processParser = happySomeParser where
 happySomeParser = happyThen (happyParse 1#) (\x -> happyReturn (happyOut8 x))

happySeq = happyDontSeq



{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 18 "<built-in>" #-}
{-# LINE 1 "/Users/banacorn/.stack/programs/x86_64-osx/ghc-8.4.3/lib/ghc-8.4.3/include/ghcversion.h" #-}
















{-# LINE 19 "<built-in>" #-}
{-# LINE 1 "/var/folders/h9/y58_wdxn5p34b5d76ms1f1lc0000gn/T/ghc25912_0/ghc_2.h" #-}

































































































































































































































































































































































































































































































































































































{-# LINE 20 "<built-in>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif

{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList








{-# LINE 65 "templates/GenericTemplate.hs" #-}


{-# LINE 75 "templates/GenericTemplate.hs" #-}










infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}
          

          case action of
                0#           -> {- nothing -}
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Int)) i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}
                                                   
                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}
                                     

                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Happy_GHC_Exts.+#  i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st




indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)


{-# LINE 180 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Happy_GHC_Exts.+#  nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Happy_GHC_Exts.+#  nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.

