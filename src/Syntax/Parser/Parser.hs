{-# OPTIONS_GHC -w #-}
{-# LANGUAGE OverloadedStrings                  #-}

module Syntax.Parser.Parser where

import Syntax.Parser.Lexer
import Syntax.Parser.Type
import Syntax.Concrete
import Data.Loc
import Prelude hiding (GT, LT, EQ)

import Data.Text (Text)
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (Program Loc)
	| HappyAbsSyn5 ([ProcDecl Loc])
	| HappyAbsSyn6 (ProcDecl Loc)
	| HappyAbsSyn7 (Process Loc)
	| HappyAbsSyn9 (Pattern Loc)
	| HappyAbsSyn10 ([Pattern Loc])
	| HappyAbsSyn11 (Clause Loc)
	| HappyAbsSyn13 ([Clause Loc])
	| HappyAbsSyn14 (SimpName Loc)
	| HappyAbsSyn15 (Name Loc)
	| HappyAbsSyn16 (Text)
	| HappyAbsSyn17 (Expr Loc)
	| HappyAbsSyn18 ([Expr Loc])
	| HappyAbsSyn21 (Label Loc)
	| HappyAbsSyn22 (Sort Loc)
	| HappyAbsSyn23 (Type Loc)
	| HappyAbsSyn24 (TypeOfLabel Loc)
	| HappyAbsSyn25 ([TypeOfLabel Loc])

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129,
 action_130,
 action_131,
 action_132,
 action_133 :: () => Int -> ({-HappyReduction (Parser) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> (Parser) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> (Parser) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (Parser) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65 :: () => ({-HappyReduction (Parser) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> (Parser) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> (Parser) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (Parser) HappyAbsSyn)

action_0 (27) = happyShift action_5
action_0 (4) = happyGoto action_6
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 (14) = happyGoto action_4
action_0 _ = happyFail

action_1 (27) = happyShift action_5
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (14) = happyGoto action_4
action_1 _ = happyFail

action_2 (27) = happyShift action_5
action_2 (6) = happyGoto action_8
action_2 (14) = happyGoto action_4
action_2 _ = happyReduce_1

action_3 _ = happyReduce_2

action_4 (34) = happyShift action_7
action_4 _ = happyFail

action_5 _ = happyReduce_24

action_6 (64) = happyAccept
action_6 _ = happyFail

action_7 (27) = happyShift action_14
action_7 (28) = happyShift action_15
action_7 (30) = happyShift action_16
action_7 (31) = happyShift action_17
action_7 (32) = happyShift action_18
action_7 (39) = happyShift action_19
action_7 (7) = happyGoto action_9
action_7 (8) = happyGoto action_10
action_7 (14) = happyGoto action_11
action_7 (15) = happyGoto action_12
action_7 (16) = happyGoto action_13
action_7 _ = happyFail

action_8 _ = happyReduce_3

action_9 (38) = happyShift action_24
action_9 _ = happyReduce_4

action_10 _ = happyReduce_6

action_11 _ = happyReduce_13

action_12 (35) = happyShift action_22
action_12 (36) = happyShift action_23
action_12 _ = happyFail

action_13 _ = happyReduce_27

action_14 (35) = happyReduce_25
action_14 (36) = happyReduce_25
action_14 _ = happyReduce_24

action_15 _ = happyReduce_26

action_16 _ = happyReduce_28

action_17 _ = happyReduce_29

action_18 _ = happyReduce_10

action_19 (27) = happyShift action_14
action_19 (28) = happyShift action_15
action_19 (30) = happyShift action_16
action_19 (31) = happyShift action_17
action_19 (32) = happyShift action_18
action_19 (33) = happyShift action_21
action_19 (39) = happyShift action_19
action_19 (7) = happyGoto action_20
action_19 (8) = happyGoto action_10
action_19 (14) = happyGoto action_11
action_19 (15) = happyGoto action_12
action_19 (16) = happyGoto action_13
action_19 _ = happyFail

action_20 (38) = happyShift action_24
action_20 (40) = happyShift action_45
action_20 _ = happyFail

action_21 (27) = happyShift action_5
action_21 (14) = happyGoto action_44
action_21 _ = happyFail

action_22 (26) = happyShift action_30
action_22 (27) = happyShift action_38
action_22 (28) = happyShift action_15
action_22 (29) = happyShift action_39
action_22 (30) = happyShift action_16
action_22 (31) = happyShift action_17
action_22 (39) = happyShift action_40
action_22 (53) = happyShift action_41
action_22 (54) = happyShift action_42
action_22 (61) = happyShift action_43
action_22 (15) = happyGoto action_33
action_22 (16) = happyGoto action_13
action_22 (17) = happyGoto action_34
action_22 (19) = happyGoto action_35
action_22 (20) = happyGoto action_36
action_22 (21) = happyGoto action_37
action_22 _ = happyFail

action_23 (26) = happyShift action_30
action_23 (27) = happyShift action_5
action_23 (39) = happyShift action_31
action_23 (45) = happyShift action_32
action_23 (9) = happyGoto action_26
action_23 (11) = happyGoto action_27
action_23 (14) = happyGoto action_28
action_23 (21) = happyGoto action_29
action_23 _ = happyFail

action_24 (27) = happyShift action_14
action_24 (28) = happyShift action_15
action_24 (30) = happyShift action_16
action_24 (31) = happyShift action_17
action_24 (32) = happyShift action_18
action_24 (39) = happyShift action_19
action_24 (8) = happyGoto action_25
action_24 (14) = happyGoto action_11
action_24 (15) = happyGoto action_12
action_24 (16) = happyGoto action_13
action_24 _ = happyFail

action_25 _ = happyReduce_5

action_26 (37) = happyShift action_67
action_26 _ = happyFail

action_27 _ = happyReduce_9

action_28 _ = happyReduce_15

action_29 _ = happyReduce_17

action_30 _ = happyReduce_52

action_31 (26) = happyShift action_30
action_31 (27) = happyShift action_5
action_31 (39) = happyShift action_31
action_31 (9) = happyGoto action_65
action_31 (10) = happyGoto action_66
action_31 (14) = happyGoto action_28
action_31 (21) = happyGoto action_29
action_31 _ = happyFail

action_32 (26) = happyShift action_30
action_32 (27) = happyShift action_5
action_32 (39) = happyShift action_31
action_32 (9) = happyGoto action_62
action_32 (12) = happyGoto action_63
action_32 (13) = happyGoto action_64
action_32 (14) = happyGoto action_28
action_32 (21) = happyGoto action_29
action_32 _ = happyFail

action_33 _ = happyReduce_40

action_34 (37) = happyShift action_51
action_34 (41) = happyShift action_52
action_34 (42) = happyShift action_53
action_34 (43) = happyShift action_54
action_34 (44) = happyShift action_55
action_34 (55) = happyShift action_56
action_34 (56) = happyShift action_57
action_34 (57) = happyShift action_58
action_34 (58) = happyShift action_59
action_34 (59) = happyShift action_60
action_34 (60) = happyShift action_61
action_34 _ = happyFail

action_35 _ = happyReduce_36

action_36 _ = happyReduce_49

action_37 _ = happyReduce_42

action_38 _ = happyReduce_25

action_39 _ = happyReduce_41

action_40 (26) = happyShift action_30
action_40 (27) = happyShift action_38
action_40 (28) = happyShift action_15
action_40 (29) = happyShift action_39
action_40 (30) = happyShift action_16
action_40 (31) = happyShift action_17
action_40 (39) = happyShift action_40
action_40 (53) = happyShift action_41
action_40 (54) = happyShift action_42
action_40 (61) = happyShift action_43
action_40 (15) = happyGoto action_33
action_40 (16) = happyGoto action_13
action_40 (17) = happyGoto action_49
action_40 (18) = happyGoto action_50
action_40 (19) = happyGoto action_35
action_40 (20) = happyGoto action_36
action_40 (21) = happyGoto action_37
action_40 _ = happyFail

action_41 _ = happyReduce_50

action_42 _ = happyReduce_51

action_43 (26) = happyShift action_30
action_43 (27) = happyShift action_38
action_43 (28) = happyShift action_15
action_43 (29) = happyShift action_39
action_43 (30) = happyShift action_16
action_43 (31) = happyShift action_17
action_43 (39) = happyShift action_40
action_43 (53) = happyShift action_41
action_43 (54) = happyShift action_42
action_43 (61) = happyShift action_43
action_43 (15) = happyGoto action_33
action_43 (16) = happyGoto action_13
action_43 (17) = happyGoto action_48
action_43 (19) = happyGoto action_35
action_43 (20) = happyGoto action_36
action_43 (21) = happyGoto action_37
action_43 _ = happyFail

action_44 (40) = happyShift action_46
action_44 (50) = happyShift action_47
action_44 _ = happyFail

action_45 _ = happyReduce_14

action_46 (27) = happyShift action_14
action_46 (28) = happyShift action_15
action_46 (30) = happyShift action_16
action_46 (31) = happyShift action_17
action_46 (32) = happyShift action_18
action_46 (39) = happyShift action_19
action_46 (8) = happyGoto action_96
action_46 (14) = happyGoto action_11
action_46 (15) = happyGoto action_12
action_46 (16) = happyGoto action_13
action_46 _ = happyFail

action_47 (27) = happyShift action_5
action_47 (32) = happyShift action_93
action_47 (35) = happyShift action_94
action_47 (36) = happyShift action_95
action_47 (14) = happyGoto action_91
action_47 (23) = happyGoto action_92
action_47 _ = happyFail

action_48 (41) = happyShift action_52
action_48 (42) = happyShift action_53
action_48 (43) = happyShift action_54
action_48 (44) = happyShift action_55
action_48 (55) = happyShift action_56
action_48 (56) = happyShift action_57
action_48 (57) = happyShift action_58
action_48 (58) = happyShift action_59
action_48 (59) = happyShift action_60
action_48 (60) = happyShift action_61
action_48 (62) = happyShift action_90
action_48 _ = happyFail

action_49 (40) = happyShift action_88
action_49 (41) = happyShift action_52
action_49 (42) = happyShift action_53
action_49 (43) = happyShift action_54
action_49 (44) = happyShift action_55
action_49 (47) = happyShift action_89
action_49 (55) = happyShift action_56
action_49 (56) = happyShift action_57
action_49 (57) = happyShift action_58
action_49 (58) = happyShift action_59
action_49 (59) = happyShift action_60
action_49 (60) = happyShift action_61
action_49 _ = happyFail

action_50 (40) = happyShift action_86
action_50 (47) = happyShift action_87
action_50 _ = happyFail

action_51 (27) = happyShift action_14
action_51 (28) = happyShift action_15
action_51 (30) = happyShift action_16
action_51 (31) = happyShift action_17
action_51 (32) = happyShift action_18
action_51 (39) = happyShift action_19
action_51 (8) = happyGoto action_85
action_51 (14) = happyGoto action_11
action_51 (15) = happyGoto action_12
action_51 (16) = happyGoto action_13
action_51 _ = happyFail

action_52 (26) = happyShift action_30
action_52 (27) = happyShift action_38
action_52 (28) = happyShift action_15
action_52 (29) = happyShift action_39
action_52 (30) = happyShift action_16
action_52 (31) = happyShift action_17
action_52 (39) = happyShift action_40
action_52 (53) = happyShift action_41
action_52 (54) = happyShift action_42
action_52 (61) = happyShift action_43
action_52 (15) = happyGoto action_33
action_52 (16) = happyGoto action_13
action_52 (17) = happyGoto action_84
action_52 (19) = happyGoto action_35
action_52 (20) = happyGoto action_36
action_52 (21) = happyGoto action_37
action_52 _ = happyFail

action_53 (26) = happyShift action_30
action_53 (27) = happyShift action_38
action_53 (28) = happyShift action_15
action_53 (29) = happyShift action_39
action_53 (30) = happyShift action_16
action_53 (31) = happyShift action_17
action_53 (39) = happyShift action_40
action_53 (53) = happyShift action_41
action_53 (54) = happyShift action_42
action_53 (61) = happyShift action_43
action_53 (15) = happyGoto action_33
action_53 (16) = happyGoto action_13
action_53 (17) = happyGoto action_83
action_53 (19) = happyGoto action_35
action_53 (20) = happyGoto action_36
action_53 (21) = happyGoto action_37
action_53 _ = happyFail

action_54 (26) = happyShift action_30
action_54 (27) = happyShift action_38
action_54 (28) = happyShift action_15
action_54 (29) = happyShift action_39
action_54 (30) = happyShift action_16
action_54 (31) = happyShift action_17
action_54 (39) = happyShift action_40
action_54 (53) = happyShift action_41
action_54 (54) = happyShift action_42
action_54 (61) = happyShift action_43
action_54 (15) = happyGoto action_33
action_54 (16) = happyGoto action_13
action_54 (17) = happyGoto action_82
action_54 (19) = happyGoto action_35
action_54 (20) = happyGoto action_36
action_54 (21) = happyGoto action_37
action_54 _ = happyFail

action_55 (26) = happyShift action_30
action_55 (27) = happyShift action_38
action_55 (28) = happyShift action_15
action_55 (29) = happyShift action_39
action_55 (30) = happyShift action_16
action_55 (31) = happyShift action_17
action_55 (39) = happyShift action_40
action_55 (53) = happyShift action_41
action_55 (54) = happyShift action_42
action_55 (61) = happyShift action_43
action_55 (15) = happyGoto action_33
action_55 (16) = happyGoto action_13
action_55 (17) = happyGoto action_81
action_55 (19) = happyGoto action_35
action_55 (20) = happyGoto action_36
action_55 (21) = happyGoto action_37
action_55 _ = happyFail

action_56 (26) = happyShift action_30
action_56 (27) = happyShift action_38
action_56 (28) = happyShift action_15
action_56 (29) = happyShift action_39
action_56 (30) = happyShift action_16
action_56 (31) = happyShift action_17
action_56 (39) = happyShift action_40
action_56 (53) = happyShift action_41
action_56 (54) = happyShift action_42
action_56 (61) = happyShift action_43
action_56 (15) = happyGoto action_33
action_56 (16) = happyGoto action_13
action_56 (17) = happyGoto action_80
action_56 (19) = happyGoto action_35
action_56 (20) = happyGoto action_36
action_56 (21) = happyGoto action_37
action_56 _ = happyFail

action_57 (26) = happyShift action_30
action_57 (27) = happyShift action_38
action_57 (28) = happyShift action_15
action_57 (29) = happyShift action_39
action_57 (30) = happyShift action_16
action_57 (31) = happyShift action_17
action_57 (39) = happyShift action_40
action_57 (53) = happyShift action_41
action_57 (54) = happyShift action_42
action_57 (61) = happyShift action_43
action_57 (15) = happyGoto action_33
action_57 (16) = happyGoto action_13
action_57 (17) = happyGoto action_79
action_57 (19) = happyGoto action_35
action_57 (20) = happyGoto action_36
action_57 (21) = happyGoto action_37
action_57 _ = happyFail

action_58 (26) = happyShift action_30
action_58 (27) = happyShift action_38
action_58 (28) = happyShift action_15
action_58 (29) = happyShift action_39
action_58 (30) = happyShift action_16
action_58 (31) = happyShift action_17
action_58 (39) = happyShift action_40
action_58 (53) = happyShift action_41
action_58 (54) = happyShift action_42
action_58 (61) = happyShift action_43
action_58 (15) = happyGoto action_33
action_58 (16) = happyGoto action_13
action_58 (17) = happyGoto action_78
action_58 (19) = happyGoto action_35
action_58 (20) = happyGoto action_36
action_58 (21) = happyGoto action_37
action_58 _ = happyFail

action_59 (26) = happyShift action_30
action_59 (27) = happyShift action_38
action_59 (28) = happyShift action_15
action_59 (29) = happyShift action_39
action_59 (30) = happyShift action_16
action_59 (31) = happyShift action_17
action_59 (39) = happyShift action_40
action_59 (53) = happyShift action_41
action_59 (54) = happyShift action_42
action_59 (61) = happyShift action_43
action_59 (15) = happyGoto action_33
action_59 (16) = happyGoto action_13
action_59 (17) = happyGoto action_77
action_59 (19) = happyGoto action_35
action_59 (20) = happyGoto action_36
action_59 (21) = happyGoto action_37
action_59 _ = happyFail

action_60 (26) = happyShift action_30
action_60 (27) = happyShift action_38
action_60 (28) = happyShift action_15
action_60 (29) = happyShift action_39
action_60 (30) = happyShift action_16
action_60 (31) = happyShift action_17
action_60 (39) = happyShift action_40
action_60 (53) = happyShift action_41
action_60 (54) = happyShift action_42
action_60 (61) = happyShift action_43
action_60 (15) = happyGoto action_33
action_60 (16) = happyGoto action_13
action_60 (17) = happyGoto action_76
action_60 (19) = happyGoto action_35
action_60 (20) = happyGoto action_36
action_60 (21) = happyGoto action_37
action_60 _ = happyFail

action_61 (26) = happyShift action_30
action_61 (27) = happyShift action_38
action_61 (28) = happyShift action_15
action_61 (29) = happyShift action_39
action_61 (30) = happyShift action_16
action_61 (31) = happyShift action_17
action_61 (39) = happyShift action_40
action_61 (53) = happyShift action_41
action_61 (54) = happyShift action_42
action_61 (61) = happyShift action_43
action_61 (15) = happyGoto action_33
action_61 (16) = happyGoto action_13
action_61 (17) = happyGoto action_75
action_61 (19) = happyGoto action_35
action_61 (20) = happyGoto action_36
action_61 (21) = happyGoto action_37
action_61 _ = happyFail

action_62 (49) = happyShift action_74
action_62 _ = happyFail

action_63 _ = happyReduce_23

action_64 (46) = happyShift action_72
action_64 (48) = happyShift action_73
action_64 _ = happyFail

action_65 (47) = happyShift action_71
action_65 _ = happyFail

action_66 (40) = happyShift action_69
action_66 (47) = happyShift action_70
action_66 _ = happyFail

action_67 (27) = happyShift action_14
action_67 (28) = happyShift action_15
action_67 (30) = happyShift action_16
action_67 (31) = happyShift action_17
action_67 (32) = happyShift action_18
action_67 (39) = happyShift action_19
action_67 (8) = happyGoto action_68
action_67 (14) = happyGoto action_11
action_67 (15) = happyGoto action_12
action_67 (16) = happyGoto action_13
action_67 _ = happyFail

action_68 _ = happyReduce_20

action_69 _ = happyReduce_16

action_70 (26) = happyShift action_30
action_70 (27) = happyShift action_5
action_70 (39) = happyShift action_31
action_70 (9) = happyGoto action_112
action_70 (14) = happyGoto action_28
action_70 (21) = happyGoto action_29
action_70 _ = happyFail

action_71 (26) = happyShift action_30
action_71 (27) = happyShift action_5
action_71 (39) = happyShift action_31
action_71 (9) = happyGoto action_111
action_71 (14) = happyGoto action_28
action_71 (21) = happyGoto action_29
action_71 _ = happyFail

action_72 _ = happyReduce_8

action_73 (26) = happyShift action_30
action_73 (27) = happyShift action_5
action_73 (39) = happyShift action_31
action_73 (9) = happyGoto action_62
action_73 (12) = happyGoto action_110
action_73 (14) = happyGoto action_28
action_73 (21) = happyGoto action_29
action_73 _ = happyFail

action_74 (27) = happyShift action_14
action_74 (28) = happyShift action_15
action_74 (30) = happyShift action_16
action_74 (31) = happyShift action_17
action_74 (32) = happyShift action_18
action_74 (39) = happyShift action_19
action_74 (8) = happyGoto action_109
action_74 (14) = happyGoto action_11
action_74 (15) = happyGoto action_12
action_74 (16) = happyGoto action_13
action_74 _ = happyFail

action_75 _ = happyReduce_48

action_76 _ = happyReduce_47

action_77 (59) = happyShift action_60
action_77 (60) = happyShift action_61
action_77 _ = happyReduce_46

action_78 (59) = happyShift action_60
action_78 (60) = happyShift action_61
action_78 _ = happyReduce_45

action_79 (57) = happyShift action_58
action_79 (58) = happyShift action_59
action_79 (59) = happyShift action_60
action_79 (60) = happyShift action_61
action_79 _ = happyReduce_44

action_80 (57) = happyShift action_58
action_80 (58) = happyShift action_59
action_80 (59) = happyShift action_60
action_80 (60) = happyShift action_61
action_80 _ = happyReduce_43

action_81 (55) = happyShift action_56
action_81 (56) = happyShift action_57
action_81 (57) = happyShift action_58
action_81 (58) = happyShift action_59
action_81 (59) = happyShift action_60
action_81 (60) = happyShift action_61
action_81 _ = happyReduce_33

action_82 (55) = happyShift action_56
action_82 (56) = happyShift action_57
action_82 (57) = happyShift action_58
action_82 (58) = happyShift action_59
action_82 (59) = happyShift action_60
action_82 (60) = happyShift action_61
action_82 _ = happyReduce_32

action_83 (43) = happyShift action_54
action_83 (44) = happyShift action_55
action_83 (55) = happyShift action_56
action_83 (56) = happyShift action_57
action_83 (57) = happyShift action_58
action_83 (58) = happyShift action_59
action_83 (59) = happyShift action_60
action_83 (60) = happyShift action_61
action_83 _ = happyReduce_31

action_84 (43) = happyShift action_54
action_84 (44) = happyShift action_55
action_84 (55) = happyShift action_56
action_84 (56) = happyShift action_57
action_84 (57) = happyShift action_58
action_84 (58) = happyShift action_59
action_84 (59) = happyShift action_60
action_84 (60) = happyShift action_61
action_84 _ = happyReduce_30

action_85 _ = happyReduce_7

action_86 _ = happyReduce_35

action_87 (26) = happyShift action_30
action_87 (27) = happyShift action_38
action_87 (28) = happyShift action_15
action_87 (29) = happyShift action_39
action_87 (30) = happyShift action_16
action_87 (31) = happyShift action_17
action_87 (39) = happyShift action_40
action_87 (53) = happyShift action_41
action_87 (54) = happyShift action_42
action_87 (61) = happyShift action_43
action_87 (15) = happyGoto action_33
action_87 (16) = happyGoto action_13
action_87 (17) = happyGoto action_108
action_87 (19) = happyGoto action_35
action_87 (20) = happyGoto action_36
action_87 (21) = happyGoto action_37
action_87 _ = happyFail

action_88 _ = happyReduce_39

action_89 (26) = happyShift action_30
action_89 (27) = happyShift action_38
action_89 (28) = happyShift action_15
action_89 (29) = happyShift action_39
action_89 (30) = happyShift action_16
action_89 (31) = happyShift action_17
action_89 (39) = happyShift action_40
action_89 (53) = happyShift action_41
action_89 (54) = happyShift action_42
action_89 (61) = happyShift action_43
action_89 (15) = happyGoto action_33
action_89 (16) = happyGoto action_13
action_89 (17) = happyGoto action_107
action_89 (19) = happyGoto action_35
action_89 (20) = happyGoto action_36
action_89 (21) = happyGoto action_37
action_89 _ = happyFail

action_90 (26) = happyShift action_30
action_90 (27) = happyShift action_38
action_90 (28) = happyShift action_15
action_90 (29) = happyShift action_39
action_90 (30) = happyShift action_16
action_90 (31) = happyShift action_17
action_90 (39) = happyShift action_40
action_90 (53) = happyShift action_41
action_90 (54) = happyShift action_42
action_90 (61) = happyShift action_43
action_90 (15) = happyGoto action_33
action_90 (16) = happyGoto action_13
action_90 (17) = happyGoto action_106
action_90 (19) = happyGoto action_35
action_90 (20) = happyGoto action_36
action_90 (21) = happyGoto action_37
action_90 _ = happyFail

action_91 _ = happyReduce_61

action_92 (40) = happyShift action_105
action_92 _ = happyFail

action_93 _ = happyReduce_62

action_94 (27) = happyShift action_5
action_94 (32) = happyShift action_93
action_94 (35) = happyShift action_94
action_94 (36) = happyShift action_95
action_94 (45) = happyShift action_104
action_94 (51) = happyShift action_100
action_94 (52) = happyShift action_101
action_94 (14) = happyGoto action_91
action_94 (22) = happyGoto action_102
action_94 (23) = happyGoto action_103
action_94 _ = happyFail

action_95 (27) = happyShift action_5
action_95 (32) = happyShift action_93
action_95 (35) = happyShift action_94
action_95 (36) = happyShift action_95
action_95 (45) = happyShift action_99
action_95 (51) = happyShift action_100
action_95 (52) = happyShift action_101
action_95 (14) = happyGoto action_91
action_95 (22) = happyGoto action_97
action_95 (23) = happyGoto action_98
action_95 _ = happyFail

action_96 _ = happyReduce_11

action_97 (37) = happyShift action_122
action_97 _ = happyFail

action_98 (37) = happyShift action_121
action_98 _ = happyFail

action_99 (26) = happyShift action_30
action_99 (21) = happyGoto action_115
action_99 (24) = happyGoto action_116
action_99 (25) = happyGoto action_120
action_99 _ = happyFail

action_100 _ = happyReduce_53

action_101 _ = happyReduce_54

action_102 (37) = happyShift action_119
action_102 _ = happyFail

action_103 (37) = happyShift action_118
action_103 _ = happyFail

action_104 (26) = happyShift action_30
action_104 (21) = happyGoto action_115
action_104 (24) = happyGoto action_116
action_104 (25) = happyGoto action_117
action_104 _ = happyFail

action_105 (27) = happyShift action_14
action_105 (28) = happyShift action_15
action_105 (30) = happyShift action_16
action_105 (31) = happyShift action_17
action_105 (32) = happyShift action_18
action_105 (39) = happyShift action_19
action_105 (8) = happyGoto action_114
action_105 (14) = happyGoto action_11
action_105 (15) = happyGoto action_12
action_105 (16) = happyGoto action_13
action_105 _ = happyFail

action_106 (41) = happyShift action_52
action_106 (42) = happyShift action_53
action_106 (43) = happyShift action_54
action_106 (44) = happyShift action_55
action_106 (55) = happyShift action_56
action_106 (56) = happyShift action_57
action_106 (57) = happyShift action_58
action_106 (58) = happyShift action_59
action_106 (59) = happyShift action_60
action_106 (60) = happyShift action_61
action_106 (63) = happyShift action_113
action_106 _ = happyFail

action_107 (41) = happyShift action_52
action_107 (42) = happyShift action_53
action_107 (43) = happyShift action_54
action_107 (44) = happyShift action_55
action_107 (55) = happyShift action_56
action_107 (56) = happyShift action_57
action_107 (57) = happyShift action_58
action_107 (58) = happyShift action_59
action_107 (59) = happyShift action_60
action_107 (60) = happyShift action_61
action_107 _ = happyReduce_38

action_108 (41) = happyShift action_52
action_108 (42) = happyShift action_53
action_108 (43) = happyShift action_54
action_108 (44) = happyShift action_55
action_108 (55) = happyShift action_56
action_108 (56) = happyShift action_57
action_108 (57) = happyShift action_58
action_108 (58) = happyShift action_59
action_108 (59) = happyShift action_60
action_108 (60) = happyShift action_61
action_108 _ = happyReduce_37

action_109 _ = happyReduce_21

action_110 _ = happyReduce_22

action_111 _ = happyReduce_19

action_112 _ = happyReduce_18

action_113 (26) = happyShift action_30
action_113 (27) = happyShift action_38
action_113 (28) = happyShift action_15
action_113 (29) = happyShift action_39
action_113 (30) = happyShift action_16
action_113 (31) = happyShift action_17
action_113 (39) = happyShift action_40
action_113 (53) = happyShift action_41
action_113 (54) = happyShift action_42
action_113 (61) = happyShift action_43
action_113 (15) = happyGoto action_33
action_113 (16) = happyGoto action_13
action_113 (17) = happyGoto action_131
action_113 (19) = happyGoto action_35
action_113 (20) = happyGoto action_36
action_113 (21) = happyGoto action_37
action_113 _ = happyFail

action_114 _ = happyReduce_12

action_115 (50) = happyShift action_130
action_115 _ = happyFail

action_116 _ = happyReduce_65

action_117 (46) = happyShift action_129
action_117 (48) = happyShift action_126
action_117 _ = happyFail

action_118 (27) = happyShift action_5
action_118 (32) = happyShift action_93
action_118 (35) = happyShift action_94
action_118 (36) = happyShift action_95
action_118 (14) = happyGoto action_91
action_118 (23) = happyGoto action_128
action_118 _ = happyFail

action_119 (27) = happyShift action_5
action_119 (32) = happyShift action_93
action_119 (35) = happyShift action_94
action_119 (36) = happyShift action_95
action_119 (14) = happyGoto action_91
action_119 (23) = happyGoto action_127
action_119 _ = happyFail

action_120 (46) = happyShift action_125
action_120 (48) = happyShift action_126
action_120 _ = happyFail

action_121 (27) = happyShift action_5
action_121 (32) = happyShift action_93
action_121 (35) = happyShift action_94
action_121 (36) = happyShift action_95
action_121 (14) = happyGoto action_91
action_121 (23) = happyGoto action_124
action_121 _ = happyFail

action_122 (27) = happyShift action_5
action_122 (32) = happyShift action_93
action_122 (35) = happyShift action_94
action_122 (36) = happyShift action_95
action_122 (14) = happyGoto action_91
action_122 (23) = happyGoto action_123
action_122 _ = happyFail

action_123 _ = happyReduce_57

action_124 _ = happyReduce_58

action_125 _ = happyReduce_60

action_126 (26) = happyShift action_30
action_126 (21) = happyGoto action_115
action_126 (24) = happyGoto action_133
action_126 _ = happyFail

action_127 _ = happyReduce_55

action_128 _ = happyReduce_56

action_129 _ = happyReduce_59

action_130 (27) = happyShift action_5
action_130 (32) = happyShift action_93
action_130 (35) = happyShift action_94
action_130 (36) = happyShift action_95
action_130 (14) = happyGoto action_91
action_130 (23) = happyGoto action_132
action_130 _ = happyFail

action_131 (41) = happyShift action_52
action_131 (42) = happyShift action_53
action_131 (43) = happyShift action_54
action_131 (44) = happyShift action_55
action_131 (55) = happyShift action_56
action_131 (56) = happyShift action_57
action_131 (57) = happyShift action_58
action_131 (58) = happyShift action_59
action_131 (59) = happyShift action_60
action_131 (60) = happyShift action_61
action_131 _ = happyReduce_34

action_132 _ = happyReduce_63

action_133 _ = happyReduce_64

happyReduce_1 = happyMonadReduce 1 4 happyReduction_1
happyReduction_1 ((HappyAbsSyn5  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ Program (reverse happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn4 r))

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 ([happy_var_1]
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_2:happy_var_1
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happyMonadReduce 3 6 happyReduction_4
happyReduction_4 ((HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ ProcDecl happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn6 r))

happyReduce_5 = happyMonadReduce 3 7 happyReduction_5
happyReduction_5 ((HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ Par happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_6 = happySpecReduce_1  7 happyReduction_6
happyReduction_6 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happyMonadReduce 5 8 happyReduction_7
happyReduction_7 ((HappyAbsSyn7  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ Send happy_var_1 happy_var_3 happy_var_5)
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_8 = happyMonadReduce 5 8 happyReduction_8
happyReduction_8 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ Recv happy_var_1 (reverse happy_var_4))
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_9 = happyMonadReduce 3 8 happyReduction_9
happyReduction_9 ((HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ Recv happy_var_1 [happy_var_3])
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_10 = happyMonadReduce 1 8 happyReduction_10
happyReduction_10 (_ `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ End)
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_11 = happyMonadReduce 5 8 happyReduction_11
happyReduction_11 ((HappyAbsSyn7  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ Nu happy_var_3 Nothing happy_var_5)
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_12 = happyMonadReduce 7 8 happyReduction_12
happyReduction_12 ((HappyAbsSyn7  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ Nu happy_var_3 (Just happy_var_5) happy_var_7)
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_13 = happyMonadReduce 1 8 happyReduction_13
happyReduction_13 ((HappyAbsSyn14  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ Call happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_14 = happySpecReduce_3  8 happyReduction_14
happyReduction_14 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happyMonadReduce 1 9 happyReduction_15
happyReduction_15 ((HappyAbsSyn14  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ PtrnName happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn9 r))

happyReduce_16 = happyMonadReduce 3 9 happyReduction_16
happyReduction_16 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ PtrnTuple (reverse happy_var_2))
	) (\r -> happyReturn (HappyAbsSyn9 r))

happyReduce_17 = happyMonadReduce 1 9 happyReduction_17
happyReduction_17 ((HappyAbsSyn21  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ PtrnLabel happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn9 r))

happyReduce_18 = happySpecReduce_3  10 happyReduction_18
happyReduction_18 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_3 : happy_var_1
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  10 happyReduction_19
happyReduction_19 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn10
		 ([ happy_var_3, happy_var_1 ]
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happyMonadReduce 3 11 happyReduction_20
happyReduction_20 ((HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( locate $  Clause happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn11 r))

happyReduce_21 = happyMonadReduce 3 12 happyReduction_21
happyReduction_21 ((HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( locate $  Clause happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn11 r))

happyReduce_22 = happySpecReduce_3  13 happyReduction_22
happyReduction_22 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_3 : happy_var_1
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  13 happyReduction_23
happyReduction_23 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn13
		 ([ happy_var_1 ]
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happyMonadReduce 1 14 happyReduction_24
happyReduction_24 ((HappyTerminal (TokenNamePos happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ SimpName happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn14 r))

happyReduce_25 = happyMonadReduce 1 15 happyReduction_25
happyReduction_25 ((HappyTerminal (TokenNamePos happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ Positive happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn15 r))

happyReduce_26 = happyMonadReduce 1 15 happyReduction_26
happyReduction_26 ((HappyTerminal (TokenNameNeg happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ Negative happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn15 r))

happyReduce_27 = happyMonadReduce 1 15 happyReduction_27
happyReduction_27 ((HappyAbsSyn16  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ Reserved happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn15 r))

happyReduce_28 = happySpecReduce_1  16 happyReduction_28
happyReduction_28 _
	 =  HappyAbsSyn16
		 ("StdOut"
	)

happyReduce_29 = happySpecReduce_1  16 happyReduction_29
happyReduction_29 _
	 =  HappyAbsSyn16
		 ("StdIn"
	)

happyReduce_30 = happyMonadReduce 3 17 happyReduction_30
happyReduction_30 ((HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ Add happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn17 r))

happyReduce_31 = happyMonadReduce 3 17 happyReduction_31
happyReduction_31 ((HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ Sub happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn17 r))

happyReduce_32 = happyMonadReduce 3 17 happyReduction_32
happyReduction_32 ((HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ Mul happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn17 r))

happyReduce_33 = happyMonadReduce 3 17 happyReduction_33
happyReduction_33 ((HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ Div happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn17 r))

happyReduce_34 = happyMonadReduce 6 17 happyReduction_34
happyReduction_34 ((HappyAbsSyn17  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ IfThenElse happy_var_2 happy_var_4 happy_var_6)
	) (\r -> happyReturn (HappyAbsSyn17 r))

happyReduce_35 = happyMonadReduce 3 17 happyReduction_35
happyReduction_35 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ ExprTuple (reverse happy_var_2))
	) (\r -> happyReturn (HappyAbsSyn17 r))

happyReduce_36 = happySpecReduce_1  17 happyReduction_36
happyReduction_36 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  18 happyReduction_37
happyReduction_37 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_3 : happy_var_1
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  18 happyReduction_38
happyReduction_38 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn18
		 ([ happy_var_3 , happy_var_1 ]
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  19 happyReduction_39
happyReduction_39 _
	(HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn17
		 (happy_var_2
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happyMonadReduce 1 19 happyReduction_40
happyReduction_40 ((HappyAbsSyn15  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ ExprName happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn17 r))

happyReduce_41 = happyMonadReduce 1 19 happyReduction_41
happyReduction_41 ((HappyTerminal (TokenInt happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ ExprDigit happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn17 r))

happyReduce_42 = happyMonadReduce 1 19 happyReduction_42
happyReduction_42 ((HappyAbsSyn21  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ ExprLabel happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn17 r))

happyReduce_43 = happyMonadReduce 3 19 happyReduction_43
happyReduction_43 ((HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ EQ  happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn17 r))

happyReduce_44 = happyMonadReduce 3 19 happyReduction_44
happyReduction_44 ((HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ NEQ happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn17 r))

happyReduce_45 = happyMonadReduce 3 19 happyReduction_45
happyReduction_45 ((HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ GT  happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn17 r))

happyReduce_46 = happyMonadReduce 3 19 happyReduction_46
happyReduction_46 ((HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ GTE happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn17 r))

happyReduce_47 = happyMonadReduce 3 19 happyReduction_47
happyReduction_47 ((HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ LT  happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn17 r))

happyReduce_48 = happyMonadReduce 3 19 happyReduction_48
happyReduction_48 ((HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ LTE happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn17 r))

happyReduce_49 = happySpecReduce_1  19 happyReduction_49
happyReduction_49 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happyMonadReduce 1 20 happyReduction_50
happyReduction_50 (_ `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ ExprBool True)
	) (\r -> happyReturn (HappyAbsSyn17 r))

happyReduce_51 = happyMonadReduce 1 20 happyReduction_51
happyReduction_51 (_ `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ ExprBool False)
	) (\r -> happyReturn (HappyAbsSyn17 r))

happyReduce_52 = happyMonadReduce 1 21 happyReduction_52
happyReduction_52 ((HappyTerminal (TokenLabel happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ Label happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn21 r))

happyReduce_53 = happyMonadReduce 1 22 happyReduction_53
happyReduction_53 (_ `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ SortInt)
	) (\r -> happyReturn (HappyAbsSyn22 r))

happyReduce_54 = happyMonadReduce 1 22 happyReduction_54
happyReduction_54 (_ `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ SortBool)
	) (\r -> happyReturn (HappyAbsSyn22 r))

happyReduce_55 = happyMonadReduce 4 23 happyReduction_55
happyReduction_55 ((HappyAbsSyn23  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ TypeSend (Left  happy_var_2) happy_var_4)
	) (\r -> happyReturn (HappyAbsSyn23 r))

happyReduce_56 = happyMonadReduce 4 23 happyReduction_56
happyReduction_56 ((HappyAbsSyn23  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ TypeSend (Right happy_var_2) happy_var_4)
	) (\r -> happyReturn (HappyAbsSyn23 r))

happyReduce_57 = happyMonadReduce 4 23 happyReduction_57
happyReduction_57 ((HappyAbsSyn23  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ TypeRecv (Left  happy_var_2) happy_var_4)
	) (\r -> happyReturn (HappyAbsSyn23 r))

happyReduce_58 = happyMonadReduce 4 23 happyReduction_58
happyReduction_58 ((HappyAbsSyn23  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ TypeRecv (Right happy_var_2) happy_var_4)
	) (\r -> happyReturn (HappyAbsSyn23 r))

happyReduce_59 = happyMonadReduce 4 23 happyReduction_59
happyReduction_59 (_ `HappyStk`
	(HappyAbsSyn25  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ TypeSele (reverse happy_var_3))
	) (\r -> happyReturn (HappyAbsSyn23 r))

happyReduce_60 = happyMonadReduce 4 23 happyReduction_60
happyReduction_60 (_ `HappyStk`
	(HappyAbsSyn25  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ TypeChoi (reverse happy_var_3))
	) (\r -> happyReturn (HappyAbsSyn23 r))

happyReduce_61 = happyMonadReduce 1 23 happyReduction_61
happyReduction_61 ((HappyAbsSyn14  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ TypeCall happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn23 r))

happyReduce_62 = happyMonadReduce 1 23 happyReduction_62
happyReduction_62 (_ `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ TypeEnd)
	) (\r -> happyReturn (HappyAbsSyn23 r))

happyReduce_63 = happyMonadReduce 3 24 happyReduction_63
happyReduction_63 ((HappyAbsSyn23  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( locate $ TypeOfLabel happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn24 r))

happyReduce_64 = happySpecReduce_3  25 happyReduction_64
happyReduction_64 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_3 : happy_var_1
	)
happyReduction_64 _ _ _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_1  25 happyReduction_65
happyReduction_65 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn25
		 ([ happy_var_1 ]
	)
happyReduction_65 _  = notHappyAtAll 

happyNewToken action sts stk
	= scan(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	TokenEOF -> action 64 64 tk (HappyState action) sts stk;
	TokenLabel happy_dollar_dollar -> cont 26;
	TokenNamePos happy_dollar_dollar -> cont 27;
	TokenNameNeg happy_dollar_dollar -> cont 28;
	TokenInt happy_dollar_dollar -> cont 29;
	TokenStdOut -> cont 30;
	TokenStdIn -> cont 31;
	TokenEnd -> cont 32;
	TokenNu -> cont 33;
	TokenDefn -> cont 34;
	TokenSend -> cont 35;
	TokenRecv -> cont 36;
	TokenSeq -> cont 37;
	TokenPar -> cont 38;
	TokenParenStart -> cont 39;
	TokenParenEnd -> cont 40;
	TokenAdd -> cont 41;
	TokenSub -> cont 42;
	TokenMul -> cont 43;
	TokenDiv -> cont 44;
	TokenBraceStart -> cont 45;
	TokenBraceEnd -> cont 46;
	TokenComma -> cont 47;
	TokenSemi -> cont 48;
	TokenArrow -> cont 49;
	TokenTypeOf -> cont 50;
	TokenSortInt -> cont 51;
	TokenSortBool -> cont 52;
	TokenTrue -> cont 53;
	TokenFalse -> cont 54;
	TokenEQ -> cont 55;
	TokenNEQ -> cont 56;
	TokenGT -> cont 57;
	TokenGTE -> cont 58;
	TokenLT -> cont 59;
	TokenLTE -> cont 60;
	TokenIf -> cont 61;
	TokenThen -> cont 62;
	TokenElse -> cont 63;
	_ -> happyError' tk
	})

happyError_ 64 tk = happyError' tk
happyError_ _ tk = happyError' tk

happyThen :: () => Parser a -> (a -> Parser b) -> Parser b
happyThen = (>>=)
happyReturn :: () => a -> Parser a
happyReturn = (return)
happyThen1 = happyThen
happyReturn1 :: () => a -> Parser a
happyReturn1 = happyReturn
happyError' :: () => (Token) -> Parser a
happyError' tk = syntaticalError tk

piParser = happySomeParser where
  happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq



{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 16 "<built-in>" #-}
{-# LINE 1 "/Library/Frameworks/GHC.framework/Versions/8.0.1-x86_64/usr/lib/ghc-8.0.1/include/ghcversion.h" #-}


















{-# LINE 17 "<built-in>" #-}
{-# LINE 1 "/tmp/ghc28433_0/ghc_2.h" #-}







































































































































































































































































































































































































































































































































































































































































































{-# LINE 18 "<built-in>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 


{-# LINE 13 "templates/GenericTemplate.hs" #-}


{-# LINE 46 "templates/GenericTemplate.hs" #-}









{-# LINE 67 "templates/GenericTemplate.hs" #-}


{-# LINE 77 "templates/GenericTemplate.hs" #-}










infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action


{-# LINE 155 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







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

