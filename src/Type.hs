module Type where

import Data.Text (Text)
import Control.Arrow ((***))

type TName = Text   -- names of declared types
type Label = Text

data BType = TInt | TBool | TTuple [BType]
  deriving (Eq, Show)

data SType = TEnd
           | TBase BType
           | TSend SType SType       -- send
           | TRecv SType SType       -- recv
           | TSele [(Label, SType)]  -- select
           | TChoi [(Label, SType)]  -- choice
           | TUn SType               -- unrestricted
  deriving (Eq, Show)


dual :: SType -> SType
dual TEnd = TEnd
dual (TBase t) = TBase t
dual (TSend t s) = TRecv t (dual s)
dual (TRecv t s) = TSend t (dual s)
dual (TChoi ss) = TSele (map (id *** dual) ss)
dual (TSele ss) = TChoi (map (id *** dual) ss)
dual (TUn t) = TUn (dual t)

unrestricted :: SType -> Bool
unrestricted TEnd      = True
unrestricted (TBase _) = True
unrestricted (TUn _)   = True
unrestricted _         = False

stripUnrest :: SType -> (SType, Bool)
stripUnrest TEnd      = (TEnd, True)
stripUnrest (TBase t) = (TBase t, True)
stripUnrest (TUn t)   = (t, True)  -- shouldn't be nested
stripUnrest t         = (t, False)

eqType :: SType -> SType -> Bool
eqType = (==)  -- needs fixing!
