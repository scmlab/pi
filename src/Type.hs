module Type where

import Data.Text (Text, pack)
import Control.Arrow ((***))

type TName = Text   -- names of declared types

data BType = TInt | TBool | TTuple [BType]
  deriving (Eq, Show)

data SType = TEnd
           | TSend (Either BType SType) SType
           | TRecv (Either BType SType) SType
           | TChoi [(String, SType)]
           | TSele [(String, SType)]
           | TCall TName
  deriving (Eq, Show)

dual :: SType -> SType
dual TEnd = TEnd
dual (TSend t s) = TRecv t (dual s)
dual (TRecv t s) = TSend t (dual s)
dual (TChoi ss) = TSele (map (id *** dual) ss)
dual (TSele ss) = TChoi (map (id *** dual) ss)
dual (TCall t) = TCall t  --no!
