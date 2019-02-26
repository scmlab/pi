module Type where

import Data.Text (Text)
import Control.Arrow ((***))

type TName = Text   -- names of declared types
type Label = Text

data BType = TInt | TBool
  deriving (Eq, Show)

data Type = TEnd                    -- end
           | TBase BType
           | TTuple [Type]
           | TSend Type Type       -- send
           | TRecv Type Type       -- recv
           | TSele [(Label, Type)]  -- select
           | TChoi [(Label, Type)]  -- choice
           | TUn Type               -- unrestricted
  deriving (Eq, Show)

tInt  = TBase TInt
tBool = TBase TBool

dual :: Type -> Type
dual TEnd = TEnd
dual (TBase t) = TBase t
dual (TTuple ts) = TTuple (map dual ts)
dual (TSend t s) = TRecv t (dual s)
dual (TRecv t s) = TSend t (dual s)
dual (TChoi ss) = TSele (map (id *** dual) ss)
dual (TSele ss) = TChoi (map (id *** dual) ss)
dual (TUn t) = TUn (dual t)

unrestricted :: Type -> Bool
unrestricted TEnd        = True
unrestricted (TBase _)   = True
unrestricted (TUn _)     = True
unrestricted (TTuple ts) = all unrestricted ts
unrestricted _           = False

stripUnres :: Type -> (Type, Bool)
stripUnres TEnd        = (TEnd, True)
stripUnres (TBase t)   = (TBase t, True)
stripUnres (TUn t)     = (t, True)  -- shouldn't be nested
stripUnres (TTuple ts) = (TTuple (map fst tts), and (map snd tts))
  where tts = map stripUnres ts
stripUnres t           = (t, False)

eqType :: Type -> Type -> Bool
eqType = (==)  -- needs fixing!
