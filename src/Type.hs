module Type where

import Data.Text (Text)
import Control.Arrow ((***))

type TName = String   -- names of declared types
type Label = Text

data BType = TInt | TBool
  deriving (Eq, Show, Ord)

data TypeVar = TypeVarIndex Int | TypeVarX
  deriving (Eq, Show, Ord)

data Type = TEnd                    -- end
           | TBase BType
           | TTuple [Type]
           | TSend Type Type       -- send
           | TRecv Type Type       -- recv
           | TSele [(Label, Type)]  -- select
           | TChoi [(Label, Type)]  -- choice
           | TUn Type               -- unrestricted

           | TVar TypeVar    -- de Bruin index?
           | TMu Type
  deriving (Eq, Show, Ord)

tInt  = TBase TInt
tBool = TBase TBool

dual :: Type -> Type
dual TEnd        = TEnd
dual (TBase t)   = TBase t
dual (TTuple ts) = TTuple (map dual ts)
dual (TSend t s) = TRecv t (dual s)
dual (TRecv t s) = TSend t (dual s)
dual (TChoi ss)  = TSele (map (id *** dual) ss)
dual (TSele ss)  = TChoi (map (id *** dual) ss)
dual (TUn t)     = TUn (dual t)
dual (TVar i)    = TVar i
dual (TMu t)     = TMu (dual t)

-- type substitution. it is assumed that s contains
--   no bound variables.

substT :: Int -> Type -> Type -> Type
substT _ _ TEnd        = TEnd
substT _ _ (TBase t)   = TBase t
substT i s (TTuple ts) = TTuple (map (substT i s) ts)
substT i s (TSend t u) = TSend (substT i s t) (substT i s u)
substT i s (TRecv t u) = TRecv (substT i s t) (substT i s u)
substT i s (TChoi ts)  = TChoi (map (id *** substT i s) ts)
substT i s (TSele ts)  = TSele (map (id *** substT i s) ts)
substT i s (TUn t)     = TUn (substT i s t)
substT i s (TVar j)    | TypeVarIndex i == j = s
                       | otherwise = TVar j
substT i s (TMu t)     = TMu (substT (1+i) s t)

unrestricted :: Type -> Bool
unrestricted TEnd        = True
unrestricted (TBase _)   = True
unrestricted (TUn _)     = True
unrestricted (TTuple ts) = all unrestricted ts
unrestricted (TMu ivy)   = unrestricted ivy
unrestricted _           = False

unfoldT :: Type -> Type
unfoldT (TMu t) = substT 0 (TMu t) t
unfoldT t = t

stripUnres :: Type -> (Type, Bool)
stripUnres TEnd        = (TEnd, True)
stripUnres (TBase t)   = (TBase t, True)
stripUnres (TUn t)     = (t, True)  -- shouldn't be nested
stripUnres (TTuple ts) = (TTuple (map fst tts), and (map snd tts))
  where tts = map stripUnres ts
stripUnres (TMu t)     = (TMu *** id) $ stripUnres t
stripUnres t           = (t, False)


eqType :: Type -> Type -> Bool
eqType = (==)
