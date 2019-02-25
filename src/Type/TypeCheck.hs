{-# LANGUAGE FlexibleContexts #-}

module Type.TypeCheck where

import Control.Arrow ((***))
import Control.Monad.Except
import Data.Text (pack)
import Data.Function ((&))

import Syntax.Abstract
import Type
import Utilities

type BEnv = FMap RName BType

liftMaybe :: MonadError e m => e -> Maybe a -> m a
liftMaybe err = maybe (throwError err) return

type TMonad a = Except ErrMsg a

liftLookup :: (MonadError ErrMsg m, Eq a, Show a) => a -> FMap a b -> m b
liftLookup x env =
  liftMaybe ("variable " ++ show x ++ " not found") (lookup x env)

inferV :: MonadError ErrMsg m => BEnv -> Val -> m BType
inferV _ (N (NR _)) = throwError "StdOut/In in expression"
inferV env (N c) = liftMaybe ("variable " ++ show c ++ " not found")
                     (lookup (depolarCH c) env)
inferV _ (VI _) = return TInt
inferV _ (VB _) = return TBool
inferV env (VT vs) = TTuple <$> mapM (inferV env) vs
inferV env (VL l) = liftMaybe "label not found" (lookup l env)
inferV _ (VS _) = throwError "panic: not implemented yet"
-- inferV _ (VS _) = return TString

inferE :: MonadError ErrMsg m => BEnv -> Expr -> m BType
inferE env (EV v) = inferV env v
inferE env (EAdd e1 e2) =
  checkE env e1 TInt >>
  checkE env e2 TInt >>
  return TInt
inferE env (ESub e1 e2) =
  checkE env e1 TInt >>
  checkE env e2 TInt >>
  return TInt
inferE env (EIf e0 e1 e2) =
  checkE env e0 TBool >>
  inferE env e1 >>= \t ->
  checkE env e2 t >>
  return t
inferE env (ETup es) =
  TTuple <$> mapM (inferE env) es
inferE _ _ = throwError "panic: not implemented yet"

checkE :: MonadError ErrMsg m =>
          BEnv -> Expr -> BType -> m ()
checkE env e t = inferE env e >>= tcheck t

tcheck :: MonadError ErrMsg m => BType -> BType -> m ()
tcheck t1 t2
   | t1 == t2 = return ()
   | otherwise = throwError ("expected: " ++ show t1 ++
                             ", found " ++ show t2)

type SEnv = FMap (PN RName) SType

checkCh :: MonadError ErrMsg m => SEnv -> PN RName -> SType -> m ()
checkCh senv c t =
  liftLookup c senv >>= \t' ->
  if t == t' then return ()
    else throwError "type mismatch"

allClosed :: SEnv -> Bool
allClosed = all (TEnd ==) . map snd
        -- = const True

checkPi :: MonadError ErrMsg m =>
   BEnv -> SEnv -> Pi -> m (SEnv, [PN RName])

checkPi benv senv End = return (senv, [])

checkPi benv senv (p1 `Par` p2) =
  checkPi benv senv p1 >>= \(senv1, l1) ->
  senv1 `sMinus` l1 >>= \senv2 ->
  checkPi benv senv2 p2

checkPi benv senv (Send (NR StdOut) e p) =
  inferE benv e >>= \_ ->
  checkPi benv senv p
checkPi benv senv (Send (NR _) e p) =
  throwError "cannot send to this channel"
checkPi benv senv (Send (NG _) e p) =
  throwError "generated name appears in type checking. a bug?"
checkPi benv senv (Send (ND c) e p) =
  lookupChType c senv >>=  -- c removed from senv
  matchSend (checkPiSend benv (c,e,p)) (checkPiSel benv e p)

checkPi _ _ (Recv (NR _) _) =
  throwError "not knowing what to do yet"
checkPi _ _ (Recv (NG _) _) =
  throwError "generated name appears in type checking. a bug?"
checkPi benv senv (Recv (ND c) ps) =
  lookupChType c senv >>=
  matchRecv (checkPiRecv benv (c,ps)) undefined

checkPiSend :: MonadError ErrMsg m =>
  BEnv -> (PN RName, Expr, Pi) ->
  SType -> SType -> Bool -> SEnv -> m (SEnv, [PN RName])
checkPiSend benv (c,e,p) (TBase s) t un senv =
  checkE benv e s >>
  (id *** addLin un c) <$> checkPi benv ((c,t):senv) p
checkPiSend benv (c,e,p) s t un senv =
  isChannel e >>= \d ->
  lookupChType d senv >>= \(s', _, senv') ->
  if eqType s s' then
     addSEnv (c,t) senv' >>= \senv'' ->
     (id *** addLin un c) <$> checkPi benv senv'' p
   else throwError ("types of sent channel mismatch: " ++ show s ++ " and " ++ show s')

addLin True  _ xs  = xs
addLin False c xs = nubcons c xs

isChannel :: MonadError ErrMsg m => Expr -> m (PN RName)
isChannel (EV (N (ND c))) = return c
isChannel _ = throwError "thrown value must be a channel"

checkPiSel :: MonadError ErrMsg m =>
  BEnv -> Expr -> Pi ->
  [(Label, SType)] -> Bool -> SEnv -> m (SEnv, [PN RName])
checkPiSel = undefined

checkPiRecv :: MonadError ErrMsg m =>
  BEnv -> (PN RName, [Clause]) ->
  SType -> SType -> Bool -> SEnv -> m (SEnv, [PN RName])
checkPiRecv benv (c,ps) s t un senv = undefined

matchSend :: MonadError ErrMsg m =>
   (SType -> SType -> Bool -> SEnv -> m a) ->
   ([(Label, SType)] -> Bool -> SEnv -> m a) -> (SType, Bool, SEnv) -> m a
matchSend fs _  (TSend s1 s2, b, senv) = fs s1 s2 b senv
matchSend _  fl (TSele ts,    b, senv) = fl ts b senv
matchSend _  _  _ = throwError "TSend expected"

matchRecv :: MonadError ErrMsg m =>
  (SType -> SType -> Bool -> SEnv -> m a) ->
  ([(Label, SType)] -> Bool -> SEnv -> m a) -> (SType, Bool, SEnv) -> m a
matchRecv fs _  (TRecv t s, b, senv) = fs t s b senv
matchRecv _  fl (TChoi ts,  b, senv) = fl ts b senv
matchRecv _  _  _ = throwError "TRecv expected"

-- env related utilities

sMinus :: MonadError ErrMsg m => SEnv -> [PN RName] -> m SEnv
senv `sMinus` []     = return senv
senv `sMinus` (x:xs) =
  senv `sMinus` xs >>= \senv' ->
  liftLookup x senv' >>= \t ->
  if unrestricted t then return (rmEntry x senv)
    else throwError ("channel " ++ show x ++ " not used up.")

lookupChType :: MonadError ErrMsg m =>
  PN RName -> SEnv -> m (SType, Bool, SEnv)
lookupChType x senv =
  liftLookup x senv >>= \t ->
  stripUnrest t & \(t', unrest) ->
  if unrest then return (t', unrest, senv)
      else return (t', unrest, rmEntry x senv)

addSEnv :: MonadError ErrMsg m =>
  (PN RName, SType) -> SEnv -> m SEnv
addSEnv (c,t) senv =
  lookup c senv &
   maybe (return ((c,t) : senv))
     (\t' -> if eqType t t' then return senv
              else throwError ("types of channels in env mismatch: " ++ show t ++ " and " ++ show t'))

{-
checkPi :: MonadError ErrMsg m => BEnv -> SEnv -> Pi -> m ()

checkPi _ senv End
  | allClosed senv = return ()
  | otherwise      = throwError "not all channels closed"

checkPi benv senv (Send (NR StdOut) e p) =
  inferE benv e >>= \_ ->
  checkPi benv senv p
checkPi _ _ (Send (NR _) _ _) =
  throwError "cannot send to this channel"
checkPi _ _ (Send (NG _) _ _) =
  throwError "generated name appears in type checking. a bug?"
checkPi benv senv (Send (ND c) e p) =
  liftLookup c senv >>= matchSend
    (\t s -> checkE benv e t  >>
             checkPi benv (fMapPut c s senv) p)
    (\t s -> isChannel e >>= \d ->
             checkCh senv d t >>
             checkPi benv (rmEntry d (fMapPut c s senv)) p)
    (\ts -> isLabel e >>= \l ->
            liftLookup l ts >>= \s ->
            checkPi benv (fMapPut c s senv) p)

checkPi _ _ (Recv (NR _) _) =
  throwError "not knowing what to do yet"
checkPi _ _ (Recv (NG _) _) =
  throwError "generated name appears in type checking. a bug?"
checkPi benv senv (Recv (ND c) ps) =
  liftLookup c senv >>= matchRecv
    (\t s -> mapM_ (checkBClause benv senv c t s) ps)
    (\t s -> mapM_ (checkSClause benv senv c t s) ps)
    (\ts -> pairChoices ts ps >>=
            mapM_ (\(s,p) -> checkPi benv (fMapPut c s senv) p))
checkPi _ _ (Repl _) = throwError "panic: not implemented yet"
checkPi _ _ (Call _) = throwError "panic: not implemented yet"

checkPi benv senv (p `Par` q) =
  splitSEnv f1 f2 senv >>= \(senv1, senv2) ->
  checkPi benv senv1 p >>
  checkPi benv senv2 q
 where (f1, f2) = (freePi p, freePi q)

checkPi _ _ (Nu _ Nothing _) =
  throwError "needing a type for new channels"
checkPi benv senv (Nu x (Just t) p) =
  checkPi benv ((Pos x, t):(Neg x, dual t):senv) p

checkBClause :: MonadError ErrMsg m =>
   BEnv -> SEnv -> PN RName -> BType -> SType -> Clause -> m ()
checkBClause benv senv c t s (Clause ptn p) =
  checkPtrn ptn t >>= \eta ->
  checkPi (eta ++ benv) (fMapPut c s senv) p

checkSClause :: MonadError ErrMsg m =>
   BEnv -> SEnv -> PN RName -> SType -> SType -> Clause -> m ()
checkSClause benv senv c t s (Clause (PN d) p) =
  checkPi benv ((Pos d,t) : fMapPut c s senv) p
checkSClause _ _ _ _ _ (Clause _ _) =
  throwError "channels cannot be pattern matched"

matchSend :: MonadError ErrMsg m =>
   (BType -> SType -> m a) ->
   (SType -> SType -> m a) ->
   ([(Label, SType)] -> m a) -> SType -> m a
matchSend fb _  _   (TSend (Left t) s) = fb t s
matchSend _  fs _   (TSend (Right s') s) = fs s' s
matchSend _  _  fsl (TSele ts) = fsl ts
matchSend _  _  _   _ = throwError "TSend expected"

isLabel :: MonadError ErrMsg m => Expr -> m Label
isLabel (EV (VL l)) = return l
isLabel _ = throwError "must be a label"

matchRecv ::  MonadError ErrMsg m =>
  (BType -> SType -> m a) ->
  (SType -> SType -> m a) ->
  ([(Label, SType)] -> m a) -> SType -> m a
matchRecv fb _  _   (TRecv (Left t) s) = fb t s
matchRecv _  fs _   (TRecv (Right t) s) = fs t s
matchRecv _  _  fsl (TChoi ts) = fsl ts
matchRecv _  _  _   _ = throwError "TRecv expected"

checkPtrn :: MonadError ErrMsg m => Ptrn -> BType -> m BEnv
checkPtrn (PN x) t = return [(x,t)]
checkPtrn (PL _) _ = undefined -- to do later
checkPtrn (PT xs) (TTuple ts)
  | length xs == length ts =
    concat <$> mapM (uncurry checkPtrn) (zip xs ts)
checkPtrn _ _ = throwError "pattern fails to type check"

splitSEnv :: MonadError ErrMsg m =>
  [PN RName] -> [PN RName] -> SEnv -> m (SEnv, SEnv)
splitSEnv _ _ [] = return ([],[])
splitSEnv fl fr ((x,t):xs)
   | bl && br  = throwError ("channel " ++ show x ++ " not linear")
   | bl        = (((x,t):) *** id) <$> splitSEnv fl fr xs
   | br        = (id *** ((x,t):)) <$> splitSEnv fl fr xs
   | otherwise = throwError ("channel " ++ show x ++ " dropped")
  where (bl, br) = (x `elem` fl, x `elem` fr)

pairChoices :: MonadError ErrMsg m =>
  [(Label, SType)] -> [Clause] -> m [(SType, Pi)]
pairChoices [] [] = return []
pairChoices _ [] =
  throwError "choices not fully covered"
pairChoices ts (Clause (PL l) p : ps) =
  liftLookup l ts >>= \s ->
  ((s,p):) <$> pairChoices (rmEntry l ts) ps
pairChoices _ (Clause _ _ : _) =
  throwError "not a label"

-}

--------------------------------------------------------------------------------
-- Tests

-- smart constructors for Types
tsend :: BType -> SType -> SType
tsend t s = TSend (TBase t) s

trecv :: BType -> SType -> SType
trecv t s = TRecv (TBase t) s

-- tsele :: [(String, SType)] -> SType
-- tsele = TSele . map (pack *** id)

-- smart constructors for Pi
nu :: String -> SType -> Pi -> Pi
nu c t p = Nu (pack c) (Just t) p

recv :: Name -> Ptrn -> Pi -> Pi
recv c ptn p = Recv c [Clause ptn p]

choices :: Name -> [(String, Pi)] -> Pi
choices c ps =
   Recv c (map (uncurry Clause . ((PL . pack) *** id)) ps)

pn :: String -> Ptrn
pn = PN . pack

--

type0 = tsend TInt (tsend TBool TEnd)
pi0 = Send (cP "c") (eI 3) (Send (cP "c") (eB True) End)

test0 :: Either String (SEnv, [PN RName])
test0 = runExcept $ checkPi [] senv p
  where senv = [(Pos . pack $ "c", type0),
                (Pos . pack $ "d", TSend (tsend TBool TEnd) TEnd)]
        p = Send (cP "c") (eI 3) $ Send (cP "d") (ePN "c") End
{-
test0 :: Either String ()
test0 = runExcept $ checkPi [] [] p2
  where
    t0 :: SType
    t0 = tsele [("NEG", tsend TInt  $ trecv TInt  $ TEnd),
                ("ID",  tsend TBool $ trecv TBool $ TEnd)]

    t1 :: SType
    t1 = tRecv t0 TEnd

    -- senv0 :: SEnv
    -- senv0 = [(Pos (pack "c"), t1),
    --          (Neg (pack "c"), dual t1),
    --          (Pos (pack "d"), t0),
    --          (Neg (pack "d"), dual t0)
    --          ]


    p0 :: Pi
    p0 = Send (cN "c") (ePN "d") $
          choices (cN "d")
            [("NEG", recv (cN "d") (pn "x") $
                       Send (cN "d") (eI 0 `ESub` ePN "x") End),
             ("ID", recv (cN "d") (pn "x") $
                        Send (cN "d") (ePN "x") End)]

    p1 :: Pi
    p1 = recv (cP "c") (pn "z") $
          Send (cP "z") (eL "NEG") $
           Send (cP "z") (eI 3) $
               recv (cP "z") (pn "w") End

    p2 :: Pi
    p2 = nu "c" t1 (nu "d" t0 p0 `Par` p1)

-- try: runExcept $ checkPi [] [] p2
-- try: run `stack test` for running these tests
-- try: run `stack repl pi:test:pi-tests` to develop thoses tests
-}
