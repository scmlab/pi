{-# LANGUAGE FlexibleContexts #-}

module Type.TypeCheck where

import Control.Arrow ((***))
import Control.Monad.Except
import Data.Text (Text, pack)

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
inferV env (N (NR _)) = throwError "StdOut/In in expression"
inferV env (N c) = liftMaybe "variable not found"
                     (lookup (depolarCH c) env)
inferV env (VI _) = return TInt
inferV env (VB _) = return TBool
inferV env (VT vs) = TTuple <$> mapM (inferV env) vs
inferV env (VL l) = liftMaybe "label not found" (lookup l env)

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

checkPi :: MonadError ErrMsg m => BEnv -> SEnv -> Pi -> m ()

checkPi benv senv End
  | allClosed senv = return ()
  | otherwise      = throwError "not all channels closed"

checkPi benv senv (Send (NR StdOut) e p) =
  inferE benv e >>= \_ ->
  checkPi benv senv p
checkPi benv senv (Send (NR _) e p) =
  throwError "cannot send to this channel"
checkPi benv senv (Send (NG _) e p) =
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

checkPi benv senv (Recv (NR _) _) =
  throwError "not knowing what to do yet"
checkPi benv senv (Recv (NG _) _) =
  throwError "generated name appears in type checking. a bug?"
checkPi benv senv (Recv (ND c) ps) =
  liftLookup c senv >>= matchRecv
    (\t s -> mapM_ (checkBClause benv senv c t s) ps)
    (\t s -> mapM_ (checkSClause benv senv c t s) ps)
    (\ts -> pairChoices ts ps >>=
            mapM_ (\(s,p) -> checkPi benv (fMapPut c s senv) p))

checkPi benv senv (p1 `Par` p2) =
  splitSEnv f1 f2 senv >>= \(senv1, senv2) ->
  checkPi benv senv1 p1 >>
  checkPi benv senv2 p2
 where (f1, f2) = (freePi p1, freePi p2)

checkPi benv senv (Nu x Nothing p) =
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
checkSClause benv senv c t s (Clause _ p) =
  throwError "channels cannot be pattern matched"

matchSend :: MonadError ErrMsg m =>
   (BType -> SType -> m a) ->
   (SType -> SType -> m a) ->
   ([(Label, SType)] -> m a) -> SType -> m a
matchSend fb fs fsl (TSend (Left t) s) = fb t s
matchSend fb fs fsl (TSend (Right s') s) = fs s' s
matchSend fb fs fsl (TSele ts) = fsl ts
mathcSend fb fs fsl _ = throwError "TSend expected"

isChannel :: MonadError ErrMsg m => Expr -> m (PN RName)
isChannel (EV (N (ND c))) = return c
isChannel _ = throwError "must be a channel"

isLabel :: MonadError ErrMsg m => Expr -> m Label
isLabel (EV (VL l)) = return l
isLabel _ = throwError "must be a label"

matchRecv ::  MonadError ErrMsg m =>
  (BType -> SType -> m a) ->
  (SType -> SType -> m a) ->
  ([(Label, SType)] -> m a) -> SType -> m a
matchRecv fb fs fsl (TRecv (Left t) s) = fb t s
matchRecv fb fs fsl (TRecv (Right t) s) = fs t s
matchRecv fb fs fsl (TChoi ts) = fsl ts
matchRecv fb fs fsl _ = throwError "TRecv expected"

checkPtrn :: MonadError ErrMsg m => Ptrn -> BType -> m BEnv
checkPtrn (PN x) t = return [(x,t)]
checkPtrn (PL l) _ = undefined -- to do later
checkPtrn (PT xs) (TTuple ts)
  | length xs == length ts =
    concat <$> mapM (uncurry checkPtrn) (zip xs ts)
checkPtrn _ _ = throwError "pattern fails to type check"

splitSEnv :: MonadError ErrMsg m =>
  [PN RName] -> [PN RName] -> SEnv -> m (SEnv, SEnv)
splitSEnv fl fr [] = return ([],[])
splitSEnv fl fr ((x,t):xs)
   | bl && br  = throwError ("channel " ++ show x ++ " not linear")
   | bl        = (((x,t):) *** id) <$> splitSEnv fl fr xs
   | br        = (id *** ((x,t):)) <$> splitSEnv fl fr xs
   | otherwise = throwError ("channel " ++ show x ++ " dropped")
  where (bl, br) = (x `elem` fl, x `elem` fr)

pairChoices :: MonadError ErrMsg m =>
  [(Label, SType)] -> [Clause] -> m [(SType, Pi)]
pairChoices [] [] = return []
pairChoices ts [] =
  throwError "choices not fully covered"
pairChoices ts (Clause (PL l) p : ps) =
  liftLookup l ts >>= \s ->
  ((s,p):) <$> pairChoices (rmEntry l ts) ps
pairChoices ts (Clause _ _ : _) =
  throwError "not a label"

-- Tests

tsend t s = TSend (Left t) s
tSend t s = TSend (Right t) s
trecv t s = TRecv (Left t) s
tRecv t s = TRecv (Right t) s
tsele = TSele . map (pack *** id)

t0 = tsele [("NEG", tsend TInt  $ trecv TInt  $ TEnd),
            ("ID",  tsend TBool $ trecv TBool $ TEnd)]

t1 = tRecv t0 TEnd

senv0 :: SEnv
senv0 = [(Pos (pack "c"), t1),
         (Neg (pack "c"), dual t1),
         (Pos (pack "d"), t0),
         (Neg (pack "d"), dual t0)
         ]

recv c ptn p = Recv c [Clause ptn p]
choices c ps =
   Recv c (map (uncurry Clause . ((PL . pack) *** id)) ps)

pn = PN . pack

p0 = Send (cN "c") (ePN "d") $
      choices (cN "d")
        [("NEG", recv (cN "d") (pn "x") $
                   Send (cN "d") (eI 0 `ESub` ePN "x") End),
         ("ID", recv (cN "d") (pn "x") $
                    Send (cN "d") (ePN "x") End)]

p1 = recv (cP "c") (pn "z") $
      Send (cP "z") (eL "NEG") $
       Send (cP "z") (eI 3) $
           recv (cP "z") (pn "w") End

nu c t p = Nu (pack c) (Just t) p

p2 = nu "c" t1 (nu "d" t0 p0 `Par` p1)

-- try: runExcept $ checkPi [] [] p2
