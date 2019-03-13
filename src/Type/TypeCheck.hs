{-# LANGUAGE FlexibleContexts #-}

module Type.TypeCheck where

import Control.Arrow ((***))
import Control.Monad.Except
import Data.Text (pack)
import Data.Function ((&))

import Syntax.Abstract
import Type
import Utilities

type Cxt = FMap SName Type

liftMaybe :: MonadError e m => e -> Maybe a -> m a
liftMaybe err = maybe (throwError err) return

type TMonad a = Except ErrMsg a

liftLookup :: (MonadError ErrMsg m, Eq a, Show a) => a -> FMap a b -> m b
liftLookup x env =
  liftMaybe ("variable " ++ show x ++ " not found") (lookup x env)

inferV :: MonadError ErrMsg m => Cxt -> Val -> m (Type, Cxt)
inferV _ (N (NR _)) =
  throwError "StdOut/In in expression"
inferV cxt (N (ND c)) =
  liftMaybe ("variable " ++ show c ++ " not found")
    (lookup c cxt >>= \t ->
     if unrestricted t then return (t, cxt)
        else return (t, rmEntry c cxt ))
inferV cxt (VI _) = return (tInt, cxt)
inferV cxt (VB _) = return (tBool, cxt)
inferV cxt (VT vs) =
  (TTuple *** id) <$> inferVs cxt vs
-- inferV env (VL l) = liftMaybe "label not found" (lookup l env)
inferV _ (VS _) = throwError "panic: not implemented yet"
-- inferV _ (VS _) = return TString

inferVs cxt [v] = ((\t -> [t]) *** id) <$> inferV cxt v
inferVs cxt (v:vs) =
   inferV  cxt v  >>= \(t,  cxt')  ->
   inferVs cxt vs >>= \(ts, cxt'') ->
   return (t:ts, cxt'')

inferE :: MonadError ErrMsg m =>
          Cxt -> Expr -> m (Type, Cxt)
inferE cxt (EV v) = inferV cxt v
inferE cxt (EAdd e1 e2) =
  checkE cxt  e1 tInt >>= \cxt' ->
  checkE cxt' e2 tInt >>= \cxt'' ->
  return (tInt, cxt'')
inferE cxt (ESub e1 e2) =
  checkE cxt  e1 tInt >>= \cxt' ->
  checkE cxt' e2 tInt >>= \cxt'' ->
  return (tInt, cxt'')
inferE cxt (EIf e0 e1 e2) =
  checkE cxt e0 tBool >>= \cxt' ->
  inferE cxt' e1      >>= \(t, cxt'') ->
  checkE cxt' e2 t    >>= \cxt''' ->
  if fMapEqBy eqType cxt'' cxt''' then
    return (t, cxt'')
   else throwError "contexts fail to unify when checking If"
inferE cxt (ETup es) =
  (TTuple *** id) <$> inferEs cxt es
inferE _ _ = throwError "panic: not implemented yet"

inferEs cxt [e] = ((\t -> [t]) *** id) <$> inferE cxt e
inferEs cxt (e:es) =
   inferE  cxt e  >>= \(t,  cxt') ->
   inferEs cxt es >>= \(ts, cxt'') ->
   return (t:ts, cxt'')

checkE :: MonadError ErrMsg m =>
          Cxt -> Expr -> Type -> m Cxt
checkE env e t =
  inferE env e >>= \(t', cxt) ->
  tcheck t t' >> return cxt

tcheck :: MonadError ErrMsg m => Type -> Type -> m ()
tcheck t1 t2
   | eqType t1 t2 = return ()
   | otherwise = throwError ("expected: " ++ show t1 ++
                             ", found " ++ show t2)

checkCh :: MonadError ErrMsg m => Cxt -> PN RName -> Type -> m ()
checkCh senv c t =
  liftLookup c senv >>= \t' ->
  if t == t' then return ()
    else throwError "type mismatch"

allClosed :: Cxt -> Bool
allClosed = all (TEnd ==) . map snd
        -- = const True

checkPi :: MonadError ErrMsg m =>
   Cxt -> Pi -> m (Cxt, [PN RName])

checkPi cxt End = return (cxt, [])

checkPi cxt (p1 `Par` p2) =
  checkPi cxt  p1 >>= \(cxt1, l1) ->
  cxt1 `sMinus` l1 >>= \cxt2 ->
  checkPi cxt2 p2

checkPi cxt (Send (NR StdOut) e p) =
  inferE  cxt e >>= \(_, cxt') ->
  checkPi cxt' p
checkPi cxt (Send (NR _) e p) =
  throwError "cannot send to this channel"
checkPi cxt (Send (NG _) e p) =
  throwError "generated name appears in type checking. a bug?"
checkPi cxt (Send (ND c) e p) =
  lookupChType c cxt >>=  -- c removed if linear
  matchSend (checkPiSend (c,e,p)) (checkPiSel (c,e,p))

checkPi _ (Recv (NR _) _) =
  throwError "not knowing what to do yet"
checkPi _ (Recv (NG _) _) =
  throwError "generated name appears in type checking. a bug?"
checkPi cxt (Recv (ND c) ps) =
  lookupChType c cxt >>= -- c removed if linear
  matchRecv (checkPiRecv (c,ps)) (checkPiChoi (c,ps))

checkPi cxt (Repl p) = -- throwError "panic: not implemented yet"
  checkPi cxt p >>= \(cxt', l) ->
  if null l then return (cxt', l)
     else throwError ("linear variable used in repetition")

checkPi _ (Nu _ Nothing _) =
  throwError "needing a type for new channels"
checkPi cxt (Nu x (Just t) p) =
  checkPi ((Pos x, t):(Neg x, dual t):cxt) p >>= \(cxt', l) ->
  cxt' `sMinus` [Pos x, Neg x] >>= \cxt'' ->
  return (cxt'', l `setminus` [Pos x, Neg x])

checkPiSend :: MonadError ErrMsg m =>
  (SName, Expr, Pi) ->
  Type -> Type -> Bool -> Cxt -> m (Cxt, [SName])
checkPiSend (c,e,p) s t un cxt =
  checkE cxt e s >>= \cxt' ->
  addCxt (c,t) cxt' >>= \cxt'' ->
  (id *** addLin un c) <$>
         checkPi cxt'' p

checkPiSel :: MonadError ErrMsg m =>
  (SName, Expr, Pi) ->
  [(Label, Type)] -> Bool -> Cxt -> m (Cxt, [SName])
checkPiSel (c,e,p) ts un cxt =
  isLabel e >>= \l ->
  liftLookup l ts >>= \s ->
  addCxt (c,s) cxt >>= \cxt' ->
  (id *** addLin un c) <$> checkPi cxt' p

checkPiRecv :: MonadError ErrMsg m =>
  (SName, [Clause]) ->
  Type -> Type -> Bool -> Cxt -> m (Cxt, [SName])
checkPiRecv (c, ps) s t un cxt =
  mapM (checkRecvClause c s t un cxt) ps >>= eqAll

checkRecvClause :: MonadError ErrMsg m =>
  SName -> Type -> Type -> Bool -> Cxt ->
  Clause -> m (Cxt, [SName])
checkRecvClause c s t un cxt (Clause ptn p) =
  matchPtn s ptn cxt >>= \cxt' ->
  addCxt (c,t) cxt' >>= \cxt'' ->
  checkPi cxt'' p >>= \(cxt''', l) ->
  return (rmEntries xs cxt''',
          let l' = l `setminus` xs
          in if un then l' else (nubcons c l'))
 where xs = ptnVars ptn

checkPiChoi :: MonadError ErrMsg m =>
  (SName, [Clause]) ->
  [(Label, Type)] -> Bool -> Cxt -> m (Cxt, [SName])
checkPiChoi (c, ps) ts un cxt =
   pairChoices ts ps >>= \xss ->
   mapM (\(s,p) -> addCxt (c,s) cxt >>= \cxt' ->
                   checkPi cxt' p) xss >>= \res ->
   (id *** addLin un c) <$> eqAll res

addLin True  _ xs  = xs
addLin False c xs = nubcons c xs

isLabel :: MonadError ErrMsg m => Expr -> m Label
isLabel (EV (VL l)) = return l
isLabel _ = throwError "must be a label"

-- generic dispatcher

matchSend :: MonadError ErrMsg m =>
   (Type -> Type -> Bool -> Cxt -> m a) ->
   ([(Label, Type)] -> Bool -> Cxt -> m a) -> (Type, Bool, Cxt) -> m a
matchSend fs _  (TSend s1 s2, b, cxt) = fs s1 s2 b cxt
matchSend _  fl (TSele ts,    b, cxt) = fl ts b cxt
matchSend _  _  t =
  throwError ("TSend expected, got " ++ show t)

matchRecv :: MonadError ErrMsg m =>
  (Type -> Type -> Bool -> Cxt -> m a) ->
  ([(Label, Type)] -> Bool -> Cxt -> m a) -> (Type, Bool, Cxt) -> m a
matchRecv fs _  (TRecv t s, b, cxt) = fs t s b cxt
matchRecv _  fl (TChoi ts,  b, cxt) = fl ts b cxt
matchRecv _  _  _ = throwError "TRecv expected"

-- pattern related stuffs

ptnVars :: Ptrn -> [SName]
ptnVars (PN x) = [Pos x]  -- right?
ptnVars (PT xs) = concat (map ptnVars xs)
ptnVars (PL _) = []

matchPtn :: MonadError ErrMsg m =>
   Type -> Ptrn -> Cxt -> m Cxt
matchPtn TEnd (PN x) cxt = addUni (Pos x,TEnd) cxt
matchPtn (TBase t) (PN x) cxt = addUni (Pos x,TBase t) cxt
matchPtn (TTuple ts) (PN x) cxt = addUni (Pos x, TTuple ts) cxt
matchPtn (TTuple ts) (PT xs) cxt = matchPtns ts xs cxt
matchPtn (TSend s t) (PN x) cxt = addUni (Pos x, TSend s t) cxt
matchPtn (TRecv s t) (PN x) cxt = addUni (Pos x, TRecv s t) cxt
matchPtn (TSele ts) (PN x) cxt = addUni (Pos x, TSele ts) cxt
matchPtn (TChoi ts) (PN x) cxt = addUni (Pos x, TChoi ts) cxt
matchPtn (TUn t) (PN x) cxt = addUni (Pos x, TUn t) cxt
matchPtn (TUn (TTuple ts)) (PT xs) cxt =
  matchPtns (map TUn ts) xs cxt
matchPtn _ _ _ = throwError "pattern mismatch"

matchPtns []     []     cxt = return cxt
matchPtns (t:ts) (x:xs) cxt =
  matchPtn t x cxt >>=
  matchPtns ts xs
matchPtns _ _ _ = throwError "tuple length mismatch"

-- env related utilities

sMinus :: MonadError ErrMsg m => Cxt -> [SName] -> m Cxt
cxt `sMinus` []     = return cxt
cxt `sMinus` (x:xs) =
  cxt `sMinus` xs >>= \cxt' ->
  lookup x cxt' & maybe
    (return cxt')
    (\t -> if unrestricted t then
               return (rmEntry x cxt)
           else throwError ("channel " ++ show x ++ " not used up."))

lookupChType :: MonadError ErrMsg m =>
  SName -> Cxt -> m (Type, Bool, Cxt)
lookupChType x cxt =
  liftLookup x cxt >>= \t ->
  stripUnres (unfoldT t) & \(t', unrest) ->
  if unrest then return (t', unrest, cxt)
      else return (t', unrest, rmEntry x cxt)

addCxt :: MonadError ErrMsg m =>
      (SName, Type) -> Cxt -> m Cxt
addCxt (c,t) cxt =
  lookup c cxt &
   maybe (return ((c,t) : cxt))
     (\t' -> if eqType t t' then return cxt
              else throwError ("types of channels in env mismatch: " ++ show t ++ " and " ++ show t'))

addUni :: MonadError ErrMsg m =>
      (SName, Type) -> Cxt -> m Cxt
addUni (x,t) cxt =
  lookup x cxt &
   maybe (return ((x,t):cxt))
     (\_ -> throwError (show x ++ " exists in context"))

eqAll :: MonadError ErrMsg m =>
   [(Cxt, [SName])] -> m (Cxt, [SName])
eqAll [x] = return x
eqAll ((s1,l1):(s2,l2):xs) =
  if (fMapEqBy eqType s1 s2) && (l1 `setEq` l2)
    then eqAll ((s2,l2):xs)
    else throwError "branches cannot be unified"

pairChoices :: MonadError ErrMsg m =>
  [(Label, Type)] -> [Clause] -> m [(Type, Pi)]
pairChoices [] [] = return []
pairChoices _ [] =
  throwError "choices not fully covered"
pairChoices ts (Clause (PL l) p : ps) =
  liftLookup l ts >>= \s ->
  ((s,p):) <$> pairChoices (rmEntry l ts) ps
pairChoices _ (Clause _ _ : _) =
  throwError "not a label"

{-
splitSEnv :: MonadError ErrMsg m =>
  [PN RName] -> [PN RName] -> SEnv -> m (SEnv, SEnv)
splitSEnv _ _ [] = return ([],[])
splitSEnv fl fr ((x,t):xs)
   | bl && br  = throwError ("channel " ++ show x ++ " not linear")
   | bl        = (((x,t):) *** id) <$> splitSEnv fl fr xs
   | br        = (id *** ((x,t):)) <$> splitSEnv fl fr xs
   | otherwise = throwError ("channel " ++ show x ++ " dropped")
  where (bl, br) = (x `elem` fl, x `elem` fr)
-}

--------------------------------------------------------------------------------
-- Tests

-- smart constructors for Types

tsend :: BType -> Type -> Type
tsend t s = TSend (TBase t) s

trecv :: BType -> Type -> Type
trecv t s = TRecv (TBase t) s

tsele :: [(String, Type)] -> Type
tsele = TSele . map (pack *** id)

-- smart constructors for Pi

nu :: String -> Type -> Pi -> Pi
nu c t p = Nu (pack c) (Just t) p

recv :: Name -> Ptrn -> Pi -> Pi
recv c ptn p = Recv c [Clause ptn p]

choices :: Name -> [(String, Pi)] -> Pi
choices c ps =
   Recv c (map (uncurry Clause . ((PL . pack) *** id)) ps)

pn :: String -> Ptrn
pn = PN . pack

--

cp = Pos . pack
cn = Neg . pack

test0 :: Either String (Cxt, [SName])
test0 = runExcept $ checkPi cxt p
  where cxt = [(cp "c", tsend TInt (tsend TBool TEnd)),
               (cp "d", TSend (TTuple [tInt, tsend TBool TEnd]) TEnd)]
        p = Send (cP "c") (eI 3) $
              Send (cP "d") (ETup [eI 4, ePN "c"]) End
        {- p = c[3].d[4,c].0
        -}

test1 :: Either String (Cxt, [SName])
test1 = runExcept $ checkPi cxt p
  where cxt = [(cn "c", trecv TInt (trecv TBool TEnd))]
        p = recv (cN "c") (pn "x")  $ recv (cN "c") (pn "y")  End

test2 :: Either String (Cxt, [SName])
test2 = runExcept $ checkPi [] p
  where p = nu "c" t (Par p1 p2)
        p1 = Send (cP "c") (eI 3) $ Send (cP "c") (eB False) End
        p2 = recv (cN "c") (pn "x") $ recv (cN "c") (pn "y") End
        t = tsend TInt (tsend TBool TEnd)
        cxt = [(cp "c", t), (cn "c", dual t)]
        {-  p  = nu (c:t) (p1 | p2)
            p1 = c[3].c[False].0
            p2 = c~(x).c~(y).0
            t  = !Int.!Bool.0
        -}

test3 :: Either String (Cxt, [SName])
test3 = runExcept $ checkPi cxt0 p2
  where
    t0 :: Type
    t0 = tsele [("NEG", tsend TInt  $ trecv TInt  $ TEnd),
                ("ID",  tsend TBool $ trecv TBool $ TEnd)]

    t1 :: Type
    t1 = TRecv t0 TEnd

    cxt0 :: Cxt
    cxt0 = [ (cp "c", t1)
           , (cn "c", dual t1)
           , (cp "d", t0)
           , (cn "d", dual t0)
           ]

    -- p0 = c[d].d~>>{NEG -> d(x).d[-x].0 ;
    --                ID -> d(x).d[x].0 }
    p0 :: Pi
    p0 = Send (cN "c") (ePN "d") $
          choices (cN "d")
            [("NEG", recv (cN "d") (pn "x") $
                       Send (cN "d") (eI 0 `ESub` ePN "x") End),
             ("ID", recv (cN "d") (pn "x") $
                        Send (cN "d") (ePN "x") End)]

    -- p1 = c(z).z<<NEG.z[3].z(w).0
    p1 :: Pi
    p1 = recv (cP "c") (pn "z") $
          Send (cP "z") (eL "NEG") $
           Send (cP "z") (eI 3) $
               recv (cP "z") (pn "w") End
    -- p2 = nu (c:t1) nu (d:t0) (p0 | p1)
    p2 :: Pi
    p2 = nu "c" t1 (nu "d" t0 p0 `Par` p1)

test4 :: Either String (Cxt, [SName])
test4 = runExcept $ checkPi cxt p
  where cxt = [(cp "c", TMu $ TUn $ trecv TInt $ TVar 0)]
        p = Repl (recv (cP "c") (pn "x") End)
        -- p = *(c(x).0)
        -- {c: mu(X)un(?Int.X)}

test5 :: Either String (Cxt, [SName])
test5 = runExcept $ checkPi cxt p
  where t = tsend TInt $ trecv TBool TEnd
        -- {c : mu(X)(un(?(!Int.?Bool.0).X))}
        -- p = *(c(d). d[3].d(x).0)
        cxt = [(cp "c", TMu $ TUn $ TRecv t $ TVar 0)]
        p = Repl (recv (cP "c") (pn "d") $
                   Send (cP "d") (eI 3) $
                     recv (cP "d") (pn "x") End)


-- try: runExcept $ checkPi [] [] p2
-- try: run `stack test` for running these tests
-- try: run `stack repl pi:test:pi-tests` to develop thoses tests
