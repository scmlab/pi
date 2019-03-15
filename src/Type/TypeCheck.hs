{-# LANGUAGE FlexibleContexts #-}

module Type.TypeCheck where

import Control.Arrow ((***))
import Control.Monad.Except
import Data.Text (pack)
import Data.Function ((&))
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.List as List

import Syntax.Abstract
import Type
import Base

import Control.Monad.Reader
import Control.Monad.Except


--------------------------------------------------------------------------------
-- | State

-- data TCState = TCState {}

--------------------------------------------------------------------------------
-- | Error

data TypeError
  = MissingProcDefn (Map ProcName Type)
  | VariableNotFound SName
  | Others String
  deriving (Show)

--------------------------------------------------------------------------------
-- | Type Checking Monad

type TCM = ExceptT TypeError (Reader Env)

runTCM :: TCM a -> Env -> Either TypeError a
runTCM = runReader . runExceptT

--------------------------------------------------------------------------------
-- | Some checkings

--------------------------------------------------------------------------------
-- | Checkings

checkAll :: TCM ()
checkAll = do

  env <- ask
  withTypes <- Map.traverseMaybeWithKey (const (return . withType)) env
  Map.traverseWithKey checkType withTypes

  return ()

  where
    checkType :: ProcName -> (Pi, Type) -> TCM ()
    checkType name (process, t) = undefined

--------------------------------------------------------------------------------
-- | Code


mapEqBy :: Ord k => (a -> a -> Bool) -> Map k a -> Map k a -> Bool
mapEqBy f a b = Map.isSubmapOfBy f a b && Map.isSubmapOfBy f b a

type Ctx = Map SName Type

liftMaybe :: TypeError -> Maybe a -> TCM a
liftMaybe err = maybe (throwError err) return

type TMonad a = Except ErrMsg a

lookupVar :: (Eq a, Ord a, Show a) => Map a Type -> a -> TCM Type
lookupVar env x = case Map.lookup x env of
  Just v -> return v
  Nothing -> throwError $ Others (show x)

inferV :: Ctx -> Val -> TCM (Type, Ctx)
inferV _ (N (NR _)) =
  throwError $ Others "StdOut/In in expression"
inferV ctx (N (ND c)) = do
  case Map.lookup c ctx of
    Nothing -> throwError $ Others $ "variable " ++ show c ++ " not found"
    Just t -> if unrestricted t
                then return (t, ctx)
                else return (t, Map.delete c ctx)
inferV ctx (VI _) = return (tInt, ctx)
inferV ctx (VB _) = return (tBool, ctx)
inferV ctx (VT vs) =
  (TTuple *** id) <$> inferVs ctx vs
-- inferV env (VL l) = liftMaybe "label not found" (lookup l env)
inferV _ (VS _) = throwError $ Others "panic: not implemented yet"
-- inferV _ (VS _) = return TString

inferVs ctx [v] = ((\t -> [t]) *** id) <$> inferV ctx v
inferVs ctx (v:vs) =
   inferV  ctx v  >>= \(t,  ctx')  ->
   inferVs ctx vs >>= \(ts, ctx'') ->
   return (t:ts, ctx'')

inferE :: Ctx -> Expr -> TCM (Type, Ctx)
inferE ctx (EV v) = inferV ctx v
inferE ctx (EAdd e1 e2) =
  checkE ctx  e1 tInt >>= \ctx' ->
  checkE ctx' e2 tInt >>= \ctx'' ->
  return (tInt, ctx'')
inferE ctx (ESub e1 e2) =
  checkE ctx  e1 tInt >>= \ctx' ->
  checkE ctx' e2 tInt >>= \ctx'' ->
  return (tInt, ctx'')
inferE ctx (EIf e0 e1 e2) = do
  ctx' <- checkE ctx e0 tBool
  (t, ctx'') <- inferE ctx' e1
  ctx''' <- checkE ctx' e2 t
  if mapEqBy eqType ctx'' ctx'''
    then return (t, ctx'')
    else throwError $ Others "contexts fail to unify when checking If"
inferE ctx (ETup es) =
  (TTuple *** id) <$> inferEs ctx es
inferE _ _ = throwError $ Others "panic: not implemented yet"

inferEs ctx [e] = ((\t -> [t]) *** id) <$> inferE ctx e
inferEs ctx (e:es) =
   inferE  ctx e  >>= \(t,  ctx') ->
   inferEs ctx es >>= \(ts, ctx'') ->
   return (t:ts, ctx'')

checkE :: Ctx -> Expr -> Type -> TCM Ctx
checkE env e t =
  inferE env e >>= \(t', ctx) ->
  tcheck t t' >> return ctx

tcheck :: Type -> Type -> TCM ()
tcheck t1 t2
   | eqType t1 t2 = return ()
   | otherwise = throwError $ Others ("expected: " ++ show t1 ++
                             ", found " ++ show t2)

checkCh :: Ctx -> PN RName -> Type -> TCM ()
checkCh senv c t = do
  t' <- lookupVar senv c
  if t == t'
    then return ()
    else throwError $ Others "type mismatch"

allClosed :: Ctx -> Bool
allClosed = all (TEnd ==) . Map.elems

checkPi :: Ctx -> Pi -> TCM (Ctx, Set SName)

checkPi ctx End = return (ctx, Set.empty)

checkPi ctx (p1 `Par` p2) = do
  (ctx1, l1) <- checkPi ctx p1
  ctx2 <- ctx1 `removeChannels` l1
  checkPi ctx2 p2

checkPi ctx (Send (NR StdOut) e p) = do
  (_, ctx') <- inferE ctx e
  checkPi ctx' p
checkPi ctx (Send (NR _) e p) =
  throwError $ Others "cannot send to this channel"
checkPi ctx (Send (NG _) e p) =
  throwError $ Others "generated name appears in type checking. a bug?"
checkPi ctx (Send (ND c) e p) =
  lookupChType c ctx >>=  -- c removed if linear
  matchSend (checkPiSend (c,e,p)) (checkPiSel (c,e,p))

checkPi _ (Recv (NR _) _) =
  throwError $ Others "not knowing what to do yet"
checkPi _ (Recv (NG _) _) =
  throwError $ Others "generated name appears in type checking. a bug?"
checkPi ctx (Recv (ND c) ps) =
  lookupChType c ctx >>= -- c removed if linear
  matchRecv (checkPiRecv (c,ps)) (checkPiChoi (c,ps))

checkPi ctx (Repl p) = -- throwError $ Others "panic: not implemented yet"
  checkPi ctx p >>= \(ctx', l) ->
  if null l then return (ctx', l)
     else throwError $ Others ("linear variable used in repetition")

checkPi _ (Nu _ Nothing _) =
  throwError $ Others "needing a type for new channels"
checkPi ctx (Nu x (Just t) p) = do
  (ctx', l) <- checkPi (Map.insert (Pos x) t $ Map.insert (Neg x) (dual t) ctx) p
  ctx'' <- ctx' `removeChannels` Set.fromList [Pos x, Neg x]
  return (ctx'', Set.difference l (Set.fromList [Pos x, Neg x]))

checkPiSend :: (SName, Expr, Pi) -> Type -> Type -> Bool -> Ctx -> TCM (Ctx, Set SName)
checkPiSend (c,e,p) s t un ctx = do
  ctx' <- checkE ctx e s
  ctx'' <- addCtx (c,t) ctx'
  (id *** addLin un c) <$>
         checkPi ctx'' p

checkPiSel :: (SName, Expr, Pi) -> Map Label Type -> Bool -> Ctx -> TCM (Ctx, Set SName)
checkPiSel (c,e,p) ts un ctx = do
  l <- isLabel e
  s <- lookupVar ts l
  ctx' <- addCtx (c,s) ctx
  (id *** addLin un c) <$> checkPi ctx' p

checkPiRecv :: (SName, [Clause]) -> Type -> Type -> Bool -> Ctx -> TCM (Ctx, Set SName)
checkPiRecv (c, ps) s t un ctx =
  mapM (checkRecvClause c s t un ctx) ps >>= eqAll

checkRecvClause :: SName -> Type -> Type -> Bool -> Ctx -> Clause -> TCM (Ctx, Set SName)
checkRecvClause c s t un ctx (Clause ptn p) = do
  ctx' <- matchPtn s ptn ctx
  ctx'' <- addCtx (c,t) ctx'
  (ctx''', l) <- checkPi ctx'' p
  return (Map.withoutKeys ctx''' xs,
          let l' = Set.difference l xs
          in if un then l' else (Set.insert c l'))
  where xs = ptnVars ptn

checkPiChoi :: (SName, [Clause]) -> Map Label Type -> Bool -> Ctx -> TCM (Ctx, Set SName)
checkPiChoi (c, ps) ts un ctx = do
   xss <- pairChoices ts ps
   res <- mapM (\(s, p) -> do
            ctx' <- addCtx (c,s) ctx
            checkPi ctx' p) $ Map.toList xss
   (id *** addLin un c) <$> eqAll res

addLin :: Ord a => Bool -> a -> Set a -> Set a
addLin True  _ xs = xs
addLin False c xs = Set.insert c xs

isLabel ::  Expr -> TCM Label
isLabel (EV (VL l)) = return l
isLabel _ = throwError $ Others "must be a label"

-- generic dispatcher

matchSend ::
   (Type -> Type -> Bool -> Ctx -> TCM a) ->
   (Map Label Type -> Bool -> Ctx -> TCM a) -> (Type, Bool, Ctx) -> TCM a
matchSend fs _  (TSend s1 s2, b, ctx) = fs s1 s2 b ctx
matchSend _  fl (TSele ts,    b, ctx) = fl (Map.fromList ts) b ctx
matchSend _  _  t =
  throwError $ Others ("TSend expected, got " ++ show t)

matchRecv ::
  (Type -> Type -> Bool -> Ctx -> TCM a) -> (Map Label Type -> Bool -> Ctx -> TCM a) -> (Type, Bool, Ctx) -> TCM a
matchRecv fs _  (TRecv t s, b, ctx) = fs t s b ctx
matchRecv _  fl (TChoi ts,  b, ctx) = fl (Map.fromList ts) b ctx
matchRecv _  _  _ = throwError $ Others "TRecv expected"

-- pattern related stuffs

ptnVars :: Ptrn -> Set SName
ptnVars (PN x) = Set.singleton (Pos x)  -- right?
ptnVars (PT xs) = Set.unions (map ptnVars xs)
ptnVars (PL _) = Set.empty

matchPtn :: Type -> Ptrn -> Ctx -> TCM Ctx
matchPtn TEnd (PN x) ctx = addUni (Pos x,TEnd) ctx
matchPtn (TBase t) (PN x) ctx = addUni (Pos x,TBase t) ctx
matchPtn (TTuple ts) (PN x) ctx = addUni (Pos x, TTuple ts) ctx
matchPtn (TTuple ts) (PT xs) ctx = matchPtns ts xs ctx
matchPtn (TSend s t) (PN x) ctx = addUni (Pos x, TSend s t) ctx
matchPtn (TRecv s t) (PN x) ctx = addUni (Pos x, TRecv s t) ctx
matchPtn (TSele ts) (PN x) ctx = addUni (Pos x, TSele ts) ctx
matchPtn (TChoi ts) (PN x) ctx = addUni (Pos x, TChoi ts) ctx
matchPtn (TUn t) (PN x) ctx = addUni (Pos x, TUn t) ctx
matchPtn (TUn (TTuple ts)) (PT xs) ctx =
  matchPtns (map TUn ts) xs ctx
matchPtn _ _ _ = throwError $ Others "pattern mismatch"

matchPtns []     []     ctx = return ctx
matchPtns (t:ts) (x:xs) ctx =
  matchPtn t x ctx >>=
  matchPtns ts xs
matchPtns _ _ _ = throwError $ Others "tuple length mismatch"

-- return the context without the given names
-- the names in the context should all be unrestricted
removeChannels :: Ctx -> Set SName -> TCM Ctx
removeChannels ctx names = do
  let inCtx = Map.elems $ Map.restrictKeys ctx names
  unless (all unrestricted inCtx) $
    throwError $ Others ("channel " ++ show inCtx ++ " not used up.")
  return $ Map.withoutKeys ctx names

lookupChType :: SName -> Ctx -> TCM (Type, Bool, Ctx)
lookupChType x ctx = do
  t <- lookupVar ctx x
  let (t', unrest) = stripUnres (unfoldT t)
  if unrest
    then return (t', unrest, ctx)
    else return (t', unrest, Map.delete x ctx)

addCtx :: (SName, Type) -> Ctx -> TCM Ctx
addCtx (c,t) ctx = case Map.lookup c ctx of
  Just t' -> if eqType t t'
    then return ctx
    else throwError $ Others ("types of channels in env mismatch: " ++ show t ++ " and " ++ show t')
  Nothing -> return $ Map.insert c t ctx

addUni :: (SName, Type) -> Ctx -> TCM Ctx
addUni (x,t) ctx = case Map.lookup x ctx of
  Just _ -> throwError $ Others (show x ++ " exists in context")
  Nothing -> return $ Map.insert x t ctx

eqAll :: [(Ctx, Set SName)] -> TCM (Ctx, Set SName)
eqAll [x] = return x
eqAll ((s1,l1):(s2,l2):xs) =
  if (mapEqBy eqType s1 s2) && (l1 == l2)
    then eqAll ((s2,l2):xs)
    else throwError $ Others "branches cannot be unified"
--
-- eqAll :: (Eq a, Eq b) => Map a b -> TCM (a, b)
-- eqAll pairs = if Map.null pairs
--   then throwError $ Others "no branches present"
--   else do
--     let keyGroups = List.group (Map.keys pairs)
--     let valueGroups = List.group (Map.elems pairs)
--     if (length keyGroups == 1 && length valueGroups == 1)
--       then return $ Map.findMin pairs
--       else throwError $ Others "branches cannot be unified"

pairChoices :: Map Label Type -> [Clause] -> TCM (Map Type Pi)
pairChoices ts [] = if Map.null ts
  then return Map.empty
  else throwError $ Others "choices not fully covered"
pairChoices ts (Clause (PL l) p : ps) = do
  s <- lookupVar ts l
  Map.insert s p <$> pairChoices (Map.delete l ts) ps
pairChoices _ (Clause _ _ : _) =
  throwError $ Others "not a label"

{-
splitSEnv ::
  [PN RName] -> [PN RName] -> SEnv -> TCM (SEnv, SEnv)
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

test0 :: Either TypeError (Ctx, Set SName)
test0 = runTCM (checkPi ctx p) Map.empty
  where ctx = Map.fromList [(cp "c", tsend TInt (tsend TBool TEnd)),
               (cp "d", TSend (TTuple [tInt, tsend TBool TEnd]) TEnd)]
        p = Send (cP "c") (eI 3) $
              Send (cP "d") (ETup [eI 4, ePN "c"]) End
        {- p = c[3].d[4,c].0
        -}

test1 :: Either TypeError (Ctx, Set SName)
test1 = runTCM (checkPi ctx p) Map.empty
  where ctx = Map.fromList [(cn "c", trecv TInt (trecv TBool TEnd))]
        p = recv (cN "c") (pn "x")  $ recv (cN "c") (pn "y")  End

test2 :: Either TypeError (Ctx, Set SName)
test2 = runTCM (checkPi ctx p) Map.empty
  where p = nu "c" t (Par p1 p2)
        p1 = Send (cP "c") (eI 3) $ Send (cP "c") (eB False) End
        p2 = recv (cN "c") (pn "x") $ recv (cN "c") (pn "y") End
        t = tsend TInt (tsend TBool TEnd)
        ctx = Map.fromList [(cp "c", t), (cn "c", dual t)]
        {-  p  = nu (c:t) (p1 | p2)
            p1 = c[3].c[False].0
            p2 = c~(x).c~(y).0
            t  = !Int.!Bool.0
        -}

test3 :: Either TypeError (Ctx, Set SName)
test3 = runTCM (checkPi ctx0 p0) Map.empty
  where
    t0 :: Type
    t0 = tsele [("NEG", tsend TInt  $ trecv TInt  $ TEnd),
                ("ID",  tsend TBool $ trecv TBool $ TEnd)]

    t1 :: Type
    t1 = TRecv t0 TEnd

    ctx0 :: Ctx
    ctx0 = Map.fromList [ (cp "c", t1)
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

test4 :: Either TypeError (Ctx, Set SName)
test4 = runTCM (checkPi ctx p) Map.empty
  where ctx = Map.fromList [(cp "c", TMu $ TUn $ trecv TInt $ TVar 0)]
        p = Repl (recv (cP "c") (pn "x") End)
        -- p = *(c(x).0)
        -- {c: mu(X)un(?Int.X)}

test5 :: Either TypeError (Ctx, Set SName)
test5 = runTCM (checkPi ctx p) Map.empty
  where t = tsend TInt $ trecv TBool TEnd
        -- {c : mu(X)(un(?(!Int.?Bool.0).X))}
        -- p = *(c(d). d[3].d(x).0)
        ctx = Map.fromList [(cp "c", TMu $ TUn $ TRecv t $ TVar 0)]
        p = Repl (recv (cP "c") (pn "d") $
                   Send (cP "d") (eI 3) $
                     recv (cP "d") (pn "x") End)


-- try: runExcept $ checkPi [] [] p2
-- try: run `stack test` for running these tests
-- try: run `stack repl pi:test:pi-tests` to develop thoses tests
