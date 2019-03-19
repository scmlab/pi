{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Type.TypeCheck where

import Control.Arrow ((***))
import Control.Monad.Except
import Data.Text (pack)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Text (Text)

import Syntax.Abstract
import Syntax.Concrete (toAbstract)
import Syntax.Parser (parseProcess)
import Type
import Base
import Debug.Trace

import Control.Monad.Reader


--------------------------------------------------------------------------------
-- | State

-- data TCState = TCState {}

--------------------------------------------------------------------------------
-- | Error

data TypeError
  = MissingProcDefn (Map ProcName Type)
  | VariableNotFound SName
  | TypeVariableNotFound TypeName
  | TypeVarIndexAtTopLevel TypeVar
  | LabelNotFound Label
  | ProcessNotFound SName
  | PatternMismatched Type Ptrn
  | TypeOfNewChannelMissing Text
  | RecvExpected Type
  | SendExpected Type
  | SNameExpected Name
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

  chanTypes <- asks envChanTypes
  procDefns <- asks envProcDefns
  typeDefns <- asks envTypeDefns

  -- not checking if some process named "test" exists
  case Map.lookup "test" procDefns of
    Nothing -> do
      case Map.lookup "main" procDefns of
        Nothing -> return ()
        Just p -> void $ checkPi chanTypes p
    Just _ -> return ()

  return ()


--------------------------------------------------------------------------------
-- | Code


mapEqBy :: Ord k => (a -> a -> Bool) -> Map k a -> Map k a -> Bool
mapEqBy f a b = Map.isSubmapOfBy f a b && Map.isSubmapOfBy f b a

-- TODO: Make CTX = Map Text Type, no polarization in the context
type Ctx = Map SName Type

liftMaybe :: TypeError -> Maybe a -> TCM a
liftMaybe err = maybe (throwError err) return

type TMonad a = Except ErrMsg a

lookupTypeVar :: TypeVar -> TCM Type
lookupTypeVar (TypeVarIndex x) = return $ TVar $ TypeVarIndex x
  -- throwError $ TypeVarIndexAtTopLevel (TypeVarIndex x)
lookupTypeVar (TypeVarText x) = do
  typeDefns <- asks envTypeDefns
  case Map.lookup x typeDefns of
    Just v -> return v
    Nothing -> throwError $ TypeVariableNotFound x

lookupChan :: SName -> Ctx -> TCM (Type, Bool, Ctx)
lookupChan x ctx = do
  t <- lookupVar ctx x
  let (t', unrest) = stripUnres (unfoldT t)
  if unrest
    then return (t', unrest, ctx)
    else return (t', unrest, Map.delete x ctx)

lookupVar :: Ctx -> SName -> TCM Type
lookupVar ctx x = case Map.lookup x ctx of
  Just v -> substituteTypeVar v
  Nothing -> case Map.lookup (dual x) ctx of
    Just v -> substituteTypeVar (dual v)
    Nothing -> throwError $ VariableNotFound x

  where
    substitutePair :: (Label, Type) -> TCM (Label, Type)
    substitutePair (label, t) = do
      t' <- substituteTypeVar t
      return (label, t')

    substituteTypeVar :: Type -> TCM Type
    substituteTypeVar TEnd        = return TEnd
    substituteTypeVar (TBase t)   = return (TBase t)
    substituteTypeVar (TTuple ts) = TTuple <$> mapM substituteTypeVar ts
    substituteTypeVar (TSend t s) = TSend <$> substituteTypeVar t <*> substituteTypeVar s
    substituteTypeVar (TRecv t s) = TRecv <$> substituteTypeVar t <*> substituteTypeVar s
    substituteTypeVar (TChoi ss)  = TChoi <$> mapM substitutePair ss
    substituteTypeVar (TSele ss)  = TSele <$> mapM substitutePair ss
    substituteTypeVar (TUn t)     = TUn <$> substituteTypeVar t
    substituteTypeVar (TVar (TypeVarText "X")) = return $ TVar (TypeVarText "X") -- mu
    substituteTypeVar (TVar i)    = lookupTypeVar i
    substituteTypeVar (TMu t)     = TMu <$> substituteTypeVar t


lookupLabel :: Map Label Type -> Label -> TCM Type
lookupLabel env x = case Map.lookup x env of
  Just v -> return v
  Nothing -> throwError $ LabelNotFound x


inferV :: Ctx -> Val -> TCM (Type, Ctx)
inferV _ (N (NR _)) =
  throwError $ Others "StdOut/In in expression"
inferV ctx (N (ND c)) = do
  case Map.lookup c ctx of
    Nothing -> throwError $ Others $ "variable " ++ show c ++ " not found"
    Just t -> if unrestricted t
                then return (t, ctx)
                else return (t, Map.delete c ctx)
inferV _ (N (NG _)) =
  throwError $ Others "Unable to infer system generated variables"
inferV ctx (VI _) = return (tInt, ctx)
inferV ctx (VB _) = return (tBool, ctx)
inferV ctx (VT vs) =
  (TTuple *** id) <$> inferVs ctx vs
-- inferV env (VL l) = liftMaybe "label not found" (lookup l env)
inferV _ (VS _) = throwError $ Others "panic: not implemented yet"
inferV _ (VL _) = throwError $ Others "panic: not implemented yet"
-- inferV _ (VS _) = return TString

inferVs :: Ctx -> [Val] -> TCM ([Type], Ctx)
inferVs _   [] = throwError $ Others "panic: no values to infer"
inferVs ctx [v] = ((\t -> [t]) *** id) <$> inferV ctx v
inferVs ctx (v:vs) = do
   (t,  _) <- inferV ctx v
   (ts, ctx'') <- inferVs ctx vs
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

inferEs :: Ctx -> [Expr] -> TCM ([Type], Ctx)
inferEs _   [] = throwError $ Others "panic: no expressions to infer"
inferEs ctx [e] = ((\t -> [t]) *** id) <$> inferE ctx e
inferEs ctx (e:es) = do
  (t,  _) <- inferE  ctx e
  (ts, ctx'') <- inferEs ctx es
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

checkCh :: Ctx -> SName -> Type -> TCM ()
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
checkPi _   (Send (NR _) _ _) =
  throwError $ Others "cannot send to this channel"
checkPi _   (Send (NG _) _ _) =
  throwError $ Others "generated name appears in type checking. a bug?"
checkPi ctx (Send (ND c) e p) = do
  (channelType, un, ctx') <- lookupChan c ctx
  case channelType of
    TSend s1 s2 -> checkPiSend (c, e, p) s1 s2 un ctx'
    TChoi ts    -> checkPiSel  (c, e, p) (Map.fromList ts) un ctx'
    others      -> throwError $ SendExpected others

checkPi _ (Recv (NR _) _) =
  throwError $ Others "not knowing what to do yet"
checkPi _ (Recv (NG _) _) =
  throwError $ Others "generated name appears in type checking. a bug?"
checkPi ctx (Recv (ND c) ps) = do

  (channelType, un, ctx') <- lookupChan c ctx
  case channelType of
    TRecv t s -> checkPiRecv (c,ps) t s un ctx'
    TChoi ts  -> checkPiChoi (c,ps) (Map.fromList ts) un ctx'
    others    -> throwError $ RecvExpected others

checkPi ctx (Repl p) = -- throwError $ Others "panic: not implemented yet"
  checkPi ctx p >>= \(ctx', l) ->
  if null l then return (ctx', l)
     else throwError $ Others ("linear variable used in repetition")

checkPi _ (Nu x Nothing _) =
  throwError $ TypeOfNewChannelMissing x
checkPi ctx (Nu x (Just t) p) = do
  (ctx', l) <- checkPi (Map.insert (Pos x) t $ Map.insert (Neg x) (dual t) ctx) p
  ctx'' <- ctx' `removeChannels` Set.fromList [Pos x, Neg x]
  return (ctx'', Set.difference l (Set.fromList [Pos x, Neg x]))

checkPi ctx (Call name) = do
  env <- asks envProcDefns
  case Map.lookup name env of
    Just p -> checkPi ctx p
    Nothing -> throwError $ ProcessNotFound (Pos name)


checkPiSend :: (SName, Expr, Pi) -> Type -> Type -> Bool -> Ctx -> TCM (Ctx, Set SName)
checkPiSend (c,e,p) s t un ctx = do
  ctx' <- checkE ctx e s
  ctx'' <- addCtx (c,t) ctx'
  (id *** addLin un c) <$>
         checkPi ctx'' p

checkPiSel :: (SName, Expr, Pi) -> Map Label Type -> Bool -> Ctx -> TCM (Ctx, Set SName)
checkPiSel (c,e,p) ts un ctx = do
  l <- isLabel e
  s <- lookupLabel ts l
  ctx' <- addCtx (c,s) ctx
  (id *** addLin un c) <$> checkPi ctx' p

checkPiRecv :: (SName, [Clause]) -> Type -> Type -> Bool -> Ctx -> TCM (Ctx, Set SName)
checkPiRecv (c, ps) s t un ctx =
  mapM (checkRecvClause c s t un ctx) ps >>= eqAll

checkRecvClause :: SName -> Type -> Type -> Bool -> Ctx -> Clause -> TCM (Ctx, Set SName)
checkRecvClause name s t un ctx (Clause ptn p) = do
  ctx'' <- matchPtn s ptn ctx >>= addCtx (name, t)
  (ctx''', l) <- checkPi ctx'' p
  return (Map.withoutKeys ctx''' xs,
          let l' = Set.difference l xs
          in if un
              then l'
              else (Set.insert name l'))
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

-- pattern related stuffs

-- free variables of a pattern
ptnVars :: Ptrn -> Set SName
ptnVars (PN x)  = Set.singleton (Pos x)
ptnVars (PT xs) = Set.unions (map ptnVars xs)
ptnVars (PL _)  = Set.empty


matchPtn :: Type -> Ptrn -> Ctx -> TCM Ctx
matchPtn TEnd         (PN x) ctx = addUni (Pos x, TEnd) ctx
matchPtn (TBase t)    (PN x) ctx = addUni (Pos x, TBase t) ctx
matchPtn (TTuple ts)  (PN x) ctx = addUni (Pos x, TTuple ts) ctx
matchPtn (TTuple ts)  (PT xs) ctx = matchPtns ts xs ctx
matchPtn (TSend s t)  (PN x) ctx = addUni (Pos x, TSend s t) ctx
matchPtn (TRecv s t)  (PN x) ctx = addUni (Pos x, TRecv s t) ctx
matchPtn (TSele ts)   (PN x) ctx = addUni (Pos x, TSele ts) ctx
matchPtn (TChoi ts)   (PN x) ctx = addUni (Pos x, TChoi ts) ctx
matchPtn (TUn t)      (PN x) ctx = addUni (Pos x, TUn t) ctx
matchPtn (TUn (TTuple ts)) (PT xs) ctx = matchPtns (map TUn ts) xs ctx
matchPtn t p _ = throwError $ PatternMismatched t p

addUni :: (SName, Type) -> Ctx -> TCM Ctx
addUni (x,t) ctx = case Map.lookup x ctx of
  Just _ -> throwError $ Others (show x ++ " exists in context")
  Nothing -> return $ Map.insert x t ctx

matchPtns :: [Type] -> [Ptrn] -> Ctx -> TCM Ctx
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

addCtx :: (SName, Type) -> Ctx -> TCM Ctx
addCtx (c, t) ctx = case Map.lookup c ctx of
  Just t' -> if eqType t t'
    then return ctx
    else throwError $ Others ("types of channels in env mismatch: " ++ show t ++ " and " ++ show t')
  Nothing -> return $ Map.insert c t ctx

eqAll :: [(Ctx, Set SName)] -> TCM (Ctx, Set SName)
eqAll [] = throwError $ Others "no branches to unify with"
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
  s <- lookupLabel ts l
  Map.insert s p <$> pairChoices (Map.delete l ts) ps
pairChoices _ (Clause _ _ : _) =
  throwError $ Others "not a label"

{-
splitSEnv ::
  [SName] -> [SName] -> SEnv -> TCM (SEnv, SEnv)
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
cp :: String -> SName
cp = Pos . pack
cn :: String -> SName
cn = Neg . pack

fromParseResult :: Show a => Either a b -> b
fromParseResult (Left err) = error $ show err
fromParseResult (Right x) = x

test0 :: Either TypeError (Ctx, Set SName)
test0 = runTCM (checkPi ctx p) initEnv
  where ctx = Map.fromList [(cp "c", tsend TInt (tsend TBool TEnd)),
               (cp "d", TSend (TTuple [tInt, tsend TBool TEnd]) TEnd)]
        p = toAbstract $ fromParseResult (parseProcess "c[3] . d[4,c] . end")

test1 :: Either TypeError (Ctx, Set SName)
test1 = runTCM (checkPi ctx p) initEnv
  where ctx = Map.fromList [(cn "c", trecv TInt (trecv TBool TEnd))]
        p = toAbstract $ fromParseResult (parseProcess "c(x) . c(y) . end")
        -- p = recv (cN "c") (pn "x")  $ recv (cN "c") (pn "y")  End

test2 :: Either TypeError (Ctx, Set SName)
test2 = runTCM (checkPi ctx p) initEnv
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
test3 = runTCM (checkPi ctx0 p0) initEnv
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
    _p1 :: Pi
    _p1 = recv (cP "c") (pn "z") $
          Send (cP "z") (eL "NEG") $
           Send (cP "z") (eI 3) $
               recv (cP "z") (pn "w") End
    -- p2 = nu (c:t1) nu (d:t0) (p0 | p1)
    _p2 :: Pi
    _p2 = nu "c" t1 (nu "d" t0 p0 `Par` _p1)

test4 :: Either TypeError (Ctx, Set SName)
test4 = runTCM (checkPi ctx p) initEnv
  where ctx = Map.fromList [(cp "c", TMu $ TUn $ trecv TInt $ TVar (TypeVarIndex 0))]
        p = Repl (recv (cP "c") (pn "x") End)
        -- p = *(c(x).0)
        -- {c: mu(X)un(?Int.X)}

test5 :: Either TypeError (Ctx, Set SName)
test5 = runTCM (checkPi ctx p) initEnv
  where t = tsend TInt $ trecv TBool TEnd
        -- {c : mu(X)(un(?(!Int.?Bool.0).X))}
        -- p = *(c(d). d[3].d(x).0)
        ctx = Map.fromList [(cp "c", TMu $ TUn $ TRecv t $ TVar (TypeVarIndex 0))]
        p = toAbstract $ fromParseResult (parseProcess "* (c(d) . d[3] . d(x) . end)")
           -- Repl (recv (cP "c") (pn "d") $
           --         Send (cP "d") (eI 3) $
           --           recv (cP "d") (pn "x") End)


-- try: runExcept $ checkPi [] [] p2
-- try: run `stack test` for running these tests
-- try: run `stack repl pi:test:pi-tests` to develop thoses tests



  --   do
  -- case parseProgram "" src of
  --   Left err  -> assertFailure $ show err
  --   Right val -> return val

-- parseProc :: ByteString -> IO Pi
-- parseProc src = do
--   case parseProcess src of
--     Left err  -> assertFailure $ show err
--     Right val -> return val
--
-- testWith :: ByteString -> PiMonad a -> IO [a]
-- testWith source program = do
--   prog <- parseProg source
--   let results = runPiMonad (programToEnv prog) initialState program
--   mapM fromEither results
--   where
--     fromEither (Left err) = assertFailure $ show err
--     fromEither (Right (val, _)) = return val
