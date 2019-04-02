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
import Data.Loc (locOf, Loc(..))

import qualified Syntax.Abstract as A
import qualified Type as A
import Syntax.Abstract (Expr(..), Val(..))
import Type (HasDual(..))

import qualified Syntax.Concrete as C
import Syntax.Concrete hiding (Expr(..))

-- import Syntax.Concrete (toAbstract, Type)
import Syntax.Parser (parseProc)
-- import Type hiding (Type)
import Base
import Debug.Trace

import Control.Monad.State


--------------------------------------------------------------------------------
-- | State

-- data TCState = TCState {}

--------------------------------------------------------------------------------
-- | Error

data TypeError
  = MissingProcDefn (Map ProcName Type)
  | VariableNotFound Chan
  | TypeVariableNotFound TypeVar
  | TypeVarIndexAtTopLevel TypeVar
  | LabelNotFound Label
  | ProcessNotFound ProcName
  | PatternMismatched Type Ptrn
  | TypeOfNewChannelMissing Chan
  | RecvExpected Type
  | SendExpected Type
  | UserDefinedNameExpected Chan
  | Others String
  deriving (Show)

--------------------------------------------------------------------------------
-- | Type Checking Monad

type TCM = ExceptT TypeError (State Env)

runTCM :: TCM a -> Env -> Either TypeError a
runTCM = evalState . runExceptT

putChanTypes :: Map Chan Type -> TCM ()
putChanTypes x = modify (\st -> st { envChanTypes = x })

putProcDefns :: Map ProcName Proc -> TCM ()
putProcDefns x = modify (\st -> st { envProcDefns = x })

--------------------------------------------------------------------------------
-- | Some checkings

--------------------------------------------------------------------------------
-- | Checkings



checkAll :: TCM ()
checkAll = do

  typeDefns <- gets envTypeDefns
  procDefns <- gets envProcDefns


  -- substitute type variables in `chanTypes`
  gets envChanTypes
    >>= Map.traverseWithKey (\_ x -> substituteTypeVar x)
    >>= putChanTypes
  chanTypes <- gets envChanTypes

  -- gets procDefns
  --     >>= Map.traverseWithKey (\_ x -> substituteTypeVar x)
  --     >>= putProcDefns


  -- not checking if some process named "test" exists
  case Map.lookup (ProcName "test" NoLoc) procDefns of
    Nothing -> do
      case Map.lookup (ProcName "main" NoLoc) procDefns of
        Nothing -> return ()
        Just p -> void $ checkProc chanTypes p
    Just _ -> return ()

  return ()


--------------------------------------------------------------------------------
-- | Code


mapEqBy :: Ord k => (a -> a -> Bool) -> Map k a -> Map k a -> Bool
mapEqBy f a b = Map.isSubmapOfBy f a b && Map.isSubmapOfBy f b a

type Ctx = Map Chan Type

liftMaybe :: TypeError -> Maybe a -> TCM a
liftMaybe err = maybe (throwError err) return

lookupTypeVar :: TypeVar -> TCM Type
lookupTypeVar (TypeVarIndex x l) = return $ TypeVar (TypeVarIndex x l ) l
  -- throwError $ TypeVarIndexAtTopLevel (TypeVarIndex x)
lookupTypeVar (TypeVarText x l) = do
  typeDefns <- gets envTypeDefns
  case Map.lookup x typeDefns of
    Just v -> return v
    Nothing -> throwError $ TypeVariableNotFound (TypeVarText x l)

lookupChan :: Chan -> Ctx -> TCM (Type, Bool, Ctx)
lookupChan x ctx = do
  t <- lookupVar ctx x
  let (t', unrest) = stripUnrestricted (unfoldType t)
  if unrest
    then return (t', unrest, ctx)
    else return (t', unrest, Map.delete x ctx)

lookupVar :: Ctx -> Chan -> TCM Type
lookupVar ctx x = case Map.lookup x ctx of
  Just v -> substituteTypeVar v
  Nothing -> case Map.lookup (dual x) ctx of
    Just v -> substituteTypeVar (dual v)
    Nothing -> throwError $ VariableNotFound x

substituteTypeOfLabel :: TypeOfLabel -> TCM TypeOfLabel
substituteTypeOfLabel (TypeOfLabel label t l) = do
  t' <- substituteTypeVar t
  return $ TypeOfLabel label t' l

substituteTypeVar :: Type -> TCM Type
substituteTypeVar (TypeEnd l)      = return (TypeEnd l)
substituteTypeVar (TypeBase t l)   = return (TypeBase t l)
substituteTypeVar (TypeTuple ts l) = TypeTuple <$> mapM substituteTypeVar ts <*> pure l
substituteTypeVar (TypeSend t s l) = TypeSend <$> substituteTypeVar t <*> substituteTypeVar s <*> pure l
substituteTypeVar (TypeRecv t s l) = TypeRecv <$> substituteTypeVar t <*> substituteTypeVar s <*> pure l
substituteTypeVar (TypeChoi ss l)  = TypeChoi <$> mapM substituteTypeOfLabel ss <*> pure l
substituteTypeVar (TypeSele ss l)  = TypeSele <$> mapM substituteTypeOfLabel ss <*> pure l
substituteTypeVar (TypeUn t l)     = TypeUn <$> substituteTypeVar t <*> pure l
substituteTypeVar (TypeVar (TypeVarText (TypeName "X" n) m) l) = return $ TypeVar (TypeVarText (TypeName "X" n) m) l -- mu
substituteTypeVar (TypeVar i l)    = lookupTypeVar i
substituteTypeVar (TypeMu t l)     = TypeMu <$> substituteTypeVar t <*> pure l


lookupLabel :: Map Label Type -> Label -> TCM Type
lookupLabel env x = case Map.lookup x env of
  Just v -> return v
  Nothing -> throwError $ LabelNotFound x


inferV :: Ctx -> Val -> TCM (Type, Ctx)
inferV _ (VC (A.NR _)) =
  throwError $ Others "StdOut/In in expression"
inferV ctx (VC (A.ND c)) = do
  let c' = case c of
            A.Pos x -> Pos x NoLoc
            A.Neg x -> Neg x NoLoc
  case Map.lookup c' ctx of
    Nothing -> throwError $ Others $ "variable " ++ show c ++ " not found"
    Just t -> if A.unrestricted (toAbstract t)
                then return (t, ctx)
                else return (t, Map.delete c' ctx)
inferV _ (VC (A.NG _)) =
  throwError $ Others "Unable to infer system generated variables"
inferV ctx (VI _) = return (tInt, ctx)
inferV ctx (VB _) = return (tBool, ctx)
inferV ctx (VT vs) =
  ((\t -> TypeTuple t NoLoc) *** id) <$> inferVs ctx vs
-- inferV env (VL l) = liftMaybe "label not found" (lookup l env)
inferV _ (VS _) = throwError $ Others "panic: string value not implemented yet"
inferV _ (VL _) = throwError $ Others "panic: label value not implemented yet"
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
inferE ctx (EAdd e1 e2) = do
  ctx' <- checkE ctx  e1 tInt
  ctx'' <- checkE ctx' e2 tInt
  return (tInt, ctx'')
inferE ctx (ESub e1 e2) = do
  ctx' <- checkE ctx  e1 tInt
  ctx'' <- checkE ctx' e2 tInt
  return (tInt, ctx'')
inferE ctx (EMul e1 e2) = do
  ctx' <- checkE ctx  e1 tInt
  ctx'' <- checkE ctx' e2 tInt
  return (tInt, ctx'')
inferE ctx (EIf e0 e1 e2) = do
  ctx' <- checkE ctx e0 tBool
  (t, ctx'') <- inferE ctx' e1
  ctx''' <- checkE ctx' e2 t
  if mapEqBy (==) ctx'' ctx'''
    then return (t, ctx'')
    else throwError $ Others "contexts fail to unify when checking If"
inferE ctx (ETup es) =
  ((\t -> TypeTuple t NoLoc) *** id) <$> inferEs ctx es
inferE _ e = throwError $ Others ("panic: not implemented yet " ++ show e )

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
   | t1 == t2 = return ()
   | otherwise = throwError $ Others ("expected: " ++ show t1 ++
                             ", found " ++ show t2)

checkCh :: Ctx -> Chan -> Type -> TCM ()
checkCh senv c t = do
  t' <- lookupVar senv c
  if t == t'
    then return ()
    else throwError $ Others "type mismatch"

allClosed :: Ctx -> Bool
allClosed = all isTypeEnd . Map.elems
  where
    isTypeEnd (TypeEnd _) = True
    isTypeEnd _           = False

checkProc :: Ctx -> Proc -> TCM (Ctx, Set Chan)

checkProc ctx (End _) = return (ctx, Set.empty)

checkProc ctx (Par p1 p2 _) = do
  (ctx1, l1) <- checkProc ctx p1
  ctx2 <- ctx1 `removeChannels` l1
  checkProc ctx2 p2

checkProc ctx (Send (Res "stdout" _) e p _) = do
  (_, ctx') <- inferE ctx (toAbstract e)
  checkProc ctx' p
checkProc _   (Send (Res "stdin" _) _ _ _) =
  throwError $ Others "cannot send to this channel"
checkProc ctx (Send chan e p _) = do
  (channelType, un, ctx') <- lookupChan chan ctx
  case channelType of
    TypeSend s1 s2 _ -> checkProcSend (chan, e, p) s1 s2 un ctx'
    TypeChoi ts    _ -> checkProcSel  (chan, e, p) (Map.fromList $ map typedLabelToPair ts) un ctx'
    others      -> do
      chanTypes <- gets envChanTypes
      traceShow chanTypes $ return ()
      throwError $ SendExpected others

checkProc _ (Recv (Res _ _) _ _) =
  throwError $ Others "not knowing what to do yet"
checkProc ctx (Recv chan ps _) = do

  (channelType, un, ctx') <- lookupChan chan ctx
  case channelType of
    TypeRecv t s _ -> checkProcRecv (chan,ps) t s un ctx'
    TypeChoi ts  _ -> checkProcChoi (chan,ps) (Map.fromList $ map typedLabelToPair ts) un ctx'
    others    -> throwError $ RecvExpected others

checkProc ctx (Repl p _) =
  checkProc ctx p >>= \(ctx', l) ->
  if null l then return (ctx', l)
     else throwError $ Others ("linear variable used in repetition")

checkProc _ (Nu x Nothing _ _) =
  throwError $ TypeOfNewChannelMissing x
checkProc ctx (Nu x (Just t) p loc) = do
  t' <- substituteTypeVar t
  -- traceShow t' $ return ()
  (ctx', l) <- checkProc (Map.insert x t' $ Map.insert (A.dual x) (dual t') ctx) p
  ctx'' <- ctx' `removeChannels` Set.fromList [x, A.dual x]
  return (ctx'', Set.difference l (Set.fromList [x, A.dual x]))

checkProc ctx (Call name _) = do
  env <- gets envProcDefns
  case Map.lookup name env of
    Just p  -> checkProc ctx p
    Nothing -> throwError $ ProcessNotFound name


checkProcSend :: (Chan, C.Expr, Proc) -> Type -> Type -> Bool -> Ctx -> TCM (Ctx, Set Chan)
checkProcSend (c,e,p) s t un ctx = do
  -- error $ show (e, s, ctx)
  ctx' <- checkE ctx (toAbstract e) s
  ctx'' <- addCtx (c,t) ctx'
  (id *** addLin un c) <$>
         checkProc ctx'' p

checkProcSel :: (Chan, C.Expr, Proc) -> Map Label Type -> Bool -> Ctx -> TCM (Ctx, Set Chan)
checkProcSel (c,e,p) ts un ctx = do
  l <- extractLabel e
  s <- lookupLabel ts l
  ctx' <- addCtx (c,s) ctx
  (id *** addLin un c) <$> checkProc ctx' p

checkProcRecv :: (Chan, [Clause]) -> Type -> Type -> Bool -> Ctx -> TCM (Ctx, Set Chan)
checkProcRecv (c, ps) s t un ctx =
  mapM (checkRecvClause c s t un ctx) ps >>= eqAll

checkRecvClause :: Chan -> Type -> Type -> Bool -> Ctx -> Clause -> TCM (Ctx, Set Chan)
checkRecvClause name s t un ctx (Clause ptn p _) = do
  ctx'' <- matchPtn s ptn ctx >>= addCtx (name, t)
  (ctx''', l) <- checkProc ctx'' p
  return (Map.withoutKeys ctx''' xs,
          let l' = Set.difference l xs
          in if un
              then l'
              else (Set.insert name l'))
  where xs = ptrnVars ptn

checkProcChoi :: (Chan, [Clause]) -> Map Label Type -> Bool -> Ctx -> TCM (Ctx, Set Chan)
checkProcChoi (c, ps) ts un ctx = do
   xss <- pairChoices ts ps
   res <- mapM (\(s, p) -> do
            ctx' <- addCtx (c,s) ctx
            checkProc ctx' p) $ Map.toList xss
   (id *** addLin un c) <$> eqAll res

addLin :: Ord a => Bool -> a -> Set a -> Set a
addLin True  _ xs = xs
addLin False c xs = Set.insert c xs

extractLabel :: C.Expr -> TCM Label
extractLabel (C.ExprLabel label _) = return label
extractLabel _ = throwError $ Others "must be a label"

-- pattern related stuffs

-- free variables of a pattern
ptrnVars :: Ptrn -> Set Chan
ptrnVars (PtrnVar v _)  = Set.singleton (varToChan v)
ptrnVars (PtrnTuple xs _) = Set.unions (map ptrnVars xs)
ptrnVars (PtrnLabel _ _)  = Set.empty

varToChan :: Var -> Chan
varToChan (Var x l) = Pos x l

matchPtn :: Type -> Ptrn -> Ctx -> TCM Ctx
matchPtn (TypeEnd l)       (PtrnVar v _) ctx = addUni (varToChan v, TypeEnd l) ctx
matchPtn (TypeBase t l)    (PtrnVar v _) ctx = addUni (varToChan v, TypeBase t l) ctx
matchPtn (TypeTuple ts l)  (PtrnVar v _) ctx = addUni (varToChan v, TypeTuple ts l) ctx
matchPtn (TypeTuple ts l)  (PtrnTuple xs _) ctx = matchPtns ts xs ctx
matchPtn (TypeSend s t l)  (PtrnVar v _) ctx = addUni (varToChan v, TypeSend s t l) ctx
matchPtn (TypeRecv s t l)  (PtrnVar v _) ctx = addUni (varToChan v, TypeRecv s t l) ctx
matchPtn (TypeSele ts l)   (PtrnVar v _) ctx = addUni (varToChan v, TypeSele ts l) ctx
matchPtn (TypeChoi ts l)   (PtrnVar v _) ctx = addUni (varToChan v, TypeChoi ts l) ctx
matchPtn (TypeUn t l)      (PtrnVar v _) ctx = addUni (varToChan v, TypeUn t l) ctx
matchPtn (TypeUn (TypeTuple ts _) _) (PtrnTuple xs _) ctx = matchPtns components xs ctx
  where   components = map (\t -> TypeUn t (locOf t)) ts
matchPtn (TypeVar t l)     (PtrnVar x m) ctx = do
  t' <- substituteTypeVar (TypeVar t l)
  matchPtn t' (PtrnVar x m) ctx
matchPtn t p _ = throwError $ PatternMismatched t p

addUni :: (Chan, Type) -> Ctx -> TCM Ctx
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
removeChannels :: Ctx -> Set Chan -> TCM Ctx
removeChannels ctx names = do
  let inCtx = Map.elems $ Map.restrictKeys ctx names
  unless (all A.unrestricted $ fmap toAbstract inCtx) $
    throwError $ Others ("channel " ++ show inCtx ++ " not used up.")
  return $ Map.withoutKeys ctx names

addCtx :: (Chan, Type) -> Ctx -> TCM Ctx
addCtx (c, t) ctx = case Map.lookup c ctx of
  Just t' -> if t == t'
    then return ctx
    else throwError $ Others ("types of channels in env mismatch: " ++ show t ++ " and " ++ show t')
  Nothing -> return $ Map.insert c t ctx

eqAll :: [(Ctx, Set Chan)] -> TCM (Ctx, Set Chan)
eqAll [] = throwError $ Others "no branches to unify with"
eqAll [x] = return x
eqAll ((s1,l1):(s2,l2):xs) =
  if (mapEqBy (==) s1 s2) && (l1 == l2)
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

pairChoices :: Map Label Type -> [Clause] -> TCM (Map Type Proc)
pairChoices ts [] = if Map.null ts
  then return Map.empty
  else throwError $ Others "choices not fully covered"
pairChoices ts (Clause (PtrnLabel l _) p _ : ps) = do
  s <- lookupLabel ts l
  Map.insert s p <$> pairChoices (Map.delete l ts) ps
pairChoices _ (Clause _ _ _: _) =
  throwError $ Others "not a label"

{-
splitSEnv ::
  [Name] -> [Name] -> SEnv -> TCM (SEnv, SEnv)
splitSEnv _ _ [] = return ([],[])
splitSEnv fl fr ((x,t):xs)
   | bl && br  = throwError ("channel " ++ show x ++ " not linear")
   | bl        = (((x,t):) *** id) <$> splitSEnv fl fr xs
   | br        = (id *** ((x,t):)) <$> splitSEnv fl fr xs
   | otherwise = throwError ("channel " ++ show x ++ " dropped")
  where (bl, br) = (x `elem` fl, x `elem` fr)
-}

-- --------------------------------------------------------------------------------
-- -- Tests
--
-- -- smart constructors for Types
--
-- tsend :: BType -> Type -> Type
-- tsend t s = TypeSend (TypeBase t) s
--
-- trecv :: BType -> Type -> Type
-- trecv t s = TypeRecv (TypeBase t) s
--
-- tsele :: [(String, Type)] -> Type
-- tsele = TypeSele . map (pack *** id)
--
-- -- smart constructors for Proc
--
-- nu :: String -> Type -> Proc -> Proc
-- nu c t p = Nu (pack c) (Just t) p
--
-- recv :: Chan -> Ptrn -> Proc -> Proc
-- recv c ptn p = Recv c [Clause ptn p]
--
-- choices :: Chan -> [(String, Proc)] -> Proc
-- choices c ps =
--    Recv c (map (uncurry Clause . ((PtrnLabel . pack) *** id)) ps)
--
-- pn :: String -> Ptrn
-- pn = PtrnVar . pack
-- cp :: String -> Name
-- cp = Pos . pack
-- cn :: String -> Name
-- cn = Neg . pack
--
-- fromParseResult :: Show a => Either a b -> b
-- fromParseResult (Left err) = error $ show err
-- fromParseResult (Right x) = x
--
-- test0 :: Either TypeError (Ctx, Set Chan)
-- test0 = runTCM (checkProc ctx p) initEnv
--   where ctx = Map.fromList [(cp "c", tsend TInt (tsend TBool TypeEnd)),
--                (cp "d", TypeSend (TypeTuple [A.tInt, tsend TBool TypeEnd]) TypeEnd)]
--         p = toAbstract $ fromParseResult (parseProc "c[3] . d[4,c] . end")
--
-- test1 :: Either TypeError (Ctx, Set Chan)
-- test1 = runTCM (checkProc ctx p) initEnv
--   where ctx = Map.fromList [(cn "c", trecv TInt (trecv TBool TypeEnd))]
--         p = toAbstract $ fromParseResult (parseProc "c(x) . c(y) . end")
--         -- p = recv (cN "c") (pn "x")  $ recv (cN "c") (pn "y")  End
--
-- test2 :: Either TypeError (Ctx, Set Chan)
-- test2 = runTCM (checkProc ctx p) initEnv
--   where p = nu "c" t (Par p1 p2)
--         p1 = Send (cP "c") (eI 3) $ Send (cP "c") (eB False) End
--         p2 = recv (cN "c") (pn "x") $ recv (cN "c") (pn "y") End
--         t = tsend TInt (tsend TBool TypeEnd)
--         ctx = Map.fromList [(cp "c", t), (cn "c", dual t)]
--         {-  p  = nu (c:t) (p1 | p2)
--             p1 = c[3].c[False].0
--             p2 = c~(x).c~(y).0
--             t  = !Int.!Bool.0
--         -}
--
-- test3 :: Either TypeError (Ctx, Set Chan)
-- test3 = runTCM (checkProc ctx0 p0) initEnv
--   where
--     t0 :: Type
--     t0 = tsele [("NEG", tsend TInt  $ trecv TInt  $ TypeEnd),
--                 ("ID",  tsend TBool $ trecv TBool $ TypeEnd)]
--
--     t1 :: Type
--     t1 = TypeRecv t0 TypeEnd
--
--     ctx0 :: Ctx
--     ctx0 = Map.fromList [ (cp "c", t1)
--            , (cn "c", dual t1)
--            , (cp "d", t0)
--            , (cn "d", dual t0)
--            ]
--
--     -- p0 = c[d].d~>>{NEG -> d(x).d[-x].0 ;
--     --                ID -> d(x).d[x].0 }
--     p0 :: Proc
--     p0 = Send (cN "c") (ePtrnVar "d") $
--           choices (cN "d")
--             [("NEG", recv (cN "d") (pn "x") $
--                        Send (cN "d") (eI 0 `ESub` ePtrnVar "x") End),
--              ("ID", recv (cN "d") (pn "x") $
--                         Send (cN "d") (ePtrnVar "x") End)]
--
--     -- p1 = c(z).z<<NEG.z[3].z(w).0
--     _p1 :: Proc
--     _p1 = recv (cP "c") (pn "z") $
--           Send (cP "z") (eL "NEG") $
--            Send (cP "z") (eI 3) $
--                recv (cP "z") (pn "w") End
--     -- p2 = nu (c:t1) nu (d:t0) (p0 | p1)
--     _p2 :: Proc
--     _p2 = nu "c" t1 (nu "d" t0 p0 `Par` _p1)
--
-- test4 :: Either TypeError (Ctx, Set Chan)
-- test4 = runTCM (checkProc ctx p) initEnv
--   where ctx = Map.fromList [(cp "c", TypeMu $ TypeUn $ trecv TInt $ TypeVar (TypeVarIndex 0))]
--         p = Repl (recv (cP "c") (pn "x") End)
--         -- p = *(c(x).0)
--         -- {c: mu(X)un(?Int.X)}
--
-- test5 :: Either TypeError (Ctx, Set Chan)
-- test5 = runTCM (checkProc ctx p) initEnv
--   where t = tsend TInt $ trecv TBool TypeEnd
--         -- {c : mu(X)(un(?(!Int.?Bool.0).X))}
--         -- p = *(c(d). d[3].d(x).0)
--         ctx = Map.fromList [(cp "c", TypeMu $ TypeUn $ TypeRecv t $ TypeVar (TypeVarIndex 0))]
--         p = toAbstract $ fromParseResult (parseProc "* (c(d) . d[3] . d(x) . end)")
--            -- Repl (recv (cP "c") (pn "d") $
--            --         Send (cP "d") (eI 3) $
--            --           recv (cP "d") (pn "x") End)


-- try: runExcept $ checkProc [] [] p2
-- try: run `stack test` for running these tests
-- try: run `stack repl pi:test:pi-tests` to develop thoses tests



  --   do
  -- case parseProgram "" src of
  --   Left err  -> assertFailure $ show err
  --   Right val -> return val

-- parseProc :: ByteString -> IO Proc
-- parseProc src = do
--   case parseProc src of
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
