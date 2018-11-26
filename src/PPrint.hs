module PPrint where


import Syntax.Abstract
import Type

import Data.Loc hiding (Pos)
import qualified Data.Text.IO as Text
import Data.Text.Prettyprint.Doc hiding (line)
import System.Console.ANSI
import System.IO

{-
Operator Precedences

infixl 3 |
infix  4 .
infix  5 if
infixl 6 + -
infixl 7 * /
infix  8 ? !

-}

--------------------------------------------------------------------------------
-- | Syntax


-- Names
instance Pretty Name where
  pretty (ND x) = pretty x
  pretty (NG i) = pretty ("X" ++ show i)
  pretty (NR StdOut) = pretty "stdout"
  pretty (NR StdIn) = pretty "stdin"

instance Pretty PName where
  pretty (PH x) = pretty x
  pretty (PG i) = pretty ("X" ++ show i)

instance Pretty a => Pretty (PN a) where
  pretty (Pos x) = pretty x
  pretty (Neg x) = pretty "`" <> pretty x

-- Values
instance Pretty Val where
  pretty (N x) = pretty x
  pretty (VI n) = pretty n
  pretty (VB b) = pretty b
  pretty (VL x) = pretty x
  pretty (VT xs) =
    encloseSep langle rangle comma (map pretty xs)

-- Patterns
instance Pretty Ptrn where
  pretty (PN x) = pretty x
  pretty (PT xs) =
     encloseSep langle rangle comma (map pretty xs)
  pretty (PL x) = pretty x

{-
data Ptrn = PN Name         -- patterns
          | PT [Ptrn]
          | PL Label
   deriving Show
-}
shParen :: Bool -> Doc a -> Doc a
shParen False d = d
shParen True d = lparen <> d <> rparen

ppInfixL :: Int -> Doc a ->
            (Int -> Doc a) -> (Int -> Doc a) ->
            (Int -> Doc a)
ppInfixL opp op pp1 pp2 p =
  shParen (p > opp)
    (group . nest 1 . vsep $ [pp1 opp <+> op,  pp2 opp])

ppExpr :: Expr -> Int -> Doc a
ppExpr (EV v) _ = pretty v
ppExpr (EAdd e1 e2) p =
  ppInfixL 6 (pretty "+") (ppExpr e1) (ppExpr e2) p
ppExpr (ESub e1 e2) p =
  ppInfixL 6 (pretty "-") (ppExpr e1) (ppExpr e2) p
ppExpr (EMul e1 e2) p =
  ppInfixL 7 (pretty "*") (ppExpr e1) (ppExpr e2) p
ppExpr (EDiv e1 e2) p =
  ppInfixL 7 (pretty "/") (ppExpr e1) (ppExpr e2) p
ppExpr (EEQ e1 e2) p =
  ppInfixL 8 (pretty "==") (ppExpr e1) (ppExpr e2) p
ppExpr (ENEQ e1 e2) p =
  ppInfixL 8 (pretty "!=") (ppExpr e1) (ppExpr e2) p
ppExpr (EGT e1 e2) p =
  ppInfixL 8 (pretty ">") (ppExpr e1) (ppExpr e2) p
ppExpr (EGTE e1 e2) p =
  ppInfixL 8 (pretty ">=") (ppExpr e1) (ppExpr e2) p
ppExpr (ELT e1 e2) p =
  ppInfixL 8 (pretty "<") (ppExpr e1) (ppExpr e2) p
ppExpr (ELTE e1 e2) p =
  ppInfixL 8 (pretty "<=") (ppExpr e1) (ppExpr e2) p
ppExpr (EIf e0 e1 e2) p =
  shParen (p > 5)
    (sep [pretty "if" <+> nest 2 (ppExpr e0 5),
          pretty "then" <+> nest 2 (ppExpr e1 5),
          pretty "else" <+> nest 2 (ppExpr e2 5)])
ppExpr (ETup es) _ =
  encloseSep langle rangle comma
    (map (flip ppExpr 5) es)
ppExpr (EPrj _ _) _ = error "ppExpr EPrj not defined"


ppPi :: Pi -> Int -> Doc a
ppPi End _ = pretty "end"
ppPi (Send c e p) pr =
  shParen (pr > 4)
   (group . nest 4 . vsep $
        [pretty c <> pretty "!" <>
           ppExpr e 8 <+> pretty ".",
         ppPi p 4])
ppPi (Recv c [Clause xs p]) pr =
  shParen (pr > 4)
   (group . nest 4 . vsep $
     [pretty c <> pretty "?" <> pretty xs <+> pretty ".",
      ppPi p 4])
ppPi (Recv c clauses) pr =
  shParen (pr > 4)
   (pretty c <> pretty "?{" <+>
    align (encloseSep mempty (pretty " }") sepa (map clause clauses)))
 where sepa = flatAlt mempty (pretty "; ")
       clause (Clause xs p) =
          pretty xs <+> pretty "->" <+>
           ppPi p 4
ppPi (Par p1 p2) pr =
  ppInfixL 3 (pretty "|") (ppPi p1) (ppPi p2) pr
ppPi (Nu x t p) pr =
  shParen (pr > 4) $
    group . nest 4 . vsep $
      [ pretty "(nu " <> pretty x <> pretty " : " <> pretty t <> pretty ")"
      , ppPi p 4
      ]
ppPi (Call p) _ = pretty p

-- ppClauses [(xs,p)] =
-- ppClauses pps =

instance Pretty Pi where
  pretty p = ppPi p 0

ppDef :: Name -> Pi -> Doc a
ppDef x p = pretty x <+> pretty "=" <+>
            nest 4 (ppPi p 0)

ppDefs :: [(Name, Pi)] -> Doc a
ppDefs = vsep . map (uncurry ppDef)


-- Types
instance Pretty SType where
  pretty TEnd = pretty "∅"
  pretty (TSend (Left expr) t) = pretty "!" <> pretty expr <> pretty " . " <> pretty t
  pretty (TSend (Right chan) t) = pretty "!" <> pretty chan <> pretty " . " <> pretty t
  pretty (TRecv (Left expr) t) = pretty "?" <> pretty expr <> pretty " . " <> pretty t
  pretty (TRecv (Right chan) t) = pretty "?" <> pretty chan <> pretty " . " <> pretty t
  pretty (TSele selections) =
    pretty "!" <+> align (encloseSep lbracket rbracket semi selections')
    where selections' = map (\(label, t) ->
              pretty label <> pretty " : " <> pretty t) selections
  pretty (TChoi choices) =
    pretty "?" <+> align (encloseSep lbracket rbracket semi choices')
    where choices' = map (\(label, t) ->
              pretty label <> pretty " : " <> pretty t) choices
  pretty (TCall name) = pretty name

instance Pretty BType where
  pretty TInt = pretty "Int"
  pretty TBool = pretty "Bool"
  pretty (TTuple elems) = encloseSep
                    (pretty "(")
                    (pretty ")")
                    (pretty ", ")
                    (map pretty elems)


--------------------------------------------------------------------------------
-- | Source Code Annotation

data SourceCodeAnnotation
  = Other
  | HighlightedLineNo
  | HighlightedArea

data SourceCode = SourceCode
                    String    -- source code
                    Loc       -- highlighted location
                    Int       -- number of the neighboring lines to be rendered

printSourceCode :: SourceCode -> IO ()
printSourceCode = printAnnotation . layoutPretty defaultLayoutOptions . prettySourceCode


printAnnotation :: SimpleDocStream SourceCodeAnnotation -> IO ()
printAnnotation x = case x of
  SFail -> error "panic: failed to render annotated source code"
  SEmpty -> do
    hFlush stdout
    return ()
  SChar c xs -> do
    putChar c
    printAnnotation xs
  SText _ t xs -> do
    Text.putStr t
    printAnnotation xs
  SLine i xs -> do
    putStr ('\n' : replicate i ' ')
    printAnnotation xs
  SAnnPush code xs -> do
    setSGR (translateSourceCodeAnnotation code)
    printAnnotation xs
  SAnnPop xs -> do
    setSGR []
    printAnnotation xs

translateSourceCodeAnnotation :: SourceCodeAnnotation -> [SGR]
translateSourceCodeAnnotation Other             = [Reset]
translateSourceCodeAnnotation HighlightedLineNo = [SetColor Foreground Dull Blue]
translateSourceCodeAnnotation HighlightedArea   = [SetColor Foreground Vivid Red]

-- instance Pretty SourceCode where
prettySourceCode :: SourceCode -> Doc SourceCodeAnnotation
prettySourceCode (SourceCode source NoLoc _) = pretty source
prettySourceCode (SourceCode source (Loc from to) spread) =
  vsep $  [softline']
      ++  zipWith (<+>) lineNos lines'
      ++  [softline', softline']

  where   sourceLines = lines source

          start = posLine from - spread `max` 0
          end   = posLine to   + spread `min` length sourceLines

          -- max width of the greatest line number
          lineNoColumnWidth = 4 * ceiling (fromIntegral (lineNoWidth end) / 4.0 :: Double)

          -- measures the width of a number (decimal)
          lineNoWidth :: Int -> Int
          lineNoWidth = succ . (floor :: Double -> Int) . logBase 10 . fromIntegral

          prettyLineNo :: Int -> Doc SourceCodeAnnotation
          prettyLineNo n =
                pretty (replicate (lineNoColumnWidth - lineNoWidth n) ' ')
            <>  pretty n
            <+> pretty '|'

          lineNos :: [Doc SourceCodeAnnotation]
          lineNos =
                [                              prettyLineNo n | n <- [ start          .. posLine from - 1 ]  ]
            ++  [ annotate HighlightedLineNo $ prettyLineNo n | n <- [ posLine from   .. posLine to       ]  ]
            ++  [                              prettyLineNo n | n <- [ posLine to + 1 .. end                  ]  ]


          lines' :: [Doc SourceCodeAnnotation]
          lines' = map prettyLine $ zip [ start .. end ]
            $ drop (start - 1)
            $ take end
            $ sourceLines

          substring :: Int -> Maybe Int -> String -> String
          substring startFrom Nothing       = drop (startFrom - 1)
          substring startFrom (Just endAt)  = drop (startFrom - 1) . take endAt

          prettyLine :: (Int, String) -> Doc SourceCodeAnnotation
          prettyLine (n, line)
            | n == posLine from && n == posLine to =
                    annotate Other            (pretty $ substring 0              (Just (posCol from - 1)) line)
                <>  annotate HighlightedArea  (pretty $ substring (posCol from)  (Just (posCol to))       line)
                <>  annotate Other            (pretty $ substring (posCol to + 1) Nothing                 line)
            | n == posLine from =
                    annotate Other            (pretty $ substring 0              (Just (posCol from - 1)) line)
                <>  annotate HighlightedArea  (pretty $ substring (posCol from)  Nothing                  line)
            | n == posLine to =
                    annotate HighlightedArea  (pretty $ substring 0              (Just (posCol to))       line)
                <>  annotate Other            (pretty $ substring (posCol to)    Nothing                  line)
            | n > posLine from && n < posLine to =
                    annotate HighlightedArea  (pretty line)
            | otherwise =
                    annotate Other            (pretty line)
