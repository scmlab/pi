module PPrint where

import Data.Text.Prettyprint.Doc
  -- https://hackage.haskell.org/package/prettyprinter
  -- cabal install prettyprinter
import Data.Text.Prettyprint.Doc.Util (putDocW)
import Control.Arrow ((***))
import Syntax

{-
Operator Precedences

infixl 3 |
infix  4 .
infix  5 if
infixl 6 + -
infixl 7 * /
infix  8 ? !

-}

instance Pretty Name where
  pretty (NS x) = pretty x
  pretty (NG i) = pretty ("X"++show i)
  pretty (NR StdOut) = pretty "StdOut"
  pretty (NR StdIn) = pretty "StdIn"

instance Pretty Val where
  pretty (N x) = pretty x
  pretty (VI n) = pretty n
  pretty (VB b) = pretty b
  pretty (VL x) = pretty x
  pretty (VT xs) =
    encloseSep langle rangle comma (map pretty xs)

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
ppExpr (EV v) p = pretty v
ppExpr (EPlus e1 e2) p =
  ppInfixL 6 (pretty "+") (ppExpr e1) (ppExpr e2) p
ppExpr (EMinus e1 e2) p =
  ppInfixL 6 (pretty "-") (ppExpr e1) (ppExpr e2) p
ppExpr (EIf e0 e1 e2) p =
  shParen (p > 5)
    (sep [pretty "if" <+> nest 2 (ppExpr e0 5),
          pretty "then" <+> nest 2 (ppExpr e1 5),
          pretty "else" <+> nest 2 (ppExpr e2 5)])
ppExpr (ETup es) p =
  encloseSep langle rangle comma
    (map (flip ppExpr 5) es)

ppPi :: Pi -> Int -> Doc a
ppPi End _ = pretty "end"
ppPi (Send c e p) pr =
  shParen (pr > 4)
   (group . nest 4 . vsep $
        [pretty c <> pretty "!" <>
           ppExpr e 8 <+> pretty ".",
         ppPi p 4])
ppPi (Recv c [(xs,p)]) pr =
  shParen (pr > 4)
   (group . nest 4 . vsep $
     [pretty c <> pretty "?" <> pretty xs <+> pretty ".",
      ppPi p 4])
ppPi (Recv c pps) pr =
  shParen (pr > 4)
   (pretty c <> pretty "?{" <+>
    align (encloseSep mempty (pretty " }") sepa (map clause pps)))
 where sepa = flatAlt mempty (pretty "; ")
       clause (xs,p) =
          pretty xs <+> pretty "->" <+>
           ppPi p 4
ppPi (Par p1 p2) pr =
  ppInfixL 3 (pretty "|") (ppPi p1) (ppPi p2) pr
ppPi (Nu x p) pr =
  shParen (pr > 4)
   (group . nest 4 . vsep $
     [pretty "(nu " <> pretty x <> pretty ")",
      ppPi p 4])
ppPi (Call p) pr = pretty p

-- ppClauses [(xs,p)] =
-- ppClauses pps =

instance Pretty Pi where
  pretty p = ppPi p 0

ppDef :: Name -> Pi -> Doc a
ppDef x p = pretty x <+> pretty "=" <+>
            nest 4 (ppPi p 0)

ppDefs :: [(Name, Pi)] -> Doc a
ppDefs = vsep . map (uncurry ppDef)
