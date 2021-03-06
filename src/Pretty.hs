{-# LANGUAGE OverloadedStrings #-}

module Pretty where

import Pretty.Syntax.Concrete ()

import Syntax.Abstract
import Type

import Data.Loc (Loc(..), posLine, posCol)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS

import Data.Monoid (mempty, (<>))
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
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
instance Pretty Chan where
  pretty (ND x) = pretty x
  pretty (NG i) = "x" <> pretty i
  pretty (NR StdOut) = "stdout"
  pretty (NR StdIn) = "stdin"

instance Pretty a => Pretty (Polarised a) where
  pretty (Pos x) = pretty x
  pretty (Neg x) = "~" <> pretty x

-- Values
instance Pretty Val where
  pretty (VC x) = pretty x
  pretty (VI n) = pretty n
  pretty (VB b) = pretty b
  pretty (VL x) = pretty x
  pretty (VT xs) =
    encloseSep langle rangle comma (map pretty xs)
  pretty (VS s) =
    pretty s

-- Patterns
instance Pretty Ptrn where
  pretty (PtrnVar x) = pretty x
  pretty (PtrnTuple xs) =
     encloseSep langle rangle comma (map pretty xs)
  pretty (PtrnLabel x) = pretty x

{-
data Ptrn = PtrnVar Name         -- patterns
          | PtrnTuple [Ptrn]
          | PtrnLabel Label
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
  ppInfixL 6 "+" (ppExpr e1) (ppExpr e2) p
ppExpr (ESub e1 e2) p =
  ppInfixL 6 "-" (ppExpr e1) (ppExpr e2) p
ppExpr (EMul e1 e2) p =
  ppInfixL 7 "*" (ppExpr e1) (ppExpr e2) p
ppExpr (EDiv e1 e2) p =
  ppInfixL 7 "/" (ppExpr e1) (ppExpr e2) p
ppExpr (EEQ e1 e2) p =
  ppInfixL 8 "==" (ppExpr e1) (ppExpr e2) p
ppExpr (ENEQ e1 e2) p =
  ppInfixL 8 "!=" (ppExpr e1) (ppExpr e2) p
ppExpr (EGT e1 e2) p =
  ppInfixL 8 ">" (ppExpr e1) (ppExpr e2) p
ppExpr (EGTE e1 e2) p =
  ppInfixL 8 ">=" (ppExpr e1) (ppExpr e2) p
ppExpr (ELT e1 e2) p =
  ppInfixL 8 "<" (ppExpr e1) (ppExpr e2) p
ppExpr (ELTE e1 e2) p =
  ppInfixL 8 "<=" (ppExpr e1) (ppExpr e2) p
ppExpr (EIf e0 e1 e2) p =
  shParen (p > 5)
    (sep ["if" <+> nest 2 (ppExpr e0 5),
          "then" <+> nest 2 (ppExpr e1 5),
          "else" <+> nest 2 (ppExpr e2 5)])
ppExpr (ETup es) _ =
  encloseSep langle rangle comma
    (map (flip ppExpr 5) es)
ppExpr (EPrj _ _) _ = error "ppExpr EPrj not defined"


ppProc :: Proc -> Int -> Doc a
ppProc End _ = "end"
ppProc (Send c e p) pr =
  shParen (pr > 4)
   (group . nest 4 . vsep $
        [pretty c <> "!" <>
           ppExpr e 8 <+> ".",
         ppProc p 4])
ppProc (Recv c [Clause xs p]) pr =
  shParen (pr > 4)
   (group . nest 4 . vsep $
     [pretty c <> "?" <> pretty xs <+> ".",
      ppProc p 4])
ppProc (Recv c clauses) pr =
  shParen (pr > 4)
   (pretty c <> "?{" <+>
    align (encloseSep mempty " }" sepa (map clause clauses)))
 where sepa = flatAlt mempty "; "
       clause (Clause xs p) =
          pretty xs <+> "->" <+>
           ppProc p 4
ppProc (Par p1 p2) pr =
  ppInfixL 3 "|" (ppProc p1) (ppProc p2) pr
ppProc (Nu x Nothing p) pr =
  shParen (pr > 4) $
    group . nest 4 . vsep $
      [ "(nu " <> pretty x <> ")"
      , ppProc p 4
      ]
ppProc (Nu x (Just t) p) pr =
  shParen (pr > 4) $
    group . nest 4 . vsep $
      [ "(nu " <> pretty x <> " : " <> pretty t <> ")"
      , ppProc p 4
      ]
ppProc (Repl p) _ = "*" <+> pretty p
ppProc (Call p) _ = pretty p

-- ppClauses [(xs,p)] =
-- ppClauses pps =

instance Pretty Proc where
  pretty p = ppProc p 0

ppDef :: Chan -> Proc -> Doc a
ppDef x p = pretty x <+> "=" <+>
            nest 4 (ppProc p 0)

ppDefs :: [(Chan, Proc)] -> Doc a
ppDefs = vsep . map (uncurry ppDef)

-- Types
instance Pretty TypeVar where
  pretty (TypeVarIndex i) = "$" <> pretty i
  pretty (TypeVarText n) = pretty n

instance Pretty Type where
  pretty TEnd = "∅"
  pretty (TBase t) = pretty t
  pretty (TTuple elems) = encloseSep "(" ")" ", " $ map pretty elems
  pretty (TSend t u) = "!" <> pretty t <> " . " <> pretty u
  pretty (TRecv t u) = "?" <> pretty t <> " . " <> pretty u
  pretty (TSele selections) =
    "!" <+> align (encloseSep lbracket rbracket semi selections')
    where selections' = map (\(label, t) ->
              pretty label <> " : " <> pretty t) selections
  pretty (TChoi choices) =
    "?" <+> align (encloseSep lbracket rbracket semi choices')
    where choices' = map (\(label, t) ->
              pretty label <> " : " <> pretty t) choices
  pretty (TUn t) = "un(" <> pretty t <> ")"
  pretty (TVar t) = pretty t
  pretty (TMu t) = "μ(" <> pretty t <> ")"


instance Pretty BType where
  pretty TInt  = "Int"
  pretty TBool = "Bool"

--------------------------------------------------------------------------------
-- | Source Code Annotation

data SourceCodeAnnotation
  = Other
  | HighlightedLineNo
  | HighlightedArea

data SourceCode = SourceCode
                    ByteString    -- source code
                    Loc       -- highlighted location
                    Int       -- number of the neighboring lines to be rendered


printSourceCode :: SourceCode -> IO ()
printSourceCode = renderIO stdout . reAnnotateS toAnsiStyle . layoutPretty defaultLayoutOptions . prettySourceCode

toAnsiStyle :: SourceCodeAnnotation -> AnsiStyle
toAnsiStyle Other = mempty
toAnsiStyle HighlightedLineNo = colorDull Blue
toAnsiStyle HighlightedArea = color Red

-- instance Pretty SourceCode where
prettySourceCode :: SourceCode -> Doc SourceCodeAnnotation
prettySourceCode (SourceCode source NoLoc _) = pretty $ BS.unpack source
prettySourceCode (SourceCode source (Loc from to) spread) =
  vsep $  [softline']
      ++  zipWith (<+>) lineNos lines'
      ++  [softline', softline']

  where   sourceLines = lines (BS.unpack source)

          start = (posLine from - spread) `max` 1
          end   = (posLine to   + spread) `min` length sourceLines

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
          prettyLine (n, s)
            | n == posLine from && n == posLine to =
                    annotate Other            (pretty $ substring 0              (Just (posCol from - 1)) s)
                <>  annotate HighlightedArea  (pretty $ substring (posCol from)  (Just (posCol to))       s)
                <>  annotate Other            (pretty $ substring (posCol to + 1) Nothing                 s)
            | n == posLine from =
                    annotate Other            (pretty $ substring 0              (Just (posCol from - 1)) s)
                <>  annotate HighlightedArea  (pretty $ substring (posCol from)  Nothing                  s)
            | n == posLine to =
                    annotate HighlightedArea  (pretty $ substring 0              (Just (posCol to))       s)
                <>  annotate Other            (pretty $ substring (posCol to)    Nothing                  s)
            | n > posLine from && n < posLine to =
                    annotate HighlightedArea  (pretty s)
            | otherwise =
                    annotate Other            (pretty s)

-- prettyError :: Text -> Maybe Text -> [Loc] -> ByteString -> Doc AnsiStyle
-- prettyError header message locations source = vsep
--   [ softline'
--   , indent 2 text
--   , indent 4 pieces
--   , softline'
--   ]
--   where
--     text = vsep
--       [ annotate (color Red) $ pretty header
--       , case message of
--           Just m -> pretty m <> line
--           Nothing -> mempty
--       ]
--     pieces = vsep $ map (\loc -> vsep
--       [ annotate (colorDull Blue) (pretty $ Loc.displayLoc loc)
--       , reAnnotate toAnsiStyle $ prettySourceCode $ SourceCode source loc 1
--       ]) locations
