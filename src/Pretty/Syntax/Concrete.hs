{-# LANGUAGE OverloadedStrings                  #-}

module Pretty.Syntax.Concrete where

import Syntax.Concrete

import Data.Monoid ((<>))
import Data.Text.Prettyprint.Doc hiding (line)

--------------------------------------------------------------------------------
-- |



instance Pretty Type where
  pretty (TypeEnd _) = "∅"
  pretty (TypeBase t _) = pretty t
  pretty (TypeTuple elems _) = encloseSep "(" ")" ", " $ map pretty elems
  pretty (TypeSend t u _) = "!" <> pretty t <> " . " <> pretty u
  pretty (TypeRecv t u _) = "?" <> pretty t <> " . " <> pretty u
  pretty (TypeSele selections _) =
    "!" <+> align (encloseSep lbracket rbracket semi selections')
    where selections' = map pretty selections
  pretty (TypeChoi choices _) =
    "?" <+> align (encloseSep lbracket rbracket semi choices')
    where choices' = map pretty choices
  pretty (TypeUn t _) = "un(" <> pretty t <> ")"
  pretty (TypeVar t _) = pretty t
  pretty (TypeMu t _) = "μ(" <> pretty t <> ")"

instance Pretty TypeOfLabel where
  pretty (TypeOfLabel label t _) = pretty label <> " : " <> pretty t

instance Pretty Label where
  pretty (Label label _) = pretty label

instance Pretty TypeVar where
  pretty (TypeVarIndex i _) = "$" <> pretty i
  pretty (TypeVarText n _) = pretty n

instance Pretty BaseType where
  pretty (BaseBool _) = "Bool"
  pretty (BaseInt _) = "Int"

instance Pretty TypeName where
  pretty (TypeName x _) = pretty x
