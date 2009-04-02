module Grin.Pretty where

import Grin.Types
import CompactString

import Text.PrettyPrint.ANSI.Leijen


ppGrin :: Grin -> Doc
ppGrin grin
    = dullblue (text "Nodes:") <$$>
      vsep (map ppNodeDef (grinNodes grin)) <$$>
      dullblue (text "Functions:") <$$>
      vsep (map ppFuncDef (grinFunctions grin))

ppNodeDef :: NodeDef -> Doc
ppNodeDef (NodeDef name nodeType args)
    = text "node" <+> ppNodeType nodeType name <+> hsep (map ppType args)

ppType PtrType  = blue (text "*")
ppType WordType = white (text "#")

ppNodeType (ConstructorNode 0) name  = char 'C' <> pretty name
ppNodeType (ConstructorNode n) name  = char 'P' <> pretty name <> char '_' <> int n
ppNodeType (FunctionNode 0) name = char 'F' <> pretty name
ppNodeType (FunctionNode n) name = char 'P' <> pretty name <> char '_' <> int n

instance Pretty Renamed where
    pretty (Aliased n var) = pretty var <> char '_' <> pretty n
    pretty (Anonymous n)   = char 'x' <> pretty n
    pretty (Builtin p)   = char '@' <> pretty p


ppFuncDef :: FuncDef -> Doc
ppFuncDef (FuncDef name args body)
    = hsep (pretty name : map pretty args) <+> equals <$$>
      indent 2 (ppBeginExpression body)

ppBeginExpression :: Expression -> Doc
ppBeginExpression e@(_ :>>= _)
    = hang 3 (text "do" <+> ppExpression e)
ppBeginExpression e = ppExpression e

ppExpression :: Expression -> Doc
ppExpression (Unit value) = text "unit" <+> ppValue value
ppExpression (Case value alts)
    = text "case" <+> ppValue value <+> text "of" <$$>
      indent 2 (vsep (map ppAlt alts))
ppExpression (Fetch v)
    = text "fetch" <+> pretty v
ppExpression (Application fn args)
    = hsep (pretty fn:map ppValue args)
ppExpression (Store v)
    = text "store" <+> parens (ppValue v)
ppExpression (a :>>= Empty :-> c)
    = ppExpression a <$$>
      ppExpression c
ppExpression (a :>>= b :-> c)
    = ppValue b <+> text "<-" <+> hang 0 (ppBeginExpression a) <$$>
      ppExpression c

ppAlt (value :-> exp) = ppValue value <$$>
                        indent 2 (text "->" <+> align (ppBeginExpression exp))

ppValue (Node name nodeType args)
    = hsep (ppNodeType nodeType name : map ppValue args)
ppValue (Hole size) = text "@hole" <+> hsep (replicate size (char '_'))
ppValue Empty = text "()"
ppValue (Integer i) = integer i
ppValue (Rational r) = text (show r)
ppValue (Char char) = text (show char)
ppValue (String string) = text (show string)
ppValue (Variable variable) = pretty variable
