module Grin.Pretty
    ( ppGrin
    , ppExpression
    ) where

import Grin.Types

import Text.PrettyPrint.ANSI.Leijen


ppGrin :: Grin -> Doc
ppGrin grin
    = dullblue (text "Nodes:") <$$>
      vsep (map ppNodeDef (grinNodes grin)) <$$>
      dullblue (text "CAFs:") <$$>
      vsep (map ppCAF (grinCAFs grin)) <$$>
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
    pretty (Aliased n var) = pretty var -- <> char '_' <> pretty n
    pretty (Anonymous n)   = char 'x' <> pretty n
    pretty (Builtin p)     = char '@' <> pretty p
    pretty (External e)    = parens (text "foreign" <+> text e)

ppCAF :: CAF -> Doc
ppCAF (CAF name value)
    = pretty name <+> equals <+> ppValue value

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
    = text "store" <+> (ppValue v)
ppExpression (a :>>= Empty :-> c)
    = ppExpression a <$$>
      ppExpression c
ppExpression (a :>>= b :-> c)
    = ppValue b <+> text "<-" <+> hang 0 (ppBeginExpression a) <$$>
      ppExpression c

ppAlt (value :-> exp) = ppValue value <$$>
                        indent 2 (text "->" <+> align (ppBeginExpression exp))

ppValue (Node name nodeType args)
    = parens (hsep (ppNodeType nodeType name : map ppValue args))
ppValue (Hole size) = parens (text "@hole" <+> hsep (replicate size (char '_')))
ppValue Empty = text "()"
ppValue (Lit lit) = ppLit lit
ppValue (Variable variable) = pretty variable

ppLit (Lint i) = integer i
ppLit (Lrational r) = text (show r)
ppLit (Lchar char) = text (show char)
ppLit (Lstring string) = text (show string)
