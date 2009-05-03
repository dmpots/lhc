module Grin.Pretty
    ( ppGrin
    , ppExpression
    ) where

import CompactString
import Grin.Types

import Text.PrettyPrint.ANSI.Leijen

import qualified Data.Map as Map

type QualMap = Map.Map CompactString Bool

grinQualMap :: Grin -> QualMap
grinQualMap grin
    = Map.unionsWith (\_ _ -> True) [nodeMap, funcMap, argsMap]
    where nodeMap = Map.fromListWith (\_ _ -> True) [ (name, False) | NodeDef{nodeName = Aliased _ name} <- grinNodes grin ]
          funcMap = Map.fromListWith (\_ _ -> True) [ (name, False) | FuncDef{funcDefName = Aliased _ name} <- grinFunctions grin ]
          argsMap = Map.fromListWith (\_ _ -> True) [ (name, False) | func <- grinFunctions grin, Aliased _ name <- funcDefArgs func ]

ppGrin :: Grin -> Doc
ppGrin grin
    = dullblue (text "Nodes:") <$$>
      vsep (map (ppNodeDef qualMap) (grinNodes grin)) <$$>
      dullblue (text "CAFs:") <$$>
      vsep (map (ppCAF qualMap) (grinCAFs grin)) <$$>
      dullblue (text "Functions:") <$$>
      vsep (map (ppFuncDef qualMap) (grinFunctions grin))
    where qualMap = grinQualMap grin

ppNodeDef :: QualMap -> NodeDef -> Doc
ppNodeDef qual (NodeDef name nodeType args)
    = text "node" <+> ppNodeType qual nodeType 0 name <+> hsep (map ppType args)

ppType PtrType  = blue (text "*")
ppType WordType = white (text "#")

ppNodeType qual ConstructorNode 0 name  = char 'C' <> ppRenamed qual name
ppNodeType qual ConstructorNode n name  = char 'P' <> int n <> ppRenamed qual name
ppNodeType qual FunctionNode 0 name = char 'F' <> ppRenamed qual name
ppNodeType qual FunctionNode n name = char 'P' <> int n <> ppRenamed qual name

ppRenamed qual (Aliased n var) = pretty var <> if True || Map.findWithDefault False var qual then char '_' <> pretty n else empty
ppRenamed qual (Anonymous n)   = char 'x' <> pretty n
ppRenamed qual (Builtin p)     = char '@' <> pretty p
ppRenamed qual (External e)    = parens (text "foreign" <+> text e)

ppCAF :: QualMap -> CAF -> Doc
ppCAF qual (CAF name value)
    = ppRenamed qual name <+> equals <+> ppValue qual value

ppFuncDef :: QualMap -> FuncDef -> Doc
ppFuncDef qual (FuncDef name args body)
    = hsep (ppRenamed qual name : map (ppRenamed qual) args) <+> equals <$$>
      indent 2 (ppBeginExpression qual body)

ppBeginExpression :: QualMap -> Expression -> Doc
ppBeginExpression qual e@(_ :>>= _)
    = hang 3 (text "do" <+> ppExpression qual e)
ppBeginExpression qual e = ppExpression qual e

ppExpression :: QualMap -> Expression -> Doc
ppExpression qual (Unit value) = text "unit" <+> ppValue qual value
ppExpression qual (Case value alts)
    = text "case" <+> ppValue qual value <+> text "of" <$$>
      indent 2 (vsep (map (ppAlt qual) alts))
ppExpression qual (Application fn args)
    = hsep (ppRenamed qual fn:map (ppRenamed qual) args)
ppExpression qual (Store v)
    = text "store" <+> ppValue qual v
ppExpression qual (a :>>= Empty :-> c)
    = ppExpression qual a <$$>
      ppExpression qual c
ppExpression qual (a :>>= b :-> c)
    = ppValue qual b <+> text "<-" <+> hang 0 (ppBeginExpression qual a) <$$>
      ppExpression qual c

ppAlt qual (value :-> exp) = ppValue qual value <$$>
                             indent 2 (text "->" <+> align (ppBeginExpression qual exp))

ppValue qual (Node name nodeType missing args)
    = parens (hsep (ppNodeType qual nodeType missing name : map (ppRenamed qual) args))
ppValue qual (Vector vs) = brackets (hsep (map (ppRenamed qual) vs))
ppValue qual (Hole size) = parens (text "@hole" <+> hsep (replicate size (char '_')))
ppValue qual Empty = text "()"
ppValue qual (Lit lit) = ppLit lit
ppValue qual (Variable variable) = ppRenamed qual variable

ppLit (Lint i) = integer i
ppLit (Lrational r) = text (show r)
ppLit (Lchar char) = text (show char)
ppLit (Lstring string) = text (show string)
