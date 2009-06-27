-- TODO: Use unicode for the symbols.
module Grin.Stage2.Pretty
    ( ppGrin
    , ppExpression
    , ppRenamed
    ) where

import CompactString
import Grin.Stage2.Types

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
ppType NodeType = white (text "!")

ppNodeType qual nt n name
    = green (worker qual nt n name)
    where worker qual ConstructorNode 0 name  = char 'C' <> ppRenamed qual name
          worker qual ConstructorNode n name  = char 'P' <> int n <> ppRenamed qual name
          worker qual FunctionNode 0 name = char 'F' <> ppRenamed qual name
          worker qual FunctionNode n name = char 'P' <> int n <> ppRenamed qual name

ppRenamed qual (Aliased n var) = pretty var <> if True || Map.findWithDefault False var qual then char '_' <> pretty n else empty
ppRenamed qual (Anonymous n)   = char 'x' <> pretty n
ppRenamed qual (Builtin p)     = char '@' <> pretty p
ppRenamed qual (External e)    = parens (text "foreign" <+> text e)

ppCAF :: QualMap -> CAF -> Doc
ppCAF qual (CAF name value)
    = ppRenamed qual name <+> equals <+> ppValue qual value

ppFuncDef :: QualMap -> FuncDef -> Doc
ppFuncDef qual (FuncDef name returns args body)
    = hsep (brackets (int returns) <+> ppRenamed qual name : map (ppRenamed qual) args) <+> equals <$$>
      indent 2 (ppBeginExpression qual body)

ppBeginExpression :: QualMap -> Expression -> Doc
ppBeginExpression qual e@(_ :>>= _)
    = hang 3 (text "do" <+> ppExpression qual e)
ppBeginExpression qual e = ppExpression qual e

ppExpression :: QualMap -> Expression -> Doc
ppExpression qual (Unit values) = blue (text "unit") <+> ppValues qual values
ppExpression qual (Constant value) = blue (text "constant") <+> ppValue qual value
ppExpression qual (Case value alts)
    = blue (text "case") <+> ppRenamed qual value <+> blue (text "of") <$$>
      indent 2 (vsep (map (ppAlt qual) alts))
ppExpression qual (Application fn args)
    = hsep (ppRenamed qual fn:map (ppRenamed qual) args)
ppExpression qual (Store v)
    = blue (text "store") <+> ppValues qual v
ppExpression qual (Fetch n p)
    = blue (text "fetch") <> brackets (int n) <+> ppRenamed qual p
ppExpression qual (a :>>= [] :-> c)
    = ppExpression qual a <$$>
      ppExpression qual c
ppExpression qual (a :>>= b :-> c)
    = ppValues qual b <+> text "<-" <+> hang 0 (ppBeginExpression qual a) <$$>
      ppExpression qual c

ppAlt qual (value :> exp) = ppValue qual value <$$>
                            indent 2 (text "->" <+> align (ppBeginExpression qual exp))

ppValues qual vals
    = brackets (hsep $ map (ppRenamed qual) vals)

ppValue qual (Node name nodeType missing)
    = (ppNodeType qual nodeType missing name)
ppValue qual Hole = text "_"
ppValue qual Empty = text "()"
ppValue qual (Lit lit) = ppLit lit

ppLit (Lint i) = integer i
ppLit (Lrational r) = text (show r)
ppLit (Lchar char) = text (show char)
ppLit (Lstring string) = text (show string)
