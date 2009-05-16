module Grin.HtmlAnnotate where

import Text.PrettyPrint
import qualified Text.XHtml
import Text.XHtml hiding (text,blue,white,align)
import qualified Data.Map as Map

import CompactString
import Grin.Types

annotate :: Map.Map Renamed Html -> Grin -> String
annotate annotations grin
    = h "html" $ h "body" $ h "pre" $ show (ppGrin grin)
    where h t s = "<"++t++">"++s++"</"++t++">"

(<$$>) = ($$)
vsep = vcat
pretty v = text (read (show v))

type QualMap = Map.Map CompactString Bool

grinQualMap :: Grin -> QualMap
grinQualMap grin
    = Map.unionsWith (\_ _ -> True) [nodeMap, funcMap, argsMap]
    where nodeMap = Map.fromListWith (\_ _ -> True) [ (name, False) | NodeDef{nodeName = Aliased _ name} <- grinNodes grin ]
          funcMap = Map.fromListWith (\_ _ -> True) [ (name, False) | FuncDef{funcDefName = Aliased _ name} <- grinFunctions grin ]
          argsMap = Map.fromListWith (\_ _ -> True) [ (name, False) | func <- grinFunctions grin, Aliased _ name <- funcDefArgs func ]

ppGrin :: Grin -> Doc
ppGrin grin
    = text "Nodes:" <$$>
      vsep (map (ppNodeDef qualMap) (grinNodes grin)) <$$>
      (text "CAFs:") <$$>
      vsep (map (ppCAF qualMap) (grinCAFs grin)) <$$>
      (text "Functions:") <$$>
      vsep (map (ppFuncDef qualMap) (grinFunctions grin))
    where qualMap = grinQualMap grin

ppNodeDef :: QualMap -> NodeDef -> Doc
ppNodeDef qual (NodeDef name nodeType args)
    = text "node" <+> ppNodeType setAnchor qual nodeType 0 name <+> hsep (map ppType args)

ppType PtrType  = (text "*")
ppType WordType = (text "#")

ppNodeType def qual ConstructorNode 0 name  = char 'C' <> ppRenamed def qual name
ppNodeType def qual ConstructorNode n name  = char 'P' <> int n <> ppRenamed def qual name
ppNodeType def qual FunctionNode 0 name = char 'F' <> ppRenamed def qual name
ppNodeType def qual FunctionNode n name = char 'P' <> int n <> ppRenamed def qual name

ppRenamed def qual (Aliased n var) -- = pretty var <> if Map.findWithDefault False var qual then char '_' <> int n else empty
    = def n (read (show var))
ppRenamed def qual (Anonymous n)
    = def n ('x':show n)
ppRenamed def qual (Builtin p)     = char '@' <> pretty p
ppRenamed def qual (External e)    = parens (text "foreign" <+> text e)

ppCAF :: QualMap -> CAF -> Doc
ppCAF qual (CAF name value)
    = ppRenamed setAnchor qual name <+> equals <+> ppValue linkToAnchor qual value

ppFuncDef :: QualMap -> FuncDef -> Doc
ppFuncDef qual (FuncDef name args body)
    = hang (hsep (ppRenamed setAnchor qual name : map (ppRenamed setAnchor qual) args) <+> equals) 2
           ((ppBeginExpression qual body))

ppBeginExpression :: QualMap -> Expression -> Doc
ppBeginExpression qual e@(_ :>>= _)
    = (text "do" <+> ppExpression qual e)
ppBeginExpression qual e = ppExpression qual e

ppExpression :: QualMap -> Expression -> Doc
ppExpression qual (Unit value) = text "unit" <+> ppValue linkToAnchor qual value
ppExpression qual (Case value alts)
    = hang (text "case" <+> ppValue linkToAnchor qual value <+> text "of") 2
           (vsep (map (ppAlt qual) alts))
ppExpression qual (Application fn args)
    = hsep (ppRenamed linkToAnchor qual fn:map (ppRenamed linkToAnchor qual) args)
ppExpression qual (Store v)
    = text "store" <+> ppValue linkToAnchor qual v
ppExpression qual (a :>> c)
    = ppExpression qual a <$$>
      ppExpression qual c
ppExpression qual (a :>>= b :-> c)
    = (ppValue setAnchor qual (Variable b) <+> text "<-" <+> (ppBeginExpression qual a)) <$$>
      ppExpression qual c

ppAlt qual (value :> exp) = hang (ppValue setAnchor qual value) 2
                                 (text "->" <+> (ppBeginExpression qual exp))

ppValue def qual (Node name nodeType missing args)
    = parens (hsep (ppNodeType linkToAnchor qual nodeType missing name : map (ppRenamed def qual) args))
ppValue def qual (Vector vs) = brackets (hsep (map (ppRenamed def qual) vs))
ppValue def qual (Hole size) = parens (text "@hole" <+> hsep (replicate size (char '_')))
ppValue def qual Empty = text "()"
ppValue def qual (Lit lit) = ppLit lit
ppValue def qual (Variable variable) = ppRenamed def qual variable

ppLit (Lint i) = integer i
ppLit (Lrational r) = text (show r)
ppLit (Lchar char) = text (show char)
ppLit (Lstring string) = text (show string)



linkToAnchor ident var
    = zeroWidthText ("<a href=\"#"++ show ident ++"\">") <> text var <> zeroWidthText "</a>"

setAnchor ident var
    = zeroWidthText ("<a name=\""++ show ident ++"\">") <> text var <> zeroWidthText "</a>"

