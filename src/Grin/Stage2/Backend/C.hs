{-# LANGUAGE StandaloneDeriving, OverloadedStrings #-}
module Grin.Stage2.Backend.C
    ( compile
    , grinToC
    ) where

import CompactString
import Grin.Stage2.Types
import qualified Grin.Stage2.Pretty as Grin (ppExpression)

import Text.PrettyPrint.ANSI.Leijen

import System.Process
import System.FilePath
import Data.Char
import qualified Data.Map as Map

fixedSize :: Int
fixedSize = 20

compile :: Grin -> FilePath -> IO ()
compile grin target
    = do writeFile (replaceExtension target "c") (show cCode)
         pid <- runCommand cmdLine
         waitForProcess pid
         return ()
    where cCode = grinToC grin
          cFile = replaceExtension target "c"
          cmdLine = unwords ["gcc", "-I/usr/include/gc/", "-lgc", "-w", "--debug", "-ggdb", cFile, "-o", target]

grinToC :: Grin -> Doc
grinToC grin
    = vsep [ comment "Header:"
           , header
           , comment "CAFs:"
           , vsep (map ppCAF (grinCAFs grin))
           , comment "Return arguments:"
           , vsep (map ppCAF returnArguments)
           , comment "Function prototypes:"
           , vsep (map ppFuncDefProtoType (grinFunctions grin))
           , comment "Functions:"
           , vsep (map ppFuncDef (grinFunctions grin))
           , comment "Main:"
           , ppMain (grinEntryPoint grin)
           , linebreak
           ]

returnArguments :: [CAF]
returnArguments = [ CAF{ cafName = Aliased n "lhc_return", cafValue = Lit (Lint 0)} | n <- [1..10] ]

header :: Doc
header = vsep [ include "stdlib.h"
              , include "stdio.h"
              , include "unistd.h"
              , include "string.h"
              , include "errno.h"
              , include "gc.h"
              , typedef <+> unsigned <+> long <+> u64 <> semi
              , typedef <+> unsigned <+> text "int" <+> u32 <> semi
              , typedef <+> unsigned <+> text "short" <+> u16 <> semi
              , typedef <+> unsigned <+> text "char" <+> u8 <> semi
              , typedef <+> signed <+> long <+> s64 <> semi
              , typedef <+> signed <+> text "int" <+> s32 <> semi
              , typedef <+> signed <+> text "short" <+> s16 <> semi
              , typedef <+> signed <+> text "char" <+> s8 <> semi
              , text "int global_argc;"
              , text "char **global_argv;" ]

ppMain :: Renamed -> Doc
ppMain entryPoint
    = text "int" <+> text "main" <> parens (text "int argc" <> comma <+> text "char *argv[]") <+> char '{' <$$>
      indent 2 ( text "global_argc = argc;" <$$>
                 text "global_argv = argv;" <$$>
                 ppRenamed entryPoint <> parens empty <> semi <$$> text "return 0" <> semi) <$$>
      char '}'

ppCAF :: CAF -> Doc
ppCAF CAF{cafName = name, cafValue = Node tag _nt _missing}
    = u64 <+> ppRenamed name <> brackets empty <+> equals <+> initList (uniqueId tag:replicate (fixedSize-1) 0) <> semi
ppCAF CAF{cafName = name, cafValue = Lit (Lstring str)}
    = u64 <> char '*' <+> ppRenamed name <+> equals <+> parens (u64<>char '*') <+> escString (str++"\0") <> semi
ppCAF CAF{cafName = name, cafValue = Lit (Lint i)}
    = u64 <> char '*' <+> ppRenamed name <+> equals <+> parens (u64<>char '*') <+> int (fromIntegral i) <> semi
ppCAF caf = error $ "Grin.Stage2.Backend.ppCAF: Invalid CAF: " ++ show (cafName caf)

ppFuncDefProtoType :: FuncDef -> Doc
ppFuncDefProtoType func
    = void <+> ppRenamed (funcDefName func) <> argList <> semi
    where argList = parens (hsep $ punctuate comma $ [ u64 <> char '*' <+> ppRenamed arg | arg <- funcDefArgs func ])

ppFuncDef :: FuncDef -> Doc
ppFuncDef func
    = void <+> ppRenamed (funcDefName func) <> argList <+> char '{' <$$>
      indent 2 (entry <$$> body <$$> text "return" <> semi) <$$>
      char '}'
    where argList = parens (hsep $ punctuate comma $ [ u64 <> char '*' <+> ppRenamed arg | arg <- funcDefArgs func ])
          entry   = empty -- puts $ "Calling: " ++ show (ppRenamed (funcDefName func))
          body    = ppExpression (map cafName (take (funcDefReturns func) returnArguments)) (funcDefBody func)

ppExpression :: [Renamed] -> Expression -> Doc
-- More than one bind can occur in case expressions. Just take the first.
ppExpression (bind:_) (Constant (Lit (Lrational r)))
    = parens (parens (text "double*") <> char '&' <> ppRenamed bind) <> brackets (int 0) <+> equals <+> double (fromRational r) <> semi
ppExpression (bind:_) (Constant value)
    = bind =: valueToDoc value
ppExpression [bind] (Application (Builtin "realWorld#") [])
    = bind =: int 0
ppExpression binds (Application fn args) | not (isBuiltin fn) && not (isExternal fn)
    = ppRenamed fn <> argList <> semi <$$>
      vsep (zipWith (=:) binds (map (ppRenamed.cafName) returnArguments))
    where argList = parens $ hsep $ punctuate comma $ map ppRenamed args
ppExpression (bind:_) (Fetch nth variable)
    = bind =: (ppRenamed variable <> brackets (int nth))
ppExpression binds (Unit variables)
    = vsep (zipWith (=:) binds (map ppRenamed variables))
ppExpression [bind] (StoreHole size)
    = vsep $ [ bind =: alloc (int $ size * 8)]
ppExpression [bind] (Store variables)
    = vsep $ [ bind =: alloc (int $ (max 2 (length variables+1)) * 8)] ++
             [ writeArray bind n var | (n,var) <- zip [0..] variables ]
ppExpression binds (Case scrut alts)
    = ppCase binds scrut alts
ppExpression binds (a :>>= binds' :-> b)
    = declareVars binds' <$$>
      ppExpression binds' a <$$>
      ppExpression binds b

ppExpression (bind:_) (Application (Builtin "noDuplicate#") [arg])
    = bind =: ppRenamed arg
ppExpression (bind:_) (Application (Builtin "==#") [a,b])
    = ifStatement (parens s64 <> ppRenamed a <+> text "==" <+> parens s64 <> ppRenamed b)
                  (bind =: int 1)
                  (bind =: int 0)
ppExpression (bind:_) (Application (Builtin ">#") [a,b])
    = ifStatement (parens s64 <> ppRenamed a <+> text ">" <+> parens s64 <> ppRenamed b)
                  (bind =: int 1)
                  (bind =: int 0)
ppExpression (bind:_) (Application (Builtin ">=#") [a,b])
    = ifStatement (parens s64 <> ppRenamed a <+> text ">=" <+> parens s64 <> ppRenamed b)
                  (bind =: int 1)
                  (bind =: int 0)
ppExpression (bind:_) (Application (Builtin "<=#") [a,b])
    = ifStatement (parens s64 <> ppRenamed a <+> text "<=" <+> parens s64 <> ppRenamed b)
                  (bind =: int 1)
                  (bind =: int 0)
ppExpression (bind:_) (Application (Builtin "<#") [a,b])
    = ifStatement (parens s64 <> ppRenamed a <+> text "<" <+> parens s64 <> ppRenamed b)
                  (bind =: int 1)
                  (bind =: int 0)
ppExpression (bind:_) (Application (Builtin "<##") [a,b])
    = ifStatement (castToDouble a <+> text "<" <+> castToDouble b)
                  (bind =: int 1)
                  (bind =: int 0)
ppExpression (bind:_) (Application (Builtin "==##") [a,b])
    = ifStatement (castToDouble a <+> text "==" <+> castToDouble b)
                  (bind =: int 1)
                  (bind =: int 0)
ppExpression (bind:_) (Application (Builtin "ord#") [a])
    = bind =: ppRenamed a
ppExpression (bind:_) (Application (Builtin "chr#") [a])
    = bind =: ppRenamed a
ppExpression (bind:_) (Application (Builtin "negateInt#") [a])
    = bind =: (text "-" <+> parens s64 <> ppRenamed a)
ppExpression (bind:_) (Application (Builtin "narrow8Word#") [a])
    = bind =: (parens u8 <+> ppRenamed a)
ppExpression (bind:_) (Application (Builtin "narrow16Word#") [a])
    = bind =: (parens u16 <+> ppRenamed a)
ppExpression (bind:_) (Application (Builtin "narrow32Word#") [a])
    = bind =: (parens u32 <+> ppRenamed a)
ppExpression (bind:_) (Application (Builtin "narrow8Int#") [a])
    = bind =: (parens s8 <+> ppRenamed a)
ppExpression (bind:_) (Application (Builtin "narrow16Int#") [a])
    = bind =: (parens s16 <+> ppRenamed a)
ppExpression (bind:_) (Application (Builtin "narrow32Int#") [val])
    = bind =: (parens u32 <+> ppRenamed val)
ppExpression (bind:_) (Application (Builtin "timesWord#") [a,b])
    = bind =: parens (parens u64 <+> ppRenamed a <+> text "*" <+> parens u64 <+> ppRenamed b)
ppExpression (bind:_) (Application (Builtin "plusWord#") [a,b])
    = bind =: parens (parens u64 <+> ppRenamed a <+> text "+" <+> parens u64 <+> ppRenamed b)
ppExpression (bind:_) (Application (Builtin "minusWord#") [a,b])
    = bind =: parens (parens u64 <+> ppRenamed a <+> text "-" <+> parens u64 <+> ppRenamed b)
ppExpression (bind:_) (Application (Builtin "*#") [a,b])
    = bind =: parens (parens s64 <+> ppRenamed a <+> text "*" <+> parens s64 <+> ppRenamed b)
ppExpression (bind:_) (Application (Builtin "+#") [a,b])
    = bind =: parens (parens s64 <+> ppRenamed a <+> text "+" <+> parens s64 <+> ppRenamed b)
ppExpression (bind:_) (Application (Builtin "-#") [a,b])
    = bind =: parens (parens s64 <+> ppRenamed a <+> text "-" <+> parens s64 <+> ppRenamed b)
ppExpression (bind:_) (Application (Builtin "quotInt#") [a,b])
    = bind =: parens (parens s64 <+> ppRenamed a <+> text "/" <+> parens s64 <+> ppRenamed b)
ppExpression (bind:_) (Application (Builtin "remInt#") [a,b])
    = bind =: parens (parens s64 <+> ppRenamed a <+> text "%" <+> parens s64 <+> ppRenamed b)
ppExpression (bind:_) (Application (Builtin "indexCharOffAddr#") [addr,idx])
    = bind =: (parens (parens (u8<>char '*') <+> ppRenamed addr) <> brackets (parens u64 <+> ppRenamed idx))
ppExpression (st:_) (Application (Builtin "writeCharArray#") [arr,idx,chr,realWorld])
    = vsep [ parens (parens (u8<>char '*') <+> ppRenamed arr) <> brackets (parens u64 <+> ppRenamed idx) <+>
             equals <+> parens u8 <+> ppRenamed chr <> semi
           , st =: ppRenamed realWorld ]
ppExpression (st:bind:_) (Application (Builtin "readAddrOffAddr#") [addr, idx, realworld])
    = vsep [ bind =: (ppRenamed addr <> brackets (parens u64 <+> ppRenamed idx))
           , st   =: ppRenamed realworld ]
ppExpression (st:bind:_) (Application (Builtin "readInt32OffAddr#") [addr,idx, realworld])
    = vsep [ bind =: (parens (parens (s32<>char '*') <+> ppRenamed addr) <> brackets (parens u64 <+> ppRenamed idx))
           , st   =: ppRenamed realworld
           ]
ppExpression (st:bind:_) (Application (Builtin "readInt8OffAddr#") [addr,idx, realworld])
    = vsep [ bind =: (parens (parens (s8<>char '*') <+> ppRenamed addr) <> brackets (parens u64 <+> ppRenamed idx))
           , st   =: ppRenamed realworld
           ]
ppExpression (st:bind:_) (Application (Builtin "unsafeFreezeByteArray#") [addr, realworld])
    = vsep [ bind =: ppRenamed addr
           , st   =: ppRenamed realworld ]
ppExpression (bind:_) (Application (Builtin "byteArrayContents#") [addr])
    = bind =: ppRenamed addr
ppExpression (st:_) (Application (Builtin "touch#") [ptr,realworld])
    = st =: ppRenamed realworld

ppExpression (st:bind:_) (Application (Builtin "mkWeak#") [key, val, finalizer, realWorld])
    = vsep [ bind =: int 0
           , st   =: ppRenamed realWorld ]
ppExpression (st:arr:_) (Application (Builtin "newPinnedByteArray#") [size,realWorld])
    = vsep [ st =: ppRenamed realWorld
           , arr =: alloc ((parens u64) <+> ppRenamed size) ]
-- FIXME: The ByteArray isn't aligned.
ppExpression (st:arr:_) (Application (Builtin "newAlignedPinnedByteArray#") [size,alignment,realWorld])
    = vsep [ st =: ppRenamed realWorld
           , arr =: alloc ((parens u64) <+> ppRenamed size) ]
ppExpression _ (Application (Builtin "update") (ptr:values))
    = vsep [ writeArray ptr n value | (n,value) <- zip [0..] values ]

ppExpression (st:bind:_) (Application (External "fdReady") args)
    = vsep [ bind =: int 1
           , st   =: ppRenamed (last args) ]
ppExpression (st:bind:_) (Application (External "isDoubleNegativeZero") [double,realworld])
    = vsep [ bind =: int 0
           , st   =: ppRenamed realworld ]
ppExpression (st:bind:_) (Application (External "isDoubleNaN") [double,realworld])
    = vsep [ bind =: (text "isnan" <> parens (parens (parens (text "double*") <> char '&' <> ppRenamed double) <> brackets (int 0) ))
           , st   =: ppRenamed realworld ]
ppExpression (st:bind:_) (Application (External "isDoubleInfinite") [double,realworld])
    = vsep [ bind =: (text "isinf" <> parens (parens (parens (text "double*") <> char '&' <> ppRenamed double) <> brackets (int 0) ))
           , st   =: ppRenamed realworld ]
ppExpression (st:_) (Application (External "getProgArgv") [argcPtr, argvPtr, realWorld])
    = vsep [ ppRenamed argcPtr <+> equals <+> char '&' <> text "global_argc" <> semi
           , ppRenamed argvPtr <+> equals <+> char '&' <> text "global_argv" <> semi
           , st   =: ppRenamed realWorld ]
ppExpression (st:bind:_) (Application (External "__hscore_get_errno") args)
    = vsep [ bind =: text "errno"
           , st   =: ppRenamed (last args) ]
ppExpression (st:bind:_) (Application (External "__hscore_PrelHandle_write") [fd,ptr,offset,size,realWorld])
    = vsep [ bind =: (text "write" <> parens (hsep $ punctuate comma $ [ parens u64 <+> ppRenamed fd
                                                                       , ppRenamed ptr <+> text "+" <+> parens u64 <+> ppRenamed offset
                                                                       , ppRenamed size]))
           , st   =: ppRenamed realWorld ]
ppExpression (st:bind:_) (Application (External "__hscore_memcpy_dst_off") [dst, off, src, size, realWorld])
    = vsep [ bind =: (text "memcpy" <> parens (hsep $ punctuate comma $ [ parens u64 <+> ppRenamed dst <+> text "+" <+> parens u64 <+> ppRenamed off
                                                                        , ppRenamed src
                                                                        , ppRenamed size ]))
           , st =: ppRenamed realWorld ]
ppExpression (st:bind:_) (Application (External fn) args)
    = vsep [bind =: (text fn <> argList)
           ,st   =: ppRenamed (last args) ]
    where argList = parens $ hsep $ punctuate comma $ map ppRenamed (init args)

ppExpression binds e = puts (show (Grin.ppExpression Map.empty e))

castToDouble ptr
    = parens (parens (text "double*") <> char '&' <> ppRenamed ptr) <> brackets (int 0)

ppCase binds scrut alts
    = switch (parens (u64) <+> ppRenamed scrut) $
        vsep (map ppAlt alts) <$$> def (last alts)
    where def (Empty :> _)  = empty
          def _             = text "default:" <$$> indent 2 (puts $ "No match for case: " ++ show scrut)
          ppAlt (value :> exp)
              = case value of
                  Empty
                    -> text "default:" <$$> rest
                  Node tag _nt _missing
                    -> text "case" <+> int (uniqueId tag) <> colon <$$> rest
                  Lit (Lint i)
                    -> text "case" <+> int (fromIntegral i) <> colon <$$> rest
                  Lit (Lchar c)
                    -> text "case" <+> int (ord c) <> colon <$$> rest
              where rest = indent 2 (cafName (head returnArguments) =: ppRenamed (cafName (head returnArguments)) <$$>
                                     ppExpression binds exp <$$>
                                     text "break;")

valueToDoc :: Value -> Doc
valueToDoc (Node tag nt missing)
    = int (uniqueId tag)
valueToDoc (Lit (Lint i))
    = int (fromIntegral i)
valueToDoc (Lit (Lchar c)) = int (ord c)
valueToDoc val = error $ "Grin.Stage2.Backend.C.valueToDoc: Can't translate: " ++ show val








puts :: String -> Doc
puts txt = text "puts" <> parens (escString txt) <> semi

alloc :: Doc -> Doc
alloc size = text "GC_MALLOC" <> parens size

writeArray :: Renamed -> Int -> Renamed -> Doc
writeArray arr nth val
    = ppRenamed arr <> brackets (int nth) <+> equals <+> parens u64 <+> ppRenamed val <> semi

(=:) :: Renamed -> Doc -> Doc
variable =: value = ppRenamed variable <+> equals <+> parens (u64 <> char '*') <+> value <> semi

declareVar :: Renamed -> Doc
declareVar var
    = u64 <> char '*' <+> ppRenamed var <> semi

declareVars :: [Renamed] -> Doc
declareVars = vsep . map declareVar

escString :: String -> Doc
escString string = char '"' <> text (concatMap worker string) <> char '"'
    where worker c | isPrint c = [c]
                   | otherwise = '\\' : pad 3 (show (ord c))
          pad n str = take (n-length str) (repeat '0') ++ str

initList :: [Int] -> Doc
initList vals = braces $ hsep $ punctuate comma $ map int vals

switch ::Doc -> Doc -> Doc
switch scrut body
    = text "switch" <> parens scrut <+> char '{' <$$>
      indent 2 body <$$>
      char '}'

ppRenamed :: Renamed -> Doc
ppRenamed (Anonymous i)
    = text "anon_" <> int i
ppRenamed (Aliased i name)
    = text "named_" <> sanitize name <> char '_' <> int i

sanitize :: CompactString -> Doc
sanitize cs = text (map sanitizeChar $ show $ pretty cs)

sanitizeChar :: Char -> Char
sanitizeChar c | isAlphaNum c = c
               | otherwise    = '_'

ifStatement :: Doc -> Doc -> Doc -> Doc
ifStatement cond true false
    = text "if" <> parens cond <$$>
      indent 2 true <$$>
      text "else" <$$>
      indent 2 false

include :: FilePath -> Doc
include headerFile
    = text "#include" <+> char '<' <> text headerFile <> char '>'

comment :: String -> Doc
comment str = text "/*" <+> text str <+> text "*/"


typedef, unsigned, signed, long, void, u64, u32, u16, u8, s64, s32, s16,s8 :: Doc
typedef  = text "typedef"
unsigned = text "unsigned"
signed   = text "signed"
long     = text "long"
void     = text "void"
u64      = text "u64"
u32      = text "u32"
u16      = text "u16"
u8       = text "u8"
s64      = text "s64"
s32      = text "s32"
s16      = text "s16"
s8       = text "s8"


