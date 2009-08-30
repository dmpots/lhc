{-# LANGUAGE StandaloneDeriving, OverloadedStrings #-}
module Grin.Stage2.Backend.C
    ( compile
    , compileFastCode
    , grinToC
    ) where

import CompactString
import Grin.Stage2.Types
import qualified Grin.Stage2.Pretty as Grin (ppExpression)

import Text.PrettyPrint.ANSI.Leijen

import System.Process
import System.FilePath
import Data.Char
import Text.Printf
import System.IO
import System.Exit

fixedSize :: Int
fixedSize = 20

compile :: Grin -> FilePath -> IO ()
compile = compile' ["--debug", "-ggdb"]

compileFastCode :: Grin -> FilePath -> IO ()
compileFastCode = compile' ["-O2"]

compile' :: [String] -> Grin -> FilePath -> IO ()
compile' gccArgs grin target
    = do writeFile (replaceExtension target "c") (show cCode)
         pid <- runCommand cmdLine
         ret <- waitForProcess pid
         case ret of
           ExitSuccess -> return ()
           _ -> do hPutStrLn stderr "C code failed to compile."
                   exitWith ret
    where cCode = grinToC grin
          cFile = replaceExtension target "c"
          cmdLine = unwords (["gcc", "-lm", "-I/usr/include/gc/", "-lgc", cFile, "-o", target] ++ gccArgs)

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
           , comment "Alloc:"
           , ppBumpAlloc
           , comment "RTS:"
           , ppRTS
           , comment "Functions:"
           , vsep (map ppFuncDef (grinFunctions grin))
           , comment "Main:"
           , ppMain (grinCAFs grin) (grinEntryPoint grin)
           , linebreak
           ]

returnArguments :: [CAF]
returnArguments = [ CAF{ cafName = Aliased n "lhc_return", cafValue = Lit (Lint 0)} | n <- [1..10] ]

header :: Doc
header = vsep [ include "stdlib.h"
              , include "stdio.h"
              , include "unistd.h"
              , include "string.h"
              , include "math.h"
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

ppMain :: [CAF] -> Renamed -> Doc
ppMain cafs entryPoint
    = text "int" <+> text "main" <> parens (text "int argc" <> comma <+> text "char *argv[]") <+> char '{' <$$>
      indent 2 ( text "global_argc = argc;" <$$>
                 text "global_argv = argv;" <$$>
                 text "GC_init();" <$$>
                 --text "GC_set_max_heap_size(1024*1024*1024);" <$$>
                 vsep [ vsep [ ppRenamed name <+> equals <+> alloc (int (4 * 8)) <> semi
                             , ppRenamed name <> brackets (int 0) <+> equals <+> int (uniqueId tag) <> semi]
                        | CAF{cafName = name, cafValue = Node tag _nt _missing} <- cafs ] <$$>
                 ppRenamed entryPoint <> parens empty <> semi <$$> text "return 0" <> semi) <$$>
      char '}'

ppRTS :: Doc
ppRTS = vsep [ ppDWUnion
             , ppWordToDouble
             , ppDoubleToWord ]

ppDWUnion
    = text "typedef union { double d; u64 *w; } DoubleOrWord;" 

ppWordToDouble
    = text "double wordToDouble(u64 *x) {" <$$>
      indent 2 (text "DoubleOrWord u;" <$$>
                text "u.w = x;" <$$>
                text "return u.d;") <$$>
      char '}'
ppDoubleToWord
    = text "u64 *doubleToWord(double x) {" <$$>
      indent 2 (text "DoubleOrWord u;" <$$>
                text "u.d = x;" <$$>
                text "return u.w;") <$$>
      char '}'

ppBumpAlloc :: Doc
ppBumpAlloc
    = text "void*" <+> text "alloc" <> parens (text "int" <+> text "size") <+> char '{' <$$>
      indent 2 (vsep [ text "static void *p = NULL, *limit = NULL;"
                     , text "void* t;"
                     , text "int max;"
                     , text "if (p == NULL) { "
                     , text "  p = GC_MALLOC(" <> int blockSize <> text " + 10*8);"
                     , text "  limit = p + " <> int blockSize <> text ";"
                     , text "}"
                     , text "if (p+size > limit) {"
                     , text "  max = " <> int blockSize <> text " > size ? " <> int blockSize <> text " : size;"
                     , text "  p = GC_MALLOC(max + 10*8);"
                     , text "  limit = p + max;"
                     , text "}"
                     , text "t = p;"
                     , text "p += size;"
                     , text "return t;"
                     ]) <$$>
      char '}'
    where blockSize = 1024*4

ppCAF :: CAF -> Doc
ppCAF CAF{cafName = name, cafValue = Node tag _nt _missing}
    = u64 <> char '*' <+> ppRenamed name <> semi -- brackets empty <+> equals <+> initList (uniqueId tag:replicate (fixedSize-1) 0) <> semi
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
    = bind =: castToWord (double (fromRational r)) <> semi
ppExpression (bind:_) (Constant value)
    = bind =: valueToDoc value
ppExpression [bind] (Application (Builtin "realWorld#") [])
    = bind =: int 0
ppExpression binds (Application fn args) | not (isBuiltin fn) && not (isExternal fn) && isTailCall
    = ppRenamed fn <> argList <> semi
    where argList = parens $ hsep $ punctuate comma $ map ppRenamed args
          isTailCall = and (zipWith (==) binds (map cafName returnArguments))
ppExpression binds (Application fn args) | not (isBuiltin fn) && not (isExternal fn)
    = ppRenamed fn <> argList <> semi <$$>
      vsep (zipWith (=:) binds (map (ppRenamed.cafName) returnArguments))
    where argList = parens $ hsep $ punctuate comma $ map ppRenamed args
ppExpression (bind:_) (Fetch nth variable)
    = bind =: (ppRenamed variable <> brackets (int nth))
ppExpression binds (Unit variables)
    = vsep (zipWith (=:) binds (map ppRenamed variables))
ppExpression [bind] (StoreHole size)
    = vsep $ [ bind =: alloc (int $ max 4 size * 8)]
ppExpression (bind:_) (Store variables)
    = vsep $ [ bind =: alloc (int $ (max 4 (length variables)) * 8)] ++
             [ writeArray bind n var | (n,var) <- zip [0..] variables ]
ppExpression binds (Case scrut alts)
    = ppCase binds scrut alts
ppExpression binds (a :>>= binds' :-> b)
    = declareVars binds' <$$>
      ppExpression binds' a <$$>
      ppExpression binds b

ppExpression (bind:_) (Application (Builtin "coerceDoubleToWord") [arg])
    = bind =: ppRenamed arg
ppExpression (bind:_) (Application (Builtin "coerceWordToDouble") [arg])
    = bind =: ppRenamed arg
ppExpression (bind:_) (Application (Builtin "uncheckedShiftL#") [w,i])
    = bind =: (parens (cu64 <> ppRenamed w <+> text "<<" <+> cs64 <> ppRenamed i))
ppExpression (bind:_) (Application (Builtin "uncheckedShiftRL#") [w,i])
    = bind =: (parens (cu64 <> ppRenamed w <+> text ">>" <+> cs64 <> ppRenamed i))
ppExpression (bind:_) (Application (Builtin "noDuplicate#") [arg])
    = bind =: ppRenamed arg
ppExpression (bind:_) (Application (Builtin "==#") [a,b])
    = ifStatement (cs64 <> ppRenamed a <+> text "==" <+> cs64 <> ppRenamed b)
                  (bind =: int 1)
                  (bind =: int 0)
ppExpression (bind:_) (Application (Builtin "eqWord#") [a,b])
    = ifStatement (cu64 <> ppRenamed a <+> text "==" <+> cu64 <> ppRenamed b)
                  (bind =: int 1)
                  (bind =: int 0)
ppExpression (bind:_) (Application (Builtin "neWord#") [a,b])
    = ifStatement (cu64 <> ppRenamed a <+> text "!=" <+> cu64 <> ppRenamed b)
                  (bind =: int 1)
                  (bind =: int 0)
ppExpression (bind:_) (Application (Builtin "/=#") [a,b])
    = ifStatement (parens s64 <> ppRenamed a <+> text "!=" <+> parens s64 <> ppRenamed b)
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
ppExpression (bind:_) (Application (Builtin "<=##") [a,b])
    = ifStatement (castToDouble a <+> text "<=" <+> castToDouble b)
                  (bind =: int 1)
                  (bind =: int 0)
ppExpression (bind:_) (Application (Builtin ">=##") [a,b])
    = ifStatement (castToDouble a <+> text ">=" <+> castToDouble b)
                  (bind =: int 1)
                  (bind =: int 0)
ppExpression (bind:_) (Application (Builtin ">##") [a,b])
    = ifStatement (castToDouble a <+> text ">" <+> castToDouble b)
                  (bind =: int 1)
                  (bind =: int 0)
ppExpression (bind:_) (Application (Builtin "==##") [a,b])
    = ifStatement (castToDouble a <+> text "==" <+> castToDouble b)
                  (bind =: int 1)
                  (bind =: int 0)
ppExpression (bind:_) (Application (Builtin "and#") [a,b])
    = bind =: parens (parens (cu64 <> ppRenamed a) <+> text "&" <+> parens (cu64 <> ppRenamed b))
ppExpression (bind:_) (Application (Builtin "or#") [a,b])
    = bind =: parens (parens (cu64 <> ppRenamed a) <+> text "|" <+> parens (cu64 <> ppRenamed b))
ppExpression (bind:_) (Application (Builtin "xor#") [a,b])
    = bind =: parens (parens (cu64 <> ppRenamed a) <+> text "^" <+> parens (cu64 <> ppRenamed b))
ppExpression (bind:_) (Application (Builtin "not#") [a])
    = bind =: parens (text "~" <> parens (cu64 <> ppRenamed a))
ppExpression (bind:_) (Application (Builtin "ord#") [a])
    = bind =: ppRenamed a
ppExpression (bind:_) (Application (Builtin "chr#") [a])
    = bind =: ppRenamed a
ppExpression (bind:_) (Application (Builtin "negateInt#") [a])
    = bind =: (text "-" <+> cs64 <+> cu64 <> ppRenamed a)
ppExpression (bind:_) (Application (Builtin "negateDouble#") [a])
    = bind =: castToWord (text "-" <+> castToDouble a)
ppExpression (bind:_) (Application (Builtin "narrow8Word#") [a])
    = bind =: (cu64 <+> cu8 <+> cu64 <+> ppRenamed a)
ppExpression (bind:_) (Application (Builtin "narrow16Word#") [a])
    = bind =: (cu64 <+> cu16 <+> cu64 <+> ppRenamed a)
ppExpression (bind:_) (Application (Builtin "narrow32Word#") [a])
    = bind =: (cu64 <+> cu32 <+> cu64 <+> ppRenamed a)
ppExpression (bind:_) (Application (Builtin "narrow8Int#") [a])
    = bind =: (cu64 <+> cs8 <+> cu64 <+> ppRenamed a)
ppExpression (bind:_) (Application (Builtin "narrow16Int#") [a])
    = bind =: (cu64 <+> cs16 <+> cu64 <+> ppRenamed a)
ppExpression (bind:_) (Application (Builtin "narrow32Int#") [val])
    = bind =: (cu64 <+> cs32 <+> cu64 <+> ppRenamed val)
ppExpression (bind:_) (Application (Builtin "timesWord#") [a,b])
    = bind =: parens (parens u64 <+> ppRenamed a <+> text "*" <+> parens u64 <+> ppRenamed b)
ppExpression (bind:_) (Application (Builtin "plusWord#") [a,b])
    = bind =: parens (parens u64 <+> ppRenamed a <+> text "+" <+> parens u64 <+> ppRenamed b)
ppExpression (bind:_) (Application (Builtin "minusWord#") [a,b])
    = bind =: parens (parens u64 <+> ppRenamed a <+> text "-" <+> parens u64 <+> ppRenamed b)
ppExpression (bind:_) (Application (Builtin "*#") [a,b])
    = bind =: parens (parens s64 <+> ppRenamed a <+> text "*" <+> parens s64 <+> ppRenamed b)
ppExpression (bind:_) (Application (Builtin "*##") [a,b])
    = bind =: castToWord (castToDouble a <+> text "*" <+> castToDouble b)
ppExpression (bind:_) (Application (Builtin "-##") [a,b])
    = bind =: castToWord (castToDouble a <+> text "-" <+> castToDouble b)
ppExpression (bind:_) (Application (Builtin "+##") [a,b])
    = bind =: castToWord (castToDouble a <+> text "+" <+> castToDouble b)
ppExpression (bind:_) (Application (Builtin "/##") [a,b])
    = bind =: castToWord (castToDouble a <+> text "/" <+> castToDouble b)
ppExpression (bind:_) (Application (Builtin "**##") [a,b])
    = bind =: castToWord (text "pow" <> parens (castToDouble a <+> text "," <+> castToDouble b))
ppExpression (bind:_) (Application (Builtin "sinDouble#") [a])
    = bind =: castToWord (text "sin" <> parens (castToDouble a))
ppExpression (bind:_) (Application (Builtin "cosDouble#") [a])
    = bind =: castToWord (text "cos" <> parens (castToDouble a))
ppExpression (bind:_) (Application (Builtin "sqrtDouble#") [a])
    = bind =: castToWord (text "sqrt" <> parens (castToDouble a))
ppExpression (bind:_) (Application (Builtin "int2Double#") [a])
    = bind =: castToWord (parens (text "double") <> cu64 <> ppRenamed a)
ppExpression (bind:_) (Application (Builtin "+#") [a,b])
    = bind =: parens (parens s64 <+> ppRenamed a <+> text "+" <+> parens s64 <+> ppRenamed b)
ppExpression (bind:_) (Application (Builtin "-#") [a,b])
    = bind =: parens (parens s64 <+> ppRenamed a <+> text "-" <+> parens s64 <+> ppRenamed b)
ppExpression (bind:_) (Application (Builtin "quotInt#") [a,b])
    = bind =: parens (parens s64 <+> ppRenamed a <+> text "/" <+> parens s64 <+> ppRenamed b)
ppExpression (bind:_) (Application (Builtin "remInt#") [a,b])
    = bind =: parens (parens s64 <+> ppRenamed a <+> text "%" <+> parens s64 <+> ppRenamed b)
ppExpression (bind:_) (Application (Builtin "indexCharOffAddr#") [addr,idx])
    = bind =: (cu64 <+> parens (cu8p <+> ppRenamed addr) <> brackets (parens u64 <+> ppRenamed idx))
ppExpression (st:bind:_) (Application (Builtin "readCharArray#") [arr,idx,realWorld])
    = vsep [ bind =: (cu64 <+> parens (cu8p <+> ppRenamed arr) <> brackets (parens u64 <+> ppRenamed idx))
           , st   =: ppRenamed realWorld ]
ppExpression (st:_) (Application (Builtin "writeArray#") [arr, idx, elt, realWorld])
    = vsep [ st =: ppRenamed realWorld
           , ppRenamed arr <> brackets (cu64 <> ppRenamed idx) <+> equals <+> cu64 <> ppRenamed elt <> semi
           ]
ppExpression (st:bind:_) (Application (Builtin "readArray#") [arr, idx, realWorld])
    = vsep [ st =: ppRenamed realWorld
           , bind =: (ppRenamed arr <> brackets (cu64 <+> ppRenamed idx))
           ]
ppExpression (bind:_) (Application (Builtin "indexArray#") [arr, idx])
    = vsep [ bind =: (ppRenamed arr <> brackets (cu64 <> ppRenamed idx)) ]
ppExpression (st:_) (Application (Builtin "writeCharArray#") [arr,idx,chr,realWorld])
    = vsep [ parens (cu8p <+> ppRenamed arr) <> brackets (parens u64 <+> ppRenamed idx) <+>
             equals <+> cu8 <+> cu64 <+> ppRenamed chr <> semi
           , st =: ppRenamed realWorld ]
ppExpression (st:_) (Application (Builtin "writeWord8Array#") [arr,idx,word,realWorld])
    = vsep [ parens (cu8p <+> ppRenamed arr) <> brackets (parens u64 <+> ppRenamed idx) <+>
             equals <+> cu8 <+> cu64 <+> ppRenamed word <> semi
           , st =: ppRenamed realWorld ]
ppExpression (st:_) (Application (Builtin "writeWord8OffAddr#") [arr,idx,word,realWorld])
    = vsep [ parens (cu8p <+> ppRenamed arr) <> brackets (parens u64 <+> ppRenamed idx) <+>
             equals <+> cu8 <+> cu64 <+> ppRenamed word <> semi
           , st =: ppRenamed realWorld ]
ppExpression (st:_) (Application (Builtin "writeDoubleOffAddr#") [arr,idx,double,realWorld])
    = vsep [ parens (cu64p <+> ppRenamed arr) <> brackets (parens u64 <+> ppRenamed idx) <+>
             equals <+> cu64 <+> ppRenamed double <> semi
           , st =: ppRenamed realWorld ]
ppExpression (st:bind:_) (Application (Builtin "readAddrOffAddr#") [addr, idx, realworld])
    = vsep [ bind =: (ppRenamed addr <> brackets (parens u64 <+> ppRenamed idx))
           , st   =: ppRenamed realworld ]
ppExpression (st:bind:_) (Application (Builtin "readDoubleOffAddr#") [addr, idx, realworld])
    = vsep [ bind =: (ppRenamed addr <> brackets (parens u64 <+> ppRenamed idx))
           , st   =: ppRenamed realworld ]
ppExpression (st:bind:_) (Application (Builtin "readInt32OffAddr#") [addr,idx, realworld])
    = vsep [ bind =: (cu64 <+> parens (cs32p <+> ppRenamed addr) <> brackets (parens u64 <+> ppRenamed idx))
           , st   =: ppRenamed realworld
           ]
ppExpression (st:bind:_) (Application (Builtin "readWord8Array#") [addr,idx, realworld])
    = vsep [ bind =: (cu64 <+> parens (cu8p <+> ppRenamed addr) <> brackets (parens u64 <+> ppRenamed idx))
           , st   =: ppRenamed realworld
           ]
ppExpression (st:bind:_) (Application (Builtin "readInt8OffAddr#") [addr,idx, realworld])
    = vsep [ bind =: (cu64 <+> parens (cs8p <+> ppRenamed addr) <> brackets (parens u64 <+> ppRenamed idx))
           , st   =: ppRenamed realworld
           ]
ppExpression (st:bind:_) (Application (Builtin "unsafeFreezeByteArray#") [addr, realworld])
    = vsep [ bind =: ppRenamed addr
           , st   =: ppRenamed realworld ]
ppExpression (st:bind:_) (Application (Builtin "unsafeFreezeArray#") [addr, realworld])
    = vsep [ bind =: ppRenamed addr
           , st   =: ppRenamed realworld ]
ppExpression (bind:_) (Application (Builtin "byteArrayContents#") [addr])
    = bind =: ppRenamed addr
ppExpression (st:_) (Application (Builtin "touch#") [ptr,realworld])
    = st =: ppRenamed realworld

ppExpression (st:bind:_) (Application (Builtin "mkWeak#") [key, val, finalizer, realWorld])
    = vsep [ bind =: int 0
           , st   =: ppRenamed realWorld ]
ppExpression (st:arr:_) (Application (Builtin "newArray#") [size, elt, realWorld])
    = vsep [ st =: ppRenamed realWorld
           , arr =: alloc (cu64 <> ppRenamed size <+> text "* 8")
           , text "int " <> i <+> equals <+> text "0;"
           , text "for(;" <> i <> text "<" <> cu64 <> ppRenamed size <> text ";" <> i <> text"++) {" <$$>
             text "  " <> ppRenamed arr <> brackets i <+> equals <+> ppRenamed elt <> semi <$$>
             text "}"
           ]
    where i = text "i_" <> ppRenamed arr
ppExpression (st:arr:_) (Application (Builtin "newByteArray#") [size,realWorld])
    = vsep [ st =: ppRenamed realWorld
           , arr =: alloc ((parens u64) <+> ppRenamed size) ]
ppExpression (st:arr:_) (Application (Builtin "newPinnedByteArray#") [size,realWorld])
    = vsep [ st =: ppRenamed realWorld
           , arr =: alloc ((parens u64) <+> ppRenamed size) ]
-- FIXME: The ByteArray isn't aligned.
ppExpression (st:arr:_) (Application (Builtin "newAlignedPinnedByteArray#") [size,alignment,realWorld])
    = vsep [ st =: ppRenamed realWorld
           , arr =: alloc (cu64 <+> ppRenamed size) ]
ppExpression _ (Application (Builtin "update") (ptr:values))
    = vsep [ writeArray ptr n value | (n,value) <- zip [0..] values ]
ppExpression (st:_) (Application (Builtin "updateMutVar") [ptr,val,realWorld])
    = vsep [ writeArray ptr 0 val
           , st =: ppRenamed realWorld ]
ppExpression (st:bind:_) (Application (External "fdReady") args)
    = vsep [ bind =: int 1
           , st   =: ppRenamed (last args) ]
ppExpression (st:bind:_) (Application (External "isDoubleNegativeZero") [double,realworld])
    = vsep [ bind =: int 0
           , st   =: ppRenamed realworld ]
ppExpression (st:bind:_) (Application (External "isFloatNegativeZero") [double,realworld])
    = vsep [ bind =: int 0
           , st   =: ppRenamed realworld ]
ppExpression (st:bind:_) (Application (External "isDoubleNaN") [double,realworld])
    = vsep [ bind =: (text "isnan" <> parens (castToDouble double))
           , st   =: ppRenamed realworld ]
ppExpression (st:bind:_) (Application (External "isDoubleInfinite") [double,realworld])
    = vsep [ bind =: (text "isinf" <> parens (castToDouble double))
           , st   =: ppRenamed realworld ]
ppExpression (st:bind:_) (Application (External "isFloatNaN") [double,realworld])
    = vsep [ bind =: (text "isnan" <> parens (castToDouble double))
           , st   =: ppRenamed realworld ]
ppExpression (st:bind:_) (Application (External "isFloatInfinite") [double,realworld])
    = vsep [ bind =: (text "isinf" <> parens (castToDouble double))
           , st   =: ppRenamed realworld ]
ppExpression (st:_) (Application (External "getProgArgv") [argcPtr, argvPtr, realWorld])
    = vsep [ parens (parens (text "int*") <>ppRenamed argcPtr) <> brackets (int 0) <+> equals <+> text "global_argc" <> semi
           , ppRenamed argvPtr <> brackets (int 0) <+> equals <+> text "global_argv" <> semi
           , st   =: ppRenamed realWorld ]
ppExpression (st:bind:_) (Application (External "__hscore_get_errno") args)
    = vsep [ bind =: (cu64 <+> text "errno")
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

ppExpression binds e = puts (show (Grin.ppExpression e)) <$$> text "exit(1);"

castToWord double
    = text "doubleToWord" <> parens double
castToDouble ptr
    = text "wordToDouble" <> parens (ppRenamed ptr)

ppCase binds scrut alts
    = switch (parens (u64) <+> ppRenamed scrut) $
        vsep (map ppAlt alts) <$$> def (last alts)
    where def (Empty :> _)  = empty
          def _             = text "default:" <$$> indent 2 (puts ("No match for case: " ++ show scrut) <$$> text "exit(1);")
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
alloc size = text "GC_MALLOC" <> parens (size)

writeArray :: Renamed -> Int -> Renamed -> Doc
writeArray arr nth val
    = ppRenamed arr <> brackets (int nth) <+> equals <+> parens u64 <+> ppRenamed val <> semi

writeArray' :: Renamed -> Int -> Doc -> Doc
writeArray' arr nth val
    = ppRenamed arr <> brackets (int nth) <+> equals <+> parens u64 <+> val <> semi

(=:) :: Renamed -> Doc -> Doc
variable =: value = ppRenamed variable <+> equals <+> cu64p <+> value <> semi

declareVar :: Renamed -> Doc
declareVar var
    = u64 <> char '*' <+> ppRenamed var <> semi

declareVars :: [Renamed] -> Doc
declareVars = vsep . map declareVar

escString :: String -> Doc
escString string = char '"' <> text (concatMap worker string) <> char '"'
    where worker c | False = [c]
                   | otherwise = printf "\\x%02x" (ord c)

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
ppRenamed (Builtin "undefined")
    = text "0"
ppRenamed (Builtin builtin)
    = error $ "Grin.Stage2.Backend.C.ppRenamed: Unknown primitive: " ++ show builtin

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

cu64 = parens u64
cu64p = parens (u64<>char '*')

cu32 = parens u32
cu32p = parens (u32<>char '*')

cu16 = parens u16
cu16p = parens (u16<>char '*')

cu8 = parens u8
cu8p = parens (u8<>char '*')

cs64 = parens s64
cs64p = parens (s64<>char '*')

cs32 = parens s32
cs32p = parens (s32<>char '*')

cs16 = parens s16
cs16p = parens (s16<>char '*')

cs8 = parens s8
cs8p = parens (s8<>char '*')



