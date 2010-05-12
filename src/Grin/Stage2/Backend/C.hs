{-# LANGUAGE StandaloneDeriving, OverloadedStrings #-}
module Grin.Stage2.Backend.C
    ( compile
    , compileFastCode
    , grinToC
    ) where

import CompactString
import Grin.Stage2.Types
import qualified Grin.Stage2.Pretty as Grin (ppExpression)

import Text.PrettyPrint.ANSI.Leijen hiding ((</>))

import System.Process
import System.FilePath
import System.Directory
import Data.Char
import Text.Printf
import System.IO
import System.Exit
import qualified Data.Map as M

import Paths_lhc

compile :: Grin -> FilePath -> IO ()
compile = compile' ["--debug", "-ggdb"]

compileFastCode :: Grin -> FilePath -> IO ()
compileFastCode = compile' ["-O2"]

compile' :: [String] -> Grin -> FilePath -> IO ()
compile' gccArgs grin target
    = do rts_c     <- getDataFileName ("rts" </> "rts.c")
         rts_ghc_c <- getDataFileName ("rts" </> "rts_ghc.c")
         rts_ghc   <- readFile rts_ghc_c
         let cTarget = replaceExtension target "c"
         copyFile rts_c cTarget
         appendFile cTarget rts_ghc 
         appendFile cTarget (show cCode)
         dDir <- getDataDir
         lDir <- getLibDir
         let wordSize   = ["-m32"]
         let libDir     = lDir </> "../" -- lDir points to the ghc-6.12.x subdir
         let incDirs    = [dDir </> "rts/include", libDir </> "include", "/opt/include/gc"]
         let ldDirs     = ["/opt/lib", libDir]
         let libs       = ["m", "gc", "iconv", "HSghc-prim-0.2.0.0", "HSBase-4.2.0.1", "HSinteger-gmp-0.1.0.0"]
         let gcOptions  = ["-DXMALLOC=GC_malloc", "-DXFREE=GC_free", "-DXREALLOC=GC_realloc"]
         let incOptions = map ("-I"++) incDirs
         let ldOptions  = map ("-L"++) ldDirs ++ map ("-l"++) libs
         let cmd        = (unwords $ cmdLine ++ wordSize ++ gcOptions ++ incOptions ++ ldOptions)
         --putStrLn ("RUNNING COMMAND: \n" ++ (show $ cmd))
         pid <- runCommand cmd
         ret <- waitForProcess pid
         case ret of
           ExitSuccess -> return ()
           _ -> do hPutStrLn stderr "C code failed to compile."
                   exitWith ret
    where cCode = grinToC grin
          cFile = replaceExtension target "c"
          cmdLine = ["gcc", "-w", cFile, "-o", target] ++ gccArgs



------------------------------------------------------
-- Grin -> C

grinToC :: Grin -> Doc
grinToC grin
    = vsep [ comment "CAFs:"
           , vsep (map ppCAF (grinCAFs grin))
           , comment "Return arguments:"
           , vsep (map ppCAF returnArguments)
           , comment "Function prototypes:"
           , vsep (map ppFuncDefProtoType (grinFunctions grin))
           , comment "Functions:"
           , vsep (map ppFuncDef (grinFunctions grin))
           , comment "Main:"
           , ppMain (grinCAFs grin) (grinEntryPoint grin)
           , linebreak
           ]

returnArguments :: [CAF]
returnArguments = [ CAF{ cafName = Aliased n "lhc_return", cafValue = Lit (Lint 0)} | n <- [1..20] ]

unitSize = 4

ppMain :: [CAF] -> Renamed -> Doc
ppMain cafs entryPoint
    = text "int" <+> text "main" <> parens (text "int argc" <> comma <+> text "char *argv[]") <+> char '{' <$$>
      indent 2 ( text "global_argc = argc;" <$$>
                 text "global_argv = argv;" <$$>
                 text "GC_init();" <$$>
                 --text "GC_set_max_heap_size(1024*1024*1024);" <$$> 
                 vsep [ vsep [ ppRenamed name <+> equals <+> alloc (int (4 * unitSize)) <> semi
                             , ppRenamed name <> brackets (int 0) <+> equals <+> int (uniqueId tag) <> semi]
                        | CAF{cafName = name, cafValue = Node tag _nt _missing} <- cafs ] <$$>
                 ppRenamed entryPoint <> parens empty <> semi <$$> {- ppFooter <$$> -} text "return 0" <> semi) <$$>
      char '}'

ppFooter :: Doc
ppFooter = vsep [ text "printf(\"Collections:       %d\\n\", GC_gc_no);"
                , text "printf(\"Total allocations: %lu\\n\", GC_get_total_bytes());"
                , text "printf(\"Heap size:         %ld\\n\", GC_get_heap_size());"
                ]

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
    = unitp <+> ppRenamed name <> semi
ppCAF CAF{cafName = name, cafValue = Lit (Lstring str)}
    = comment str <$$>
      unitp <+> ppRenamed name <+> equals <+> cunitp <+> escString (str++"\0") <> semi
ppCAF CAF{cafName = name, cafValue = Lit (Lint i)}
    = unitp <+> ppRenamed name <+> equals <+> cunitp <+> int (fromIntegral i) <> semi
ppCAF caf = error $ "Grin.Stage2.Backend.ppCAF: Invalid CAF: " ++ show (cafName caf)

ppFuncDefProtoType :: FuncDef -> Doc
ppFuncDefProtoType func
    = void <+> ppRenamed (funcDefName func) <> argList <> semi
    where argList = parens (hsep $ punctuate comma $ [ unitp <+> ppRenamed arg | arg <- funcDefArgs func ])

ppFuncDef :: FuncDef -> Doc
ppFuncDef func
    = void <+> ppRenamed (funcDefName func) <> argList <+> char '{' <$$>
      indent 2 (body <$$> text "return" <> semi) <$$>
      char '}'
    where argList = parens (hsep $ punctuate comma $ [ unitp <+> ppRenamed arg | arg <- funcDefArgs func ])
          body    = ppExpression (map cafName (take (funcDefReturns func) returnArguments)) (funcDefBody func)

mkBind binds vals
    = vsep [ bind =: val | (bind, val) <- zip binds (vals ++ repeat (int 0)) ]

ppExpression :: [Renamed] -> Expression -> Doc
ppExpression binds exp
    = case exp of
        Constant value      -> out [valueToDoc value]
        Application fn args ->
          case fn of
            Builtin prim    -> ppBuiltin binds prim args
            External ext tys-> ppExternal binds ext tys args
            _other          -> ppFunctionCall binds fn args
        Fetch nth variable  -> out [ ppRenamed variable <> brackets (int nth) ] -- out = var[nth];
        Unit variables      -> out (map ppRenamed variables)
        StoreHole size      -> out [ alloc (int $ max 4 size * unitSize) ]
        Store variables     -> out [ alloc (int $ max 4 (length variables) * unitSize) ] <$$>
                               vsep [ writeArray (head binds) n var | (n,var) <- zip [0..] variables ]
        Case scrut alts     -> ppCase binds scrut alts
        a :>>= binds' :-> b -> vsep [ declareVars binds'
                                    , ppExpression binds' a
                                    , ppExpression binds b ]
    where out = mkBind binds

ppBuiltin binds prim args
    = case M.lookup prim builtins of
        Nothing -> panic $ "unknown builtin: " ++ show prim
        Just fn -> fn args
    where builtins = M.fromList
           [ "coerceDoubleToWord" ~> \[arg] -> out [ ppRenamed arg ]
           , "noDuplicate#"       ~> \[arg] -> out [ ppRenamed arg ]
           , "chr#"               ~> \[arg] -> out [ ppRenamed arg ]
           , "ord#"               ~> \[arg] -> out [ ppRenamed arg ]
           , "byteArrayContents#" ~> \[arg] -> out [ ppRenamed arg ]
           , "realWorld#"         ~> \_     -> out [ int 0 ]
           , "unreachable"        ~> \_     -> panic "unreachable"

             -- Word arithmetics
           , "timesWord#"         ~> binOp cunit "*"
           , "plusWord#"          ~> binOp cunit "+"
           , "minusWord#"         ~> binOp cunit "-"
           , "quotWord#"          ~> binOp cunit "/"
           , "remWord#"           ~> binOp cunit "%"

             -- Int arithmetics
           , "*#"                 ~> binOp csunit "*"
           , "+#"                 ~> binOp csunit "+"
           , "-#"                 ~> binOp csunit "-"
           , "quotInt#"           ~> binOp csunit "/"
           , "remInt#"            ~> binOp csunit "%"
           , "negateInt#"         ~> unOp csunit "-"

             -- Comparing
           , "==#"                ~> cmpOp csunit "=="
           , "/=#"                ~> cmpOp csunit "!="
           , ">#"                 ~> cmpOp csunit ">"
           , ">=#"                ~> cmpOp csunit ">="
           , "<#"                 ~> cmpOp csunit "<"
           , "<=#"                ~> cmpOp csunit "<="

           , "eqWord#"            ~> cmpOp cunit "=="
           , "neWord#"            ~> cmpOp cunit "!="
           , "gtWord#"            ~> cmpOp cunit ">"
           , "geWord#"            ~> cmpOp cunit ">="
           , "ltWord#"            ~> cmpOp cunit "<"
           , "leWord#"            ~> cmpOp cunit "<="

             -- Bit operations
           , "and#"               ~> binOp cunit "&"
           , "or#"                ~> binOp cunit "|"
           , "xor#"               ~> binOp cunit "^"
           , "not#"               ~> unOp cunit "~"

             -- Narrowing
           , "narrow8Word#"       ~> unOp cu8 ""

             -- Mics IO
           , "newPinnedByteArray#" ~> \[size, realWorld] -> out [ ppRenamed realWorld
                                                                , alloc (cunit <+> ppRenamed size) ]
           , "newByteArray#" ~> \[size, realWorld] -> out [ ppRenamed realWorld
                                                          , alloc (cunit <+> ppRenamed size) ]
             -- FIXME: Array not aligned.
           , "newAlignedPinnedByteArray#" ~> \[size, alignment, realWorld]
                                          -> out [ ppRenamed realWorld
                                                 , alloc (cunit <+> ppRenamed size) ]
           , "unsafeFreezeByteArray#" ~> \[arr, realWorld] -> out [ ppRenamed realWorld, ppRenamed arr ]
           , "unsafeFreezeArray#" ~> \[arr, realWorld] -> out [ ppRenamed realWorld, ppRenamed arr ]
           , "updateMutVar"       ~> \[ptr, val, realWorld] -> vsep [ writeArray ptr 0 val
                                                                    , out [ ppRenamed realWorld ] ]
           , "mkWeak#"            ~> \[key, val, finalizer, realWorld]
                                     -> out [ ppRenamed realWorld, int 0 ]
           , "update"             ~> \(ptr:values) -> vsep [ writeArray ptr n value | (n,value) <- zip [0..] values ]
           , "touch#"             ~> \[ptr, realWorld] -> out [ ppRenamed realWorld ]
           , "newArray#"          ~> \[size, elt, realWorld] -> out [ ppRenamed realWorld
                                                                    , text "rts_newArray" <> parens (sep $ punctuate comma [alloc (cunit <> ppRenamed size <+> text "*" <+> int unitSize)
                                                                                                                           ,cunit <+> ppRenamed elt
                                                                                                                           ,cunit <> ppRenamed size])
                                                                    ]
           , "writeArray#"        ~> \[arr, idx, elt, realWorld] -> vsep [ writeAnyArray unit arr idx elt
                                                                         , out [ ppRenamed realWorld ] ]
           , "readArray#"         ~> \[arr, idx, realWorld] -> out [ ppRenamed realWorld
                                                                   , indexAnyArray cunitp arr idx ]
           , "indexArray#"        ~> \[arr, idx] -> out [ indexAnyArray cunitp arr idx ]

             -- Arrays
           , "writeCharArray#"    ~> \[arr,idx,chr,realWorld] -> vsep [ writeAnyArray u8 arr idx chr
                                                                      , out [ ppRenamed realWorld ] ]
           , "writeWord8Array#"    ~> \[arr,idx,chr,realWorld] -> vsep [ writeAnyArray u8 arr idx chr
                                                                       , out [ ppRenamed realWorld ] ]
           , "indexCharOffAddr#"  ~> \[arr,idx] -> out [ indexAnyArray cu8p arr idx ]
           , "readCharArray#"  ~> \[arr,idx,realWorld] -> out [ ppRenamed realWorld
                                                              , indexAnyArray cu8p arr idx ]
           , "readInt32OffAddr#"  ~> \[arr,idx,realWorld] -> out [ ppRenamed realWorld
                                                                 , indexAnyArray cs32p arr idx ]
           , "readInt8OffAddr#"  ~> \[arr,idx,realWorld] -> out [ ppRenamed realWorld
                                                                , indexAnyArray cs8p arr idx ]
           , "readAddrOffAddr#"  ~> \[arr,idx,realWorld] -> out [ ppRenamed realWorld
                                                                , indexAnyArray cunitp arr idx ]
           ]
          (~>) = (,)
          out = mkBind binds
          binOp ty fn [a,b] = out [ parens (ty <+> ppRenamed a <+> text fn <+> ty <+> ppRenamed b) ]
          unOp ty fn [a]    = out [ parens (text fn <+> parens (ty <+> ppRenamed a)) ]
          cmpOp ty fn [a,b] = ifStatement (ty <> ppRenamed a <+> text fn <+> ty <> ppRenamed b)
                                (out [ int 1 ])
                                (out [ int 0 ])
          writeAnyArray ty arr idx elt
              = parens (parens (ty <> char '*') <+> ppRenamed arr) <> brackets (cunit <+> ppRenamed idx)
                <+> equals <+>
                parens ty <+> cunit <+> ppRenamed elt <> semi
          indexAnyArray ty arr idx
              = parens (ty <+> ppRenamed arr) <> brackets (cunit <+> ppRenamed idx)

ppExternal binds "hs_free_stable_ptr" tys [ptr, realWorld]
    = text "hs_free_stable_ptr" <> parens (cvoidp <> ppRenamed ptr) <> semi
ppExternal binds "isDoubleNaN" tys [double, realWorld]
    = mkBind binds [ ppRenamed realWorld
                   , text "isnan" <> parens (castToDouble double) ]
ppExternal binds "isDoubleInfinite" tys [double, realWorld]
    = mkBind binds [ ppRenamed realWorld
                   , text "isinf" <> parens (castToDouble double) ]
ppExternal binds "isDoubleNegativeZero" tys [double, realWorld]
    = mkBind binds [ ppRenamed realWorld
                   , int 0 ]
ppExternal binds "isFloatNaN" tys [double, realWorld]
    = mkBind binds [ ppRenamed realWorld
                   , text "isnan" <> parens (castToDouble double) ]
ppExternal binds "isFloatInfinite" tys [double, realWorld]
    = mkBind binds [ ppRenamed realWorld
                   , text "isinf" <> parens (castToDouble double) ]
ppExternal binds "isFloatNegativeZero" tys [double, realWorld]
    = mkBind binds [ ppRenamed realWorld
                   , int 0 ]
ppExternal binds fn tys args
    = if returnType == UnitType
      then mkBind binds [ ppRenamed (last args) ] <$$>
           text fn <> argList <> semi
      else mkBind binds [ ppRenamed (last args)
                        , text fn <> argList ]
    where argList = parens $ hsep $ punctuate comma $ map ppRenamed (init args)
          returnType = last tys

ppFunctionCall binds fn args
    = vsep $ [ ppRenamed fn <> argList <> semi ] ++
             if isTailCall then [] else [ mkBind binds (map (ppRenamed.cafName) returnArguments) ]
    where argList = parens $ hsep $ punctuate comma $ map ppRenamed args
          isTailCall = and (zipWith (==) binds (map cafName returnArguments))

{-
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
    = bind =: (parens (cu32 <> ppRenamed w <+> text "<<" <+> cs32 <> ppRenamed i))
ppExpression (bind:_) (Application (Builtin "uncheckedShiftRL#") [w,i])
    = bind =: (parens (cu32 <> ppRenamed w <+> text ">>" <+> cs32 <> ppRenamed i))
ppExpression (bind:_) (Application (Builtin "noDuplicate#") [arg])
    = bind =: ppRenamed arg
ppExpression (bind:_) (Application (Builtin "==#") [a,b])
    = ifStatement (csunit <> ppRenamed a <+> text "==" <+> csunit <> ppRenamed b)
                  (bind =: int 1)
                  (bind =: int 0)
ppExpression (bind:_) (Application (Builtin "gtWord#") [a,b])
    = ifStatement (cunit <> ppRenamed a <+> text ">" <+> cunit <> ppRenamed b)
                  (bind =: int 1)
                  (bind =: int 0)
ppExpression (bind:_) (Application (Builtin "geWord#") [a,b])
    = ifStatement (cunit <> ppRenamed a <+> text ">=" <+> cunit <> ppRenamed b)
                  (bind =: int 1)
                  (bind =: int 0)
ppExpression (bind:_) (Application (Builtin "eqWord#") [a,b])
    = ifStatement (cunit <> ppRenamed a <+> text "==" <+> cunit <> ppRenamed b)
                  (bind =: int 1)
                  (bind =: int 0)
ppExpression (bind:_) (Application (Builtin "neWord#") [a,b])
    = ifStatement (cunit <> ppRenamed a <+> text "!=" <+> cunit <> ppRenamed b)
                  (bind =: int 1)
                  (bind =: int 0)
ppExpression (bind:_) (Application (Builtin "leWord#") [a,b])
    = ifStatement (cunit <> ppRenamed a <+> text "<=" <+> cunit <> ppRenamed b)
                  (bind =: int 1)
                  (bind =: int 0)
ppExpression (bind:_) (Application (Builtin "ltWord#") [a,b])
    = ifStatement (cunit <> ppRenamed a <+> text "<" <+> cunit <> ppRenamed b)
                  (bind =: int 1)
                  (bind =: int 0)
ppExpression (bind:_) (Application (Builtin "/=#") [a,b])
    = ifStatement (csunit <> ppRenamed a <+> text "!=" <+> csunit <> ppRenamed b)
                  (bind =: int 1)
                  (bind =: int 0)
ppExpression (bind:_) (Application (Builtin ">#") [a,b])
    = ifStatement (csunit <> ppRenamed a <+> text ">" <+> csunit <> ppRenamed b)
                  (bind =: int 1)
                  (bind =: int 0)
ppExpression (bind:_) (Application (Builtin ">=#") [a,b])
    = ifStatement (csunit <> ppRenamed a <+> text ">=" <+> csunit <> ppRenamed b)
                  (bind =: int 1)
                  (bind =: int 0)
ppExpression (bind:_) (Application (Builtin "<=#") [a,b])
    = ifStatement (csunit <> ppRenamed a <+> text "<=" <+> csunit <> ppRenamed b)
                  (bind =: int 1)
                  (bind =: int 0)
ppExpression (bind:_) (Application (Builtin "<#") [a,b])
    = ifStatement (csunit <> ppRenamed a <+> text "<" <+> csunit <> ppRenamed b)
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
    = bind =: parens (parens (cunit <> ppRenamed a) <+> text "&" <+> parens (cunit <> ppRenamed b))
ppExpression (bind:_) (Application (Builtin "or#") [a,b])
    = bind =: parens (parens (cunit <> ppRenamed a) <+> text "|" <+> parens (cunit <> ppRenamed b))
ppExpression (bind:_) (Application (Builtin "xor#") [a,b])
    = bind =: parens (parens (cunit <> ppRenamed a) <+> text "^" <+> parens (cunit <> ppRenamed b))
ppExpression (bind:_) (Application (Builtin "not#") [a])
    = bind =: parens (text "~" <> parens (cunit <> ppRenamed a))
ppExpression (bind:_) (Application (Builtin "ord#") [a])
    = bind =: ppRenamed a
ppExpression (bind:_) (Application (Builtin "chr#") [a])
    = bind =: ppRenamed a
ppExpression (bind:_) (Application (Builtin "negateInt#") [a])
    = bind =: (text "-" <+> csunit <+> cunit <> ppRenamed a)
ppExpression (bind:_) (Application (Builtin "negateDouble#") [a])
    = bind =: castToWord (text "-" <+> castToDouble a)
ppExpression (bind:_) (Application (Builtin "narrow8Word#") [a])
    = bind =: (cunit <+> cu8 <+> cunit <+> ppRenamed a)
ppExpression (bind:_) (Application (Builtin "narrow16Word#") [a])
    = bind =: (cunit <+> cu16 <+> cunit <+> ppRenamed a)
ppExpression (bind:_) (Application (Builtin "narrow32Word#") [a])
    = bind =: (cunit <+> cu32 <+> cunit <+> ppRenamed a)
ppExpression (bind:_) (Application (Builtin "narrow8Int#") [a])
    = bind =: (cunit <+> cs8 <+> cunit <+> ppRenamed a)
ppExpression (bind:_) (Application (Builtin "narrow16Int#") [a])
    = bind =: (cunit <+> cs16 <+> cunit <+> ppRenamed a)
ppExpression (bind:_) (Application (Builtin "narrow32Int#") [val])
    = bind =: (cunit <+> cs32 <+> cunit <+> ppRenamed val)
ppExpression (bind:_) (Application (Builtin "timesWord#") [a,b])
    = bind =: parens (cunit <+> ppRenamed a <+> text "*" <+> cunit <+> ppRenamed b)
ppExpression (bind:_) (Application (Builtin "plusWord#") [a,b])
    = bind =: parens (cunit <+> ppRenamed a <+> text "+" <+> cunit <+> ppRenamed b)
ppExpression (bind:_) (Application (Builtin "minusWord#") [a,b])
    = bind =: parens (cunit <+> ppRenamed a <+> text "-" <+> cunit <+> ppRenamed b)
ppExpression (bind:_) (Application (Builtin "*#") [a,b])
    = bind =: parens (csunit <+> ppRenamed a <+> text "*" <+> csunit <+> ppRenamed b)
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
ppExpression (bind:_) (Application (Builtin "logDouble#") [a])
    = bind =: castToWord (text "log" <> parens (castToDouble a))
ppExpression (bind:_) (Application (Builtin "expDouble#") [a])
    = bind =: castToWord (text "exp" <> parens (castToDouble a))
ppExpression (bind:_) (Application (Builtin "int2Double#") [a])
    = bind =: castToWord (parens (text "double") <> cunit <> ppRenamed a)
ppExpression (bind:_) (Application (Builtin "double2Int#") [a])
    = bind =: (cunit <> castToDouble a)
ppExpression (bind:_) (Application (Builtin "+#") [a,b])
    = bind =: parens (csunit <+> ppRenamed a <+> text "+" <+> csunit <+> ppRenamed b)
ppExpression (bind:_) (Application (Builtin "-#") [a,b])
    = bind =: parens (csunit <+> ppRenamed a <+> text "-" <+> csunit <+> ppRenamed b)
ppExpression (bind:_) (Application (Builtin "quotInt#") [a,b])
    = bind =: parens (csunit <+> ppRenamed a <+> text "/" <+> csunit <+> ppRenamed b)
ppExpression (bind:_) (Application (Builtin "quotWord#") [a,b])
    = bind =: parens (cunit <+> ppRenamed a <+> text "/" <+> cunit <+> ppRenamed b)
ppExpression (bind:_) (Application (Builtin "remInt#") [a,b])
    = bind =: parens (csunit <+> ppRenamed a <+> text "%" <+> csunit <+> ppRenamed b)
ppExpression (bind:_) (Application (Builtin "remWord#") [a,b])
    = bind =: parens (cunit <+> ppRenamed a <+> text "%" <+> cunit <+> ppRenamed b)
ppExpression (bind:_) (Application (Builtin "indexCharOffAddr#") [addr,idx])
    = bind =: (cunit <+> parens (cu8p <+> ppRenamed addr) <> brackets (cunit <+> ppRenamed idx))
ppExpression (st:bind:_) (Application (Builtin "readCharArray#") [arr,idx,realWorld])
    = vsep [ bind =: (cunit <+> parens (cu8p <+> ppRenamed arr) <> brackets (cunit <+> ppRenamed idx))
           , st   =: ppRenamed realWorld ]
ppExpression (st:_) (Application (Builtin "writeArray#") [arr, idx, elt, realWorld])
    = vsep [ st =: ppRenamed realWorld
           , ppRenamed arr <> brackets (cunit <> ppRenamed idx) <+> equals <+> cunit <> ppRenamed elt <> semi
           ]
ppExpression (st:bind:_) (Application (Builtin "readArray#") [arr, idx, realWorld])
    = vsep [ st =: ppRenamed realWorld
           , bind =: (ppRenamed arr <> brackets (cunit <+> ppRenamed idx))
           ]
ppExpression (bind:_) (Application (Builtin "indexArray#") [arr, idx])
    = vsep [ bind =: (ppRenamed arr <> brackets (cunit <> ppRenamed idx)) ]
ppExpression (st:_) (Application (Builtin "writeCharArray#") [arr,idx,chr,realWorld])
    = vsep [ parens (cu8p <+> ppRenamed arr) <> brackets (cunit <+> ppRenamed idx) <+>
             equals <+> cu8 <+> cunit <+> ppRenamed chr <> semi
           , st =: ppRenamed realWorld ]
ppExpression (st:_) (Application (Builtin "writeWord8Array#") [arr,idx,word,realWorld])
    = vsep [ parens (cu8p <+> ppRenamed arr) <> brackets (cunit <+> ppRenamed idx) <+>
             equals <+> cu8 <+> cunit <+> ppRenamed word <> semi
           , st =: ppRenamed realWorld ]
ppExpression (st:_) (Application (Builtin "writeWord8OffAddr#") [arr,idx,word,realWorld])
    = vsep [ parens (cu8p <+> ppRenamed arr) <> brackets (cunit <+> ppRenamed idx) <+>
             equals <+> cu8 <+> cunit <+> ppRenamed word <> semi
           , st =: ppRenamed realWorld ]
ppExpression (st:_) (Application (Builtin "writeInt8OffAddr#") [arr,idx,word,realWorld])
    = vsep [ parens (cs8p <+> ppRenamed arr) <> brackets (cunit <+> ppRenamed idx) <+>
             equals <+> cs8 <+> cunit <+> ppRenamed word <> semi
           , st =: ppRenamed realWorld ]
ppExpression (st:_) (Application (Builtin "writeDoubleOffAddr#") [arr,idx,double,realWorld])
    = vsep [ parens (cunitp <+> ppRenamed arr) <> brackets (cunit <+> ppRenamed idx) <+>
             equals <+> cunit <+> ppRenamed double <> semi
           , st =: ppRenamed realWorld ]
ppExpression (st:bind:_) (Application (Builtin "readAddrOffAddr#") [addr, idx, realworld])
    = vsep [ bind =: (ppRenamed addr <> brackets (cunit <+> ppRenamed idx))
           , st   =: ppRenamed realworld ]
ppExpression (st:bind:_) (Application (Builtin "readDoubleOffAddr#") [addr, idx, realworld])
    = vsep [ bind =: (ppRenamed addr <> brackets (cunit <+> ppRenamed idx))
           , st   =: ppRenamed realworld ]
ppExpression (st:bind:_) (Application (Builtin "readInt32OffAddr#") [addr,idx, realworld])
    = vsep [ bind =: (cunit <+> parens (cs32p <+> ppRenamed addr) <> brackets (cunit <+> ppRenamed idx))
           , st   =: ppRenamed realworld
           ]
ppExpression (st:bind:_) (Application (Builtin "readWord8Array#") [addr,idx, realworld])
    = vsep [ bind =: (cunit <+> parens (cu8p <+> ppRenamed addr) <> brackets (cunit <+> ppRenamed idx))
           , st   =: ppRenamed realworld
           ]
ppExpression (st:bind:_) (Application (Builtin "readInt8OffAddr#") [addr,idx, realworld])
    = vsep [ bind =: (cunit <+> parens (cs8p <+> ppRenamed addr) <> brackets (cunit <+> ppRenamed idx))
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
           , arr =: alloc (cunit <> ppRenamed size <+> text "* 8")
           , text "int " <> i <+> equals <+> text "0;"
           , text "for(;" <> i <> text "<" <> cunit <> ppRenamed size <> text ";" <> i <> text"++) {" <$$>
             text "  " <> ppRenamed arr <> brackets i <+> equals <+> ppRenamed elt <> semi <$$>
             text "}"
           ]
    where i = text "i_" <> ppRenamed arr
ppExpression (st:arr:_) (Application (Builtin "newByteArray#") [size,realWorld])
    = vsep [ st =: ppRenamed realWorld
           , arr =: alloc (cunit <+> ppRenamed size) ]
ppExpression (st:arr:_) (Application (Builtin "newPinnedByteArray#") [size,realWorld])
    = vsep [ st =: ppRenamed realWorld
           , arr =: alloc (cunit <+> ppRenamed size) ]
-- FIXME: The ByteArray isn't aligned.
ppExpression (st:arr:_) (Application (Builtin "newAlignedPinnedByteArray#") [size,alignment,realWorld])
    = vsep [ st =: ppRenamed realWorld
           , arr =: alloc (cunit <+> ppRenamed size) ]
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
    = vsep [ bind =: (cunit <+> text "errno")
           , st   =: ppRenamed (last args) ]
ppExpression (st:bind:_) (Application (External "__hscore_PrelHandle_write") [fd,ptr,offset,size,realWorld])
    = vsep [ bind =: (text "write" <> parens (hsep $ punctuate comma $ [ cunit <+> ppRenamed fd
                                                                       , ppRenamed ptr <+> text "+" <+> cunit <+> ppRenamed offset
                                                                       , ppRenamed size]))
           , st   =: ppRenamed realWorld ]
ppExpression (st:bind:_) (Application (External "__hscore_memcpy_dst_off") [dst, off, src, size, realWorld])
    = vsep [ bind =: (text "memcpy" <> parens (hsep $ punctuate comma $ [ cunit <+> ppRenamed dst <+> text "+" <+> cunit <+> ppRenamed off
                                                                        , ppRenamed src
                                                                        , ppRenamed size ]))
           , st =: ppRenamed realWorld ]
ppExpression (st:bind:_) (Application (External fn) args)
    = vsep [bind =: (text fn <> argList)
           ,st   =: ppRenamed (last args) ]
    where argList = parens $ hsep $ punctuate comma $ map ppRenamed (init args)

ppExpression binds e = panic (show (Grin.ppExpression e))
-}

castToWord double
    = text "doubleToWord" <> parens double
castToDouble ptr
    = text "wordToDouble" <> parens (ppRenamed ptr)

ppCase binds scrut alts
    = switch (cunit <+> ppRenamed scrut) $
        vsep (map ppAlt alts) <$$> def (last alts)
    where def (Empty :> _)  = empty
          def _             = text "default:" <$$> indent 2 (panic ("No match for case: " ++ show scrut))
          ppAlt (value :> exp)
              = case value of
                  Empty
                    -> text "default:" <$$> braces rest
                  Node tag _nt _missing
                    -> text "case" <+> int (uniqueId tag) <> colon <$$> braces rest
                  Lit (Lint i)
                    -> text "case" <+> int (fromIntegral i) <> colon <$$> braces rest
                  Lit (Lchar c)
                    -> text "case" <+> int (ord c) <> colon <$$> braces rest
              where rest = indent 2 ({-cafName (head returnArguments) =: ppRenamed (cafName (head returnArguments)) <$$>-}
                                     ppExpression binds exp <$$>
                                     text "break;")

valueToDoc :: Value -> Doc
valueToDoc (Node tag nt missing)    = int (uniqueId tag)
valueToDoc (Lit (Lint i))           = int (fromIntegral i)
valueToDoc (Lit (Lchar c))          = int (ord c)
valueToDoc (Lit (Lrational r))      = castToWord (double (fromRational r))
valueToDoc val                      = error $ "Grin.Stage2.Backend.C.valueToDoc: Can't translate: " ++ show val




{-
primOps = Map.fromList [ "==##" +> \ ~[bind] [a,b] ->
                         ifStatement (castToDouble a <+> text "==" <+> castToDouble b)
                         (bind =: int 1)
                         (bind =: int 0)
                       ]
    where (+>) = (,)
-}









panic :: String -> Doc
panic txt = text "panic" <> parens (escString txt) <> semi

alloc :: Doc -> Doc
--alloc size = text "GC_MALLOC" <> parens (size)
alloc size = text "alloc" <> parens (size)

writeArray :: Renamed -> Int -> Renamed -> Doc
writeArray arr nth val
    = ppRenamed arr <> brackets (int nth) <+> equals <+> cunit <+> ppRenamed val <> semi

writeArray' :: Renamed -> Int -> Doc -> Doc
writeArray' arr nth val
    = ppRenamed arr <> brackets (int nth) <+> equals <+> cunit <+> val <> semi

(=:) :: Renamed -> Doc -> Doc
variable =: value = ppRenamed variable <+> equals <+> cunitp <+> value <> semi

declareVar :: Renamed -> Doc
declareVar var
    = unit <> char '*' <+> ppRenamed var <> semi

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
      indent 2 (braces true) <$$>
      text "else" <$$>
      indent 2 (braces false)

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
unit     = text "unit"
unitp    = text "unit*"
sunit    = text "sunit"
sunitp   = text "sunit*"
u64      = text "u64"
u32      = text "u32"
u16      = text "u16"
u8       = text "u8"
s64      = text "s64"
s32      = text "s32"
s16      = text "s16"
s8       = text "s8"

cunit = parens unit
cunitp = parens unitp

csunit = parens sunit
csunitp = parens sunitp

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

cvoidp = parens (void <>char '*')

