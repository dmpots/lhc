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

            -- GHC 6.12
           , "narrow8Int#"        ~> unOp cu8 ""
           , "narrow16Int#"       ~> unOp cu16 ""
           , "narrow32Int#"       ~> unOp cu32 ""

           , "narrow8Word#"        ~> unOp cu8 ""
           , "narrow16Word#"       ~> unOp cu16 ""
           , "narrow32Word#"       ~> unOp cu32 ""

           , "writeInt8OffAddr#"    ~> \[arr,idx,chr,realWorld] -> vsep [ writeAnyArray u8 arr idx chr
                                                                      , out [ ppRenamed realWorld ] ]
           , "writeWideCharOffAddr#"    ~> \[arr,idx,chr,realWorld] -> vsep [ writeAnyArray unit arr idx chr
                                                                      , out [ ppRenamed realWorld ] ]
           , "uncheckedIShiftL#"      ~> binOp csunit "<<"
           , "uncheckedIShiftRA#"     ~> binOp cunit  ">>"
           , "uncheckedIShiftRL#"     ~> binOp csunit ">>"

           , "uncheckedShiftL#"      ~> binOp csunit "<<"
           , "uncheckedShiftRL#"     ~> binOp csunit ">>"

           , "writeAddrOffAddr#"    ~> \[arr,idx,addr,realWorld] -> vsep [ writeAnyArray unit arr idx addr
                                                                      , out [ ppRenamed realWorld ] ]
           , "writeWord32OffAddr#"    ~> \[arr,idx,word32,realWorld] -> vsep [ writeAnyArray unit arr idx word32
                                                                      , out [ ppRenamed realWorld ] ]
           , "readWord32OffAddr#"  ~> \[arr,idx,realWorld] -> out [ ppRenamed realWorld
                                                                 , indexAnyArray cs32p arr idx ]
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


panic :: String -> Doc
panic txt = text "panic" <> parens (escString txt) <> semi

alloc :: Doc -> Doc
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

