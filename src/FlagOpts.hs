module FlagOpts(Flag(..),process,helpMsg,helpFlags) where

import qualified Data.Set as Set

-- | Flags
data Flag =
      Boehm             -- ^ use Boehm garbage collector
    | Controlled        -- ^ with the '-f' flag, the following options are availible, you can
    | Cpp               -- ^ pass haskell source through c preprocessor
    | Cpr               -- ^ do CPR analysis
    | Debug             -- ^ enable debugging code in generated executable
    | Defaulting        -- ^ perform defaulting of ambiguous types
    | Ffi               -- ^ support foreign function declarations
    | FloatIn           -- ^ perform float inward transform
    | GlobalOptimize    -- ^ perform whole program E optimization
    | InlinePragmas     -- ^ use inline pragmas
    | Lint              -- ^ perform lots of extra type checks
    | M4                -- ^ pass haskell source through m4 preprocessor
    | MonomorphismRestriction -- ^ enforce monomorphism restriction
    | Negate            -- ^ any particular one by prepending 'no-' to it.
    | Profile           -- ^ enable profiling code in generated executable
    | Raw               -- ^ just evaluate main to WHNF and nothing else.
    | Rules             -- ^ use rules
    | Strictness        -- ^ perform strictness analysis
    | TypeAnalysis      -- ^ perhaps a basic points-to analysis on types right after method generation
    | UnboxedTuples     -- ^ allow unboxed tuple syntax to be recognized
    | UnboxedValues     -- ^ allow unboxed value syntax
    | Unsafe            -- ^ disable runtime assertions
    | Wrapper           -- ^ wrap main in exception handler
    deriving(Eq,Ord,Bounded)

instance Show Flag where
    show Controlled = "controlled"
    show Negate = "negate"
    show UnboxedTuples = "unboxed-tuples"
    show UnboxedValues = "unboxed-values"
    show Ffi = "ffi"
    show Cpp = "cpp"
    show M4 = "m4"
    show Unsafe = "unsafe"
    show MonomorphismRestriction = "monomorphism-restriction"
    show Defaulting = "defaulting"
    show Lint = "lint"
    show InlinePragmas = "inline-pragmas"
    show Rules = "rules"
    show FloatIn = "float-in"
    show Strictness = "strictness"
    show Cpr = "cpr"
    show TypeAnalysis = "type-analysis"
    show GlobalOptimize = "global-optimize"
    show Wrapper = "wrapper"
    show Boehm = "boehm"
    show Profile = "profile"
    show Debug = "debug"
    show Raw = "raw"

one "profile" = Right $ Set.insert Profile
one "no-profile" = Right $ Set.delete Profile
one "boehm" = Right $ Set.insert Boehm
one "no-boehm" = Right $ Set.delete Boehm
one "cpr" = Right $ Set.insert Cpr
one "no-cpr" = Right $ Set.delete Cpr
one "m4" = Right $ Set.insert M4
one "no-m4" = Right $ Set.delete M4
one "defaulting" = Right $ Set.insert Defaulting
one "no-defaulting" = Right $ Set.delete Defaulting
one "lint" = Right $ Set.insert Lint
one "no-lint" = Right $ Set.delete Lint
one "ffi" = Right $ Set.insert Ffi
one "no-ffi" = Right $ Set.delete Ffi
one "strictness" = Right $ Set.insert Strictness
one "no-strictness" = Right $ Set.delete Strictness
one "rules" = Right $ Set.insert Rules
one "no-rules" = Right $ Set.delete Rules
one "monomorphism-restriction" = Right $ Set.insert MonomorphismRestriction
one "no-monomorphism-restriction" = Right $ Set.delete MonomorphismRestriction
one "controlled" = Right $ Set.insert Controlled
one "no-controlled" = Right $ Set.delete Controlled
one "debug" = Right $ Set.insert Debug
one "no-debug" = Right $ Set.delete Debug
one "wrapper" = Right $ Set.insert Wrapper
one "no-wrapper" = Right $ Set.delete Wrapper
one "unsafe" = Right $ Set.insert Unsafe
one "no-unsafe" = Right $ Set.delete Unsafe
one "float-in" = Right $ Set.insert FloatIn
one "no-float-in" = Right $ Set.delete FloatIn
one "unboxed-values" = Right $ Set.insert UnboxedValues
one "no-unboxed-values" = Right $ Set.delete UnboxedValues
one "inline-pragmas" = Right $ Set.insert InlinePragmas
one "no-inline-pragmas" = Right $ Set.delete InlinePragmas
one "unboxed-tuples" = Right $ Set.insert UnboxedTuples
one "no-unboxed-tuples" = Right $ Set.delete UnboxedTuples
one "global-optimize" = Right $ Set.insert GlobalOptimize
one "no-global-optimize" = Right $ Set.delete GlobalOptimize
one "default" = Right $ foldr (.) id [ f | Right f <- [ one "inline-pragmas",one "rules",one "wrapper",one "float-in",one "strictness",one "defaulting",one "type-analysis",one "monomorphism-restriction",one "boxy",one "eval-optimize",one "global-optimize"]]
one "negate" = Right $ Set.insert Negate
one "no-negate" = Right $ Set.delete Negate
one "cpp" = Right $ Set.insert Cpp
one "no-cpp" = Right $ Set.delete Cpp
one "raw" = Right $ Set.insert Raw
one "no-raw" = Right $ Set.delete Raw
one "type-analysis" = Right $ Set.insert TypeAnalysis
one "no-type-analysis" = Right $ Set.delete TypeAnalysis
one x = Left x

{-# NOINLINE process #-}
process s xs = foldr f (s,[]) (map one xs) where
   f (Right g) (s,xs) = (g s,xs)
   f (Left x) (s,xs) = (s,x:xs)

{-# NOINLINE helpMsg #-}
helpMsg = "\n-- Code options --\n cpp\n    pass haskell source through c preprocessor\n ffi\n    support foreign function declarations\n m4\n    pass haskell source through m4 preprocessor\n unboxed-tuples\n    allow unboxed tuple syntax to be recognized\n unboxed-values\n    allow unboxed value syntax\n unsafe\n    disable runtime assertions\n\n-- Typechecking --\n defaulting\n    perform defaulting of ambiguous types\n monomorphism-restriction\n    enforce monomorphism restriction\n\n-- Debugging --\n lint\n    perform lots of extra type checks\n\n-- Optimization Options --\n cpr\n    do CPR analysis\n float-in\n    perform float inward transform\n global-optimize\n    perform whole program E optimization\n inline-pragmas\n    use inline pragmas\n rules\n    use rules\n strictness\n    perform strictness analysis\n type-analysis\n    perhaps a basic points-to analysis on types right after method generation\n\n-- Code Generation --\n boehm\n    use Boehm garbage collector\n debug\n    enable debugging code in generated executable\n profile\n    enable profiling code in generated executable\n raw\n    just evaluate main to WHNF and nothing else.\n wrapper\n    wrap main in exception handler\n\n-- Default settings --\n default\n    inline-pragmas rules wrapper float-in strictness defaulting type-analysis monomorphism-restriction boxy eval-optimize global-optimize\n"
helpFlags = ["boehm", "controlled", "cpp", "cpr", "debug", "default", "defaulting", "ffi", "float-in", "global-optimize", "inline-pragmas", "lint", "m4", "monomorphism-restriction", "negate", "profile", "raw", "rules", "strictness", "type-analysis", "unboxed-tuples", "unboxed-values", "unsafe", "wrapper"]

