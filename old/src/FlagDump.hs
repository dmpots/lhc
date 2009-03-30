module FlagDump(Flag(..),process,helpMsg,helpFlags) where

import qualified Data.Set as Set

-- | Flags
data Flag =
      AllDcons          -- ^ show unified data constructor table
    | AllKind           -- ^ show unified kind table after everything has been typechecked
    | AllTypes          -- ^ show unified type table, after everything has been typechecked
    | Aspats            -- ^ show as patterns
    | Bindgroups        -- ^ show bindgroups
    | BoxySteps         -- ^ show step by step what the type inferencer is doing
    | Class             -- ^ detailed information on each class
    | ClassSummary      -- ^ summary of all classes
    | Core              -- ^ show intermediate core code
    | CoreAfterlift     -- ^ show final core before writing ho file
    | CoreBeforelift    -- ^ show core before lambda lifting
    | CoreInitial       -- ^ show core right after E.FromHs conversion
    | CoreMangled       -- ^ de-typed core right before it is converted to grin
    | CoreMini          -- ^ show details even when optimizing individual functions
    | CorePass          -- ^ show each iteration of code while transforming
    | CoreSteps         -- ^ show what happens in each pass
    | Datatable         -- ^ show data table of constructors
    | Dcons             -- ^ data constructors
    | Decls             -- ^ processed declarations
    | Defs              -- ^ Show all defined names in a module
    | DepGraph          -- ^ Dump module dependancy graph to deps.dot
    | Derived           -- ^ show generated derived instances
    | EAlias            -- ^ show expanded aliases
    | EInfo             -- ^ show info tags on all bound variables
    | ESize             -- ^ print the size of E after each pass
    | EVerbose          -- ^ print very verbose version of E code always
    | Eval              -- ^ show detailed eval inlining info
    | Exports           -- ^ show which names are exported from each module
    | Grin              -- ^ show final grin code
    | GrinGraph         -- ^ print dot file of final grin code to outputname_grin.dot
    | GrinInitial       -- ^ grin right after conversion from core
    | GrinNormalized    -- ^ grin right after first normalization
    | GrinPass          -- ^ show each iteration of code while transforming
    | GrinPosteval      -- ^ show grin code just before eval\/apply inlining
    | GrinPreeval       -- ^ show grin code just before eval\/apply inlining
    | GrinSteps         -- ^ show what happens in each transformation
    | Html              -- ^ use html escape codes in output
    | Imports           -- ^ show in scope names for each module
    | Instance          -- ^ show instances
    | Kind              -- ^ show results of kind inference for each module
    | KindSteps         -- ^ show steps of kind inference
    | OptimizationStats -- ^ show combined stats of optimization passes
    | Parsed            -- ^ parsed code
    | Preprocessed      -- ^ code after preprocessing\/deliting
    | Program           -- ^ impl expls, the whole shebang.
    | Progress          -- ^ show basic progress indicators
    | Renamed           -- ^ code after uniqueness renaming
    | Rules             -- ^ show all user rules and catalysts
    | RulesSpec         -- ^ show specialization rules
    | SccModules        -- ^ show strongly connected modules in dependency order
    | Sigenv            -- ^ initial signature environment
    | SquareStats       -- ^ use square corners (this is now the only option, though)
    | Srcsigs           -- ^ processed signatures from source code
    | Stats             -- ^ show extra information about stuff
    | Steps             -- ^ show interpreter go
    | Tags              -- ^ list of all tags and their types
    | Types             -- ^ display unified type table containing all defined names
    | Tyvar             -- ^ show original tyvars rather than renaming them.
    deriving(Eq,Ord,Bounded)

instance Show Flag where
    show Preprocessed = "preprocessed"
    show Renamed = "renamed"
    show Parsed = "parsed"
    show Derived = "derived"
    show Imports = "imports"
    show Exports = "exports"
    show SccModules = "scc-modules"
    show DepGraph = "dep-graph"
    show Defs = "defs"
    show Kind = "kind"
    show KindSteps = "kind-steps"
    show Dcons = "dcons"
    show ClassSummary = "class-summary"
    show Class = "class"
    show Instance = "instance"
    show Bindgroups = "bindgroups"
    show Types = "types"
    show AllKind = "all-kind"
    show AllDcons = "all-dcons"
    show AllTypes = "all-types"
    show Sigenv = "sigenv"
    show Srcsigs = "srcsigs"
    show Program = "program"
    show Decls = "decls"
    show BoxySteps = "boxy-steps"
    show Aspats = "aspats"
    show Tyvar = "tyvar"
    show CorePass = "core-pass"
    show CoreSteps = "core-steps"
    show CoreMini = "core-mini"
    show CoreInitial = "core-initial"
    show CoreBeforelift = "core-beforelift"
    show CoreAfterlift = "core-afterlift"
    show Core = "core"
    show CoreMangled = "core-mangled"
    show Datatable = "datatable"
    show OptimizationStats = "optimization-stats"
    show Rules = "rules"
    show RulesSpec = "rules-spec"
    show EInfo = "e-info"
    show EVerbose = "e-verbose"
    show EAlias = "e-alias"
    show ESize = "e-size"
    show Tags = "tags"
    show GrinPreeval = "grin-preeval"
    show GrinPosteval = "grin-posteval"
    show Grin = "grin"
    show GrinInitial = "grin-initial"
    show GrinNormalized = "grin-normalized"
    show Steps = "steps"
    show Eval = "eval"
    show GrinPass = "grin-pass"
    show GrinSteps = "grin-steps"
    show GrinGraph = "grin-graph"
    show Progress = "progress"
    show Stats = "stats"
    show Html = "html"
    show SquareStats = "square-stats"

one :: String -> Either String (Set.Set Flag -> Set.Set Flag)
one "verbose" = Right $ foldr (.) id [ f | Right f <- [ one "progress"]]
one "core-mini" = Right $ Set.insert CoreMini
one "no-core-mini" = Right $ Set.delete CoreMini
one "kind-steps" = Right $ Set.insert KindSteps
one "no-kind-steps" = Right $ Set.delete KindSteps
one "veryverbose" = Right $ foldr (.) id [ f | Right f <- [ one "progress",one "stats"]]
one "program" = Right $ Set.insert Program
one "no-program" = Right $ Set.delete Program
one "grin-preeval" = Right $ Set.insert GrinPreeval
one "no-grin-preeval" = Right $ Set.delete GrinPreeval
one "tyvar" = Right $ Set.insert Tyvar
one "no-tyvar" = Right $ Set.delete Tyvar
one "grin-graph" = Right $ Set.insert GrinGraph
one "no-grin-graph" = Right $ Set.delete GrinGraph
one "e-alias" = Right $ Set.insert EAlias
one "no-e-alias" = Right $ Set.delete EAlias
one "renamed" = Right $ Set.insert Renamed
one "no-renamed" = Right $ Set.delete Renamed
one "aspats" = Right $ Set.insert Aspats
one "no-aspats" = Right $ Set.delete Aspats
one "all-dcons" = Right $ Set.insert AllDcons
one "no-all-dcons" = Right $ Set.delete AllDcons
one "all-kind" = Right $ Set.insert AllKind
one "no-all-kind" = Right $ Set.delete AllKind
one "instance" = Right $ Set.insert Instance
one "no-instance" = Right $ Set.delete Instance
one "square-stats" = Right $ Set.insert SquareStats
one "no-square-stats" = Right $ Set.delete SquareStats
one "defs" = Right $ Set.insert Defs
one "no-defs" = Right $ Set.delete Defs
one "e-size" = Right $ Set.insert ESize
one "no-e-size" = Right $ Set.delete ESize
one "grin-pass" = Right $ Set.insert GrinPass
one "no-grin-pass" = Right $ Set.delete GrinPass
one "core-initial" = Right $ Set.insert CoreInitial
one "no-core-initial" = Right $ Set.delete CoreInitial
one "class" = Right $ Set.insert Class
one "no-class" = Right $ Set.delete Class
one "datatable" = Right $ Set.insert Datatable
one "no-datatable" = Right $ Set.delete Datatable
one "core-afterlift" = Right $ Set.insert CoreAfterlift
one "no-core-afterlift" = Right $ Set.delete CoreAfterlift
one "steps" = Right $ Set.insert Steps
one "no-steps" = Right $ Set.delete Steps
one "all-types" = Right $ Set.insert AllTypes
one "no-all-types" = Right $ Set.delete AllTypes
one "core" = Right $ Set.insert Core
one "no-core" = Right $ Set.delete Core
one "types" = Right $ Set.insert Types
one "no-types" = Right $ Set.delete Types
one "preprocessed" = Right $ Set.insert Preprocessed
one "no-preprocessed" = Right $ Set.delete Preprocessed
one "rules" = Right $ Set.insert Rules
one "no-rules" = Right $ Set.delete Rules
one "exports" = Right $ Set.insert Exports
one "no-exports" = Right $ Set.delete Exports
one "core-steps" = Right $ Set.insert CoreSteps
one "no-core-steps" = Right $ Set.delete CoreSteps
one "sigenv" = Right $ Set.insert Sigenv
one "no-sigenv" = Right $ Set.delete Sigenv
one "kind" = Right $ Set.insert Kind
one "no-kind" = Right $ Set.delete Kind
one "html" = Right $ Set.insert Html
one "no-html" = Right $ Set.delete Html
one "rules-spec" = Right $ Set.insert RulesSpec
one "no-rules-spec" = Right $ Set.delete RulesSpec
one "optimization-stats" = Right $ Set.insert OptimizationStats
one "no-optimization-stats" = Right $ Set.delete OptimizationStats
one "srcsigs" = Right $ Set.insert Srcsigs
one "no-srcsigs" = Right $ Set.delete Srcsigs
one "class-summary" = Right $ Set.insert ClassSummary
one "no-class-summary" = Right $ Set.delete ClassSummary
one "grin-steps" = Right $ Set.insert GrinSteps
one "no-grin-steps" = Right $ Set.delete GrinSteps
one "dcons" = Right $ Set.insert Dcons
one "no-dcons" = Right $ Set.delete Dcons
one "eval" = Right $ Set.insert Eval
one "no-eval" = Right $ Set.delete Eval
one "grin-posteval" = Right $ Set.insert GrinPosteval
one "no-grin-posteval" = Right $ Set.delete GrinPosteval
one "grin-initial" = Right $ Set.insert GrinInitial
one "no-grin-initial" = Right $ Set.delete GrinInitial
one "parsed" = Right $ Set.insert Parsed
one "no-parsed" = Right $ Set.delete Parsed
one "core-pass" = Right $ Set.insert CorePass
one "no-core-pass" = Right $ Set.delete CorePass
one "e-verbose" = Right $ Set.insert EVerbose
one "no-e-verbose" = Right $ Set.delete EVerbose
one "core-mangled" = Right $ Set.insert CoreMangled
one "no-core-mangled" = Right $ Set.delete CoreMangled
one "progress" = Right $ Set.insert Progress
one "no-progress" = Right $ Set.delete Progress
one "imports" = Right $ Set.insert Imports
one "no-imports" = Right $ Set.delete Imports
one "stats" = Right $ Set.insert Stats
one "no-stats" = Right $ Set.delete Stats
one "core-beforelift" = Right $ Set.insert CoreBeforelift
one "no-core-beforelift" = Right $ Set.delete CoreBeforelift
one "e-info" = Right $ Set.insert EInfo
one "no-e-info" = Right $ Set.delete EInfo
one "decls" = Right $ Set.insert Decls
one "no-decls" = Right $ Set.delete Decls
one "tags" = Right $ Set.insert Tags
one "no-tags" = Right $ Set.delete Tags
one "derived" = Right $ Set.insert Derived
one "no-derived" = Right $ Set.delete Derived
one "bindgroups" = Right $ Set.insert Bindgroups
one "no-bindgroups" = Right $ Set.delete Bindgroups
one "boxy-steps" = Right $ Set.insert BoxySteps
one "no-boxy-steps" = Right $ Set.delete BoxySteps
one "scc-modules" = Right $ Set.insert SccModules
one "no-scc-modules" = Right $ Set.delete SccModules
one "grin-normalized" = Right $ Set.insert GrinNormalized
one "no-grin-normalized" = Right $ Set.delete GrinNormalized
one "grin" = Right $ Set.insert Grin
one "no-grin" = Right $ Set.delete Grin
one "dep-graph" = Right $ Set.insert DepGraph
one "no-dep-graph" = Right $ Set.delete DepGraph
one x = Left x

{-# NOINLINE process #-}
process :: Set.Set Flag -> [String] -> (Set.Set Flag, [String])
process s xs = foldr f (s,[]) (map one xs) where
   f (Right g) (s,xs) = (g s,xs)
   f (Left x) (s,xs) = (s,x:xs)

{-# NOINLINE helpMsg #-}
helpMsg :: String
helpMsg = "\n-- Front End --\n defs\n    Show all defined names in a module\n dep-graph\n    Dump module dependancy graph to deps.dot\n derived\n    show generated derived instances\n exports\n    show which names are exported from each module\n imports\n    show in scope names for each module\n parsed\n    parsed code\n preprocessed\n    code after preprocessing/deliting\n renamed\n    code after uniqueness renaming\n scc-modules\n    show strongly connected modules in dependency order\n\n-- Type Checker --\n all-dcons\n    show unified data constructor table\n all-kind\n    show unified kind table after everything has been typechecked\n all-types\n    show unified type table, after everything has been typechecked\n aspats\n    show as patterns\n bindgroups\n    show bindgroups\n boxy-steps\n    show step by step what the type inferencer is doing\n class\n    detailed information on each class\n class-summary\n    summary of all classes\n dcons\n    data constructors\n decls\n    processed declarations\n instance\n    show instances\n kind\n    show results of kind inference for each module\n kind-steps\n    show steps of kind inference\n program\n    impl expls, the whole shebang.\n sigenv\n    initial signature environment\n srcsigs\n    processed signatures from source code\n types\n    display unified type table containing all defined names\n tyvar\n    show original tyvars rather than renaming them.\n\n-- Intermediate code --\n core\n    show intermediate core code\n core-afterlift\n    show final core before writing ho file\n core-beforelift\n    show core before lambda lifting\n core-initial\n    show core right after E.FromHs conversion\n core-mangled\n    de-typed core right before it is converted to grin\n core-mini\n    show details even when optimizing individual functions\n core-pass\n    show each iteration of code while transforming\n core-steps\n    show what happens in each pass\n datatable\n    show data table of constructors\n e-alias\n    show expanded aliases\n e-info\n    show info tags on all bound variables\n e-size\n    print the size of E after each pass\n e-verbose\n    print very verbose version of E code always\n optimization-stats\n    show combined stats of optimization passes\n rules\n    show all user rules and catalysts\n rules-spec\n    show specialization rules\n\n-- Grin code --\n eval\n    show detailed eval inlining info\n grin\n    show final grin code\n grin-graph\n    print dot file of final grin code to outputname_grin.dot\n grin-initial\n    grin right after conversion from core\n grin-normalized\n    grin right after first normalization\n grin-pass\n    show each iteration of code while transforming\n grin-posteval\n    show grin code just before eval/apply inlining\n grin-preeval\n    show grin code just before eval/apply inlining\n grin-steps\n    show what happens in each transformation\n steps\n    show interpreter go\n tags\n    list of all tags and their types\n\n-- General --\n html\n    use html escape codes in output\n progress\n    show basic progress indicators\n square-stats\n    use square corners (this is now the only option, though)\n stats\n    show extra information about stuff\n verbose\n    progress\n veryverbose\n    progress stats\n"
helpFlags :: [String]
helpFlags = ["all-dcons", "all-kind", "all-types", "aspats", "bindgroups", "boxy-steps", "class", "class-summary", "core", "core-afterlift", "core-beforelift", "core-initial", "core-mangled", "core-mini", "core-pass", "core-steps", "datatable", "dcons", "decls", "defs", "dep-graph", "derived", "e-alias", "e-info", "e-size", "e-verbose", "eval", "exports", "grin", "grin-graph", "grin-initial", "grin-normalized", "grin-pass", "grin-posteval", "grin-preeval", "grin-steps", "html", "imports", "instance", "kind", "kind-steps", "optimization-stats", "parsed", "preprocessed", "program", "progress", "renamed", "rules", "rules-spec", "scc-modules", "sigenv", "square-stats", "srcsigs", "stats", "steps", "tags", "types", "tyvar", "verbose", "veryverbose"]

