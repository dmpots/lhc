module Ho.Type where

import Data.Monoid
import qualified Data.Map as Map

import Data.DeriveTH
import Data.Derive.All
import DataConstructors(DataTable)
import E.Rules(Rules)
import E.Type
import E.TypeCheck()
import FrontEnd.Class(ClassHierarchy)
import FrontEnd.Infix(FixityMap)
import FrontEnd.KindInfer(KindEnv)
import FrontEnd.SrcLoc(SrcLoc)
import FrontEnd.Tc.Type(Type())
import FrontEnd.HsSyn(Module)
import Name.Id
import Name.Name(Name)
import FrontEnd.TypeSynonyms(TypeSynonyms)
import PackedString
import Data.Binary
import qualified Data.Digest.Pure.MD5 as MD5
import Control.Monad

type SourceHash = MD5.MD5Digest
type HoHash     = MD5.MD5Digest

-- | The collected information that is passed around.
data CollectedHo = CollectedHo {
    -- | This is a list of external names that are valid but that we may not know anything else about.
    -- It is used to recognize invalid ids.
    choExternalNames :: IdSet,
    choCombinators  :: IdMap Comb,
    -- | This is a map of ids to their full 'TVr's with all rules and whatnot attached.
    -- 'choVarMap' will never contain any @Nothing@ elements. The @Maybe@ is only there
    -- because the map is handed over to 'E.Annotate.annotateDs'.
    choVarMap :: IdMap (Maybe E),
    -- | These are rules that may need to be retroactively applied to other modules
    choOrphanRules :: Rules,
    -- | The 'Ho's
    choHoMap :: Map.Map String Ho
    }


-- | This is the immutable information about modules that depends only on their contents.
-- It can be trusted even if the ho file itself is out of date.
newtype HoIDeps = HoIDeps {
    hoIDeps :: Map.Map SourceHash (Module,[Module])
    }

data HoHeader = HoHeader {
    -- | My identifying hash
    hohHash       :: HoHash,
    -- | Haskell Source files depended on
    hohDepends    :: [(Module,Maybe SourceHash)],
    -- | Other objects depended on to be considered up to date.
    hohModDepends :: [HoHash],
    -- | Meta-information, filled for hl-files, empty for normal objects.
    hohMetaInfo   :: [(String,PackedString)]
    }

-- | Data only needed for name resolution
data HoExp = HoExp {
    hoExports :: Map.Map Module [Name],
    hoDefs :: Map.Map Name (SrcLoc,[Name])
    }


data HoBuild = HoBuild {
    hoAssumps :: Map.Map Name Type,        -- ^ Used for typechecking
    hoFixities :: FixityMap,
    hoKinds :: KindEnv,                    -- ^ Used for typechecking
    hoClassHierarchy :: ClassHierarchy,
    hoTypeSynonyms :: TypeSynonyms,
    -- Filled in by E generation
    hoDataTable :: DataTable,
    hoEs :: [(TVr,E)],
    hoRules :: Rules
    }

data Ho = Ho {
    hoExp :: HoExp,
    hoBuild :: HoBuild
    }
$(derive makeUpdate ''Ho)
$(derive makeUpdate ''CollectedHo)
$(derive makeBinary ''HoIDeps)
$(derive makeUpdate ''HoBuild)

instance Monoid Ho where
    mempty = Ho mempty mempty
    mappend a b = Ho {
        hoExp = hoExp a `mappend` hoExp b,
        hoBuild = hoBuild a `mappend` hoBuild b
    }

instance Monoid HoExp where
    mempty = HoExp mempty mempty
    mappend a b = HoExp {
        hoExports = hoExports a `mappend` hoExports b,
        hoDefs = hoDefs a `mappend` hoDefs b
    }

instance Monoid HoBuild where
    mempty = HoBuild mempty mempty mempty mempty mempty mempty mempty mempty
    mappend a b = HoBuild {
        hoAssumps = hoAssumps a `mappend` hoAssumps b,
        hoFixities = hoFixities a `mappend` hoFixities b,
        hoKinds = hoKinds a `mappend` hoKinds b,
        hoClassHierarchy = hoClassHierarchy a `mappend` hoClassHierarchy b,
        hoTypeSynonyms = hoTypeSynonyms a `mappend` hoTypeSynonyms b,
        hoDataTable = hoDataTable a `mappend` hoDataTable b,
        hoEs = hoEs a `mappend` hoEs b,
        hoRules = hoRules a `mappend` hoRules b
    }


