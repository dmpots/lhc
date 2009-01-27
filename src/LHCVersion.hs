-- | Defines some info about the compiler
module LHCVersion where
import Data.Version
import Data.List
import System.Info
import qualified Paths_lhc as P

package        = "lhc"
tag            = "scio"
version        = showVersion P.version
shortVersion   = concat $ intersperse "." $ map show $ take 3 $ versionBranch P.version

-- | Simple version string
{-# NOINLINE versionSimple #-}
versionSimple = unwords [package, version, "("++tag++")"]

-- | Full version string containing OS/compiler info
{-# NOINLINE versionString #-}
versionString = concat [versionSimple, " compiled by ",compilerName,"-",showVersion compilerVersion," on a ",arch," running ",os]
