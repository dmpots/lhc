module Properties
    ( properties
    ) where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck (testProperty)
import Test.QuickCheck

import Data.Monoid
import Control.Monad
import Grin.Types
import Grin.HPT.Environment as Env

instance Arbitrary Rhs where
    arbitrary = do rhsValues <- arbitrary
                   return $ mconcat (map singleton rhsValues)

instance Arbitrary RhsValue where
    arbitrary = oneof [ liftM3 Extract arbitrary arbitrary arbitrary
                      , liftM2 ExtractVector arbitrary arbitrary
                      , liftM Eval arbitrary
                      , liftM2 Env.Update arbitrary arbitrary
                      , liftM2 Apply arbitrary arbitrary
                      , liftM2 PartialApply arbitrary arbitrary
                      , liftM Ident arbitrary
                      , liftM Fetch arbitrary
                      , return Base
                      , liftM Heap arbitrary
                      , sized $ \n -> liftM4 Tag arbitrary arbitrary arbitrary (resize (n `div` 2) arbitrary)
                      , sized $ \n -> liftM VectorTag (resize (n `div` 2) arbitrary)
                      ]

instance Arbitrary Renamed where
    arbitrary = liftM Anonymous arbitrary

instance Arbitrary NodeType where
    arbitrary = elements [ ConstructorNode
                         , FunctionNode ]


properties
    = [ testGroup "HPT"
        [ testProperty "isSubsetOf" prop_isSubsetOf ]
      ]


prop_isSubsetOf a b = a `isSubsetOf` b == (b == (a `mappend` b))

