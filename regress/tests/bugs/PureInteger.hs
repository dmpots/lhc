--LICENSE: BSD3 (or also similar, like GHC, etc.)
--INITIAL AUTHOR: Isaac Dupree <id@isaac.cedarswampstudios.org>
--
-- modified by austin seipp for use with LHC
{-
            (search file for INTERESTING to customize)

  What is this? It is a reimplementation, in Haskell, of the
  Integer type that Haskell provides in its Prelude.  It is designed
  in mind of being actually usable as the implementation of that type,
  for compilers.  It is also a module that exports a working
  Integer type.  It is in terms of only basic Prelude functions, [],
  and Int. It is NOT a purely inductive definition, because Int is
  much faster than a purely inductive definition would allow, and
  nevertheless often easier to come by (more portable, license-wise,
  size-wise, nuisance-wise...) than GMP or other C bignum libraries.

SPEED:
  It is not too slow on small numbers (smallish constant - much
  larger than for Int of course), not too slow on medium-size
  numbers (which I've been testing it with), and not too slow on
  large numbers (asymptotically; karatsuba multiplication,
  O(n^1.585) is used to split up large numbers, and division by
  large numbers uses multiplication and Newton's method).
  Also see BUGS for the speed of 'show'.

CORRECTNESS:
  It seems to be correct, after a fairly thorough million-iteration
  QuickCheck in GHC plus a lot of quickcheck testing using
  debugging-"Int"s that tell you when they overflow and have
  (minBound,maxBound)=(-31,31).  Each of these caught an incredible
  number of bugs, which is why I am inclined to trust them.
  Unfortunately, most Haskell implementations are somewhat incorrect
   - see COMPILERS. Also see CAVEATS(c) for *very* large numbers.

CAVEATS:
 a- It is obviously much slower than GMP. (although I don't know if
      the penalties for calling primitive/foreign functions
      counterbalance that for small numbers.)
 b- It assumes that Int operations are fairly fast, although it
      doesn't tend to waste them that much (e.g. it uses `quotRem`
      when it needs both, which is almost always).
 c- It is expected to break when handling values with magnitude
      greater than around ( (maxBound::Int)^(maxBound::Int) ).
      (just like GMP.  Probably the assumption is that you'll run
      out of memory at the same time, or that the operations will
      take SO LONG, so no-one cares.
      Prelude.length will (relatedly) also break at this point.)

CODE SIZE:
  Could be smaller, but then, could be larger too.  Possibly
  some environments will not appreciate when I have duplicated
  functionality in order to make some size of operation go faster.
  Of course these could be changed.  So far I have refrained from
  CPP, but there are probably some ways CPP could be used to make it
  easier to customize. The prime / single-quote symbol is
  deliberately not used as part of any identifiers, there are no
  string gaps or backslashes at end of lines,
  // /* */ are treated nicely, so I hope CPP won't mess anything up.

USAGE AS NATIVE INTEGER:
  Completely untried so far (search file for INTERESTING to
  customize).

  The algorithms are quite separable from the newtype Integer and
  its instances, and Bits, Ix... parts can be separated out too
  (in which case a proper export list would have to be made for the
  algorithm functions, which might hinder optimization a little,
  if assuming separate compilation...).
  See <http://www.haskell.org/ghc/docs/
                latest/html/libraries/base/Prelude.html#t%3AInteger>
  for all things GHC's Prelude.Integer be an instance of, including
     Typeable? Data? NFData? PrintfArg? Typeable? Random?
  This is also a purpose CPP could be useful for, to define (#define?)
  quotRemDInt and such type-specific things, conditionally on how
  this Integer-implementation is being used.

  The internal format is (currently) a list([]) of Ints
  in base "intLargeBase", least-significant "digit" first
  (negative x is represented by negating all elements of
    the list that represents positive x)
  (No most-significant zeroes are allowed
    (so zero is represented by the empty list, for example))
  ("intLargeBase" is customizable, although there is an optimal
    value for any particular size of Int, and a limit based on the
    Int's size)

TODO/BUGS:
  COMPILERS.
  a- There is one WORKAROUND so Hugs can compile it and two for
       nhc/yhc.  Even so, I refused to keep a third workaround for
       N/Yhc, but that is for a nyhc bug that Neil thinks should be
       reasonably fixable.
  b- though extensively QuickChecked in GHC, Hugs occasionally fails
       QuickCheck, but when the particular example is run in Hugs,
       it gives the correct answer!  I think Hugs is buggy.
       (Hugs Version September 2006)
  c- ghc -O -fvia-C may miscompile "quotRem x (some power of two)".
       (ghc trac#1603)
       If QuickCheck fails badly on you, try adding -fasm.
  d- Beware testing in interpreters - some (at least GHCi 6.6.1)
       will default to Prelude.Integer even if the module has
       "default ()".

  Make show faster than O(n^2), e.g. see
        http://darcs.haskell.org/packages/base/GHC/Num.lhs ?
      ...but converting that, it seemed altogether slower than the
       current
    `signum (2 P.^ (1000000::P.Int)) :: Integer` works in a few
       seconds in ghci
    `P.length (show ((2 P.^ (1000000::P.Int)) :: Integer))`
       takes much longer, in fact hasn't finished yet
       (slower for "reasonable-size" Integers to be shown, I mean,
         which were the only ones I tested with)
       With -O2 and the "O(n^2)" implementation of show,
    `length (show ((2 P.^ (100000::Prelude.Int))
                        :: IntegerInTermsOfInt.Integer))`
        took my computer about 15 seconds, and
    `length (show ((2 P.^ (1000000::Prelude.Int))
                        :: IntegerInTermsOfInt.Integer))`
        about 5000 seconds.  Looks quadratic to me, but what to do
        about it? NB. This has a real effect on testing with
        LargeBase=16(Int=[-31..31])

  The multiplication and division code, which is the most
     complicated, is not documented well enough. (N.B. refactoring
     and renaming are often part of documenting) (This is somewhat
     improved now, but it's still not very organized...)

  Bits, of course, requires a power-of-two base... and assumes two's
   complement Int; however, it does not need ((-maxBound)-1 ::Int)
   to be a possible value, because of how the digits are
   represented.
  Also the Bits code has not been cleaned up at all.

  Maybe `par` stuff could be inserted... (division is the slowest,
    not sure quite why it's so bad, but it might be worth examining
    (except 'show' on incredibly large inputs, of course)) (Remember
    to test with -threaded; apparently that allows `par` to use
    CPU-level parallelism; do you have to give an RTS option to tell
    it your number of CPUs too?)

  try using data LD = LDNil | LDCons {-#UNPACK#-}!DInt !LD as rep
  and see how GHC performs
    names?:   :+*  low :+* highs  for low + base * highs

  addition.. is still O(max) as long as mkInteger traverses the
  whole thing.  Since I use the strict constructor ((!:) currently)
  everywhere, this is not necessary, only for assertions
  (also careful of Prelude.{map,splitAt} producing non-strict lists)

  CONVERSION code uses literal 0 as overloaded

-}
{-# OPTIONS_LHC -N #-}
module Main where
--cleaned of \t , [ \t]$ , .\{80,\} ,
-- \(let\|where\|do\|of\)[ ] such that the layout following them extends
--onto another line ,
--ghc -Wall #except a warning when not using intBaseGuesses (Bounded Int)
--   -fno-warn-unused-binds #for now
--   -fno-warn-incomplete-patterns #ghc's incomplete pattern checker
--      #isn't very strong.  While I haven't been able to get Catch
--      #to tell me anything useful, QuickCheck passes.
--   #Should I just make all my patterns artificially complete?
--Also avoiding "'"s in identifiers, for cpp's sake ,
--syntactic sugar that is likely not to work pre-Prelude as well as
--in case the list type wants to be replaced...

import Prelude (
   --all the Int operations are only portably available from typeclasses
   --(also, Integer has to be made an instance of most of these classes)
   Eq(..), Ord(..), Num(..), Enum(..), Integral(..), Show(..), Real(..),
   --[](..), --lists are built-in syntax
   Bool(..), Ordering(..),
   seq, -- we need to use seq on the Ints,
        -- to make (seq (someInteger)) work as expected
   -- some things are just too convenient:
   (&&), (||)--, not
   , id, const
 )

--WORKAROUND (for Hugs and NYhc)
--For hugs to import (:) (and 2-tuples? other list bits?) we have to import
--Prelude unqualified, and not like import Prelude ()
-- -- conformant implementations like GHC will not even
--accept explicit imports of those things.
--Also a June 19 2007 YHC from darcs seems quite broken without this import.
--But we don't want to import anything but [](..), ()(..), (,)(..),
-- (,,)(..) and so on
--so we hide everything that according to Haskell98 would be importable.
import Prelude hiding
 --manually copied down everything from :browse Prelude in ghci
 ((++),error,foldr,seq,concat,filter,zip,print,fst,snd,otherwise,(&&),(||)
 ,Bounded(..),Enum(..),Eq(..),Floating(..),Fractional(..),Integral(..)
 ,Monad(..),Functor(..),Num(..),Ord(..),Read(..),Real(..),RealFloat(..)
 ,RealFrac(..),Show(..),Bool(..),Char,Double,Float,Int
 ,Integer,Ordering(..),Rational,IO,Either(..)
 ,putChar,putStr,putStrLn,getChar,getLine,getContents,interact,readFile
 ,writeFile,appendFile,readLn,readIO,($!),String,map,not,id,const,(.),flip,($)
 ,until,asTypeOf,IOError,FilePath,ioError,userError,ReadS,catch,unwords
 ,unlines,words,lines,minimum,maximum,product,sum,foldl1,either,lex,read
 ,readParen,reads,ShowS,showParen,showString,showChar,shows,subtract
 ,realToFrac,fromIntegral,(^^),(^),lcm,gcd,odd,even,unzip3,unzip,zipWith3
 ,zipWith,zip3,lookup,notElem,elem,break,span,dropWhile,takeWhile,splitAt
 ,drop,take,cycle,replicate,repeat,iterate,scanr1,scanr,scanl1,scanl,concatMap
 ,all,any,or,and,foldr1,foldl,reverse,(!!),length,null,init,tail,last,head
 ,undefined,uncurry,curry,maybe,(=<<),sequence_,sequence,mapM_,mapM,Maybe(..))

-- useful list stuff:  (some list functions use a hardcoded Int type...)
import qualified Prelude as L ((++), map, last, Int
               , take, reverse, drop, length, splitAt, foldr1
               , all)
-- other ghastly debug stuff, currently only (++) and (show) in an assertion
-- that could be deleted though:
import qualified Prelude as P
import Prelude as Print (print)
--import qualified Prelude as BaseOp (numerical...)
--import qualified Prelude as LengthOp (numerical...)
--literals? negative literals?

--INTERESTING
import qualified Prelude as D (Int)
--import qualified TestingInt as D (Int)
--     --  ^ assert(we are not being the native integer)
--             because TestingInt is implemented in terms of it.



--  **** for guessing the base ***
import Prelude (minBound, maxBound)

-- *** for conversions with Num/Integral? ***
-- use this to convert from an arbitrary integral - it might be optimized,
-- and it's as good as (fromInteger . toInteger)
import qualified Prelude as Misc (fromIntegral)
-- SPECIALIZE for this type
import Prelude as N (Integer, Int)

-- ****** instances, newtype imports ********

--for defining Integer instances
import qualified Prelude as Class (
  Eq(..),Ord(..),Num(..),Enum(..),Integral(..),Show(..),Real(..),Read(..) )
import qualified {-"Data." optional of course-}Data.Ix as Class ( Ix(..) )
--import qualified Random as Class ( Random(..) )
--Copy Random instance from System.Random where it's normally defined,
--if you want.
--Same for Typeable? Data? NFData? PrintfArg?
import {-qualified-} Data.Bits as Class ( Bits(..) )

--used for Show Integer auxiliary functions' type signatures
import Prelude (String)

--for defining Read Integer
import Numeric (readSigned, readDec)

--for defining Integral Integer (quotRem)
import Prelude as Error (error)

--INTERESTING probably the second definition (or deleting all asserts)
--             is better for some purposes...
--import TestingDebug
assert :: b -> c -> a -> a; assert _ _ a = a

--WORKAROUND
--true, but breaks nhc/yhc currently:
--default ()



--could be cpp macros... and each usage defined explicitly
--e.g. listLit0, listLit1, listLitNeg1
--listLIT
--listLITNeg
--intLIT
--lenLIT
--integralLIT(Neg)
--D(.)Int: a digit, as in our lists of ints.
--PInt: length/list/prelude Int
--HInteger: the integer we're defining here
--NInteger: the native integer
--HInteger__: HInteger, just not constructor-boxed nor necessarily deep-seq'd
--dIntNeg1 --may be a macro
--only [DInt] and [[DInt]] are actually used, never in a way that actually
--has to be polymorphic! And we never use value-level [x] sugar.
--Now we just need to change type-signatures a bit
--(and allow prelude list functions to be replaced)
--and we can use (L for list, D for digit)
--data LD = LDNil | LDCons {-#UNPACK#-}(?)!DInt !(?)LD
--or similar, and also define LLD or: type LLD = [] LD
--Some places may still rely on the laziness of lists though
--(those should be fixed, for easier experimenting)
type PInt = L.Int
type DInt = D.Int
--         PInt
--         TestingInt.Int
type HInteger__ = [DInt]

type IdF t = t -> t --or "Endo"...
-- "nat" = natural = nonnegative; "neg" = negative
-- these must be used as if they could be functions OR macros,
-- i.e. generally "(xNeg(1))" if it is to be usable wherever "3" is.
-- (numeric literals in patterns are avoided anyway.)
dNat, dNeg :: IdF DInt; dNat x = x; dNeg = negate
pNat{-, pNeg-} :: IdF PInt; pNat x = x--; pNeg = negate
-- if (nonnegative) numeric literals are already working, these should work:
-- #define dNat(x) ((x) :: DInt)
-- #define dNeg(x) (negate (x) :: DInt)
-- #define pNat(x) ((x) :: PInt)
---- #define pNeg(x) (negate (x) :: PInt)
-- If integer literals won't do at all,(assuming DInt==PInt - easy to change)
-- #define dNat(x) (nat/**/x)
-- #define dNeg(x) (neg/**/x)
-- #define pNat(x) (nat/**/x)
---- #define pNeg(x) (neg/**/x)
-- (is /**/ the right concatenation syntax there? should it be ##? ...)
-- and define nat0, nat1, neg1, nat2 ... (whatever ones are used)
-- somehow.

infixr 5 !:
(!:) :: DInt -> [DInt] -> [DInt]
d !: ds = d `seq` ds `seq` (d : ds)
strictMap :: (DInt -> DInt) -> [DInt] -> [DInt]
strictMap f = m
  where
    m (d:ds) = f d !: m ds
    m [] = []

--S = signed, LE = little-endian
--Large = within the largeIntBase (or at least base that, for the LELists);
--similarly for Small...
--Large is the default when neither is mentioned in a name.
type LargeSInt = DInt
type LargeBaseLEList = [LargeSInt]
type SmallSInt = DInt
type SmallBaseLEList = [SmallSInt]

validBase :: DInt -> [DInt] -> Bool
validBase _base [] = True
validBase base ds@(_:_) =
   let sign = signum (L.last ds)
   in ((dNat(1)) == sign || (dNeg(1)) == sign)
       && L.all (\digit ->
        negate base < digit && digit < base &&
                       (sign * digit) >= (dNat(0))) ds

--Prefer if/then/else to guards with otherwise, so an interpreter
--doesn't have to evaluate "otherwise"

--let's collect all literals here for easy reference/hackability
--(for different implementations possibly)
--(also to try to make sure interpreters don't duplicate work...)
--zero, one, two, negativeOne,
intLargeBase, intSmallBase :: DInt
intLargeExponentOfTwo, intSmallExponentOfTwo :: PInt
--zero = 0; one = 1; two = 2; negativeOne = -1

{-plusOne, minusOne, twice,-}
twicePlusOne :: DInt -> DInt
{-plusOne n = succ n --succ n or 1+n
minusOne n = pred n --pred n or n - 1 or (-1)+n
twice n = n + n --two*n or n+n ...-}
twicePlusOne n = succ (n + n) --or plusOne (twice n)

--INTERESTING
--must be true:
-- * intSmallBase^2 = intLargeBase
-- *          (intLargeBase-1)*2+1   <= (maxBound::DInt)
-- * negate ( (intLargeBase-1)*2+1 ) >= (minBound::DInt)
--if the ExponentOfTwos are ever USED by the code, these need to be true
--as well:
-- * 2^intLargeExponentOfTwo = intLargeBase
-- * 2^intSmallExponentOfTwo = intSmallBase
(intLargeBase, intLargeExponentOfTwo, intSmallBase, intSmallExponentOfTwo)
  = {-sees "ibg"-} intBaseGuesses --whatever powers of 2 are best
--  = (268435456, 28, 16384, 14) --Haskell98 minimum (30 or 31 bit, signed)
--  = (1073741824, 30, 32768, 15) --32-bit (signed)
--  = (4611686018427387904, 62, 2147483648, 31) --64-bit (signed)
--  = (1000000, nonsense, 1000, nonsense) where nonsense = nonsense


--Originally I had intLargeBase=2^28, intSmallBase=2^14
--to stay within Haskell98 (-2^29,2^29).

--if we don't consider class Bits or hypothetical machine speed,
--there is no good reason we should require anything other than
--  intLargeBase == intSmallBase^2   | (base - 1)*2 + 1 <= maxBound
--But either way the minimum is DInt [-7..7] (large [-3..3], small [-1..1])
--and we don't lose that much by rounding down to only the power-of-two bases.
--(although, base large=100/small=10 might help people understand/debug it,
--though it is already permissible to _imagine_ that -- and it would
--make read/show quite efficient...)

--A search is tricky since we have to stay within unknown capabilities
--(we don't have Integer yet, it _depends_ on these results!)
--Also since it is not _required_ that minBound is not a great deal larger
--in magnitude than maxBound, for example, we need to be careful about that.

--It is rather recommended to have SmallBase > 10 (for Show)
-- ==> LargeBase > 100 ==> a range of at least +-199;
-- which rules out single-byte;
-- and powers of two are also recommended; leaving a reasonable minimum
-- at SmallBase=16; LargeBase=256; range at least +- (2^9 - 1):
-- 16-bit Ints should work just fine (although larger sizes are still
-- preferable of course, as long as they have fast arithmetic
-- including quotRem :)

intBaseGuesses :: (DInt,PInt,DInt,PInt)
intBaseGuesses
  = assert (
     --This may happen if someone tries an unsigned, e.g. Word...
     "IntegerInTermsOfInt: (Signed) range of DInt (way) too small:\n" P.++
     "[" P.++P.show minDInt P.++ ".." P.++P.show maxDInt P.++ "]::DInt" P.++
     "; [-31..31]::needed." --or [-17..17] if we allow/guess non-powers-of-2
    ) (
          (minDInt < (dNat(0)) && maxDInt > (dNat(0))
           && minDInt < (dNat(1)) && maxDInt > (dNat(1))
           && smallExponentResult > (pNat(1)) --for division - was >0=[-7..7]
            )
    ) result
  where
    result@(_,_,_,smallExponentResult) = f (dNat(0)) (pNat(0))
                                           (dNat(0)) (pNat(0))
    minDInt, maxDInt :: DInt
    minDInt = minBound; maxDInt = maxBound
    --if the remainder has a magnitude of 1 rather than 0, the modulus
    --may be one more than half the bound rounded down
    --(there's then room for a carry of 1 in n+n)
    -- iff <, safe to double (>=, can't double); if <=, safe to use/accept.
    maxLargeBase = case maxDInt `quotRem` (dNat(2)) of (q,r) -> q + r
    minNegLargeBase = case minDInt `quotRem` (dNat(2)) of (q,r) -> q + r
    safeToDoublePlusOne n = n < maxLargeBase && minNegLargeBase < negate n
--    --safeToDouble implies safeToUse
--    safeToDouble n = n <  maxLargeBase && minNegLargeBase <  negate n
--    safeToUse n = n <= maxLargeBase && minNegLargeBase <= negate n
    -- f takes arguments known to be in-bounds for valid use
    -- (i.e., safeToDoublePlusOne) and returns the largest that work.
    f nLarge nLargeExponentOfTwo nSmall nSmallExponentOfTwo =
     let
       twoNLarge = twicePlusOne nLarge; fourNLarge = twicePlusOne twoNLarge
     in {-traces "f" traces nLarge traces twoNLarge P.$-}
     if safeToDoublePlusOne twoNLarge && safeToDoublePlusOne fourNLarge
     then f  (fourNLarge)           (nLargeExponentOfTwo + (pNat(2)))
             (twicePlusOne nSmall)  (nSmallExponentOfTwo + (pNat(1)))
     else ( (succ nLarge), (nLargeExponentOfTwo)
          , (succ nSmall), (nSmallExponentOfTwo) )

--no overflow allowed to be possible when calling these:
quotRemByIntLargeBase, quotRemByIntSmallBase :: DInt -> (DInt, DInt)
--the OrZero is needed for dealing with carries (a.k.a. borrows)...
--                                        preconditions:
{-addIntLargeBaseToMakePositiveOrZero -- negate intLargeBase <= input < 0
 , addIntSmallBaseToMakePositiveOrZero -- as above but with intSmallBase
 , subtractIntLargeBaseToMakeNegativeOrZero -- 0 < input <= intLargeBase
 , subtractIntSmallBaseToMakeNegativeOrZero -- as above but with intSmallBase
 , complementInt
 , multiplyByIntSmallBase
        :: DInt -> DInt
-}
--might implement via Bits (they all can be, assuming a two's complement DInt)
--(although it seems divMod is more natural than quotRem
--to implement via Bits)
quotRemByIntLargeBase input = input `quotRem` intLargeBase
quotRemByIntSmallBase input = input `quotRem` intSmallBase
{---adding and subtracting are lightning-fast machine instructions, we don't
--need alternatives!
addIntLargeBaseToMakePositiveOrZero input = input + intLargeBase
addIntSmallBaseToMakePositiveOrZero input = input + intSmallBase
--these don't like the OrZero for Bits...
subtractIntLargeBaseToMakeNegativeOrZero input = input - intLargeBase
subtractIntSmallBaseToMakeNegativeOrZero input = input - intSmallBase
complementInt input = minusOne (negate input)
multiplyByIntSmallBase input = input * intSmallBase
-}

--inverse of quotRemByIntSmallBase
--perhaps should be named unQuotRemByIntSmallBase? be uncurried?
highLowFromIntSmallBase :: DInt->DInt -> DInt
highLowFromIntSmallBase high low = high * intSmallBase + low

--  [-intLargeBase, intLargeBase)  -intLargeBase becomes 0
repairNegIntLargeBase :: DInt -> DInt
repairNegIntLargeBase d = d `rem` intLargeBase

isZero{-, isPositive, isNegative-} :: DInt -> Bool
isZero = ((dNat(0)) ==)
--isNegative = ((dNat(0)) >)
--isPositive = ((dNat(0)) <)

--could use bit-shifts: and addition and comparison are, I think, the
--only other operations PInt needs. Maybe even could use Double????
--I'm not sure if PInt ever even needs to be negative.
pTwice :: PInt -> PInt
pTwice x = x + x
-- may not use on negative numbers:
pHalfRoundingDown, pHalfRoundingUp :: PInt -> PInt
pHalfRoundingDown x = x `quot` 2
pHalfRoundingUp x = pHalfRoundingDown (succ x)





-- *********************** SIGN, and COMPARISON **********************

--requires the two to each have consistent signs throughout
--little-endian all-same-sign no-most-significant-zero required
--Works with any one base.
compareInteger :: HInteger__ -> HInteger__ -> Ordering
--Either we can go looking for integer1 and integer2 having opposite
--signs early, avoiding zero digits,
--or we can not bother (which will often be faster due to simplicity).
compareInteger (d1:ds1) (d2:ds2) =
                case compareInteger ds1 ds2 of
                        EQ -> compare d1 d2
                        answer -> answer
compareInteger ds1@(_:_) [] = compareNonzeroIntegerZero ds1
compareInteger [] ds2@(_:_) = compareZeroNonzeroInteger ds2
compareInteger []  []       = EQ

--requires the two to each have consistent signs throughout
--little-endian all-same-sign no-most-significant-zero required
--Works with any one base.
compareAbsInteger :: HInteger__ -> HInteger__ -> Ordering
compareAbsInteger (d1:ds1) (d2:ds2) =
                case compareAbsInteger ds1 ds2 of
                        EQ -> compare (abs d1) (abs d2)
                        answer -> answer
compareAbsInteger (_:_) [] = GT
compareAbsInteger [] (_:_) = LT
compareAbsInteger []  []   = EQ


zeroInteger, oneInteger, negativeOneInteger :: HInteger__
zeroInteger = []
oneInteger = (dNat(1)) !: []
negativeOneInteger = (dNeg(1)) !: []

isZeroInteger :: HInteger__ -> Bool
isZeroInteger [] = True
isZeroInteger (_:_) = False

--these can only return GT and LT ...
--A case for [] would be enough to remove the "nonzero" restriction! But then
--the postcondition would not be automatically checked (it might return EQ
--somewhere that would act oddly if it received EQ).
compareNonzeroIntegerZero, compareZeroNonzeroInteger :: HInteger__ -> Ordering
compareNonzeroIntegerZero =
     \(d:ds) -> case compare d (dNat(0)) of
                        EQ -> compareNonzeroIntegerZero ds
                        answer -> answer
compareZeroNonzeroInteger =
     \(d:ds) -> case compare (dNat(0)) d of
                        EQ -> compareZeroNonzeroInteger ds
                        answer -> answer

compareIntegerZero :: HInteger__ -> Ordering
compareIntegerZero [] = EQ
compareIntegerZero integer@(_:_) = compareNonzeroIntegerZero integer

isNegativeInteger :: HInteger__ -> Bool
isNegativeInteger i = compareIntegerZero i == LT

signumInteger :: HInteger__ -> HInteger__
signumInteger a = case compareIntegerZero a of
       { GT -> oneInteger; LT -> negativeOneInteger; EQ -> zeroInteger }

-- they work on just about anything... maybe not absInteger on
--  mixed-sign lists.
negateInteger, absInteger :: [DInt] -> [DInt]
negateInteger = strictMap negate
absInteger = strictMap abs





--or could judge 0 as positive, or,,,:
signNonzeroInteger :: HInteger__ -> DInt
signNonzeroInteger (d:ds) = let s = signum d in
                    if isZero s then signNonzeroInteger ds else s


-- ******************* ADDITION / SUBTRACTION *************************

-- PUTTING TOGETHER VALID INTEGER BITS:
-- prepend is like (:)/cons except it won't add a most-significant zero.
prepend :: DInt -> [DInt] -> [DInt]
prepend d [] | isZero d = []
prepend d ds = d !: ds
prependZero :: [DInt] -> [DInt]
prependZero [] = []
prependZero ds = (dNat(0)) !: ds
prependNonzero :: DInt -> [DInt] -> [DInt]
prependNonzero d ds = d !: ds
--it doesn't check, as it doesn't even know what the intended base is,
--but it should not be used on an overflowing DInt:
fromDInt :: DInt -> [DInt]
fromDInt d = if isZero d then [] else d !: []

-- For us, adding numbers with opposite signs is much the same as
-- subtracting numbers with the same sign ("destructive"),
-- and adding-same is similar to subtracting opposites ("synergistic"),
-- but the two are rather different from each other.
-- (and adding zero is trivial.)
-- average O(min(m,n)), worst-case O(max(m,n))=O(m+n)
addInteger :: HInteger__ -> HInteger__ -> HInteger__
addInteger [] integer = integer
addInteger integer [] = integer
addInteger integer1 integer2 =
   if compareNonzeroIntegerZero integer1 == compareNonzeroIntegerZero integer2
    then synergisticAdd integer1 integer2
    else destructiveAdd intLargeBase integer1 integer2


-- Positive plus positive or negative plus negative,
-- no most-significant zeroes in arguments nor results.
-- (zero works fine as either(?))
--carrying can only be by +-1 at most
synergisticAdd :: LargeBaseLEList -> LargeBaseLEList -> LargeBaseLEList
synergisticAdd integer1 integer2 =
      synergisticAddWithCarry integer1 integer2 (dNat(0))

synergisticAddWithCarry :: LargeBaseLEList -> LargeBaseLEList -> DInt
                        -> LargeBaseLEList
synergisticAddWithCarry (d1:ds1) (d2:ds2) carry =
   case quotRemByIntLargeBase (d1 + d2 + carry) of
    (carry_, d_) -> d_ !: synergisticAddWithCarry ds1 ds2 carry_
synergisticAddWithCarry [] [] carry = fromDInt carry
synergisticAddWithCarry ds@(_:_) [] carry = synergisticAddOnlyCarry ds carry
synergisticAddWithCarry [] ds@(_:_) carry = synergisticAddOnlyCarry ds carry

-- like adding. carry is zero or +-one. (or maybe it can be >1)
synergisticAddOnlyCarry :: LargeBaseLEList -> DInt -> LargeBaseLEList
synergisticAddOnlyCarry integer carry =
  if isZero carry --then optimize even if integer is nonzero
   then integer
   else case integer of
          [] -> carry !: []
          (d:ds) -> case quotRemByIntLargeBase (d + carry) of
                  (carry_, d_) -> d_ !: synergisticAddOnlyCarry ds carry_
synergisticAddOnlyCarrySmall :: SmallBaseLEList -> DInt -> SmallBaseLEList
synergisticAddOnlyCarrySmall integer carry =
  if isZero carry
   then integer
   else case integer of
          [] -> carry !: []
          (d:ds) -> case quotRemByIntSmallBase (d + carry) of
                  (carry_, d_) -> d_ !: synergisticAddOnlyCarrySmall ds carry_

destructive
  :: (DInt -> DInt -> DInt) -> {-base::-}DInt
  -> HInteger__{-base-}
  -> HInteger__{-base-} -> HInteger__{-base-}
--destructive _ _ [] [] = [] --not needed, with non-strictness...
destructive op base integer1 integer2
  = destructive_ op base (signNonzeroInteger integer1) integer1 integer2

--Now works on (<=0)-(<=0)=(<=0) if told to:
--{-pass negative base-}, and change compare i1 i2 to
--compare (abs i1) (abs i2) (= comparing abs i1 i2 = on compare abs i1 i2)
--, and succ->(+sign), oneInteger->sign:[],base->sign*base
--to allow subtracting negatives just as well
--Now with abs, should be easy addDestructive too. or destructive (+)...
destructive_
  :: (DInt -> DInt -> DInt) -> {-base::-}DInt
  -> DInt -> HInteger__{-base-}
  -> HInteger__{-base-} -> HInteger__{-base-}
destructive_ (+- {-either (+) or (-)-} ) base sign = sub
 where
  --s=signed
  sBase = sign * base
--  sInteger = sign !: []
--  sPlusOne = (+)sign--(`minus` (negate 1))--sign))
  sub [] [] = []
  sub ds@(_:_) [] = ds
--sub [] ds@(_:_) = disallowed
  sub (d1:ds1) (d2:ds2) = case compare (abs d1) (abs d2) of
--Avoids sending any DInt to the opposite sign even temporarily
    GT -> prependNonzero ((        d1) +- d2) (sub       ds1 ds2)
    EQ -> prependZero                         (sub       ds1 ds2)
    LT -> prependNonzero ((sBase + d1) +- d2) (subBorrow ds1 ds2)
--    LT -> prepend        ((sBase + d1) - d2) (sub ds1 (inc ds2))
  --borrowing: may produce d2=base:
  --(which yields the only reason the LT case isn't prependNonzero)
--  inc [] = sInteger
--  inc (i2raw:ds2) = (sPlusOne i2raw) !: ds2
--subBorrow [] _ = disallowed
  subBorrow (d1:ds1) (d2:ds2) = case compare (abs d1) (succ (abs d2)) of
    GT -> prependNonzero (((        d1) +- d2) - sign) (sub       ds1 ds2)
    EQ -> prependZero                                  (sub       ds1 ds2)
    LT -> prepend        (((sBase + d1) +- d2) - sign) (subBorrow ds1 ds2)
  subBorrow ds1@(_:_) [] = borrow ds1
  borrow (d1:ds1) = if isZero d1
     then prependNonzero (  sBase{- + 0 +- 0-} - sign) (borrow ds1) --LT 1
     else prepend        (          d1{-+- 0-} - sign) (ds1)      --EQ/GT 1
{--
--One argument should be all-nonpositive, the other all-nonnegative.
--Result can only have most-significant zeroes if an argument does.
--Arguments/result may be in any base.
destructiveAddProducingMixedSign :: [DInt] -> [DInt]
                                 -> [DInt]
destructiveAddProducingMixedSign (d1:ds1) (d2:ds2) =
  prepend (d1 + d2) (destructiveAddProducingMixedSign ds1 ds2)
destructiveAddProducingMixedSign ds@(_:_) [] = ds
destructiveAddProducingMixedSign [] ds@(_:_) = ds
destructiveAddProducingMixedSign [] [] = []
--}
destructiveAdd :: {-base::-}DInt -> HInteger__{-base-} -> HInteger__{-base-}
                                       -> HInteger__{-base-}
destructiveAdd base integer1 integer2 =
  case compareAbsInteger integer1 integer2 of
    GT -> destructive (+) base integer1 integer2
    LT -> destructive (+) base integer2 integer1
    EQ -> zeroInteger
--}
{-
destructiveAdd base integer1 integer2 =
  assert ("destructiveAdd: good arguments",integer1,integer2) (
     validBase base integer1 && validBase base integer2 &&
   (isZeroInteger integer1 || isZeroInteger integer2 ||
   compareNonzeroIntegerZero integer1 /= compareNonzeroIntegerZero integer2))
   P.$
  (\a -> assert ("destructiveAdd: good result",a,(integer1,integer2))
     (validBase base a) a) P.$
  makeSignConsistent base (destructiveAddProducingMixedSign integer1 integer2)
--}
{--
--uh oh, this separation is meaning that the whole result will be traversed,
--time-wasting. er, in fact, having to find the sign does risk that, for
--"round" numbers at least - but same for deciding syn. vs. destr., and
--adding/subtracting a little near there always risks O(n)
--integer must have no most-significant zeroes (so we can find its sign)
--and the most-significant digit (or being the null list)
--is the ONLY sure indicator of its sign.
makeSignConsistent :: {-base::-}DInt -> [DInt] -> HInteger__{-base-}
makeSignConsistent base integer = makeSign_ integer
  where
    --addBaseToFlipSign starts with a nonzero, and MAY produce zero
    --If we have a special case for integer=0, then the other branch
    --(not so recursive) would clearly be strict in this: ...
    (compareToDesiredSign, addBaseToFlipSign, carryReducingOne) =
         case compare (L.last integer) (dNat(0)) of
                GT -> (\x -> compare x (dNat(0)), \x -> x + base, \x -> pred x)
                LT -> (\x -> compare (dNat(0)) x, \x -> x - base, \x -> succ x)
    makeSign_ :: [DInt] -> [DInt]
    makeSign_ [] = []
    makeSign_ (d:ds) =
       case compareToDesiredSign d of
        GT{-already correct sign-} -> prependNonzero d (makeSign_ ds)
        LT{-wrong sign (but abs value IS within base)-} ->
          --    +Base in d's place == -1 in ds's place
              prepend
                   (addBaseToFlipSign d)  --    +Base in d's place...
                   (case ds of
                      --If the sign ds inconsistent, making the
                      -- sign consistent won't increase the magnitude
                      -- of the number, as it can only conflict with itself.
                      --So "ds" must be non-null here.
                      --                  --   ... == -1 in ds's place
                       (d_:ds_) -> makeSign_ (carryReducingOne d_ !: ds_)
                   )
        EQ{-zero-} -> prependZero (makeSign_ ds)
--}

-- ************************* MULTIPLICATION ***************************

-- Multiplication (and division...) need smaller bases of list
-- in order not to lose any precision on DInt-multiplies (and
-- not rely on any particular overflow behavior either).

-- never allows a most-significant zero in result
-- assuming the argument had none
largeToSmallBaseLEList :: LargeBaseLEList -> SmallBaseLEList
largeToSmallBaseLEList [] = []
largeToSmallBaseLEList (d:ds) =
  case quotRemByIntSmallBase d of
   (high,low) -> low !: prepend high (largeToSmallBaseLEList ds)
   --if the argument had no most-significant zero, then, processing
   --its most-significant digit, at least one of high and low will be
   --nonzero.

smallToLargeBaseLEList :: SmallBaseLEList -> LargeBaseLEList
smallToLargeBaseLEList [] = []
smallToLargeBaseLEList (low:high:ds) = (highLowFromIntSmallBase high low)
                                                   !: smallToLargeBaseLEList ds
smallToLargeBaseLEList (low:[]) = low !: []


-- ************* Naive O(m*n) multiplication. ***********
-- It is the simplest and most efficient when either factor is small.

--we need DInt multiply not to overflow (that wastes information)
--so we split the Ints up, turning it into a signed base sqrt(intLargeBase)


--no *0! also see other preconditions of naiveMultiplyIntegerSmall_
naiveMultiplyInteger_ :: HInteger__ -> HInteger__ -> HInteger__
naiveMultiplyInteger_ i1{-@(_:_)-} i2{-@(_:_)-} =
     smallToLargeBaseLEList
      (naiveMultiplyIntegerSmall_
       (largeToSmallBaseLEList i1)
       (largeToSmallBaseLEList i2))
--naiveMultiplyInteger_ _ _ = [] --multiplying by zero yields zero

type Overflow = [DInt]
--[Overflow] is a list in smallBase whose members are:
-- lists of all the dInts that have to be summed to represent the value at
-- that radix-place; each dInt may be as large as (smallBase - 1) ^ 2


mulBySmall :: DInt{-<smallBase-} -> SmallBaseLEList -> SmallBaseLEList
mulBySmall factor = f (dNat(0))
  where
    f carry [] = fromDInt carry
    f carry (d:ds) = case quotRemByIntSmallBase (factor * d + carry) of
        (high,low) -> low !: f high ds

-- this requires max list length around smallBase - much too small,
-- (2^32)^(2^17) = 2^4194304, or 10^100000,
--  shouldn't go wacko being squared on a 32bit machine.
--  However, this is significantly faster...
--  WAIT A MOMENT will the karatsuba reduce the huge ones instead?
--  at (2^32)^10
--  so unless we have (2^32)^5 * (2^32)^(2^17)...
--FALSE:  and it's no problem, since the arguments are biased, as long as
--  we put the short one first
--  and karatsuba has their lengths, it knows how to do that
--However, then it will be symmetrically biased (1 2 3 3 3 ... 3 3 2 1)
-- and each sublist will be just the length of the shorter factor, max.
-- Add in the top of a quotRem and it adds ONE to that length, no more.
--
{-they're not worth it HERE for division's time to be wasted?
 - depends on what is common in division - but I think `div` (<smallBase)
 - is specially optimized anyway
naiveMultiplyIntegerSmall_ (d1:[]) (d2:[]) =
  case quotRemByIntSmallBase (d1 * d2) of--just (d1*d2):[] to create LargeBase
    (high,low) -> low !: fromDInt high
naiveMultiplyIntegerSmall_ i1 (d2:[]) = mulBySmall d2 i1
naiveMultiplyIntegerSmall_ (d1:[]) i2 = mulBySmall d1 i2
-}
--precondition:
--  internally, always (sum overflow < largeBase)
--  As each overflow is < smallBase and largeBase/smallBase=smallBase,
--  this means in worst-case of each being smallBase-1, there can be
--  smallBase+1 of them (x-1)(x+1) = x^2-1 = largeBase-1 .
--  One (1) of the overflow can be carry from the previous.
--  Otherwise the maximum length from multiplyToOverflowing is
--   min m n + 1, where m and n are the factors' lengths. mulBySmall
--   can only overflow once, as (x-1)(x-1) + (x-1) = (x)(x-1) < x^2 .
--  So
--   (min m n + 1) + 1 <= smallBase + 1
--   min m n + 1 <= smallBase
--   min m n < smallBase,
--  as a conservative precondition.
-- Actually... it's slightly more generous than that. The
--   overflow-lengths always trail off at the end like ..3 2 1 [end]
--   (at least, the last list is at least one)
--  and this way our carry is at max, (smallBase-1)*2 (+1?)
--  so in the middle of overflows
--  we have (x-1)(2(x+1)) = 2(x^2-1) = 2x^2-2 < 2x^2
--   2(x+1) = (min m n + 1) + 2
--   min m n + 1 + 2 <= 2*(smallBase+1)
--   min m n + 1 + 2 <= 2*smallBase + 2
--   min m n + 1 <= 2*smallBase
--   min m n < 2*smallBase
--  . Judging potential factors in largeBase, m and n are half as much as in
--  smallBase:
--   min (2M) (2N) < 2*smallBase
--   2*(min M N) < 2*smallBase
--   min M N < smallBase
--  But min M N <= smallBase, tested even with smallBase=4 and
--   error-on-overflow, works fine as a condition. Also with smallBase=8.
--   Maybe it has something to do with the fact that in any base b,
--   (b-1)(b-1) = (b^1)(b-2) + (b^0)(something) = b^2 - 2b + something
--   (b-1)(b-1) = b^2 - 2b + 1   so something = 1
--   This is in mulBySmall.
--   (now examples in smallBase=10)
--   9 * 999
--   1 carry 8 (9*9 = 81)
--   9 carry 8 (9*9 + 8 = 89)
--   9 carry 8 ...
--  hmm.
--  Here is a sample of 'high's with smallBase=4 in effect:
-- (from ./CheckIntegerInTermsOfInt 50 2>&1 |grep '^-\?[0-9]\+$'|sort|uniq -c
-- with (traces high) inserted)
--  count  'high'-value
-- ------ + ---------
-- 1210402 0
--  555992 1
--  166442 -1
--  192299 2
--   62072 -2
--   50194 3
--   19838 -3
--   10141 4
--    4664 -4
--    1621 5
--    1149 -5
--     344 6
--     257 -6
--      73 7
--      43 -7
--I think negatives are less common because the reciprocal algorithm uses
--  numbers that will always multiply to a positive, in some part of it.
--  Generally the frequency seems to be exponentially unlikely in the value,
--  limited of course by the maximum.
-- Here is a sample trace with smallBase=4 from ghci (formatted for clarity)
--   > naiveMultiplyIntegerSmall_ [3,3,3,3] [3,3,3,3]
--   "overflows"
--   (("sees","overflows"),[[1],[3,1],[3,3,1],[3,3,3,1],[2,3,3,3],[2,3,3]
--                          ,[2,3],[2]])
--   [1,0,0,0,2,3,3,3]
--Although that is not automatically the most dastardly layout of large
--digits,
--   > naiveMultiplyIntegerSmall_ [3,3] [3,3]
--   "overflows"
--   (("sees","overflows"),[[1],[3,1],[2,3],[2]])
--   [1,0,2,3]
--   > naiveMultiplyIntegerSmall_ [3,3,3] [3,3,3]
--   "overflows"
--   (("sees","overflows"),[[1],[3,1],[3,3,1],[2,3,3],[2,3],[2]])
--   [1,0,0,2,3,3]
--I seem to have overestimated by one the max number of, and the symmetry of
--overflows?
--   > naiveMultiplyIntegerSmall_ [3,3] [3,3,3]
--   "overflows"
--   (("sees","overflows"),[[1],[3,1],[3,3],[2,3],[2]])
--   [1,0,3,2,3]
--   > naiveMultiplyIntegerSmall_ [3,3,3] [3,3]
--   "overflows"
--   (("sees","overflows"),[[1],[3,1],[2,3,1],[2,3],[2]])
--   [1,0,3,2,3]
--Each pack sums to the same thing... in those examples anyway. Of course I
--know from quickcheck that the _answer_ is always right...
--   > naiveMultiplyIntegerSmall_ [3,3,3,3] [3]
--   "overflows"
--   (("sees","overflows"),[[1],[2,1],[2,1],[2,1],[2]])
--   [1,3,3,3,2]
--   > naiveMultiplyIntegerSmall_ [3] [3,3,3,3]
--   "overflows"
--   (("sees","overflows"),[[1],[3],[3],[3],[2]])
--   [1,3,3,3,2]
--When one argument is much bigger than the other, a relevant maximum
-- is 1+(min m n).  Apparently when both arguments are pushing the limits,
-- the maximum is just (min m n). I'm sure that can be justified somehow.



naiveMultiplyIntegerSmall_ i1 i2 =
  collapseMassiveOverflow ({-sees "overflows" P.$-} multiplyToOverflowing i1)
  where
    --as long as neither argument is zero, there should be no empty lists
    --in this [Overflow] structure
    multiplyToOverflowing :: [DInt]{-integer1-} -> [Overflow]
    multiplyToOverflowing (d1:ds1) = zipCons (mulBySmall d1 i2)
     (case multiplyToOverflowing ds1 of [] -> []; os@(_:_) -> [] {-!!!:-}: os)
    multiplyToOverflowing [] = []
    collapseMassiveOverflow :: [Overflow] -> [DInt]
    collapseMassiveOverflow x =
      case x of
       [] -> []
--never trigger:
--       []:[] -> []
--       []:overflows -> (dNat(0)) : collapseMassiveOverflow overflows
       (overflow@(_:_)) : overflows ->
          case quotRemByIntSmallBase (L.foldr1 (+) overflow) of
            (high, low) -> {-traces high P.$-} low !:
              if isZero high then collapseMassiveOverflow overflows
              else case overflows of
                   (o:os) -> collapseMassiveOverflow ((high:o):os)
                   [] -> assert
                       ("naive multiplication trails off nicely at the end"
                       ,overflow,high) (abs high < intSmallBase)
                     (high !: [])
--collapseMassiveOverflow (if isZero high then overflows else
--              case overflows of (o:os) -> (high:o):os; [] -> (high:[]):[])
--}

--precondition: neither argument is zero. Most-significant zeroes in arguments
-- will only mean that the result may have most-significant zeroes and the
-- computation may be significantly slower
naiveMultiplyIntegerSmall_
   :: SmallBaseLEList -> SmallBaseLEList -> SmallBaseLEList
{--optimizations? :
naiveMultiplyIntegerSmall_ (d1:[]) (d2:[]) =
  case quotRemByIntSmallBase (d1 * d2) of--just (d1*d2):[] to create LargeBase
    (high,low) -> low !: fromDInt high
naiveMultiplyIntegerSmall_ i1 (d2:[]) = mulBySmall d2 i1
naiveMultiplyIntegerSmall_ (d1:[]) i2 = mulBySmall d1 i2
naiveMultiplyIntegerSmall_ integer1 integer2 =
  collapseMassiveOverflow (multiplyToOverflowing integer1)
  where
    --as long as neither argument is zero, there should be no empty lists
    --in this [Overflow] structure... though since some digits may be zero,
    --some of the dInts in the Overflows may be zero
    multiplyToOverflowing :: [DInt]{-integer1-} -> [Overflow]
    multiplyToOverflowing (d1:ds1) = zipCons (scaleInteger2 d1)
                                       ([] : multiplyToOverflowing ds1)
    multiplyToOverflowing [] = []
    scale d i = L.map (d *) i
    scaleInteger2 d = scale d integer2
    --produces a well-behaved smallBase HInteger!
    collapseMassiveOverflow :: [Overflow] -> [DInt]
    collapseMassiveOverflow x =
      case x of
       [] -> []
--never trigger:
--       []:[] -> []
--       []:overflows -> (dNat(0)) !: collapseMassiveOverflow overflows
       (overflow@(_:_)) : overflows ->
          case determineDigitAndOverflow overflow of
            (high, low) -> low !: collapseMassiveOverflow
                                      (zipIntoHead high overflows)
--}

--zipCons is something like zipWith (:), but has the proper behavior when
--either list argument ends (don't lose or add any elements).
zipCons :: [a] -> [[a]] -> [[a]]
zipCons [] xss2 = xss2
zipCons (x1:xs1) (xs2:xss2) = (x1:xs2) : zipCons xs1 xss2
zipCons xs1@(_:_) [] = L.map (:[]) xs1
{-zipIntoHead :: [a] -> [[a]] -> [[a]]
zipIntoHead [] xss2 = xss2
zipIntoHead xs1@(_:_) [] = xs1 : []
zipIntoHead xs1@(_:_) (xs2:xss2) = (xs1 L.++ xs2) : xss2


--type DiffList x = [x] -> [x]

--needs commenting/description:
determineDigitAndOverflow :: Overflow -> {-[DInt]
             ->-} {-DInt{-smallBase-} ->-} ({-DiffList-} Overflow, DInt)
  -- maxSe14 = 2^14 - 1   --(all symmetric for negatives)
  -- maxProduct = maxSe14 * maxSe14
  -- maxInt = 2^29 - 1  --potentially
  --(maxProduct * 2 < maxInt), but (maxProduct * 3 > maxInt),
  --so we can only add two products at a time
{-determineDigitAndOverflow (d:ds){- highs-} low =
    case quotRemByIntSmallBase (low + d) of
     (high,low_) ->
      if isZero high
       then determineDigitAndOverflow ds {-highs-} low_
       else determineDigitAndOverflow ds {-(high:highs) -}low_
determineDigitAndOverflow [] highs low = (highs, low)-}
determineDigitAndOverflow = f (dNat(0)) []--(\x->x)
  where
    f dCurrent furtherOverflows [] = (furtherOverflows, dCurrent)
    f dCurrent furtherOverflows (d:ds) =
        case quotRemByIntSmallBase (dCurrent + d) of
          (high,low) -> f low furtherOverflows_ ds
            where
              furtherOverflows_ = if isZero high --wouldn't be a problem
                then furtherOverflows --not to weed out zeroes...
                else high !: furtherOverflows
-- \x -> furtherOverflows (high:x) --order is immaterial...
--}
-- *** Karatsuba O(n^1.585)(for m approx.= n) multiplication ******

leadingZeroes :: PInt -> [DInt] -> [DInt]
--leadingZeroes n nil = if 0 == n then nil
--      else leadingZeroes n (zero !: nil) --zero !: leadingZeroes n nil
leadingZeroes howManyToAdd nil = f howManyToAdd
  where
   f n | (pNat(0)) == n = nil
   f n = (dNat(0)) !: f (pred n)

synergisticAddLeadingZeroes :: LargeBaseLEList -> LargeBaseLEList
         -> PInt -> LargeBaseLEList
synergisticAddLeadingZeroes i1_ i2 zeroes_ = f i1_ zeroes_
  where
    f i1 zeroes | (pNat(0)) == zeroes = synergisticAdd i1 i2
    f (d1:ds1) zeroes = d1 !: f ds1 (pred zeroes)
    f [] zeroes = leadingZeroes zeroes i2

--avoiding fromIntegral... it involves the NInteger type, which
--might be us.
minSmallPDInt :: PInt -> DInt -> PInt
--minSmallPDInt p d = min p (fromIntegral d)
minSmallPDInt p d = f (pNat(0)) (dNat(0))
  where
   f pThreshold dThreshold =
    if p == pThreshold || d == dThreshold
     then pThreshold
     else f (succ pThreshold) (succ dThreshold)

-- http://en.wikipedia.org/wiki/Karatsuba_algorithm
-- hmm, should we wait to convert to small base when we can
-- (or even convert to large base when starting with small)
-- so the recursion is less wasteful?
--
-- <= this value = switch to naive / long multiplication
-- Hmm, although this karatsuba isn't incorrect and isn't slowing
-- it down a whole lot, it's not useful for "reasonable size" numbers.
-- I think division needs to be scrutinized for why it's slow.
-- Even with the estimate of ten digits as when karatsuba is worth it,
-- this still makes multiplication less than O(n^2), which is good.
karatsubaLargeBaseThreshold, karatsubaSmallBaseThreshold :: PInt
karatsubaLargeBaseThreshold =
  minSmallPDInt (pNat(10)) intSmallBase --INTERESTING?
     -- the min with intSmallBase is necessary,
     -- see analysis before naiveMultiplyIntegerSmall_

karatsubaSmallBaseThreshold = pTwice karatsubaLargeBaseThreshold

--no *0!
karatsubaMultiplyInteger_ :: HInteger__ -> PInt
                          -> HInteger__ -> PInt
                          -> HInteger__
karatsubaMultiplyInteger_ i1 len1 i2 len2 =
    let minLen = min len1 len2 in
    if minLen <= karatsubaLargeBaseThreshold
    then {-if (pNat(1)) == minLen then --shortNaiveMultiplyInteger i1 i2 else
--precondition: min (length i1) (length i2) == 1
--shortNaiveMultiplyInteger :: HInteger__ -> HInteger__ -> HInteger__
--shortNaiveMultiplyInteger i1@(d1:ds1) i2@(d2:ds2) = f ds1 ds2
 let
  (d1:ds1) = i1
  (d2:ds2) = i2
  i1small = (pNat(1)) == len1 && abs d1 < intSmallBase
  i2small = (pNat(1)) == len2 && abs d2 < intSmallBase
 in if i1small then if i2small then (d1 * d2) !: []
      else smallToLargeBaseLEList (mulBySmall d1 (largeToSmallBaseLEList i2))
    else if i2small
      then smallToLargeBaseLEList (mulBySmall d2 (largeToSmallBaseLEList i1))
    else smallToLargeBaseLEList
           (naiveMultiplyIntegerSmall_
            (largeToSmallBaseLEList i1)
            (largeToSmallBaseLEList i2))
 else-}
        {-if 0 == minLen then [] else-} naiveMultiplyInteger_ i1 i2
    else let
      b = pHalfRoundingDown (max len1 len2) --should we round up or down?
      len1high = len1 - b
      len2high = len2 - b
      --hmm, should we chop any we find now? We're not particularly
      --likely to find any... we assume that i1 and i2 have no
      --most-significant zeroes
      --
      --distance to split at named n (the list MUST BE at least this long).
      --returns named ((length low, low), high).
      --initially (non-recursive) must be called with n=b.
      --Example:
      --splitAtDroppingMostSignificantZeroes 6
      --       -- 1 2 3 4 5 6 7 8 9
      --         [0,2,0,4,0,0,0,8,0]
      --   = ((4,[0,2,0,4]), [0,8,0])
      -- or return (number of) zeroesDropped instead of len?
      splitAtDroppingMostSignificantZeroes :: PInt -> [DInt]
                -> ((PInt, [DInt]), [DInt])
      splitAtDroppingMostSignificantZeroes n high
                                | (pNat(0)) == n = ((b,[]),high)
--      splitAtDroppingMostSignificantZeroes _ [] = MAY NOT happen
      splitAtDroppingMostSignificantZeroes n (d:ds) =
      --this is weird and complicated, can't we just do L.length
      --and be about as efficient?
            case splitAtDroppingMostSignificantZeroes (pred n) ds of
             (lowInfo, high) ->
              ( case lowInfo of
                 (maxLen, low_) ->
                  let
                   low = prepend d low_
                   len = case low of [] -> b - n; (_:_) -> maxLen
                  in (len, low)
              , high)
--         (if isZero d && L.null ds_ then (b - n,[]) else (len, d!:ds_), rest)
--        where
--         ((len,ds_),rest) = splitAtDroppingMostSignificantZeroes (pred n) ds
      ((len1low, i1low), i1high) = splitAtDroppingMostSignificantZeroes b i1
      ((len2low, i2low), i2high) = splitAtDroppingMostSignificantZeroes b i2
      synergisticPlus = synergisticAdd
      destructivePlus = destructiveAdd intLargeBase
      neg = negateInteger
     in
      -- for the big * small cases ( <= rather than < is not
      -- necessary, just convenient - it does eliminate some zeroes later) :
      if len1high <= (pNat(0)) then let
         y = karatsubaMultiplyInteger_ i1 len1 i2low len2low
         z = karatsubaMultiplyInteger_ i1 len1 i2high len2high
        in synergisticAddLeadingZeroes y z b
      else if len2high <= (pNat(0)) then let
         y = karatsubaMultiplyInteger_ i1low len1low i2 len2
         z = karatsubaMultiplyInteger_ i1high len1high i2 len2
        in synergisticAddLeadingZeroes y z b
      else let
 -- in case one (both?) of them happened to have lots of low digits be zero
 -- nah, we'll just let that handle itself
      --everywhere, we're either adding parts of i1, parts of i2,
      --or parts of the answer, so the signs are always the same
      --(except when they're always opposite because we're subtracting.
      --Also zp is always >= magnitude than x and than y.
         x = karatsubaMultiplyInteger_ i1high len1high i2high len2high
         y = karatsubaMultiplyInteger_ i1low len1low i2low len2low
         z1 = i1low `synergisticPlus` i1high
         lenZ1 = L.length z1
         z2 = i2low `synergisticPlus` i2high
         lenZ2 = L.length z2
         zp = karatsubaMultiplyInteger_ z1 lenZ1 z2 lenZ2
         -- or (zp `destructivePlus` (neg (x `synergisticPlus` y))
         z = (zp `destructivePlus` (neg x)) `destructivePlus` (neg y)
        in synergisticAddLeadingZeroes y
              (synergisticAddLeadingZeroes z x b) b
--associating the other way than this will be nicer:
--         ( (synergisticAddLeadingZeroes y z b)
--                          `synergisticAddLeadingZeroes` x) (pTwice b)

{-
karatsubaMultiplyIntegerSmall :: SmallBaseLEList
                              -> SmallBaseLEList
                              -> SmallBaseLEList
karatsubaMultiplyIntegerSmall i1 i2 =
  let
   len1 = pHalfRoundingUp len1Small --rounding up, as necessary
   len2 = pHalfRoundingUp len2Small --rounding up, as necessary
   len1Small = L.length i1
   len2Small = L.length i2
   minLenSmall = min len1Small len2Small
         --roughly twice what it would be in LargeBase
  in if minLenSmall <= karatsubaSmallBaseThreshold
    then if (pNat(0)) == minLenSmall then []
                             else naiveMultiplyIntegerSmall_ i1 i2
    else largeToSmallBaseLEList
      (karatsubaMultiplyInteger_ (smallToLargeBaseLEList i1) len1
                                (smallToLargeBaseLEList i2) len2)
-}

--this version doesn't check for multiplying by zero, which
--mayn't be done with it!
karatsubaMultiplyIntegerSmall_ :: SmallBaseLEList
                              -> SmallBaseLEList
                              -> SmallBaseLEList
karatsubaMultiplyIntegerSmall_ i1 i2 =
  let
   len1 = pHalfRoundingUp len1Small --rounding up, as necessary
   len2 = pHalfRoundingUp len2Small --rounding up, as necessary
   len1Small = L.length i1
   len2Small = L.length i2
   minLenSmall = min len1Small len2Small
         --roughly twice what it would be in LargeBase
  in if minLenSmall <= karatsubaSmallBaseThreshold
    then naiveMultiplyIntegerSmall_ i1 i2
    else largeToSmallBaseLEList (
            karatsubaMultiplyInteger_
                   (smallToLargeBaseLEList i1) len1
                   (smallToLargeBaseLEList i2) len2
          )



-- **** multiplications that should "normally" be used ****

multiplyInteger :: HInteger__ -> HInteger__ -> HInteger__
--hmm, optimizations. They seem to help slightly with my test set,
--which is to say, quite a lot more when the numbers are actually
--commonly small.
multiplyInteger (d1:[]) (d2:[]) =
  let
    (d1high,d1low) = quotRemByIntSmallBase d1
    (d2high,d2low) = quotRemByIntSmallBase d2
  in if isZero d1high && isZero d2high then (d1*d2) !: [] else
     smallToLargeBaseLEList (
             naiveMultiplyIntegerSmall_
                              (d1low !: fromDInt d1high)
                              (d2low !: fromDInt d2high)
                            )
--checking for *0 is now required!
multiplyInteger [] _ = []
multiplyInteger _ [] = []
-- when it's nonzero and at least one argument is somewhat long:
multiplyInteger i1@(_:_) i2@(_:_) = karatsubaMultiplyInteger_
                                      i1 (L.length i1)
                                      i2 (L.length i2)

--neither argument may be zero, and it uses SmallBase.
--Just used internally in places where the arguments are not likely to be
--as small as one digit in length.
multiplyIntegerSmall_ :: SmallBaseLEList -> SmallBaseLEList -> SmallBaseLEList
multiplyIntegerSmall_ = karatsubaMultiplyIntegerSmall_


--    12
--    94
--   ---
--     8
--   18
--    4
--   9
-- -----
--  1128

--    99
--    99
--   ---
--    81
--   81
--   81
--  81
--  ----


-- **************************** DIVISION *************************

-- extendToN 3 [1]   [4,5,6,7,8]
--           = [1,0,0,4,5,6,7,8]
-- Also ensures no-most-significant zeroes as long as neither
-- argument list had any.
-- length of the first list must not exceed targetLen.
extendToN, extendToN_ :: PInt -> [DInt] -> [DInt] -> [DInt]
extendToN targetLen l [] =
 assert ("digits aren't overflowing",targetLen,l) (L.length l <= targetLen)
    (l)
extendToN targetLen l nil =
 assert ("digits aren't overflowing",targetLen,l,nil) (L.length l<=targetLen)
    (extendToN_ targetLen l nil)
extendToN_ targetLen l nil =
 assert ("digits_aren't_overflowing",targetLen,l,nil) (L.length l<=targetLen)
    (f targetLen l)
  where
   f n _ | (pNat(0)) == n     = nil
   f n []                     = leadingZeroes n nil
   f n (x:xs) = x !: f (pred n) xs

-- Wikipedia is not great at explaining long division, especially where the
-- denominator has >1 digit... http://en.wikipedia.org/wiki/Long_division
-- Long division essentially allows to reduce the size of the numerator.
-- Not the denominator.  We choose not to do long division with >1digit
-- denominator because it's inefficient.

--Either we break up the list beforehand and provide more reciprocal digits,
--because we are not always dividing as soon as possible,
--or we wait, and apply divide n times rather than n/d times (and depending
-- on the remainders, we may actually have to! so let's do the other).
--does not strip most-significant zeroes from quotient
{-longDivide :: (SmallBaseLEList{-long numerator digit-}
                 -> (SmallBaseLEList{-quot-}, SmallBaseLEList{-rem-}))
           -> [SmallBaseLEList{-long numerator digits-}]
           -> (SmallBaseLEList{-quotient-},
               SmallBaseLEList{-remainder <= maximum rem-}
longDivide divide [] = ([],[]) -- zero divided by anything
longDivide divide (n:ns)
  = let
     (q,r) = divide n
     longDivide
    in
--may be used in base 2^(14*8) :))
--concat quotient may be used if quots all have the appropriate
--number of most-significant zeroes...
longDivideBySingleDigit :: (digit{-long numerator digit or two-}
                 -> (digit{-quot-}, digit{-rem-}))
           -> [digit{-long numerator digits-}]
           -> ([digit]{-quotient-}, digit{-remainder <= maximum rem-})
longDivideBySingleDigit divide [] = ([],[]) -- zero divided by anything
longDivideBySingleDigit divide (n:ns)
  = let
     (q,r) = divide n
     (qs,) = longDivide divide ns r
    in qs ++ [q]
    in-}

--first what must be done is dropping low-order numerator
--digits to increase the? seems unlikely...
--assumes not dividing by zero
--May produce most-significant zeroes, currently.
longDivideBySingleDigit ::
   {-  (digit -> Bool) -> -- ^ isZero, not strictly necessary
         --but allows us to easily eliminate most-significant zeroes-}
  --these quotRem functions already know the denominator somehow:
   (digit{-only low (high=0)-} -> (digit{-quot-},digit{-rem-}))
  -> (digit{-high-} -> digit{-low-} -> (digit{-quot-},digit{-rem-}))
  -> [digit] -> ([digit], digit)
longDivideBySingleDigit {-isZero1-} quotRemBy1 quotRemBy2 = f
  where
    f (d:[]) = case quotRemBy1 d of (q,r) -> (q{-!!!:-}:[], r) --hmm
    f (d:ds) =
      case f ds of
        (qs, prevRem) ->
           case quotRemBy2 prevRem d of
             (q,r) -> (q{-!!!:-}:qs, r)
-- digit has to be (at least?) as big as the size of the denominator.
-- the size of the remainders is limited by the size of the denominator.

--assumes not dividing by zero
--if the list-argument has no most-significant zeroes and doesn't contain
--digits of opposite signs, the result contains no most-significant zeroes.
longDivideBySmall :: DInt{-<intSmallBase-} -> SmallBaseLEList
                  -> (SmallBaseLEList{-quot-},DInt{-rem-})
longDivideBySmall denom = f
  where
    f [] = ( [], (dNat(0)) )
    f (d:[]) = case d `quotRem` denom of
              (q,r) -> ( fromDInt q , r )
              --this check is sufficient because/when d is nonzero
              --and denom is smaller than the base, so then the
              --remainder here is nonzero and survives in the
              --previous digit (if any)
    f (d:ds) =
      case f ds of
        (qs, prevRem) ->
           case (highLowFromIntSmallBase prevRem d) `quotRem` denom of
             (q,r) -> (q:qs, r)

--only for large base, and doesn't check for the error of dividing by zero.
quotRemInteger :: HInteger__ -> HInteger__{-nonzero-}
             -> (HInteger__, HInteger__)
-- must come first: even zero divided by zero is erroneous
-- _ `quotRemInteger` [] = assert "quotRemInteger used to divide by zero"
--                                     False  (zeroInteger, zeroInteger)
-- other than that, zero divided by anything is trivial
[] `quotRemInteger` _ = (zeroInteger, zeroInteger)
-- it should go much faster on not-too-big size things
-- to use DInt's native quotRem without further ado
(num:[]) `quotRemInteger` (denom:[]) =
  case num `quotRem` denom of
   (q,r) -> ( fromDInt q, fromDInt r )
-- divisions by 1 and -1 must not be passed along
num `quotRemInteger` (denom:[])
  | denom == (dNat(1)) = (              num, zeroInteger)
  | denom == (dNeg(1)) = (negateInteger num, zeroInteger)
 -- | abs denom == (dNat(1)) = (L.map (denom *) num, zeroInteger)
--quotRemSmallBase will optimize the abs denominator>abs numerator case (hmm)
--and the small-denominator, large numerator case particularly too.
num `quotRemInteger` denom =
  case largeToSmallBaseLEList num
            `quotRemSmallBase` largeToSmallBaseLEList denom of
   (q,r) -> (smallToLargeBaseLEList q, smallToLargeBaseLEList r)

--assumes not dividing by zero (or by 1 or by -1?)
--should I make special cases for 0 1 -1 outside and
--remove the checks from reciprocalToPrecision? yep
quotRemSmallBase, quotRemSmallSectioned--, quotRemSmallGulp
    :: SmallBaseLEList -> SmallBaseLEList
             -> (SmallBaseLEList, SmallBaseLEList)
-- 'num' is short for 'numerator' here, also denom for denominator
num `quotRemSmallBase` (denom:[])
-- | abs denom < intSmallBase --of course it is, since this is _in_ SmallBase!
--optimization for small denominator
   = case longDivideBySmall denom num of
       (q,r) ->  ( q , fromDInt r )
num `quotRemSmallBase` denom =
 --don't do anything stupid in this trivial case
 --(we could be more precise and check their magnitudes...)
 -- if lenDenom > lenNum then (zeroInteger, num)
  case compareAbsInteger num denom of
    LT -> (zeroInteger, num)
    --EQ -> (+-oneInteger, zeroInteger)
    _ -> num `quotRemSmallSectioned`{-Gulp`-} denom

dropMostSignificantZeroes :: [DInt] -> [DInt]
dropMostSignificantZeroes [] = []
dropMostSignificantZeroes (d:ds) = prepend d (dropMostSignificantZeroes ds)

--some version of the sectioned code:
-- ./CheckIntegerInTermsOfInt  123.34s user 0.26s system 98% cpu 2:05.19 total
-- ./CheckIntegerInTermsOfInt  123.44s user 0.46s system 98% cpu 2:05.60 total
num `quotRemSmallSectioned` denom =
  quotRemSmallSectioned_ denom (minimalReciprocal denom) num

minimalReciprocal :: SmallBaseLEList -> (SmallBaseLEList,PInt)
minimalReciprocal denom =
    reciprocalToPrecision (succ (pTwice (L.length denom))) denom

quotRemSmallSectioned_ ::
           SmallBaseLEList -> (SmallBaseLEList,PInt)
             -> SmallBaseLEList
             -> (SmallBaseLEList, SmallBaseLEList)
quotRemSmallSectioned_ denom (recipDigits,recipExp) =
--  assert ("quotRemSmallBase not dividing by zero",denom)
--         (not (L.null denom)) P.$
--  assert ("denominator has no most-significant zeroes",denom)
--         (L.null denom || (dNat(0)) P./= (L.last denom)) P.$
--  assert ("numerator has no most-significant zeroes",num)
--         (L.null num || (dNat(0)) P./= (L.last num)) P.$
  let
   lenDenom = L.length denom
   --they're not necessarily all the same length anyway because of the
   --most-significant digits:
   split [] = []
   split l = let (digit, rest) = L.splitAt lenDenom l
             in dropMostSignificantZeroes digit {-!!!:-}: split rest
   join [] = []
   join (x:xs) = extendToN lenDenom x (join xs)
   negDenom = negateInteger denom
   quotRemBy1 moderateSizeNum =
    -- traces ("qrb1",moderateSizeNum) P.$
--   assert ("expected length digits",moderateSizeNum,lenDenom)
--          (L.length moderateSizeNum <= pTwice lenDenom{-quotRemBy2-}) P.$
--   assert ("moderateSizeNumerator has no most-significant zeroes",
--     moderateSizeNum)
--     (L.null moderateSizeNum || (dNat(0)) P./= (L.last moderateSizeNum)) P.$
     case moderateSizeNum of
      [] ->  ([],[]) -- zero digit divided by something = 0, remainder 0
      (_:_) -> --now we can use nonzero multiplication
       let
         quotient = L.drop recipExp
                       (moderateSizeNum `multiplyIntegerSmall_` recipDigits)
         remainder = case quotient of
          [] -> moderateSizeNum -- quotient=0 ==> remainder= the whole thing.
          -- Remainder = moderateSizeNumerator - (quotient * denominator) :
          -- The remainder is certainly no greater than the numerator here,
          -- so it's safe to consider it a destructive add/subtraction
          (_:_) -> destructiveAdd intSmallBase
                       (moderateSizeNum)
                       (quotient `multiplyIntegerSmall_` negDenom)
       in
       -- traces ("qrb1'",(recipExp,recipDigits,negDenom),
       --                          (moderateSizeNum,quotient)) P.$
--       assert ("okay length quotient",quotient,lenDenom)
--                               (L.length quotient <= lenDenom) P.$
--       assert ("len remainder",remainder,lenDenom)
--                      (L.length remainder <= lenDenom) P.$
--       assert ("quotient has no most-significant zeroes"
--                    ,quotient,(remainder,moderateSizeNum),(num,denom))
--         (L.null quotient || (dNat(0)) P./= (L.last quotient)) P.$
--       assert ("remainder has no most-significant zeroes"
--                    ,(remainder,moderateSizeNum),quotient,(num,denom))
--         (L.null remainder || (dNat(0)) P./= (L.last remainder)) P.$
       --remainder sign may vary with sub-parts- hmm.... is that true?
       --The digit-parts of the split numerator should all be the same sign
       --(or zero).
--       assert ("remainder size",remainder,denom)
--       (L.length remainder < lenDenom || (L.length remainder == lenDenom &&
--        L.map abs (L.reverse remainder) < L.map abs (L.reverse denom))) P.$
       (quotient, remainder)
   quotRemBy2 highNum{-a remainder-} lowNum =
    -- traces ("qrb2",highNum,lowNum) P.$
--     assert ("lowNum fits",lowNum) (L.length lowNum <= lenDenom) P.$
--             if L.null highNum then quotRemBy1 lowNum else
              quotRemBy1 (extendToN lenDenom lowNum highNum)
  in \num ->
   case longDivideBySingleDigit quotRemBy1 quotRemBy2 (split num) of
    (quotDigits,remDigit) -> (join quotDigits, remDigit)

{--
-- ./CheckIntegerInTermsOfInt  178.31s user 0.32s system 99% cpu 2:58.87 total
num `quotRemSmallGulp` denom = let
   (recipDigits,recipExp) =
           reciprocalToPrecision (succ (L.length num + L.length denom)) denom
   quotient = L.drop recipExp (num `multiplyIntegerSmall_` recipDigits)
   remainder = {-case quotient of
          [] -> num -- quotient=0 ==> remainder= the whole thing.
            --(we could check for too-big denominator (precisely) earlier...)
          -- Remainder = moderateSizeNumerator - (quotient * denominator) :
          -- The remainder is certainly no greater than the numerator here,
          -- so it's safe to consider it a destructive add/subtraction
          (_:_) ->-} destructiveAdd intSmallBase
                       (num)
                       (quotient `multiplyIntegerSmall_` negateInteger denom)
   in (quotient,remainder)
--}



{-
--The first argument, 2 . 0 0 0 0 , is implicit because we can deduce it
--from knowing that the second argument is between 0.5 and 1.5 (roughly),
--as demonstrated by the second argument's patterns (and this means
--we don't have to figure out how to find the value of the 2, also!).
--Otherwise the implementation is very similar to destructive_'s
--implementation, whose cases are referenced GT/EQ/LT.
   --base cases
sub       (2:[])   ( 1 : []) = 1 !: []
sub       (0:2:[]) (d>1: []) = base - d !: (1 !: [])
--GT[destructive/sub] is impossible considering all the zeroes on the left
sub       (0:x)    ( 0 : ds) = 0 !: minus (x) ds --EQ
sub       (0:x)    ( d : ds) = base - d !: borrow (x) ds --LT

   --base cases
subBorrow (2:[])   ( 1 : []) = 1        - 1 !: []
subBorrow (0:2:[]) (d>1: []) = base - d - 1 !: (1 !: [])
--GT and EQ are impossible considering zeroes borrowed from, on the left.
subBorrow (0:x)    ( d : ds) = base - d - 1 !: borrow (x) ds --LT

        --as soon as borrowing/carrying begins, it must continue
        --until we reach the point where '2' provides anything nonzero
      -- 2 - 1.00001  = 0.?????   ; 2 - 0.99999 = 1.??????
      -- This requires that intSmallBase be sufficiently big.
      -- 1.9 and 0.1 are just NOT ALLOWED to happen
      -- (they would cause trouble, and are quite poor estimates)
-}
twoMinusSomethingNearOne :: {-base::-}DInt -> [DInt] -> [DInt]
twoMinusSomethingNearOne base = sub_2
 where
  sub_2, subBorrow_2 :: SmallBaseLEList -> SmallBaseLEList
  sub_2 (d:[]) = if (dNat(1)) == d then oneInteger else
     (base - d) !: oneInteger
  sub_2 (d:ds) = if isZero d then (dNat(0)) !: sub_2 ds else
     (base - d) !: subBorrow_2 ds
  subBorrow_2 (d:[]) = if (dNat(1)) == d then zeroInteger else
     pred (base - d) !: oneInteger
  subBorrow_2 (d:ds) =
     pred (base - d) !: subBorrow_2 ds

type FloatingInteger = (SmallBaseLEList, PInt)
-- (i, e); i /= 0; e >= 0 (is e always >= 0 ??)
--  represents the value
--   (i * (smallBase ^ (negate{-??-} e)))
-- Sometimes they are normalized by turning least-significant
-- zeroes into larger exponents.

--input: not zero, not one, not negative one.
--output: no most-significant zeroes, possibly has least-significant zeroes.
--  exactness doesn't matter, we just need a sufficiently good
--  estimate (for some particular definition of "sufficiently good")
reciprocalEstimate :: SmallBaseLEList -> FloatingInteger
reciprocalEstimate = rE (pNat(0))
 where
  toBase d = largeToSmallBaseLEList (d !: [])
  largeOver n e = ( toBase (intLargeBase `quot` n)
                  , (pNat(2)){-for largeBase factor-} + e
                  )
   --very small ( < smallBase ) numbers get simple estimates
   --(actually this function is never used on them
   --since there's a faster division method for small denominator,
   --so this could be commented out :)
  rE e ( d : [] ) = largeOver d e
   --the rest get counted down
  rE e ( _ :(ds@(_:(_:_)))) = rE (succ e) ds
   --and the result with most precision chosen
  rE e (low: high : []    ) =
      if high * high >= intSmallBase --high > sqrt intSmallBase
        -- at this point n references smallBase*high+low, so just 'high'
        -- is a factor of intSmallBase less than that,
        then largeOver high (succ{-for failing to *smallBase-} e)
        else largeOver (highLowFromIntSmallBase high low) e

--an exact function?
--naive
--recip This Much Precision
--(places after radix point, or after the first nonzero digit?)

--This may be the only place that the precision of DInt (rather than
--available memory) limits the length of an Integer!!! Also, using
--Prelude.drop/take requires Prelude.DInt
--I guess the required precision only refers to accuracy, not having
--a bunch of unneeded trailing zero digits in the answer if the answer
--turns out round like that. likewise, some extra precision in result
--(een if it's wrong) is not forbidden?   expected rounding = give more
--precision? that may not be enough (see analysis below) -- it is,
--round up
--DO NOT PASS 0, 1 or -1 to take the reciprocal of!
reciprocalToPrecision :: PInt{-smallBase-digits required after radix point
           (should be one more than you might think, as we'll round up...?)-}
        -> SmallBaseLEList{-to take the reciprocal of-}
        -> FloatingInteger
--internally we need to use... we'll try some things?
--Newton-Raphson method
--http://en.wikipedia.org/wiki/Newton%27s_method
--specific to getting more and more precise reciprocal digits
--(solving, for a known x, r = 1/x (a hyperbola) (1/x - r = 0)
--The specific instantiation used here, r_(n+1) = r_(n) * (2 - x * r_(n)),
--appears in a similar form (replace y with 2y-xy^2) on
-- http://en.wikipedia.org/wiki/Reciprocal_%28mathematics%29
-- .  We get our initial estimate from a native DInt division.
reciprocalToPrecision resultPrecision reciprocee =
   dropExcessPrecisionRoundingUpInMagnitude resultPrecision
      (doSeries
           (dropLeastSignificantZeroes
                (reciprocalEstimate reciprocee)))
  where
    doSeries = fixPointBy (==) (\(i,e) -> (e, makeShort i)) nextIter
      where
        --only to be fast and checkable for equality:
        makeShort i = L.take resultPrecision (L.reverse i)

    -- e*(2-i*e)
    nextIter :: FloatingInteger -> FloatingInteger
    nextIter (estimate, estimateExp) =
      dropExcessPrecisionRoundingUpInMagnitude internalPrecision
        ( dropLeastSignificantZeroes
            ( estimate `times` (twoMinus (reciprocee `times` estimate))
            --estimateExp +    (id       (   0         +  estimateExp))
            , pTwice estimateExp  -- equivalent to above.
            --Multiplication adds exps; reciprocee is exp 0;
            --  twoMinus doesn't change exp.
            )
        )
      where
        internalPrecision = pTwice resultPrecision
        --2 > (n * estimated reciprocal of n) > 0, MUST be true.
        --(the estimate has to be good enough, that is)
        --so subtracting is destructive and yields an overall-positive answer.
        twoMinus = twoMinusSomethingNearOne intSmallBase
        times = {-if internalPrecision > karatsubaSmallBaseThreshold
            then-} multiplyIntegerSmall_ --yep, no factors should be zero here
          --  else naiveMultiplyIntegerSmall_

fixPointBy :: (canon -> canon -> Bool) -> (a -> canon) -> (a -> a) -> a -> a
fixPointBy eqCanon canonicalizer iter initial =
   f (canonicalizer initial) initial
  where
    f canonic value = let
        value_ = iter value
        canonic_ = canonicalizer value_
      in if eqCanon canonic canonic_ then value_ else f canonic_ value_

countingDropWhile :: (a -> Bool) -> [a] -> (PInt, [a])
countingDropWhile p = f (pNat(0))
  where
    f n (x:xs) | p x = f (succ n) xs
    f n l = (n, l)
    --f n [] = (n, [])
    --f n l@(x:xs) = if p x then f (succ n) xs else (n, l)

dropLeastSignificantZeroes
            :: FloatingInteger -> FloatingInteger
dropLeastSignificantZeroes (d,e) = case countingDropWhile isZero d of
          (de, d_) -> (d_, e - de)
    -- requires dropLeastSignificantZeroes-effect first.
    -- hmm, is something with least-significant zeroes allowed? certainly
dropExcessPrecisionRoundingUpInMagnitude
            :: PInt -> FloatingInteger -> FloatingInteger
dropExcessPrecisionRoundingUpInMagnitude precis ie@(i@(int:_), e) =
       let nToDrop = (L.length i) - precis in
        -- if the "rounding up" causes an extra digit, then
        -- the exponent remains the same, but the number is
        -- ... 0 !: 0 !: 0 !: 0 !: +-1 !: [].  Fewer least-significant
        -- zeroes can also be created by less-extreme carrying.
        -- Either way these are eliminated by dropLeastSignificantZeroes.
         if nToDrop > (pNat(0))
          then
           --should we dropLeastSignificantZeroes first too (otherwise
           --this "might" unnecessarily keep the precision long, dependently
           --on meaningless stuff, which would be bad)
           --Er, _should_ we doing weird rounding-up like this on every step
           --of Newton's? well, the alternative is implicitly rounding down ;)
           --(unless we want to really slow things down by keeping all the
           --digits, which doesn't seem necessary...)
           dropLeastSignificantZeroes --add AFTER drop?
               ( synergisticAddOnlyCarrySmall (L.drop nToDrop i) (signum int)
               , e - nToDrop
               )
          else ie --if it is already within precision, we have no
            --safe way of "rounding up" without extending the precision,
            --which would be meaningless. It's pretty much exact here.

--reciprocalOfNDigits :: DInt{-=length digits(?)-}
--    -> SmallBaseLEList{-digits-}
--    -> (
--        SmallBaseLEList{-number of reciprocal digits that is required-}
--       , DInt)
                              {-length rdigits = length digits +
(0.09 = 0.1 = 10^-1) * 9 < 1
the numerator may be up to one more digit than the denominator
in each division in a long division:

   0065 r 5   --6=quot,5=quot,5=rem
   ---------
43|2800       --not shown: 0,2=quotRem; 0,28=quotRem
   258
   ---
    220       --22=rem
    215
    ---
      5       --5=rem

and ((d+1) over d) = (d+1) * recip d
9 requires 0.2 but not 0.09999999999999...
99 requires 0.02 but not 0.00999999999999...
If we assume all reciprocals will be less in magnitude than 1
 (we can check for 0, 1, -1 denominators in advance, or just
do that implicitly by specializing for one-or-less-digit denominators)
and only count the digits _after_ the decimal point, that makes us
need only as many digits in the result as in the sub-numerator; i.e., d+1.

Of course we may need more digits temporarily, in order
to find the reciprocal to that precision.

342787745 / 3
qr  3 3 = (1,0)
qr 04 3 = (1,1)
qr 12 3 = (4,0)
qr 07 3 = (2,1)
qr 18 3 = (6,0)
qr 07 3 = (2,1)
qr 17 3 = (5,2)
qr 24 3 = (8,0)
qr 05 3 = (1,2)
114262581 + 2/3
but it is simple: convert to however large a base is needed for the
denominator to fit, then do repeated quotRem. It is the previous remainder
that contributes most of the difficulty in the digit-division.

3/3 = 3*0.333333 = 0.999999 = 0
6/6 = 6*0.16 = 0.96
        0.166  0.996
        0.1666 0.9996
8/9 = 8*0.111111 = 0.888888
2/2 = 2*0.500000 = 1.000000 = 0
9/9 = 9*0.111111 = 0.999999
    = 1*0.999999
perhaps round _up_ (or even, +1) the last, insignificant digit always,
then round _down_ the product? (careful with adding to negative numbers)

we require numeratorDigits after radix point, plus one that is incremented
from the correct value (this last digit may be max possible first, in which
case it carries.) (just adding 1 to the last of numeratorDigits
 seems to work?)

we happen to choose our numeratorDigits to be up to (twice)
denominatorDigits (I think) (it just requires enough that the recip has any
digits and can be used?)


-}


-- ********************** CONVERSION ************************
--        between HInteger and other numerical types


-- We are interested in small code footprint anyway so it's fine
-- if they aren't noticed by some compilers.  They are used to
-- define these more specific types in some instances, though.
-- This pragma is even described by Haskell-98!
-- Using Bits (INTERESTING?), from/to DInt may be implemented
-- rather differently...
{-# SPECIALIZE versatileFromIntegral :: N.Int -> HInteger__      #-}
{-# SPECIALIZE uncheckedFromIntegral :: N.Int -> HInteger__      #-}
{-# SPECIALIZE uncheckedFromIntegral :: N.Integer -> HInteger__  #-}
{-# SPECIALIZE uncheckedToNum        :: HInteger__ -> N.Int      #-}
{-# SPECIALIZE uncheckedToNum        :: HInteger__ -> N.Integer  #-}

--hmm. or toInteger, which must be implemented _somehow_ in Integrals?
versatileFromIntegral :: Integral a => a -> LargeBaseLEList
versatileFromIntegral integral =
   -- fromIntegral = fromInteger . toInteger; the toInteger's
   -- implementation is not our responsibility, as long as
   -- no exported function relies on the one we're defining here, and
   -- DInt's fromInteger::Integer->DInt doesn't depend on x->Integer either.
     if Misc.fromIntegral integralLargeBase_ /= intLargeBase
       --presumably the integral is small enough to fit in DInt
       -- if intLargeBase did not fit in it
       --(this might fail for "NonPositiveInteger" or so...)
     then uncheckedFromIntegral (integralToDInt_ integral)
     else fromIntegral_ integral
  where
   -- locally monomorphic type matches argument,
   -- and this computation is shared.  (This implementation
   -- will fail for bounded types in which intLargeBase doesn't fit,
   -- so we try to detect that problem - above.)
   integralLargeBase_ = Misc.fromIntegral intLargeBase
   integralIsZero_ = (0 ==)
   integralToDInt_ = Misc.fromIntegral
   fromIntegral_ =
     \i -> if integralIsZero_ i then [] else
             case i `quotRem` integralLargeBase_ of
              (above, digit) -> integralToDInt_ digit !: fromIntegral_ above

-- intLargeBase must fit in the Integral type - not checked.
uncheckedFromIntegral :: Integral a => a -> LargeBaseLEList
uncheckedFromIntegral integral = fromIntegral_ integral
   -- fromIntegral = fromInteger . toInteger; the toInteger's
   -- implementation is not our responsibility, as long as
   -- no exported function relies on the one we're defining here, and
   -- DInt's fromInteger::Integer->DInt doesn't depend on x->Integer either.
  where
   -- locally monomorphic type matches argument,
   -- and this computation is shared.
   integralLargeBase_ = Misc.fromIntegral intLargeBase
   integralIsZero_ = (0 ==)
   integralToDInt_ = Misc.fromIntegral
   fromIntegral_ =
     \i -> if integralIsZero_ i then [] else
             case i `quotRem` integralLargeBase_ of
              (above, digit) -> integralToDInt_ digit !: fromIntegral_ above

--Does not check for overflow. after all, bounded is not a superclass
--of num, integral or enum (consider Integer!).  However it is not as unsafe
--as uncheckedFromIntegral, because it wasn't a very meaningful operation
--anyway, when uncheckedToNum overflows.
uncheckedToNum :: Num a => LargeBaseLEList -> a
uncheckedToNum [] = 0
uncheckedToNum integer@(_:_) = toIntegral_ integer
  where
   integralLargeBase_ = Misc.fromIntegral intLargeBase
   intToIntegral_ = Misc.fromIntegral
   toIntegral_ =
     \(d:ds) -> case ds of
                 [] -> intToIntegral_ d
                 _  -> intToIntegral_ d + integralLargeBase_ * toIntegral_ ds

{-
instance Num Int / fromInteger will have to be implemented somehow
when we are the native integer.
uncheckedToNum works fine internally... we should export
intFromInteger :: HInteger -> PInt
from the exterior module.

-- the integer must be fully organized like it was in an Integer,
-- to call toIntInteger
toIntInteger :: LargeBaseLEList -> Int
--if we assume two's-complement Bits the implementation is
--rather different
toIntInteger [] = zero
toIntInteger (d:[]) = d --good for the common case
toIntInteger integer =
    --When the integer doesn't fit in an int...
    --we could do an overflow error, like Hugs,
    -- or a modulus / bit-chopping, like GHC.. what is right?
    -- Hugs still implements wrapping ((maxBound + maxBound :: Int) /= _|_)
    -- and a third option is to just do something like
    -- uncheckedToIntNonZeroInteger even if it would overflow like that
  uncheckedToIntNonZeroInteger integer
{-
  if CInteger integer <= maxIntInteger && CInteger integer >= minIntInteger
   then uncheckedToIntNonZeroInteger integer
   else
    overflowError
-}
{-
    let
     shortened = CInteger integer `mod` toIntModulus
    in if shortened > maxIntInteger
        then uncheckedToIntNonZeroInteger
               (case shortened - maxIntInteger of CInteger i -> i)
        else uncheckedToIntNonZeroInteger
                (case shortened                 of CInteger i -> i)
-}
uncheckedToIntNonZeroInteger :: LargeBaseLEList -> Int
uncheckedToIntNonZeroInteger (d:[]) = d
uncheckedToIntNonZeroInteger (d:ds) = d
                   + intLargeBase * uncheckedToIntNonZeroInteger ds
toIntModulus = maxIntInteger - minIntInteger
maxIntInteger, minIntInteger, toIntModulus :: Integer
maxIntInteger = Misc.fromIntegral (maxBound :: Int)
minIntInteger = Misc.fromIntegral (minBound :: Int)
--how is overflow handled? we could use maxBound,minBound
--(are those required to be closely related?), Bits...
-}




-- ********************** EXPORTED ********************** ...


intFromInteger :: Integer -> PInt
intFromInteger (CInteger a) = uncheckedToNum a
integerFromInt :: PInt -> Integer
integerFromInt int = mkInteger (
                      --if PInt == DInt
 --(if we knew PInt had as big a capacity as DInt (it is the very same type
 --except in some testing circumstances!), we could use uncheckedFromIntegral
 --here) :
                       --uncheckedFromIntegral --INTERESTING
                     --if PInt might have a rather smaller capacity than DInt
                       versatileFromIntegral
                          int )

-- ... including the ***** Integer type **** ...

--(Little-endian does not mean the machine Ints have to be
--         stored any particular way!)
--They are represented as a little-endian list of
--quantities in base N.  What is N?  Unless customized otherwise,
--this is 2^28 for minimal (30-bit) Haskell-98 Ints, 2^30 for 32-bit Ints,
--2^62 for 64-bit Ints, etc. - the maximal power of four that works, see below
--for details.  Negative numbers have all elements of the list be negative.
--So, for each int d in the list: -base < d < base.
--"Leading" zeroes (those at the end of the list) are not permitted.
--(so 0 == Integer []).  This form should not be too much
--of a burden for compilers to produce (as they must for
--[at least, large] numeric literals), since they can already
--do String::[Char], and we are assuming they can support Int well.
--Examples (assuming base 2^28) :
--           0 is []
--   0xfffffff is 0xfffffff : []
--  -0xfffffff is -0xfffffff : []
--  0x10000000 is 0 : 1 : []
-- -0x10000000 is 0 : -1 : []
newtype Integer = CInteger { unI :: LargeBaseLEList }


--no need for strictness annotations in interpreter,
--and a smart enough compiler might figure them out?
--For now, instead of carefully examining all the code
--this should be semantically correct..........
--Now I believe all the code is strict enough! of course if the assertion
--  is tested, that will also force the whole list...
mkInteger :: LargeBaseLEList -> Integer
mkInteger integer = --eval integer `seq`
   assert ("mkInteger validity",integer) (validBase intLargeBase integer)
     (CInteger integer)
--  where
--   eval :: LargeBaseLEList -> ()
--   eval (d:ds) = d `seq` eval ds
--   eval [] = ()



-- ... and ******* INSTANCES ****** ...

--or, could be derived...:
instance Class.Eq Integer where
  CInteger a == CInteger b = a == b

instance Class.Ord Integer where
  compare (CInteger a) (CInteger b) = compareInteger a b

instance Class.Num Integer where
   CInteger a + CInteger b = mkInteger (addInteger a b)
   -- (-) is default based on negate and (+)
   CInteger a * CInteger b = mkInteger (multiplyInteger a b)
   negate (CInteger a) = mkInteger (negateInteger a)
   abs (CInteger a) = mkInteger (absInteger a)
   signum (CInteger a) = mkInteger (signumInteger a)

   --fromInteger :: NInteger -> HInteger
   --Can be used when we are the native integer, which improves
   --  efficiency but is not necessary:
   --fromInteger integer = integer --INTERESTING
   fromInteger unboundedIntegral = mkInteger
                                    (uncheckedFromIntegral unboundedIntegral)

   --can be used when fromInt exists,
   -- though it's not very useful anyway
   --fromInt :: DInt -> HInteger
   --fromInt = integerFromInt

instance Class.Real Integer where
  --toRational :: HInteger -> Ratio NInteger
  --INTERESTING
  -- if we are the native integer
  -- toRational i = i % oneInteger --hmph, requires importing (Data.)Ratio...
  -- toRational = fromInteger
  toRational = Misc.fromIntegral

--INTERESTING? "messages" are the same as hugs'.
divZeroError :: a
divZeroError = --{-hmm-}((dNat(1)) `div` (dNat(0))) `seq`
                             Error.error "divide by zero"
--overflowError :: a
--overflowError = {-hmm-} Error.error "arithmetic overflow"

instance Class.Integral Integer where
  --toInteger :: HInteger -> NInteger
  -- if we are the native integer
  --toInteger i = i
  toInteger (CInteger a) = uncheckedToNum a
  CInteger a `quotRem` CInteger b =
      if isZeroInteger b
         then  divZeroError
         else  case a `quotRemInteger` b of
                 (q,r) -> (mkInteger q, mkInteger r)

--possibly could be made more efficient:
succInteger, predInteger :: HInteger__ -> HInteger__
succInteger i = addInteger i oneInteger
predInteger i = addInteger i negativeOneInteger

instance Class.Enum Integer where
  succ (CInteger a) = mkInteger (succInteger a)
  pred (CInteger a) = mkInteger (predInteger a)
  enumFrom a = a : enumFrom (succ a)
  enumFromThen a b = eF a
     where add = b - a; next = (add +); eF a_ = a_ : eF (next a_)
  enumFromTo a e = eF a
     where eF a_ = if e < a_ then [] else a_ : eF (succ a_)
  enumFromThenTo a b e = eF a
     where
       add = b - a; next = (add +)
       eF a_ = if e < a_ then [] else a_ : eF (next a_)
  --toEnum :: PInt -> HInteger
  toEnum = integerFromInt
  --fromEnum :: HInteger -> PInt
  fromEnum = intFromInteger





{-
--THIS ONE WORKS WITH ANY POSITIVE BASE WHATSOEVER
--but the argument being split, must be positive (not zero)
--and the resulting list is most-significant first(?)
convertPositiveToBase :: Integer -> Integer -> [Integer]
convertPositiveToBase base = jsplitf base
    where
    jsplitf :: Integer -> Integer -> [Integer]
    jsplitf p n = if p > n
                   then n : []
                   else jsplith p (jsplitf (p * p) n)

    jsplith :: Integer -> [Integer] -> [Integer]
    jsplith p (n:ns) =
        case n `quotRem` p of
         (q,r) -> if 0 == q
                   then        r : jsplitb p ns
                   else    q : r : jsplitb p ns

    jsplitb :: Integer -> [Integer] -> [Integer]
    jsplitb _ []     = []
    jsplitb p (n:ns) = case n `quotRem` p of
         (q,r) -> q : r : jsplitb p ns
-}


instance Class.Show Integer where
--  libraries/base/GHC/Num.lhs  has a pretty fancy implementation
--  which we could duplicate if we wanted.
--  I'm afraid that even if we have O(n) division-by-10,
--  iterated O(n) times, show is O(n^2) here.
  showsPrec _ (CInteger []) r = '0':r
  showsPrec p (CInteger (d:[])) r = showsPrec p d r
  showsPrec p (CInteger integer) r
    = if isNeg && p > (pNat(6))
      then '(' : showsSignInteger isNeg positiveInteger (')' : r)
      else showsSignInteger isNeg positiveInteger r
    where
      isNeg = compareNonzeroIntegerZero integer == LT
      positiveInteger = if isNeg then negateInteger integer else integer

showsSignInteger :: Bool -> LargeBaseLEList -> String -> String
showsSignInteger isNeg positiveInteger cs =
  if isNeg then '-' : showsPositiveInteger positiveInteger cs
           else       showsPositiveInteger positiveInteger cs

showsPositiveInteger :: LargeBaseLEList -> String -> String
{--
showsPositiveInteger integer cs = P.dropWhile (== '0') P.$
   P.foldr myshows cs (convertPositiveToBase (1000000000) (CInteger integer))
 where
    myshows i = jblock (P.fromIntegral{-hmm-} i)
    jhead :: P.Int -> String -> String
    jhead n cs
        = if n < 10
           then case {-unsafeChr-}toEnum ({-ord-}fromEnum '0' + n) of
                      c -> c `seq`          (c : cs)
           else case {-unsafeChr-}toEnum ({-ord-}fromEnum '0' + r) of
                      c -> c `seq` (jhead q (c : cs))
        where
        (q, r) = n `quotRem`{-Int`-} 10

    jblock = jblock' {- ' -} 9

    jblock' :: P.Int -> P.Int -> String -> String
    jblock' d n cs
        = if d == 1
           then case {-unsafeChr-}toEnum ({-ord-}fromEnum '0' + n) of
                      c -> c `seq`                    (c : cs)
           else case {-unsafeChr-}toEnum ({-ord-}fromEnum '0' + r) of
                      c -> c `seq` (jblock' (d - 1) q (c : cs))
        where
        (q, r) = n `quotRem`{-Int`-} 10
--}
--
showsPositiveInteger integer = go (largeToSmallBaseLEList integer)
  where
  --really this would work with the highest power of ten < intSmallBase
    go n cs =
     case quotRem10 n of
      (q,r) -> if isZeroInteger q then      (showsDInt0through9 r cs)
                                  else go q (showsDInt0through9 r cs)
--}
showsDInt0through9 :: DInt -> String -> String
showsDInt0through9 = showsPrec (pNat(0))

quotRem10 :: SmallBaseLEList -> (SmallBaseLEList, DInt)
quotRem10 =
   case compare intSmallBase (dNat(10)) of
    GT -> longDivideBySmall (dNat(10))
    LT -> let
           small10 = largeToSmallBaseLEList ( (dNat(10)) !: [] )
           recipSmall10 = minimalReciprocal small10
           qrs10 = quotRemSmallSectioned_ small10 recipSmall10
     in \n ->
         case qrs10 n of
           (q, r) -> ( q, uncheckedToNum (smallToLargeBaseLEList r) )
    EQ -> \n -> case n of (d:ds) -> (ds, d); [] -> ([],(dNat(0)))

--Read is just too horrible to implement ourselves
--By the way, this is inconsistent with GHC's instance Read Integer in
--trivial way that no one will care about, by virtue of GHC being slightly
--inconsistent with Haskell98.  That however makes it sometimes fail
--with QuickCheck :-)
instance Class.Read Integer where
  Class.readsPrec _ = readSigned readDec

--INTERESTING? same as hugs and haskell98 report:
indexError :: a
indexError = Error.error "Ix.index: Index out of range."

instance Class.Ix Integer where
  Class.range (l,h) = enumFromTo l h
  Class.inRange (l,h) i = l <= i && i <= h

  Class.index (l,h) i = if l <= i && i <= h
                     then intFromInteger (i - l)
                     else indexError
  --INTERESTING?
  --unsafeIndex (l,_h) i = intFromInteger (i - l)


instance Class.Bits Integer where
  isSigned _ = True
--"The function bitSize is undefined for types that do not have a fixed
-- bitsize, like Integer."
  (CInteger a) .&. (CInteger b) = mkInteger (tcAndInteger a b)
  (CInteger a) .|. (CInteger b) = mkInteger (tcOrInteger a b)
  xor (CInteger a) (CInteger b) = mkInteger (tcXOrInteger a b)
  complement (CInteger a) = mkInteger (tcComplementInteger a)
--"For unbounded types like Integer, rotate is equivalent to shift."
  shiftR (CInteger a) b = mkInteger (tcShiftRInteger a b)
  shiftL (CInteger a) b = mkInteger (tcShiftLInteger a b)
  rotateR (CInteger a) b = mkInteger (tcShiftRInteger a b)
  rotateL (CInteger a) b = mkInteger (tcShiftLInteger a b)
--}

-- ******************** Bits DInt => Bits HInteger ********************
--    code ought to be cleaned up a lot here and commented.

{-
zipWithDefaults :: (a -> b -> c) -> a -> b -> [a] -> [b] -> [c]
zipWithDefaults f defa defb = zp
  where
    zp (a:as) (b:bs) = f a b : zp as bs
    zp [] [] = []
    zp [] bs = L.map (\b -> f defa b) bs
    zp as [] = L.map (\a -> f a defb) as

signed :: HInteger__ -> DInt
signed = if compareInteger i zeroInteger == LT
          then (dNeg(1)) else (dNat(0))

zipWithSigns :: (DInt -> DInt -> DInt) -> HInteger__ -> HInteger__
                                             -> HInteger__
zipWithSigns f a b = zipWithDefaults f (signed a) (signed b) a b
-}
signRep :: Bool -> DInt
signRep n = if n then (dNeg(1)) else (dNat(0))
--tc = two's complement. For Bits, we assume DInt is two's complement
--and we act as if our Integer is an infinite two's complement bit-sequence.
--Could use +/- 1/0 instead of succ/id
--n = negative, f = final, m = modify, s = sign(Rep),
--(f = function), d = digit, (s = plural)
--Nat = natural = nonnegative
type Bin a = a -> a -> a
type Mon a = a -> a
{--prependMightBeNegBase :: DInt -> [DInt] -> [DInt]
prependMightBeNegBase d ds | negate intLargeBase >= d
    = sees ("ph",d,ds) (intLargeBase + d) !: case ds of
         [] -> negativeOneInteger
         (d_:ds_) -> prependMightBeNegBase (pred d_) ds_
prependMightBeNegBase d ds = prepend d ds--}
--negate(abs) it, subtract 1 (making sure to stay nat throughout),
-- flip all the bits, add 1
--how will the (1)00000 bit pattern happen?
--   ((negate ((negate it)-1)) - 1) + 1 ==it+1?

--tcn === become two's complement representation
--utcn === two's complement representation -> normal, dropping
--          most-significant zeroes that result
--etcn === inefficient, possibly only-negative, utcn that works more by the
--          definition of two's complement. It found a bug during development.
--ck (\(I ni) -> (\i -> i == mkInteger (utcn (tcn (case i of CInteger x->x))))
--                    (fromInteger (if ni >= 0 then complement ni else ni)))
--ck (\(I ni) -> (\i -> i == mkInteger (etcn (tcn (case i of CInteger x->x))))
--                    (fromInteger (if ni >= 0 then complement ni else ni)))
--etcn,
utcn :: [DInt] -> HInteger__
tcn :: HInteger__ -> [DInt]
--etcn = (\x->case x of CInteger y ->y)P.. complement P.. P.foldr
--        (\d r-> d + 2*r) 0 P.. P.concatMap
--           (\i ->(P.map (\n -> (if testBit i n then 0 else 1))
--               ([0..P.fromIntegral P.$intLargeExponentOfTwo-1])))
tcn i = --P.concatMap (\i ->' ':(P.concatMap (\n -> (if (n+1) `mod` 88 == 0
 -- then (' ':) else id) P.$ if testBit i n then "1" else "0")
 --                               (P.reverse [0..intLargeExponentOfTwo-1])))
 -- P.$ P.reverse
   (f i)
 where
  f [] = []
  f (d:ds) = d !: if (dNat(0)) <= d {-isZero (d {-.&. pred intLargeBase-})-}
                      then f ds else f_ ds
  f_ = strictMap pred
--  f_ [] = []
--  f_ (d:ds) = pred d !: if isZero (pred d .&. pred intLargeBase)
--                              then f_ ds else f_ ds
utcn i = f i
 where
  f (d:ds) = let d_ = repairNegIntLargeBase d in
               d_ `prepend` if (dNat(0)) <= d_ {-isZero d_-}
        {-isZero (d .&. pred intLargeBase)-} then f ds else f_ ds
  f [] = []
--  f_ = strictMap succ
  f_ (d:ds) = succ d `prepend` f_ ds
  f_ [] = []
--  f_ (d:ds) = if isZero (d .&. pred intLargeBase)
--                 then (negate intLargeBase + 1) !: f_ ds
--                 else succ d `prepend`  f_ ds
--  f_ [] = []
-- = let d_ = complement d
--     in if (dNeg(1)) == d_ then {-0-}negate intLargeBase :
--           case ds of (d__:ds__) ->
--     else
sr :: DInt -> DInt -> DInt
sr s d = if isZero s && (dNat(0)) <= d then (dNat(0)) else (dNeg(1))
lowBits, highBits :: DInt
lowBits = pred intLargeBase
highBits = complement lowBits
--Don't ask me to give a good complete answer of why the code works: study it
--and trust QuickCheck.
tcBinOpInteger :: Bin DInt{-symmetric-} -> Bin Bool
               -> Mon HInteger__ -> Mon HInteger__
               -> Bin HInteger__
tcBinOpInteger op signOp endOpWithNat endOpWithNeg = \i1 i2 -> let
   n1 = isNegativeInteger i1; n2 = isNegativeInteger i2; nf = n1 `signOp` n2
   --rs1 = signRep n1; rs2 = signRep n2; rsf = signRep nf
--   mask = sees ("mask",i1,i2,nf) P.$ if nf then high else (dNat(0))
--   m = if nf then (high .|.) else (low .&.)
--   n x = if negate intLargeBase == x then (dNat(0)) else x
   repair = if nf then (\d -> repairNegIntLargeBase (highBits .|. d))
                  else (\d -> (lowBits .&. d))
--   m1 = \d -> d + s1; m2 = \d -> d + s2; mf = \d -> d - sf
--   end n i = if n then endOpWithNeg i else endOpWithNat i--}
   fi (d1:ds1) (d2:ds2) = prepend df dsf
     where
       df = repair (((d1     ) `op` (d2     ))     )
       dsf = f ds1 ds2
              (signRep ((dNat(0)) > d1))
              (signRep ((dNat(0)) > d2))
              (signRep ((dNat(0)) > df))
   fi ds1@(_:_) [] = endOpWithNat ds1
   fi [] ds2@(_:_) = endOpWithNat ds2
   fi [] [] = []
   f (d1:ds1) (d2:ds2) s1 s2 sf = prepend df dsf
     where
       df = repair (((d1 + s1) `op` (d2 + s2)) - sf)
       dsf = f ds1 ds2 (sr s1 d1) (sr s2 d2) (sr sf df)
   f ds1@(_:_) [] s1 s2 sf = f1 ds1 s1 s2 sf
   f [] ds2@(_:_) s1 s2 sf = f1 ds2 s2 s1 sf
   f [] [] s1 s2 sf = f0 s1 s2 sf
--   f (d1:ds1) [] s1 s2 sf = prepend df dsf
--     where
--       df = ((d1 + s1) `op` (     s2)) - sf
--       dsf = f ds1 []  (sr s1 d1) (   s2   ) (sr sf df)
--   f [] [] s1 s2 sf = fromDInt df--prepend df dsf
--     where
--       df = ((     s1) `op` (     s2)) - sf
--       --dsf = []--f [] [] (   s1   ) (   s2   ) (sr sf df)
   --asymptotic-efficiency-adding optimization:
   f1 i s1 s2 sf | s1 == sf = if isZero s2 then endOpWithNat i
                                           else endOpWithNeg i
   f1 [] s1 s2 sf = f0 s1 s2 sf
   f1 (d1:ds1) s1 s2 sf = prepend df dsf
     where
       df = repair (((d1 + s1) `op` (     s2)) - sf)
       dsf = f1 ds1 (sr s1 d1) (   s2   ) (sr sf df)
   f0 s1 s2 sf = fromDInt df--prepend df dsf
     where
       df = repair (((     s1) `op` (     s2)) - sf)
       --dsf = []--f [] [] (   s1   ) (   s2   ) (sr sf df)
{--
   f1 i s sf = if s == sf then i else
      case i of
       [] -> (sf - s) !: []
       (d:ds) -> prepend df dsf
         where
           df = d + s - sf
           dsf = f1 ds (sr s d) (sr sf df)--}
{--   f21 (d1:ds1) (d2:ds2) sf = prepend df
                     (f2_ (isZero d1) (isZero d2) (isZero df))
     df = d1 `op` d2
     f False False False = f00 ds1 ds2
     f False True  = f1 ds2 ds1
     f True  False = f1 ds1 ds2
     f True  True  = f2 ds1 ds2
   f1
   f0
   f (d1:ds1) (d2:ds2) s1 s2 sf = sees ("f",(d1,ds1),(d2,ds2)) P.$
       prepend (mf (m1 d1 `op` m2 d2)) (f (if ds1 ds2)
   f [] [] = sees "f[]" []
   f [] ds2 = sees ("f2",ds2) P.$ end n1 ds2
   f ds1 [] = sees ("f1",ds1) P.$ end n2 ds1
   f_ (d1:ds1) (d2:ds2) = sees "f_1" P.$
              prependMightBeNegBase (d1 `op` d2) (f ds1 ds2)
   f_ [] [] = []
   f_ [] ds2 = endOpWithNat ds2
   f_ ds1 [] = endOpWithNat ds1--}
  in fi i1 i2 --(dNat(0)) (dNat(0)) (dNat(0))
tcXOrInteger, tcOrInteger, tcAndInteger :: Bin HInteger__
tcXOrInteger = tcBinOpInteger (xor) (/=) (id) (tcComplementInteger)
tcOrInteger  = tcBinOpInteger (.|.) (||) (id) (const [])
tcAndInteger = tcBinOpInteger (.&.) (&&) (const []) (id)

-- complement i === -1 - i
tcComplementInteger :: HInteger__ -> HInteger__
tcComplementInteger i = predInteger (negateInteger i)

tcShiftRInteger, tcShiftLInteger :: HInteger__ -> PInt -> HInteger__

tcShiftRInteger i r = (utcn (tcShiftR_TC (tcn i) r))
tcShiftLInteger i l = (utcn (tcShiftL_TC (tcn i) l))


--we would need fromDInt/prepend ... that treated -1 as badly as zero
--when most-significant, for these, if we don't just let utcn do that
tcShiftR_TC, tcShiftL_TC :: [DInt] -> PInt -> [DInt]

tcShiftR_TC (d:ds) r | r >= intLargeExponentOfTwo
    = case ds of
        [] -> fromDInt (signRep ((dNat(0)) > d))
        (_:_) -> tcShiftR_TC ds (r - intLargeExponentOfTwo)
tcShiftR_TC [] _r = []
tcShiftR_TC i@(_:_) r = f i
  where
   l = intLargeExponentOfTwo - r
   ourLowBits = pred (bit l)
   --higherBits = complement ourLowBits
   --don't overflow DInt even temporarily and even when using Bits
   --operations :) my testing int complains and there's no need to
   --higherBits = shiftR highBits l
   onlyLowerBits = pred (bit r)
   lowerBits = highBits .|. ourLowBits
   f (d:[]) = {-fromDInt-} (shiftR d r) !: fromDInt (signRep ((dNat(0)) > d))
   f (d:(ds@(dAbove:_))) = df !: f ds --prepend df dsf
    where
      df = {-(highBits .&. dAbove) .|.-} (lowerBits .&. shiftR d r)
          .|. (shiftL (onlyLowerBits .&. dAbove) l)
                --(higherBits .&. shiftL dAbove l)
--      dsf = f ds

tcShiftL_TC [] _l = []
tcShiftL_TC i l | l >= intLargeExponentOfTwo
    = (dNat(0)) !: tcShiftL_TC i (l - intLargeExponentOfTwo)
tcShiftL_TC i@(d1:_) l = dFirst !: f i--increase decrease left right
  where
   dFirst = (highBits .&. d1) .|. shiftL (onlyLowerBits .&. d1) l
   r = intLargeExponentOfTwo - l
   ourLowBits = pred (bit l)
   --higherBits = complement ourLowBits
   --don't overflow DInt even temporarily and even when using Bits
   --operations :) my testing int complains and there's no need to
   --higherBits = shiftR highBits l
   onlyLowerBits = pred (bit r)
   lowerBits = highBits .|. ourLowBits
   f (dBelow:[]) = {-fromDInt-} (shiftR dBelow r) !: []
   f (dBelow:(ds@(d:_))) = df !: f ds --prepend df dsf
    where
      df = (highBits .&. d) .|. (lowerBits .&. shiftR dBelow r)
          .|. (shiftL (onlyLowerBits .&. d) l)
                --(higherBits .&. shiftL d l)
--      dsf = f ds
{-





   foldr (\d (bitwiseDAbove,result) ->
              let bitwiseD = mi d in
                    (  bitwiseD
                    ,  prepend
                        (mf (
                          (nonHighMask .&. shiftR r bitwiseD)
                          `xor` --or .|.
                          (highMask .&. shiftL l bitwiseDAbove)
                        ))
                        result
                    )
            )
            (wrong?s,[])

--optimize small cases:
shiftRInteger [] _ = []
shiftRInteger (d:[]) b = fromDInt (shiftR d b)
shiftRInteger i b = let
   n = isNegativeInteger i  --shifting preserves sign
   s = signRep n
   mi = \d -> d + s1; mf = \d -> d - s1
  case b `quotRem` intLargeExponentOfTwo of
    (q,r) -> let
      l = intLargeExponentOfTwo - r
      highRealBits = (pred intLargeBase) `xor` (pred (shiftL (dNat(1)) r))
      otherBits = complement highRealBits
      i_ = L.drop q i
      --we don't care where the high bits indicating sign
      --come from; they're the same everywhere... except from
      --shiftL
      foldr (\d (bitwiseDAbove,result) ->
              let bitwiseD = mi d in
                    (  bitwiseD
                    ,  prepend
                        (mf (
                          (nonHighMask .&. shiftR r bitwiseD)
                          `xor` --or .|.
                          (highMask .&. shiftL l bitwiseDAbove)
                        ))
                        result
                    )
            )
            (wrong?s,[])
    [] = []
    (d:[]) = (bitwiseD, fromDInt (shiftR r bitwiseD
    (d:ds) = case r ds of (bitwiseDAbove,result) ->
      (nonHighMask .&. shiftR r (mi )) `xor` (highMask .&. shiftL l (mi ))
-}

-- -}

main = let large :: [Integer]
           large = P.map (\n -> 2 P.^ n) [1..100 :: Integer]
       in Print.print large
