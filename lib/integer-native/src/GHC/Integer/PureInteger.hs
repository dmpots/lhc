{-# LANGUAGE CPP, MagicHash, BangPatterns, UnboxedTuples, NoImplicitPrelude #-}
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
module GHC.Integer.PureInteger
   ( HInteger__
   , LargeBaseLEList
   , tcAndInteger
   , tcComplementInteger
   , tcXOrInteger
   , tcShiftRInteger
   , tcShiftLInteger
   , tcOrInteger
   , addInteger
   , quotRemInteger
   , remInteger
   , quotInteger
   , multiplyInteger
   , negateInteger
   , absInteger
   , signumInteger
   , succInteger
   , predInteger
   , integerFromInt
   , intFromInteger
   , isZeroInteger
   , compareInteger
   ) where

import GHC.Bool
import GHC.Prim
import GHC.Ordering

--for defining Integer instances
--import {-qualified-} Data.Bits ( Bits(..) )

import GHC.Integer.Int

infixr 2 ||
infixr 3 &&
True || _ = True
False || b = b

True && True = True
_ && _ = False

id x = x
const x y = x

min x y = if gtInt x y then y else x
max x y = if gtInt x y then x else y

infix 4 >=
infix 4 >
infix 4 <=
(>) = gtInt
(<=) = leInt
(>=) = geInt

map fn = worker
    where worker [] = []
          worker (x:xs) = fn x : worker xs

take (I# 0#) _ = []
take n [] = []
take n (x:xs) = x : take (n `minusInt` I# 1#) xs

drop (I# 0#) lst = lst
drop n [] = []
drop n (x:xs) = drop (n `minusInt` I# 1#) xs

succ x = plusInt x (I# 1#)
pred x = minusInt x (I# 1#)

length lst = worker (I# 0#) lst
    where worker acc [] = acc
          worker !acc (x:xs) = worker (acc `plusInt` I# 1#) xs


foldr fn i [] = i
foldr fn i (x:xs)
    = fn x (foldr fn i xs)

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 fn (x:xs) = foldr fn x xs

splitAt (I# 0#) lst = ([],lst)
splitAt n [] = ([],[])
splitAt n (x:xs) = let (left,right) = splitAt (n `minusInt` I# 1#) xs
                   in (x:left,right)

reverse lst
    = worker [] lst
    where worker acc [] = acc
          worker acc (x:xs) = worker (x:acc) xs

(>>) :: () -- [Int] -> [Int] -> [Int]
(>>) = (>>)

type PInt = Int
type DInt = Int

type HInteger__ = [DInt]

type IdF t = t -> t --or "Endo"...
dNat, dNeg :: IdF DInt; dNat x = x; dNeg = negateInt
pNat :: IdF PInt; pNat x = x--; pNeg = negate


infixr 5 !:
(!:) :: DInt -> [DInt] -> [DInt]
d !: ds =  d `seq` ds `seq` (d : ds)
strictMap :: (DInt -> DInt) -> [DInt] -> [DInt]
strictMap f = m
  where
    m (d:ds) = f d !: m ds
    m [] = []

type LargeSInt = DInt
type LargeBaseLEList = [LargeSInt]
type SmallSInt = DInt
type SmallBaseLEList = [SmallSInt]

{-
validBase :: DInt -> [DInt] -> Bool
validBase _base [] = True
validBase base ds@(_:_) =
   let sign = signum (L.last ds)
   in ((dNat(1)) == sign || (dNeg(1)) == sign)
       && L.all (\digit ->
        negate base < digit && digit < base &&
                       (sign * digit) >= (dNat(0))) ds
-}
intLargeBase, intSmallBase :: DInt
intLargeExponentOfTwo, intSmallExponentOfTwo :: PInt


#if WORD_SIZE == 4
intLargeBase = I# 268435456# -- 2^28
intLargeExponentOfTwo = I# 28#
intSmallBase = I# 16384# -- 2^14
intSmallExponentOfTwo = I# 14#
#else
intLargeBase = I# 4611686018427387904# -- 2^62
intLargeExponentOfTwo = I# 62#
intSmallBase = I# 2147483648# -- 2^31
intSmallExponentOfTwo = I# 31#
#endif

--no overflow allowed to be possible when calling these:
quotRemByIntLargeBase, quotRemByIntSmallBase :: DInt -> (# DInt, DInt #)

quotRemByIntLargeBase input = input `quotRemInt` intLargeBase
quotRemByIntSmallBase input = input `quotRemInt` intSmallBase

--inverse of quotRemByIntSmallBase
--perhaps should be named unQuotRemByIntSmallBase? be uncurried?
highLowFromIntSmallBase :: DInt->DInt -> DInt
highLowFromIntSmallBase high low = high `timesInt` intSmallBase `plusInt` low

--  [-intLargeBase, intLargeBase)  -intLargeBase becomes 0
repairNegIntLargeBase :: DInt -> DInt
repairNegIntLargeBase d = d `remInt` intLargeBase

isZero{-, isPositive, isNegative-} :: DInt -> Bool
isZero = ((dNat(I# 0#)) `eqInt`)
--isNegative = ((dNat(0)) >)
--isPositive = ((dNat(0)) <)

--could use bit-shifts: and addition and comparison are, I think, the
--only other operations PInt needs. Maybe even could use Double????
--I'm not sure if PInt ever even needs to be negative.
pTwice :: PInt -> PInt
pTwice x = x `plusInt` x
-- may not use on negative numbers:
pHalfRoundingDown, pHalfRoundingUp :: PInt -> PInt
pHalfRoundingDown x = x `quotInt` I# 2#
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
                        EQ -> compareInt d1 d2
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
                        EQ -> compareInt (absInt d1) (absInt d2)
                        answer -> answer
compareAbsInteger (_:_) [] = GT
compareAbsInteger [] (_:_) = LT
compareAbsInteger []  []   = EQ


zeroInteger, oneInteger, negativeOneInteger :: HInteger__
zeroInteger = []
oneInteger = (dNat(I# 1#)) !: []
negativeOneInteger = (dNeg(I# 1#)) !: []

isZeroInteger :: HInteger__ -> Bool
isZeroInteger [] = True
isZeroInteger (_:_) = False

--these can only return GT and LT ...
--A case for [] would be enough to remove the "nonzero" restriction! But then
--the postcondition would not be automatically checked (it might return EQ
--somewhere that would act oddly if it received EQ).
compareNonzeroIntegerZero, compareZeroNonzeroInteger :: HInteger__ -> Ordering
compareNonzeroIntegerZero =
     \(d:ds) -> case compareInt d (dNat(I# 0#)) of
                        EQ -> compareNonzeroIntegerZero ds
                        answer -> answer
compareZeroNonzeroInteger =
     \(d:ds) -> case compareInt (dNat(I# 0#)) d of
                        EQ -> compareZeroNonzeroInteger ds
                        answer -> answer

compareIntegerZero :: HInteger__ -> Ordering
compareIntegerZero [] = EQ
compareIntegerZero integer@(_:_) = compareNonzeroIntegerZero integer

isNegativeInteger :: HInteger__ -> Bool
isNegativeInteger i = case compareIntegerZero i of
                        LT -> True
                        _  -> False

signumInteger :: HInteger__ -> HInteger__
signumInteger a = case compareIntegerZero a of
       { GT -> oneInteger; LT -> negativeOneInteger; EQ -> zeroInteger }

-- they work on just about anything... maybe not absInteger on
--  mixed-sign lists.
negateInteger, absInteger :: [DInt] -> [DInt]
negateInteger = strictMap negateInt
absInteger = strictMap absInt





--or could judge 0 as positive, or,,,:
signNonzeroInteger :: HInteger__ -> DInt
signNonzeroInteger (d:ds) = let s = signumInt d in
                    if isZero s then signNonzeroInteger ds else s


-- ******************* ADDITION / SUBTRACTION *************************

-- PUTTING TOGETHER VALID INTEGER BITS:
-- prepend is like (:)/cons except it won't add a most-significant zero.
prepend :: DInt -> [DInt] -> [DInt]
prepend d [] | isZero d = []
prepend d ds = d !: ds
prependZero :: [DInt] -> [DInt]
prependZero [] = []
prependZero ds = (dNat(I# 0# )) !: ds
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
   if compareNonzeroIntegerZero integer1 `orderingEq` compareNonzeroIntegerZero integer2
    then synergisticAdd integer1 integer2
    else destructiveAdd intLargeBase integer1 integer2

orderingEq LT LT = True
orderingEq EQ EQ = True
orderingEq GT GT = True
orderingEq _ _   = False

-- Positive plus positive or negative plus negative,
-- no most-significant zeroes in arguments nor results.
-- (zero works fine as either(?))
--carrying can only be by +-1 at most
synergisticAdd :: LargeBaseLEList -> LargeBaseLEList -> LargeBaseLEList
synergisticAdd integer1 integer2 =
      synergisticAddWithCarry integer1 integer2 (dNat(I# 0#))

synergisticAddWithCarry :: LargeBaseLEList -> LargeBaseLEList -> DInt
                        -> LargeBaseLEList
synergisticAddWithCarry (d1:ds1) (d2:ds2) carry =
   case quotRemByIntLargeBase (d1 `plusInt` d2 `plusInt` carry) of
    (# carry_, d_ #) -> d_ !: synergisticAddWithCarry ds1 ds2 carry_
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
          (d:ds) -> case quotRemByIntLargeBase (d `plusInt` carry) of
                  (# carry_, d_ #) -> d_ !: synergisticAddOnlyCarry ds carry_
synergisticAddOnlyCarrySmall :: SmallBaseLEList -> DInt -> SmallBaseLEList
synergisticAddOnlyCarrySmall integer carry =
  if isZero carry
   then integer
   else case integer of
          [] -> carry !: []
          (d:ds) -> case quotRemByIntSmallBase (d `plusInt` carry) of
                  (# carry_, d_ #) -> d_ !: synergisticAddOnlyCarrySmall ds carry_

destructive
  :: (DInt -> DInt -> DInt) -> {-base:: -}DInt
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
  :: (DInt -> DInt -> DInt) -> {-base:: -}DInt
  -> DInt -> HInteger__{-base-}
  -> HInteger__{-base-} -> HInteger__{-base-}
destructive_ (+- {-either (+) or (-)-} ) base sign = sub
 where
  --s=signed
  sBase = sign `timesInt` base
--  sInteger = sign !: []
--  sPlusOne = (+)sign--(`minus` (negate 1))--sign))
  sub [] [] = []
  sub ds@(_:_) [] = ds
--sub [] ds@(_:_) = disallowed
  sub (d1:ds1) (d2:ds2) = case compareInt (absInt d1) (absInt d2) of
--Avoids sending any DInt to the opposite sign even temporarily
    GT -> prependNonzero ((        d1) +- d2) (sub       ds1 ds2)
    EQ -> prependZero                         (sub       ds1 ds2)
    LT -> prependNonzero ((sBase `plusInt` d1) +- d2) (subBorrow ds1 ds2)
--    LT -> prepend        ((sBase + d1) - d2) (sub ds1 (inc ds2))
  --borrowing: may produce d2=base:
  --(which yields the only reason the LT case isn't prependNonzero)
--  inc [] = sInteger
--  inc (i2raw:ds2) = (sPlusOne i2raw) !: ds2
--subBorrow [] _ = disallowed
  subBorrow (d1:ds1) (d2:ds2) = case compareInt (absInt d1) (succ (absInt d2)) of
    GT -> prependNonzero (((        d1) +- d2) `minusInt` sign) (sub       ds1 ds2)
    EQ -> prependZero                                  (sub       ds1 ds2)
    LT -> prepend        (((sBase `plusInt` d1) +- d2) `minusInt` sign) (subBorrow ds1 ds2)
  subBorrow ds1@(_:_) [] = borrow ds1
  borrow (d1:ds1) = if isZero d1
     then prependNonzero (  sBase{- + 0 +- 0-} `minusInt` sign) (borrow ds1) --LT 1
     else prepend        (          d1{-+- 0-} `minusInt` sign) (ds1)      --EQ/GT 1

destructiveAdd :: {-base:: -}DInt -> HInteger__{-base-} -> HInteger__{-base-}
                                       -> HInteger__{-base-}
destructiveAdd base integer1 integer2 =
  case compareAbsInteger integer1 integer2 of
    GT -> destructive (plusInt) base integer1 integer2
    LT -> destructive (plusInt) base integer2 integer1
    EQ -> zeroInteger

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
   (# high,low #) -> low !: prepend high (largeToSmallBaseLEList ds)
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
mulBySmall factor = f (dNat(I# 0#))
  where
    f carry [] = fromDInt carry
    f carry (d:ds) = case quotRemByIntSmallBase (factor `timesInt` d `plusInt` carry) of
        (# high,low #) -> low !: f high ds

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
  collapseMassiveOverflow ({-sees "overflows" P.$ -} multiplyToOverflowing i1)
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
          case quotRemByIntSmallBase (foldr1 (plusInt) overflow) of
            (# high, low #) -> {-traces high P.$ -} low !:
              if isZero high then collapseMassiveOverflow overflows
              else case overflows of
                   (o:os) -> collapseMassiveOverflow ((high:o):os)
                   [] -> (high !: [])
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
zipCons xs1@(_:_) [] = map (:[]) xs1
{-

--type DiffList x = [x] -> [x]

--needs commenting/description:
determineDigitAndOverflow :: Overflow -> {-[DInt]
             -> -} {-DInt{-smallBase-} -> -} ({-DiffList-} Overflow, DInt)
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
   f n | (pNat(I# 0#)) `eqInt` n = nil
   f n = (dNat(I# 0#)) !: f (pred n)

synergisticAddLeadingZeroes :: LargeBaseLEList -> LargeBaseLEList
         -> PInt -> LargeBaseLEList
synergisticAddLeadingZeroes i1_ i2 zeroes_ = f i1_ zeroes_
  where
    f i1 zeroes | (pNat(I# 0#)) `eqInt` zeroes = synergisticAdd i1 i2
    f (d1:ds1) zeroes = d1 !: f ds1 (pred zeroes)
    f [] zeroes = leadingZeroes zeroes i2

--avoiding fromIntegral... it involves the NInteger type, which
--might be us.
minSmallPDInt :: PInt -> DInt -> PInt
--minSmallPDInt p d = min p (fromIntegral d)
minSmallPDInt p d = f (pNat(I# 0#)) (dNat(I# 0#))
  where
   f pThreshold dThreshold =
    if p `eqInt` pThreshold || d `eqInt` dThreshold
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
  minSmallPDInt (pNat(I# 10#)) intSmallBase --INTERESTING?
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
    then naiveMultiplyInteger_ i1 i2
    else let
      b = pHalfRoundingDown (max len1 len2) --should we round up or down?
      len1high = len1 `minusInt` b
      len2high = len2 `minusInt` b
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
                                | (pNat(I# 0#)) `eqInt` n = ((b,[]),high)
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
                   len = case low of [] -> b `minusInt` n; (_:_) -> maxLen
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
      if len1high <= (pNat(I# 0#)) then let
         y = karatsubaMultiplyInteger_ i1 len1 i2low len2low
         z = karatsubaMultiplyInteger_ i1 len1 i2high len2high
        in synergisticAddLeadingZeroes y z b
      else if len2high <= (pNat(I# 0#)) then let
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
         lenZ1 = length z1
         z2 = i2low `synergisticPlus` i2high
         lenZ2 = length z2
         zp = karatsubaMultiplyInteger_ z1 lenZ1 z2 lenZ2
         -- or (zp `destructivePlus` (neg (x `synergisticPlus` y))
         z = (zp `destructivePlus` (neg x)) `destructivePlus` (neg y)
        in synergisticAddLeadingZeroes y
              (synergisticAddLeadingZeroes z x b) b
--associating the other way than this will be nicer:
--         ( (synergisticAddLeadingZeroes y z b)
--                          `synergisticAddLeadingZeroes` x) (pTwice b)

--this version doesn't check for multiplying by zero, which
--mayn't be done with it!
karatsubaMultiplyIntegerSmall_ :: SmallBaseLEList
                              -> SmallBaseLEList
                              -> SmallBaseLEList
karatsubaMultiplyIntegerSmall_ i1 i2 =
  let
   len1 = pHalfRoundingUp len1Small --rounding up, as necessary
   len2 = pHalfRoundingUp len2Small --rounding up, as necessary
   len1Small = length i1
   len2Small = length i2
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
    (# d1high,d1low #) = quotRemByIntSmallBase d1
    (# d2high,d2low #) = quotRemByIntSmallBase d2
  in if isZero d1high && isZero d2high then (d1 `timesInt` d2) !: [] else
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
                                      i1 (length i1)
                                      i2 (length i2)

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
    (l)
extendToN targetLen l nil =
    (extendToN_ targetLen l nil)
extendToN_ targetLen l nil =
    (f targetLen l)
  where
   f n _ | (pNat(I# 0#)) `eqInt` n     = nil
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
    f [] = ( [], (dNat(I# 0#)) )
    f (d:[]) = case d `quotRemInt` denom of
              (# q,r #) -> ( fromDInt q , r )
              --this check is sufficient because/when d is nonzero
              --and denom is smaller than the base, so then the
              --remainder here is nonzero and survives in the
              --previous digit (if any)
    f (d:ds) =
      case f ds of
        (qs, prevRem) ->
           case (highLowFromIntSmallBase prevRem d) `quotRemInt` denom of
             (# q,r #) -> (q:qs, r)


remInteger :: HInteger__ -> HInteger__ -> HInteger__
remInteger a b
    = case a `quotRemInteger` b of
        (_quot,rem) -> rem

quotInteger :: HInteger__ -> HInteger__ -> HInteger__
quotInteger a b
    = case a `quotRemInteger` b of
        (quot,_rem) -> quot

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
  case num `quotRemInt` denom of
   (# q,r #) -> ( fromDInt q, fromDInt r )
-- divisions by 1 and -1 must not be passed along
num `quotRemInteger` (denom:[])
  | denom `eqInt` (dNat(I# 1#)) = (              num, zeroInteger)
  | denom `eqInt` (dNeg(I# 1#)) = (negateInteger num, zeroInteger)
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
    reciprocalToPrecision (succ (pTwice (length denom))) denom

quotRemSmallSectioned_ ::
           SmallBaseLEList -> (SmallBaseLEList,PInt)
             -> SmallBaseLEList
             -> (SmallBaseLEList, SmallBaseLEList)
quotRemSmallSectioned_ denom (recipDigits,recipExp) =
  let
   lenDenom = length denom
   --they're not necessarily all the same length anyway because of the
   --most-significant digits:
   split [] = []
   split l = let (digit, rest) = splitAt lenDenom l
             in dropMostSignificantZeroes digit {-!!!:-}: split rest
   join [] = []
   join (x:xs) = extendToN lenDenom x (join xs)
   negDenom = negateInteger denom
   quotRemBy1 moderateSizeNum =
     case moderateSizeNum of
      [] ->  ([],[]) -- zero digit divided by something = 0, remainder 0
      (_:_) -> --now we can use nonzero multiplication
       let
         quotient = drop recipExp
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
       (quotient, remainder)
   quotRemBy2 highNum{-a remainder-} lowNum =
              quotRemBy1 (extendToN lenDenom lowNum highNum)
  in \num ->
   case longDivideBySingleDigit quotRemBy1 quotRemBy2 (split num) of
    (quotDigits,remDigit) -> (join quotDigits, remDigit)


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
twoMinusSomethingNearOne :: {-base:: -}DInt -> [DInt] -> [DInt]
twoMinusSomethingNearOne base = sub_2
 where
  sub_2, subBorrow_2 :: SmallBaseLEList -> SmallBaseLEList
  sub_2 (d:[]) = if (dNat(I# 1#)) `eqInt` d then oneInteger else
     (base `minusInt` d) !: oneInteger
  sub_2 (d:ds) = if isZero d then (dNat(I# 0#)) !: sub_2 ds else
     (base `minusInt` d) !: subBorrow_2 ds
  subBorrow_2 (d:[]) = if (dNat(I# 1#)) `eqInt` d then zeroInteger else
     pred (base `minusInt` d) !: oneInteger
  subBorrow_2 (d:ds) =
     pred (base `minusInt` d) !: subBorrow_2 ds

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
reciprocalEstimate = rE (pNat(I# 0#))
 where
  toBase d = largeToSmallBaseLEList (d !: [])
  largeOver n e = ( toBase (intLargeBase `quotInt` n)
                  , (pNat(I# 2#)){-for largeBase factor-} `plusInt` e
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
      if high `timesInt` high >= intSmallBase --high > sqrt intSmallBase
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
    doSeries = fixPointBy (\(a1,b1) (a2,b2) -> a1 `eqInt` a2 && eqList b1 b2) (\(i,e) -> (e, makeShort i)) nextIter
      where
        --only to be fast and checkable for equality:
        makeShort i = take resultPrecision (reverse i)

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

eqList :: [Int] -> [Int] -> Bool
eqList [] [] = True
eqList (x:xs) (y:ys) = eqInt x y && eqList xs ys
eqList _ _ = False

fixPointBy :: (canon -> canon -> Bool) -> (a -> canon) -> (a -> a) -> a -> a
fixPointBy eqCanon canonicalizer iter initial =
   f (canonicalizer initial) initial
  where
    f canonic value = let
        value_ = iter value
        canonic_ = canonicalizer value_
      in if eqCanon canonic canonic_ then value_ else f canonic_ value_

countingDropWhile :: (a -> Bool) -> [a] -> (PInt, [a])
countingDropWhile p = f (pNat(I# 0#))
  where
    f n (x:xs) | p x = f (succ n) xs
    f n l = (n, l)
    --f n [] = (n, [])
    --f n l@(x:xs) = if p x then f (succ n) xs else (n, l)

dropLeastSignificantZeroes
            :: FloatingInteger -> FloatingInteger
dropLeastSignificantZeroes (d,e) = case countingDropWhile isZero d of
          (de, d_) -> (d_, e `minusInt` de)
    -- requires dropLeastSignificantZeroes-effect first.
    -- hmm, is something with least-significant zeroes allowed? certainly
dropExcessPrecisionRoundingUpInMagnitude
            :: PInt -> FloatingInteger -> FloatingInteger
dropExcessPrecisionRoundingUpInMagnitude precis ie@(i@(int:_), e) =
       let nToDrop = (length i) `minusInt` precis in
        -- if the "rounding up" causes an extra digit, then
        -- the exponent remains the same, but the number is
        -- ... 0 !: 0 !: 0 !: 0 !: +-1 !: [].  Fewer least-significant
        -- zeroes can also be created by less-extreme carrying.
        -- Either way these are eliminated by dropLeastSignificantZeroes.
         if nToDrop > (pNat(I# 0#))
          then
           --should we dropLeastSignificantZeroes first too (otherwise
           --this "might" unnecessarily keep the precision long, dependently
           --on meaningless stuff, which would be bad)
           --Er, _should_ we doing weird rounding-up like this on every step
           --of Newton's? well, the alternative is implicitly rounding down ;)
           --(unless we want to really slow things down by keeping all the
           --digits, which doesn't seem necessary...)
           dropLeastSignificantZeroes --add AFTER drop?
               ( synergisticAddOnlyCarrySmall (drop nToDrop i) (signumInt int)
               , e `minusInt` nToDrop
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

--hmm. or toInteger, which must be implemented _somehow_ in Integrals?
integerFromInt :: Int -> LargeBaseLEList
integerFromInt integral =
   -- fromIntegral = fromInteger . toInteger; the toInteger's
   -- implementation is not our responsibility, as long as
   -- no exported function relies on the one we're defining here, and
   -- DInt's fromInteger::Integer->DInt doesn't depend on x->Integer either.
     if integralLargeBase_ `neqInt` intLargeBase
       --presumably the integral is small enough to fit in DInt
       -- if intLargeBase did not fit in it
       --(this might fail for "NonPositiveInteger" or so...)
     then uncheckedFromIntegral integral
     else fromIntegral_ integral
  where
   -- locally monomorphic type matches argument,
   -- and this computation is shared.  (This implementation
   -- will fail for bounded types in which intLargeBase doesn't fit,
   -- so we try to detect that problem - above.)
   integralLargeBase_ = intLargeBase
   integralIsZero_ = (I# 0# `eqInt`)
   fromIntegral_ =
     \i -> if integralIsZero_ i then [] else
             case i `quotRemInt` integralLargeBase_ of
              (# above, digit #) -> digit !: fromIntegral_ above

-- intLargeBase must fit in the Integral type - not checked.
uncheckedFromIntegral :: Int -> LargeBaseLEList
uncheckedFromIntegral integral = fromIntegral_ integral
   -- fromIntegral = fromInteger . toInteger; the toInteger's
   -- implementation is not our responsibility, as long as
   -- no exported function relies on the one we're defining here, and
   -- DInt's fromInteger::Integer->DInt doesn't depend on x->Integer either.
  where
   -- locally monomorphic type matches argument,
   -- and this computation is shared.
   integralLargeBase_ = intLargeBase
   integralIsZero_ = (I# 0# `eqInt`)
   fromIntegral_ =
     \i -> if integralIsZero_ i then [] else
             case i `quotRemInt` integralLargeBase_ of
              (# above, digit #) -> digit !: fromIntegral_ above

--Does not check for overflow. after all, bounded is not a superclass
--of num, integral or enum (consider Integer!).  However it is not as unsafe
--as uncheckedFromIntegral, because it wasn't a very meaningful operation
--anyway, when uncheckedToNum overflows.
intFromInteger :: LargeBaseLEList -> Int#
intFromInteger [] = 0#
intFromInteger integer@(_:_) = toIntegral_ integer
  where
   I# integralLargeBase_ = intLargeBase
   toIntegral_ =
     \(I# d:ds) -> case ds of
                 [] -> d
                 _  -> d +# integralLargeBase_ *# toIntegral_ ds





-- ********************** EXPORTED ********************** ...


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




-- ... and ******* INSTANCES ****** ...

--possibly could be made more efficient:
succInteger, predInteger :: HInteger__ -> HInteger__
succInteger i = addInteger i oneInteger
predInteger i = addInteger i negativeOneInteger





--"The function bitSize is undefined for types that do not have a fixed
-- bitsize, like Integer."

-- ******************** Bits DInt => Bits HInteger ********************
--    code ought to be cleaned up a lot here and commented.

signRep :: Bool -> DInt
signRep n = if n then (dNeg(I# 1#)) else (dNat(I# 0#))
type Bin a = a -> a -> a
type Mon a = a -> a
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
  f (d:ds) = d !: if (dNat(I# 0#)) <= d {-isZero (d {-.&. pred intLargeBase-})-}
                      then f ds else f_ ds
  f_ = strictMap pred
--  f_ [] = []
--  f_ (d:ds) = pred d !: if isZero (pred d .&. pred intLargeBase)
--                              then f_ ds else f_ ds
utcn i = f i
 where
  f (d:ds) = let d_ = repairNegIntLargeBase d in
               d_ `prepend` if (dNat(I# 0#)) <= d_ {-isZero d_-}
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
sr s d = if isZero s && (dNat(I# 0#)) <= d then (dNat(I# 0#)) else (dNeg(I# 1#))
lowBits, highBits :: DInt
lowBits = pred intLargeBase
highBits = complementInt lowBits
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
   repair = if nf then (\d -> repairNegIntLargeBase (highBits `orInt` d))
                  else (\d -> (lowBits `andInt` d))
--   m1 = \d -> d + s1; m2 = \d -> d + s2; mf = \d -> d - sf
--   end n i = if n then endOpWithNeg i else endOpWithNat i--}
   fi (d1:ds1) (d2:ds2) = prepend df dsf
     where
       df = repair (((d1     ) `op` (d2     ))     )
       dsf = f ds1 ds2
              (signRep ((dNat(I# 0#)) > d1))
              (signRep ((dNat(I# 0#)) > d2))
              (signRep ((dNat(I# 0#)) > df))
   fi ds1@(_:_) [] = endOpWithNat ds1
   fi [] ds2@(_:_) = endOpWithNat ds2
   fi [] [] = []
   f (d1:ds1) (d2:ds2) s1 s2 sf = prepend df dsf
     where
       df = repair (((d1 `plusInt` s1) `op` (d2 `plusInt` s2)) `minusInt` sf)
       dsf = f ds1 ds2 (sr s1 d1) (sr s2 d2) (sr sf df)
   f ds1@(_:_) [] s1 s2 sf = f1 ds1 s1 s2 sf
   f [] ds2@(_:_) s1 s2 sf = f1 ds2 s2 s1 sf
   f [] [] s1 s2 sf = f0 s1 s2 sf
   --asymptotic-efficiency-adding optimization:
   f1 i s1 s2 sf | s1 `eqInt` sf = if isZero s2 then endOpWithNat i
                                           else endOpWithNeg i
   f1 [] s1 s2 sf = f0 s1 s2 sf
   f1 (d1:ds1) s1 s2 sf = prepend df dsf
     where
       df = repair (((d1 `plusInt` s1) `op` (     s2)) `minusInt` sf)
       dsf = f1 ds1 (sr s1 d1) (   s2   ) (sr sf df)
   f0 s1 s2 sf = fromDInt df--prepend df dsf
     where
       df = repair (((     s1) `op` (     s2)) `minusInt` sf)
       --dsf = []--f [] [] (   s1   ) (   s2   ) (sr sf df)
  in fi i1 i2 --(dNat(0)) (dNat(0)) (dNat(0))
tcXOrInteger, tcOrInteger, tcAndInteger :: Bin HInteger__
tcXOrInteger = tcBinOpInteger (xorInt) (eqBool) (id) (tcComplementInteger)
tcOrInteger  = tcBinOpInteger (orInt) (||) (id) (const [])
tcAndInteger = tcBinOpInteger (andInt) (&&) (const []) (id)

eqBool True True = True
eqBool False False = True
eqBool _ _ = False

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
        [] -> fromDInt (signRep ((dNat(I# 0#)) > d))
        (_:_) -> tcShiftR_TC ds (r `minusInt` intLargeExponentOfTwo)
tcShiftR_TC [] _r = []
tcShiftR_TC i@(_:_) r = f i
  where
   l = intLargeExponentOfTwo `minusInt` r
   ourLowBits = pred (bitInt l)
   --higherBits = complement ourLowBits
   --don't overflow DInt even temporarily and even when using Bits
   --operations :) my testing int complains and there's no need to
   --higherBits = shiftR highBits l
   onlyLowerBits = pred (bitInt r)
   lowerBits = highBits `orInt` ourLowBits
   f (d:[]) = {-fromDInt-} (shiftRInt d r) !: fromDInt (signRep ((dNat(I# 0#)) > d))
   f (d:(ds@(dAbove:_))) = df !: f ds --prepend df dsf
    where
      df = {-(highBits .&. dAbove) .|.-} (lowerBits `andInt` shiftRInt d r)
          `orInt` (shiftInt (onlyLowerBits `andInt` dAbove) l)
                --(higherBits .&. shiftL dAbove l)
--      dsf = f ds

tcShiftL_TC [] _l = []
tcShiftL_TC i l | l >= intLargeExponentOfTwo
    = (dNat(I# 0#)) !: tcShiftL_TC i (l `minusInt` intLargeExponentOfTwo)
tcShiftL_TC i@(d1:_) l = dFirst !: f i--increase decrease left right
  where
   dFirst = (highBits `andInt` d1) `orInt` shiftInt (onlyLowerBits `andInt` d1) l
   r = intLargeExponentOfTwo `minusInt` l
   ourLowBits = pred (bitInt l)
   --higherBits = complement ourLowBits
   --don't overflow DInt even temporarily and even when using Bits
   --operations :) my testing int complains and there's no need to
   --higherBits = shiftR highBits l
   onlyLowerBits = pred (bitInt r)
   lowerBits = highBits `orInt` ourLowBits
   f (dBelow:[]) = {-fromDInt-} (shiftRInt dBelow r) !: []
   f (dBelow:(ds@(d:_))) = df !: f ds --prepend df dsf
    where
      df = (highBits `andInt` d) `orInt` (lowerBits `andInt` shiftRInt dBelow r)
          `orInt` (shiftInt (onlyLowerBits `andInt` d) l)
                --(higherBits .&. shiftL d l)
--      dsf = f ds
