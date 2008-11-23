{-# LANGUAGE ForeignFunctionInterface, TypeSynonymInstances, DeriveDataTypeable #-}
module StringTable.Atom(
    Atom(),
    ToAtom(..),
    FromAtom(..),
    HasHash(..),
    intToAtom,
    isValidAtom,
    unsafeIntToAtom,
    atomCompare,
    unsafeByteIndex,
    dumpTable,
    dumpToFile,
    dumpStringTableStats
    ) where

#include "StringTable_cbits.h"

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe as BS
import Data.ByteString.UTF8 as BSUTF8
import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Foreign
import Foreign.Marshal
import Data.Word
import Data.Char
import Foreign.C
import Data.Monoid
import Data.Dynamic
import PackedString(PackedString(..))
import Data.Bits



newtype Atom = Atom (#type atom_t)
    deriving(Typeable,Eq,Ord)

class FromAtom a where
    fromAtom :: Atom -> a
    fromAtomIO :: Atom -> IO a

    fromAtomIO a = return (fromAtom a)
    fromAtom a = unsafePerformIO (fromAtomIO a)

class ToAtom a where
    toAtom :: a -> Atom
    toAtomIO :: a -> IO Atom

    toAtomIO a = return (toAtom a)
    toAtom a = unsafePerformIO (toAtomIO a)

class HasHash a where
    hash32 :: a -> Word32

instance HasHash Atom where
    hash32 a = let (x,y) = fromAtom a :: CStringLen in unsafePerformIO $ hash2 0 x (fromIntegral y)

instance HasHash BS.ByteString where
    hash32 bs = unsafePerformIO $ do
        BS.unsafeUseAsCStringLen bs $ \ (x,y) -> hash2 0 x (fromIntegral y)

instance HasHash String where
    hash32 s = unsafePerformIO $ withCStringLen s $ \ (x,y) -> hash2 0 x (fromIntegral y)

instance FromAtom (String -> String) where
    fromAtom x = shows (fromAtom x :: String)

instance ToAtom PackedString where
    toAtomIO (PS x) = toAtomIO x
instance FromAtom PackedString where
    fromAtomIO atom = PS `liftM` fromAtomIO atom



instance ToAtom Atom where
    toAtom x = x

instance FromAtom Atom where
    fromAtom x = x

instance ToAtom Char where
    toAtom x = toAtom [x]

instance ToAtom CStringLen where
    toAtomIO (cs,len) = do
        if (len > (#const MAX_ENTRY_SIZE))
            then fail "StringTable: atom is too big"
            else stAdd cs (fromIntegral len)



instance ToAtom CString where
    toAtomIO cs = do
        len <- BS.c_strlen cs
        toAtomIO (cs,fromIntegral len :: Int)

instance ToAtom String where
    toAtomIO s = toAtomIO (BSUTF8.fromString s)

instance FromAtom String where
    fromAtom = BSUTF8.toString . fromAtom

instance ToAtom BS.ByteString where
    toAtomIO bs = BS.unsafeUseAsCStringLen bs toAtomIO

instance FromAtom CStringLen where
    fromAtom a@(Atom v) = (stPtr a,fromIntegral $ (v `shiftR` (#const ATOM_LEN_SHIFT)) .&. (#const ATOM_LEN_MASK))

instance FromAtom Word where
    fromAtom (Atom i) = fromIntegral i

instance FromAtom Int where
    fromAtom (Atom i) = fromIntegral i

instance FromAtom BS.ByteString where
    fromAtomIO a = do
        sl <- fromAtomIO a :: IO CStringLen
        BS.unsafePackCStringLen sl

instance Monoid Atom where
    mempty = toAtom BS.empty
    mappend x y = unsafePerformIO $ atomAppend x y

instance Show Atom where
    showsPrec _ atom = (fromAtom atom ++)

instance Read Atom where
    readsPrec _ s = [ (toAtom s,"") ]

intToAtom :: Monad m => Int -> m Atom
intToAtom i = if isValidAtom i then return (Atom $ fromIntegral i) else fail $ "intToAtom: " ++ show i

isValidAtom :: Int -> Bool
isValidAtom i = odd i

unsafeIntToAtom :: Int -> Atom
unsafeIntToAtom x = Atom (fromIntegral x)

unsafeByteIndex :: Atom -> Int -> Word8
unsafeByteIndex atom off = fromIntegral (unsafePerformIO $ peek (stPtr atom `advancePtr` off))

foreign import ccall unsafe "stringtable_lookup" stAdd :: CString -> CInt -> IO Atom
foreign import ccall unsafe "stringtable_ptr" stPtr :: Atom -> CString
foreign import ccall unsafe "stringtable_stats" dumpStringTableStats :: IO ()
foreign import ccall unsafe "dump_table" dumpTable :: IO ()
foreign import ccall unsafe "atom_append" atomAppend :: Atom -> Atom -> IO Atom
foreign import ccall unsafe "lexigraphic_compare" c_atomCompare :: Atom -> Atom -> CInt
foreign import ccall unsafe "dump_to_file" dumpToFile :: IO ()
foreign import ccall unsafe hash2  :: Word32 -> CString -> CInt -> IO Word32

atomCompare a b = if c == 0 then EQ else if c > 0 then GT else LT where
    c = c_atomCompare a b

instance Binary Atom where
    get = do
        x <- getWord8
        bs <- getBytes (fromIntegral x)
        return $ toAtom bs
    put a = do
        let bs = fromAtom a
        putWord8 $ fromIntegral $ BS.length bs
        putByteString bs
