module Name.Name(
    NameType(..),
    Name(..),
    nameName,
    nameType,
    getModule,
    toUnqualified,
    qualifyName,
    ToName(..),
    fromTypishHsName,
    fromValishHsName,
    parseName,
    ffiExportName,
    isConstructorLike,
--    toId,
--    fromId,
    Module,
    isTypeNamespace,
    isValNamespace,
    mainModule,
    nameParts,
    mapName,
    setModule
    ) where

import Data.Char
import Data.Typeable

import StringTable.Atom
import Data.Binary
import C.FFI
import Doc.DocLike
import Doc.PPrint
import GenUtil
import FrontEnd.HsSyn

import Data.Word
import Data.ByteString (ByteString)

import Debug.Trace

data NameType =
    TypeConstructor
    | DataConstructor
    | ClassName
    | TypeVal
    | Val
    | SortName
    | FieldLabel
    | RawType
    deriving(Ord,Eq,Enum,Read,Show)


data Name = Name { nameAtom   :: Atom
                 , nameHash   :: Word32
                 , nameType   :: NameType
                 , nameModule :: Maybe String
                 , nameIdent  :: String
                 }
    deriving(Typeable) --,Eq,Binary,ToAtom,FromAtom)

nameString :: Name -> ByteString
nameString = fromAtom . nameAtom

instance Binary Name where
    put = put . nameAtom
    get = fmap fromAtom get

instance FromAtom Name where
    fromAtom atom = let (nt, mod, i) = genNameParts atom
                        name = Name { nameAtom = atom
                                    , nameHash = hash32 atom
                                    , nameType = nt
                                    , nameModule = mod
                                    , nameIdent = i }
                    in name
instance ToAtom Name where
    toAtom = nameAtom

instance Eq Name where
    a == b = (nameHash a, nameAtom a) == (nameHash b, nameAtom b)
    a /= b = (nameHash a, nameAtom a) /= (nameHash b, nameAtom b)

instance Ord Name where
   a `compare` b = case nameHash a `compare` nameHash b of
                     EQ -> if nameAtom a == nameAtom b
                           then EQ
                           else nameString a `compare` nameString b
                     other -> other

isTypeNamespace TypeConstructor = True
isTypeNamespace ClassName = True
isTypeNamespace TypeVal = True
isTypeNamespace _ = False

isValNamespace DataConstructor = True
isValNamespace Val = True
isValNamespace _ = False

isConstructorLike xs@(x:_) =  isUpper x || x `elem` ":("  || xs == "->"
isConstructorLike [] = error "isConstructorLike: empty"

fromTypishHsName, fromValishHsName :: HsName -> Name
fromTypishHsName name
    | isUpper x || x `elem` ":(" = toName TypeConstructor name
    | otherwise = toName TypeVal name
    where (x:_) = (hsIdentString . hsNameIdent  $ name)
fromValishHsName name
    | isUpper x || x `elem` ":(" = toName DataConstructor name
    | otherwise = toName Val name
    where (x:_) = (hsIdentString . hsNameIdent  $ name)

createName _ "" i = error $ "createName: empty module "  ++ i
createName _ m "" = error $ "createName: empty ident "   ++ m
createName t m i = fromAtom $  toAtom $ (chr $  ord '1' + fromEnum t):m ++ ";" ++ i
createUName :: NameType -> String -> Name
createUName _ "" = error $ "createUName: empty ident"
createUName t i =  fromAtom $ toAtom $ (chr $ fromEnum t + ord '1'):";" ++ i

class ToName a where
    toName :: NameType -> a -> Name
    fromName :: Name -> (NameType, a)

instance ToName HsName where
    toName nt n = m where
        i = hsIdentString $ hsNameIdent n
        m | Qual (Module m) _ <- n = createName nt m i
          | otherwise = createUName nt i
    fromName n = (nameType n, nameName n)

instance ToName (String,String) where
    toName nt (m,i) = createName nt m i
    fromName n = case nameParts n of
            (nt,Just m,i) -> (nt,(m,i))
            (nt,Nothing,i) -> (nt,("",i))

instance ToName (Maybe String,String) where
    toName nt (Just m,i) = createName nt m i
    toName nt (Nothing,i) = createUName nt i
    fromName n = case nameParts n of
        (nt,a,b) -> (nt,(a,b))

instance ToName (Maybe Module,String) where
    toName nt (Just (Module m),i) = createName nt m i
    toName nt (Nothing,i) = createUName nt i
    fromName n = case nameParts n of
        (nt,a,b) -> (nt,(fmap Module a,b))

instance ToName String where
    toName nt i = createUName nt i
    fromName n = (nameType n, mi ) where
        mi = case snd $ fromName n of
            (Just m,i) -> m ++ "." ++ i
            (Nothing,i) -> i


getModule :: Monad m => Name -> m Module
getModule n = case nameParts n of
    (_,Just m,_)  -> return (Module m)
    _ -> fail "Name is unqualified."

toUnqualified :: Name -> Name
toUnqualified n = case nameParts n of
    (_,Nothing,_) -> n
    (t,Just _,i) -> toName t i

qualifyName :: Module -> Name -> Name
qualifyName m n = case nameParts n of
    (t,Nothing,n) -> toName t (Just m, n)
    _ -> n

setModule :: Module -> Name -> Name
setModule m n = qualifyName m  $ toUnqualified n


parseName :: NameType -> String -> Name
parseName t name = toName t (intercalate "." ms, intercalate "." (ns ++ [last sn])) where
    sn = (split (== '.') name)
    (ms,ns) = span validMod (init sn)
    validMod (c:cs) = isUpper c && all (\c -> isAlphaNum c || c `elem` "_'") cs
    validMod _ = False


genNameType :: Atom -> NameType
genNameType a = toEnum $ fromIntegral ( a `unsafeByteIndex` 0) - ord '1'

nameName :: Name -> HsName
nameName (Name _ _ _ (Just mod) i)
    = Qual (Module mod) (HsIdent i)
nameName (Name _ _ _ Nothing i)
    = UnQual $ HsIdent i

nameParts :: Name -> (NameType, Maybe String, String)
nameParts name = (nameType name, nameModule name, nameIdent name)

genNameParts :: Atom -> (NameType,Maybe String,String)
genNameParts n = f $ tail (fromAtom n) where
    f (';':xs) = (genNameType n,Nothing,xs)
    f xs = (genNameType n,Just a,b) where
        (a,_:b) = span (/= ';') xs

instance Show Name where
    show a = show $ nameName a

instance DocLike d => PPrint d Name  where
    pprint n = text (show n)

mapName :: (String -> String,String -> String) -> Name -> Name
mapName (f,g) n = case nameParts n of
    (nt,Nothing,i) -> toName nt (g i)
    (nt,Just m,i) -> toName nt (Just (f m :: String),g i)

mainModule = Module "Main@"

ffiExportName :: FfiExport -> Name
ffiExportName (FfiExport cn _ cc) = toName Val ("FE@", show cc ++ "." ++ cn)

