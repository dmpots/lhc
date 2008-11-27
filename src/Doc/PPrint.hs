
-- | A Pretty printing class using multiparameter type classes for
-- maximal generality with some useful instances.
--
-- the pprinted type comes as the last argument so newtype deriving can be used
-- in more places.

module Doc.PPrint where

import Doc.DocLike
import qualified Data.Map as Map


class DocLike d => PPrint d a  where
    pprint ::  a -> d
    pprintPrec :: Int -> a -> d

    pprintPrec _ a = pprint a
    pprint a = pprintPrec 0 a


    pplist    ::  [a] -> d
    pplist    xs = brackets (hcat (punctuate comma (map pprint xs)))

pprintParen :: PPrint d a => a -> d
pprintParen = pprintPrec 11

instance PPrint d a => PPrint d [a] where
    pprint  = pplist

instance DocLike d => PPrint d Char where
  pprint  = char
  pplist  = text

instance DocLike d => PPrint d Integer where
  pprint  = tshow

instance DocLike d => PPrint d Int where
  pprint  = tshow

instance DocLike d => PPrint d Float where
  pprint  = tshow

instance DocLike d => PPrint d Double where
  pprint  = tshow

instance DocLike d => PPrint d () where
    pprint () = text "()"

instance (PPrint d a, PPrint d b) => PPrint d (a,b) where
  pprint (x,y) = parens (hsep [pprint x <> comma, pprint y])

instance (PPrint d a, PPrint d b) => PPrint d (Either a b) where
  pprintPrec n (Left x)  | n <= 9  = text "Left" <+> pprintPrec 10 x
  pprintPrec n (Right x) | n <= 9  = text "Right" <+> pprintPrec 10 x
  pprintPrec _ x = parens (pprint x)

instance (PPrint d a, PPrint d b, PPrint d c) => PPrint d (a,b,c) where
  pprint (x,y,z) = parens (hsep [pprint x <> comma,
                                pprint y <> comma,
                                pprint z])

instance (PPrint d a, PPrint d b) => PPrint d (Map.Map a b) where
    pprint m = vcat [ pprint x <+> text "=>" <+> pprint y | (x,y) <- Map.toList m]


