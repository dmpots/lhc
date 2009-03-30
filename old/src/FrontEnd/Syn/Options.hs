module FrontEnd.Syn.Options(parseOptions) where


import Text.ParserCombinators.ReadP
import Char
import List



parseOptions :: String -> [(String,String)]
parseOptions s = case readP_to_S parse s of
    os -> head $ sortBy (\x y -> compare (negate $ length x) (negate $ length y)) [ x | (x,_) <- os ]

token :: ReadP a -> ReadP a
token x = x >>= \r -> spaces >> return r

parse :: ReadP [(String, String)]
parse = do
    spaces
    many (token pragma)


spaces :: ReadP ()
spaces = do
    skipSpaces
    optional (comment >> spaces)


pragma :: ReadP (String, String)
pragma = do
    string "{-#"
    skipSpaces
    nn <- munch1 (\c -> isAlpha c || c == '_')
    skipSpaces
    body <- manyTill get (string "#-}")
    return $ (nn,body)


comment :: ReadP ()
comment = plone +++ pline +++ line +++ block where
    line = do
        string "--"
        manyTill get (char '\n')
        return ()
    pline = do
        string "# "
        manyTill get (char '\n')
        return ()
    plone = do
        string "#line "
        manyTill get (char '\n')
        return ()
    block = do
        string "{-"
        satisfy (/= '#')
        manyTill get (string "-}")
        return ()



