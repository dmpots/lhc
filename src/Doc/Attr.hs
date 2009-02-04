module Doc.Attr(Attr(..), attrEmpty, ansi, html)  where

import Text.PrettyPrint.ANSI.Leijen

-- FIXME: Use a datatype for the color.
data Attr = Attr { attrBold :: Doc -> Doc
                 , attrColor :: String -> Doc -> Doc
                 }

attrEmpty :: Attr
attrEmpty = Attr { attrBold = id, attrColor = \_ -> id }


ansi,html :: Attr

ansi = attrEmpty {
    attrBold = \x -> bold x,
    attrColor = \c x -> ansiColor c x
        }

html = attrEmpty {
    attrBold = \x -> text "<b style=\"color: white\">" <> x <> text "</b>",
    attrColor = \c x -> text ("<span style=\"color: " ++ c ++ ";\">") <> x <> text "</span>"
        }


ansiColor :: String -> Doc -> Doc
ansiColor "black"       = dullblack -- "0;30"
ansiColor "red"         = dullred -- "0;31"
ansiColor "green"       = dullgreen -- "0;32"
ansiColor "yellow"      = dullyellow -- "0;33"
ansiColor "blue"        = dullblue -- "0;94"
ansiColor "magenta"     = dullmagenta -- "0;35"
ansiColor "cyan"        = dullcyan -- "0;36"
ansiColor "white"       = dullwhite -- "0;37"
ansiColor "lightgreen"  = green -- "0;92"
ansiColor "lightred"    = red -- "0;91"
ansiColor "brightblue"  = blue -- "0;94"
ansiColor _ = id -- "0"

attrClear :: String
attrClear = "\27[0m"


