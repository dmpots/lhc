{-# LANGUAGE NoMonomorphismRestriction #-}
-----------------------------------------------------------
-- Daan Leijen (c) 2000, http://www.cs.uu.nl/~daan
--
--
-- Pretty print module based on Philip Wadlers "prettier printer"
--      "A prettier printer"
--      Draft paper, April 1997, revised March 1998.
--      http://cm.bell-labs.com/cm/cs/who/wadler/papers/prettier/prettier.ps
--
-- Haskell98 compatible
-----------------------------------------------------------

module Doc.Pretty
        ( Doc

        , putDoc, hPutDoc
        , putDocM, putDocMLn
        --, (<>)
        --, (<+>)
        , (</>), (<//>)
        --, (<$>)
        , (<$$>)

        , sep, fillSep, hsep, vsep
        , cat, fillCat, hcat, DocLike.vcat

        , align, hang, indent
        , fill, fillBreak
        , errorDoc, failDoc

       -- , string, bool, int, integer, float, double, rational

        , softline, softbreak
        , line, linebreak, nest, group
        , column, nesting, width
          
        , SimpleDoc(..)
        , renderPretty, renderCompact
        , displayS, displayIO, displayM
        ) where

import Doc.DocLike hiding(empty)
import qualified Doc.DocLike as DocLike

import Text.PrettyPrint.ANSI.Leijen hiding (hsep, hcat)
import System.Console.ANSI


errorDoc :: Doc -> a
errorDoc = error . ('\n':) . show

failDoc :: Monad m => Doc -> m a
failDoc = fail . ('\n':) . show

displayM :: Monad m => (String -> m ()) -> SimpleDoc -> m ()
displayM putStr simpleDoc = display simpleDoc where
      display SEmpty        = return ()
      display (SChar c x)   = do{ putStr [c]; display x}
      display (SText l s x) = do{ putStr s; display x}
      display (SLine i x)   = do{ putStr ('\n':indentation i); display x}
      display (SSGR sgr x)  = do putStr (setSGRCode sgr); display x

putDocM :: Monad m => (String -> m ()) -> Doc -> m ()
putDocM putStr d = displayM putStr (renderPretty 0.4 80 d)

putDocMLn :: Monad m => (String -> m ()) -> Doc -> m ()
putDocMLn putStr d = displayM putStr (renderPretty 0.4 80 d) >> putStr "\n"


-----------------------------------------------------------
-- insert spaces
-- "indentation" used to insert tabs but tabs seem to cause
-- more trouble than they solve :-)
-----------------------------------------------------------
spaces n        | n <= 0    = ""
                | otherwise = replicate n ' '

indentation n   = spaces n

