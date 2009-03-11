module Parser where

import Data.Char
import Text.ParserCombinators.Parsec
import Control.Monad

import ExternalCore

import Printer

test :: IO ()
test = do inp <- readFile --"./hcr/Tuple.hcr"
                          "./src/ExternalCore.hcr"
          parseTest (moduleP >>= \m -> notFollowedBy anyToken >> return (ppModule m)) inp

moduleP :: Parser Module
moduleP = do keyword "module"
             pkg <- pkgname
             char ':'
             modName <- mident
             spaces
             tdefs <- try tdef `endBy` char ';'
             spaces
             vdefgs <- try vdefg `endBy` char ';'
             spaces
             return $ Module (pkg,modName) tdefs vdefgs

vdefg = choice
        [ do keyword "rec"
             braces $ liftM Rec (vdef `sepBy1` char ';')
        , do v <- vdef
             return $ Nonrec v
        ] <?> "vdefg"

vdef = do spaces
          name <- try qvar <|> do name <- lname; return ("","",name)
          spaces
          string "::"
          spaces
          t <- ty
          spaces
          char '='
          spaces
          e <- expP
          spaces
          return (False, name, t, e)
       <?> "vdef"

expP = choice
       [ do fn <- aexp
            spaces
            args <- many (try arg)
            let app e (Left t)  = Appt e t
                app e (Right a) = App e a
            return $ foldl app fn args
       , do char '\\'
            spaces
            binds <- binder `sepBy1` spaces
            spaces; string "->"; spaces
            e <- expP
            return $ foldr Lam e binds
       , try $ do keyword "case"
                  t <- aty
                  e <- expP
                  keyword "of"
                  b <- vbind
                  spaces
                  alts <- braces $ alt `sepBy1` (char ';' >> spaces)
                  return $ Case e b t alts
       , try $ do keyword "let"
                  def <- vdefg
                  keyword "in"
                  e <- expP
                  return $ Let def e
       , try $ do keyword "note"
                  char '"'
                  note <- many charP
                  char '"'
                  spaces
                  e <- expP
                  return $ Note note e
       ]

alt = choice [ do con <- qdcon
                  spaces;
                  tbinds <- (char '@' >> spaces >> tbind) `sepBy` spaces
                  vbinds <- vbind `sepBy` spaces
                  spaces; string "->"; spaces
                  e <- expP
                  return $ Acon con tbinds vbinds e
             ]

binder = choice
         [ do char '@'
              spaces
              liftM Tb tbind
         , liftM Vb vbind
         ]

arg = spaces >> choice
      [ do char '@'
           spaces
           liftM Left aty
      , liftM Right aexp
      ]

aexp = spaces >> choice
       [ try $ liftM Dcon qdcon
       , try $ liftM Var qvar
       , try $ liftM Var (lname >>= \name -> return ("","",name))
       , try $ liftM Lit lit
       , try $ parens expP
       ]

lit = choice
      [ try $ parens $
        do char '"'
           cs <- many charP
           char '"'
           spaces; string "::"; spaces
           t <- ty
           return (Lstring cs t)
      , try $ parens $
        do char '\''
           c <- charP
           char '\''
           spaces; string "::"; spaces
           t <- ty
           return (Lchar c t)
      , try $ parens $
        do m <- optional (char '-')
           ds <- many1 digit
           spaces; string "::"; spaces
           t <- ty
           return (Lint (read ds) t)
      ]

charP = choice
        [ do char '\\'
             [a,b] <- count 2 hexDigit
             return (chr $ digitToInt a*0x10+digitToInt b)
        , satisfy (\c -> c `notElem` ['\x22','\x27','\x5c'] && c >= '\x20' && c <= '\x7E')
        ]

tdef = spaces >> choice [dataP, newtypeP]
       <?> "tdef"

dataP = do keyword "data"
           name <- qtycon
           spaces
           tbinds <- many (try $ spaces >> tbind)
           spaces
           char '='
           spaces
           cdefs <- braces $ cdef `sepBy1` char ';'
           return $ Data name tbinds cdefs
        <?> "dataP"

newtypeP = do keyword "newtype"
              fail ""

cdef = do spaces
          name <- qdcon
          spaces
          tys <- aty `sepBy` spaces
          spaces
          return $ Constr name [] tys

aty = choice
      [ try $ liftM Tcon qtycon
      , liftM Tvar tyvar
      , parens ty ]

bty = do ts <- many1 (try $ spaces >> aty)
         return $ foldl1 Tapp ts

ty = choice
     [ do keyword "forall"
          spaces
          binds <- many1 (try $ spaces >> tbind)
          spaces; char '.'; spaces
          t <- ty
          return $ foldr Tforall t binds
     , try $ do a <- bty
                spaces; string "->"; spaces
                b <- ty
                return (Tapp (Tapp (Tcon tcArrow) a) b)
     , bty
     ]


tbind = choice
        [ do var <-tyvar
             return $ (var, Klifted)
        , parens $
          do var <- tyvar
             spaces
             string "::"
             spaces
             k <- kind
             return (var, k)
        ]

vbind = parens $ do v <- lname
                    spaces; string "::"; spaces
                    t <- ty
                    return (v,t)


akind = choice [ char '*' >> return Klifted
               , char '#' >> return Kunlifted
               , char '?' >> return Kopen
               , parens $ kind ]

kind = choice [ try $ do atomic <- akind
                         spaces
                         string "->"
                         spaces
                         k <- kind
                         return (Karrow atomic k)
              , akind ]

tyvar = lname

qdcon  = qual uname
qtycon = qual uname
qvar   = qual lname

qual a = do pkg <- pkgname
            char ':'
            mod <- uname
            char '.'
            t <- a
            return (pkg, mod, t)

tycon = uname

pkgname = many1 namechar

mident = uname

uname = liftM2 (:) upper (many namechar)
lname = liftM2 (:) lower (many namechar)

namechar = lower <|> upper <|> digit <|> char '\''

-- Utilities
parens = between (spaces >> char '(' >> spaces)
                 (spaces >> char ')' >> spaces)

braces = between (spaces >> char '{' >> spaces)
                 (spaces >> char '}' >> spaces)

keyword txt = spaces >> char '%' >> string txt >> spaces

a <+> b = a >> spaces >> b
spaced a = do r <- a
              spaces
              return r
