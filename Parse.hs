{-# OPTIONS_GHC -XTypeSynonymInstances -XOverlappingInstances -XIncoherentInstances -XOverloadedStrings -XFlexibleInstances #-}

module Parse where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language ( haskellDef )
import Pattern
import Data.Ratio
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB
import GHC.Exts( IsString(..) )

class (Pattern p) => ParseablePattern p where
  parsePattern :: Parseable a => String -> p a

instance ParseablePattern Sequence where
  parsePattern = p

instance ParseablePattern Signal where
  parsePattern = toSignal . p

class Parseable a where
  p :: String -> Sequence a

instance Parseable Double where
  p = parseRhythm pDouble

instance Parseable String where
  p = parseRhythm pVocable

instance Parseable Bool where
  p = parseRhythm pBool

instance Parseable Int where
  p = parseRhythm pInt

type ColourD = Colour Double

instance Parseable ColourD where
  p = parseRhythm pColour

instance (ParseablePattern p, Parseable a) => IsString (p a) where
  fromString = parsePattern

--instance (Parseable a, Pattern p) => IsString (p a) where
--  fromString = p :: String -> p a

lexer   = P.makeTokenParser haskellDef
braces  = P.braces lexer
brackets = P.brackets lexer
parens = P.parens lexer
angles = P.angles lexer
symbol  = P.symbol lexer
natural = P.natural lexer
float = P.float lexer
naturalOrFloat = P.naturalOrFloat lexer

data Sign      = Positive | Negative

applySign          :: Num a => Sign -> a -> a
applySign Positive =  id
applySign Negative =  negate

sign  :: Parser Sign
sign  =  do char '-'
            return Negative
         <|> do char '+'
                return Positive
         <|> return Positive

intOrFloat :: Parser (Either Integer Double)
intOrFloat =  do s   <- sign
                 num <- naturalOrFloat
                 return (case num of
                            Right x -> Right (applySign s x)
                            Left  x -> Left  (applySign s x)
                        )

r :: Parseable a => String -> Sequence a -> IO (Sequence a)
r s orig = do catch (return $ p s)
                (\err -> do putStrLn (show err)
                            return orig
                )

parseRhythm :: Parser (Sequence a) -> String -> (Sequence a)
parseRhythm f input = either (const silence) id $ parse (pRhythm f') "" input
  where f' = f
             <|> do symbol "~" <?> "rest"
                    return silence

pRhythm :: Parser (Sequence a) -> GenParser Char () (Sequence a)
pRhythm f = do spaces
               pSequence f

pSequence :: Parser (Sequence a) -> GenParser Char () (Sequence a)
pSequence f = do d <- pDensity
                 ps <- many $ pPart f
                 return $ density d $ cat ps

pPart :: Parser (Sequence a) -> Parser (Sequence a)
pPart f = do part <- parens (pSequence f) <|> f <|> pPoly f
             spaces
             return part

pPoly :: Parser (Sequence a) -> Parser (Sequence a)
pPoly f = do ps <- brackets (pRhythm f `sepBy` symbol ",")
             spaces
             m <- pMult
             return $ density m $ combine ps

pString :: Parser (String)
pString = many1 (letter <|> oneOf "0123456789" <|> char '/') <?> "string"

pVocable :: Parser (Sequence String)
pVocable = do v <- pString
              return $ atom v

pDouble :: Parser (Sequence Double)
pDouble = do nf <- intOrFloat <?> "float"
             let f = either fromIntegral id nf
             return $ atom f

pBool :: Parser (Sequence Bool)
pBool = do oneOf "t1"
           return $ atom True
        <|>
        do oneOf "f0"
           return $ atom False

pInt :: Parser (Sequence Int)
pInt = do i <- natural <?> "integer"
          return $ atom (fromIntegral i)

pColour :: Parser (Sequence ColourD)
pColour = do name <- many1 letter <?> "colour name"
             colour <- readColourName name <?> "known colour"
             return $ atom colour

pMult :: Parser (Rational)
pMult = do char '*'
           spaces
           r <- pRatio
           return r
        <|>
        do char '/'
           spaces
           r <- pRatio
           return $ 1 / r
        <|>
        return 1
           

pRatio :: Parser (Rational)
pRatio = do n <- natural <?> "numerator"
            d <- do oneOf "/%"
                    natural <?> "denominator"
                 <|>
                 return 1
            return $ n % d

pDensity :: Parser (Rational)
pDensity = angles (pRatio <?> "ratio")
           <|>
           return (1 % 1)

