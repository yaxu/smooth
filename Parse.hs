{-# OPTIONS_GHC -XTypeSynonymInstances -XOverlappingInstances -XIncoherentInstances -XOverloadedStrings -XFlexibleInstances #-}

module Parse where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language ( haskellDef )
import Pattern
import Data.Ratio

import GHC.Exts( IsString(..) )

class Parseable a where
  p :: String -> Pattern a

instance Parseable Double where
  p = parseRhythm pDouble

instance Parseable String where
  p = parseRhythm pVocable

instance Parseable Bool where
  p = parseRhythm pBool

instance Parseable Int where
  p = parseRhythm pInt

-- type ColourD = Colour Double

-- instance Parseable ColourD where
--     p = parseRhythm pColour

instance (Parseable a) => IsString (Pattern a) where
  fromString = p

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


r :: Parseable a => String -> Pattern a -> IO (Pattern a)
r s orig = do catch (return $ p s)
                (\err -> do putStrLn (show err)
                            return orig
                )

parseRhythm :: Parser (Pattern a) -> String -> (Pattern a)
parseRhythm f input = either (const silence) id $ parse (pRhythm f') "" input
  where f' = f
             <|> do symbol "~" <?> "rest"
                    return silence


pRhythm :: Parser (Pattern a) -> GenParser Char () (Pattern a)
pRhythm f = do spaces
               pSequence f

pSequence :: Parser (Pattern a) -> GenParser Char () (Pattern a)
pSequence f = do --x <-pReps
                 ps <- many $ pPart f
                 --let p = Arc (cat ps) 0 1 x
                 return $ cat ps

pPart :: Parser (Pattern a) -> Parser (Pattern a)
pPart f = do part <- parens (pSequence f) <|> f <|> pPoly f
             spaces
             return part

pPoly :: Parser (Pattern a) -> Parser (Pattern a)
pPoly f = do ps <- brackets (pRhythm f `sepBy` symbol ",")
             return $ combine ps

pString :: Parser (String)
pString = many1 (letter <|> oneOf "0123456789" <|> char '/') <?> "string"

pVocable :: Parser (Pattern String)
pVocable = do v <- pString
              return $ atom v

pDouble :: Parser (Pattern Double)
pDouble = do nf <- intOrFloat <?> "float"
             let f = either fromIntegral id nf
             return $ atom f

pBool :: Parser (Pattern Bool)
pBool = do oneOf "t1"
           return $ atom True
        <|>
        do oneOf "f0"
           return $ atom False

pInt :: Parser (Pattern Int)
pInt = do i <- natural <?> "integer"
          return $ atom (fromIntegral i)

pRatio :: Parser (Rational)
pRatio = do n <- natural <?> "numerator"
            d <- do char '/'
                    natural <?> "denominator"
                 <|>
                 do return 1
            return $ n % d

pReps :: Parser (Rational)
pReps = angles (pRatio <?> "ratio")
        <|>
        do return (1 % 1)

