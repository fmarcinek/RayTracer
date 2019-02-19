module RayTracerParser (readScene) where

import RayTracerTypes

import Data.Char (isAlpha, isDigit, isSpace)

data Token
    = TokenInt Int
    | TokenDouble Double
    | TokenResolution
    | TokenDepth
    | TokenBackground
    | TokenFocus
    | TokenDimension
    | TokenObjects
    | TokenPlane
    | TokenSphere
    | TokenDiffuse
    | TokenSpecular
    | TokenLights
    | TokenPointLight
    | TokenAmbientLight deriving (Show)


lexer :: String -> [Token]
lexer [] = []
lexer (x:xs)
      | isSpace x = lexer xs
      | isAlpha x = lexWord (x:xs)
      | isDigit x = lexNum (x:xs) 1
lexer ('-':xs) = lexNum xs (-1)


lexWord xs =
    case span isAlpha xs of
        ("resolution", text)    -> TokenResolution : lexer text
        ("depth", text)         -> TokenDepth : lexer text
        ("background", text)    -> TokenBackground : lexer text
        ("focus", text)         -> TokenFocus : lexer text
        ("dimension", text)     -> TokenDimension : lexer text
        ("objects", text)       -> TokenObjects : lexer text
        ("lights", text)        -> TokenLights : lexer text
        ("Am", text)            -> TokenAmbientLight : lexer text
        ("Pp", text)            -> TokenPointLight : lexer text
        ("P", text)             -> TokenPlane : lexer text
        ("S", text)             -> TokenSphere : lexer text
        ("d", text)             -> TokenDiffuse : lexer text
        ("s", text)             -> TokenSpecular : lexer text

lexNum xs sign =
    case text1 of
        ('.':rest)  -> TokenDouble (sign * (read (num1 ++ ['.'] ++ num2) :: Double)) : lexer text2
            where
                (num2, text2) = span isDigit rest
        otherwise   -> TokenInt (round (sign * (read num1))) : lexer text1
        where
            (num1, text1) = span isDigit xs


parser :: [Token] -> ParserDescription
parser = (scene . depth . resolution)

resolution tokens
    | [TokenResolution, TokenInt n1, TokenInt n2] <- take 3 tokens
            = (tokensRest, ParserDescription (n1, n2))
    | otherwise = error "Nieprawidłowy format: resolution"
    where
        tokensRest = drop 3 tokens

depth (tokens, parsDescr)
    | [TokenDepth, TokenInt n] <- take 2 tokens
            = (tokensRest, parsDescr n)
    | otherwise = error "Nieprawidłowy format: depth"
    where
        tokensRest = drop 2 tokens

scene (tokens, parsDescr) = parsDescr sceneDescr
    where
        sceneDescr = (lights . objects . observer . background) tokens

background tokens
    | [TokenBackground, TokenDouble c1,
       TokenDouble c2, TokenDouble c3] <- take 4 tokens
            = (tokensRest, Scene (c1, c2, c3))
    | otherwise = error "Nieprawidłowy format: background"
    where
        tokensRest = drop 4 tokens

observer (tokens, sceneDescr) = (tokensRest, sceneDescr obsDescr)
    where
        (tokensRest, obsDescr) = (dimension . focus) tokens

focus tokens
    | [TokenFocus, TokenDouble x,
       TokenDouble y, TokenDouble z] <- take 4 tokens
            = (tokensRest, Observer (x, y, z))
    | otherwise = error "Nieprawidłowy format: focus"
    where
        tokensRest = drop 4 tokens

dimension (tokens, obsDescr)
    | [TokenDimension, TokenDouble x,  TokenDouble y,  TokenDouble z,
       TokenDouble v1, TokenDouble v2, TokenDouble v3,
       TokenDouble u1, TokenDouble u2, TokenDouble u3] <- take 10 tokens
            = (tokensRest, obsDescr ((x,y,z), (v1,v2,v3), (u1,u2,u3)))
    | otherwise = error "Nieprawidłowy format: dimension"
    where
        tokensRest = drop 10 tokens

objects ((tok:tokens), sceneDescr) =
    case tok of
        TokenObjects -> (tokensRest, sceneDescr objDescr)
        otherwise    -> ((tok:tokens), sceneDescr [])
    where
        (tokensRest, objDescr) = parseObj tokens []

parseObj :: [Token] -> [TexturedObject] -> ([Token], [TexturedObject])
parseObj (obj : tokens) acc =
    case obj of
        TokenSphere -> parseObj (drop 8 tokens) (sphere tokens : acc)
        TokenPlane  -> parseObj (drop 8 tokens) (plane tokens : acc)
        otherwise   -> ((obj:tokens), acc)

sphere tokens
    | [TokenDouble x,  TokenDouble y,
       TokenDouble z, TokenDouble rad] <- take 4 tokens
            = (Sphere (x,y,z) rad, tex)
    | otherwise = error "Nieprawidłowy format obiektu sfery"
    where
        tex = texture $ drop 4 tokens

plane tokens
    | [TokenDouble a, TokenDouble b,
       TokenDouble c, TokenDouble d] <- take 4 tokens
            = (Plane a b c d, tex)
    | otherwise = error "Nieprawidłowy forma obiektu płaszczyzny"
    where
        tex = texture $ drop 4 tokens

texture :: [Token] -> Texture
texture tokens
    | [texToken, TokenDouble c1,
       TokenDouble c2, TokenDouble c3] <- take 4 tokens
            = case texToken of
                TokenDiffuse  -> Diffuse (c1, c2, c3)
                TokenSpecular -> Specular (c1, c2, c3)
                otherwise     -> err
    | otherwise = err
    where
        err = error "Nieprawidłowy format tekstury obiektu"


lights ((tok:tokens), sceneDescr) =
    case tok of
        TokenLights -> sceneDescr lightDescr
        otherwise   -> sceneDescr []
    where
        lightDescr = parseLight tokens []

parseLight :: [Token] -> [Light] -> [Light]
parseLight [] acc = acc
parseLight (light : tokens) acc =
    case light of
        TokenAmbientLight -> parseLight (drop 3 tokens) (ambientLight tokens : acc)
        TokenPointLight   -> parseLight (drop 6 tokens) (pointLight tokens : acc)
        otherwise         -> error "Nieprawidłowy koniec pliku"

ambientLight tokens
    | [TokenDouble t1, TokenDouble t2, TokenDouble t3] <- take 3 tokens
            = AmbientLight (t1, t2, t3)
    | otherwise = error "Nieprawidłowy format: ambient light"

pointLight tokens
    | [TokenDouble x,  TokenDouble y,  TokenDouble z,
       TokenDouble t1, TokenDouble t2, TokenDouble t3] <- take 6 tokens
            = PointLight (x, y, z) (t1, t2, t3)
    | otherwise = error "Nieprawidłowy format: point light"


readScene :: String -> ParserDescription
readScene = parser . lexer
