module Main (main) where

import System.Directory (doesFileExist)
import System.Environment (getArgs)

import RayTracerParser
import RayTracerGraphics
import RayTracerFunction
import RayTracerTypes (ParserDescription(ParserDescription))

main :: IO ()
main = do
    args <- getArgs
    let len = length args
    case len of
        1 -> do
            let input = args !! 0
            fileExists <- doesFileExist input
            case fileExists of
                True -> do
                    fileContent <- readFile input
                    let (ParserDescription res depth scene) = readScene fileContent
                    renderAndDisplayImage depth res scene
                False -> error $ "Plik źródłowy '" ++ input ++ "' nie istnieje.\n"
        2 -> do
            let input = args !! 0
            let output = args !! 1
            fileExists <- doesFileExist input
            case fileExists of
                True -> do
                    fileContent <- readFile input
                    let (ParserDescription res depth scene) = readScene fileContent
                    renderAndSaveImage output depth res scene
                    putStr $ "Zapisano obraz do pliku " ++ output ++ ".\n"
                False -> error $ "Plik źródłowy '" ++ input ++ "' nie istnieje.\n"
        _ -> error "rayTracer <nazwa_pliku_ze_sceną> <nazwa_pliku_do_zapisu> (drugi argument jest opcjonalny)\n"
