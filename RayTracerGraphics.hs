module RayTracerGraphics (renderAndDisplayImage, renderAndSaveImage) where

import qualified Graphics.Image as Im

import RayTracerFunction (rayTracer)
import RayTracerTypes


figureOutColors :: Depth -> Resolution -> Scene -> [[Color]]
figureOutColors depth res@(rx,ry) scene =
    [[figureColor (fromIntegral x, fromIntegral y) | x <- [0..(rx-1)]] | y <- [0..(ry-1)]]
    where
        figureColor = rayTracer depth res scene

changeColorsToPixels :: [[Color]] -> [[Im.Pixel Im.RGB Double]]
changeColorsToPixels colorsList =
    map toPixel colorsList
    where
        toPixel = map (\ (r,g,b) -> Im.PixelRGB r g b)

createPicture :: Depth -> Resolution -> Scene -> Im.Image Im.VU Im.RGB Double
createPicture depth resolution scene =
    Im.fromLists pixelsList
    where
        colorsList = figureOutColors depth resolution scene
        pixelsList = changeColorsToPixels colorsList

renderAndDisplayImage :: Depth -> Resolution -> Scene -> IO()
renderAndDisplayImage depth resolution scene =
    Im.displayImage $ createPicture depth resolution scene

renderAndSaveImage :: FilePath -> Depth -> Resolution -> Scene -> IO()
renderAndSaveImage path depth resolution scene =
    Im.writeImage path $ createPicture depth resolution scene
