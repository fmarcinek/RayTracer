module RayTracerVector where

import RayTracerTypes

-- Poniższe operatory mogą działać zarówno na punktach 3D, jak i na wektorach
(<+>) :: (Double,Double,Double) -> (Double,Double,Double) -> (Double,Double,Double)
(x0,y0,z0) <+> (x1,y1,z1) = (x0+x1, y0+y1, z0+z1)

(<->) :: (Double,Double,Double) -> (Double,Double,Double) -> (Double,Double,Double)
(x0,y0,z0) <-> (x1,y1,z1) = (x0-x1,y0-y1,z0-z1)

(<**>) :: (Double,Double,Double) -> (Double,Double,Double) -> (Double,Double,Double)
(x0,y0,z0) <**> (x1,y1,z1) = (x0*x1,y0*y1,z0*z1)

(>*) :: (Double,Double,Double) -> Double -> (Double,Double,Double)
(x,y,z) >* a = (x*a,y*a,z*a)

(.*) :: (Double,Double,Double) -> (Double,Double,Double) -> Double
(x0,y0,z0) .* (x1,y1,z1) = x0*x1 + y0*y1 + z0*z1

-- Tworzy wektor długości 1 z a do b
createNormVect :: Point3D -> Point3D -> Vector
createNormVect a b = normalize $ b <-> a

-- Oblicza normę wektora
norm :: Vector -> Double
norm v = sqrt (v .* v)

-- Tworzy wektor normalny do obiektu w danym punkcie
normal :: Point3D -> Object -> Vector
normal p (Sphere cen rad) = normalize ((p <-> cen) >* (1/rad))
normal _ (Plane a b c _) = normalize (a,b,c)

-- Normalizuje wektor
normalize :: Vector -> Vector
normalize v
    | (vNorm < ɛ) = (0.0, 0.0, 0.0)
    | otherwise   = v >* (1 / vNorm)
    where
        vNorm = norm v
