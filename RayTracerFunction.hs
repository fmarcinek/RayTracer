module RayTracerFunction (rayTracer) where

import Data.Maybe

import RayTracerVector
import RayTracerTypes


-- Dla pewności domykamy wartości koloru RGB funkcją closure
closure :: Color -> Color
closure (r, g, b) =
    (f r, f g, f b)
    where f = (max 0.0 . min 1.0)

-- Funkcja obliczająca punkt w 3D na wirtualnej matrycy, przez który ma przejść promień dla danego piksela
mapPixelTo3D :: Resolution -> Dimension -> Point2D -> Point3D
mapPixelTo3D (rw,rh) (lg,w,h) (px,py) = lg <+> xVect <+> yVect
    where
        (rwD, rhD) = (fromIntegral rw, fromIntegral rh)
        (pxD, pyD) = (fromIntegral px, fromIntegral py)
        xVect = w >* ((pxD + 0.5)/rwD)
        yVect = h >* ((pyD + 0.5)/rhD)

-- Funkcja obliczająca odległość między punktami w 3D
distance :: Point3D -> Point3D -> Double
distance (x0,y0,z0) (x1,y1,z1) =
    sqrt $ (x0 - x1)**2 + (y0 - y1)**2 + (z0 - z1)**2

-- Funkcja tworząca promień
createRay :: Point3D -> Point3D -> Ray
createRay p0 p1 = Ray p0 $ normalize (p1 <-> p0)

-- Funkcja rozwiązująca równanie kwadratowe
solveQuadEq :: (Double, Double, Double) -> [Double]
solveQuadEq (a, b, c)
    | (d < 0)   = []
    | (d == 0)  = [-b / (2*a)]
    | otherwise = [(-b + sqrt d)/(2*a), (-b - sqrt d)/(2*a)]
    where
        d = b**2 - 4*a*c

-- Funkcja najmniejszą odległość do punktu przecięcia promienia z obiektem 3D
closestIntersectionWithObject :: Ray -> Object -> Double
closestIntersectionWithObject (Ray start dir) (Sphere centr rad) =
    if sol == [] then 0.0 else minimum sol
    where
        sol = filter (>0.0) $ solveQuadEq (dir .* dir, 2*(v .* dir), v .* v - (rad^2))
        v = start <-> centr
closestIntersectionWithObject (Ray start dir) (Plane a b c d) =
    if abs(α) < ɛ  then 0.0
                    else if dist > ɛ then dist else 0.0
    where
        α = (a,b,c) .* dir
        dist = -(d + (a,b,c) .* start) / α

-- Funkcje ułatwiające dostęp do pól typu przecięcia (Intersection)
intDist :: (Maybe Intersection) -> Double
intDist Nothing = 0.0
intDist (Just (Intersection dist _ _)) = dist

intColor :: (Maybe Intersection) -> Color
intColor Nothing = (0.0,0.0,0.0)
intColor (Just (Intersection _ _ (_, (Specular color)))) = color
intColor (Just (Intersection _ _ (_, (Diffuse color)))) = color

intNormalVect :: (Maybe Intersection) -> Vector
intNormalVect Nothing = (0.0,0.0,0.0)
intNormalVect i@(Just (Intersection _ _ (o,_) )) = normal (intPoint i) o

intPoint :: (Maybe Intersection) -> Point3D
intPoint Nothing = (0.0,0.0,0.0)
intPoint (Just (Intersection d (Ray start dir) _)) = start <+> (dir >* d)

getTexture :: Intersection -> Texture
getTexture (Intersection _ _ (_,t) ) = t

closestInt :: Ray -> (Maybe Intersection) -> TexturedObject -> (Maybe Intersection)
closestInt ray i (obj,tex) = if dist > ɛ && ((isNothing i) || dist < (intDist i))
    then Just (Intersection dist ray (obj,tex)) else i
    where
        dist = closestIntersectionWithObject ray obj

-- Funkcja obliczająca najbliższe przecięcie promienia
intersect :: Ray -> [TexturedObject] -> (Maybe Intersection)
intersect ray objects = foldl (closestInt ray) Nothing objects

diffuse :: (Maybe Intersection) -> Light -> Color
diffuse i (AmbientLight _) = intColor i
diffuse i (PointLight pos int) =
    (int >* ((createNormVect (intPoint i) pos) .* (intNormalVect i))) <**> (intColor i)

specular :: (Maybe Intersection) -> Vector -> Light -> Color
specular i _ (AmbientLight _) = intColor i
specular i d (PointLight pos int) = int >* ((intNormalVect i) .* h)
    where
        h = normalize ((d >* (-1)) <+> (createNormVect (intPoint i) pos))

reflectionVect :: Vector -> Vector -> Vector
reflectionVect intersect normal =
    normalize $ intersect <-> (normal >* (2 * (intersect .* normal)))

reflectPt :: Depth -> Intersection -> Vector -> [TexturedObject] -> [Light] -> Color
reflectPt depth i d =
    colorPt depth (Ray (intPoint (Just i)) (reflectionVect d (intNormalVect (Just i)))) (0.0,0.0,0.0)

shadePt :: Intersection -> Vector -> [TexturedObject] -> Light -> Color
shadePt i d o (AmbientLight int) = int
shadePt i d o l@(PointLight pos int)
    | s = (0.0,0.0,0.0)
    | otherwise =
        case texture of
            (Diffuse _)  -> (diffuse (Just i) l)
            (Specular _) -> (specular (Just i) d l)
    where
        s = not (isNothing nearestInt) &&
                (intDist nearestInt) <= distance (intPoint (Just i)) pos
        nearestInt = intersect (createRay (intPoint (Just i)) pos) o
        texture = getTexture i

colorPt :: Depth -> Ray -> BackgroundColor -> [TexturedObject] -> [Light] -> Color
colorPt 0 _ _ _ _ = (0.0, 0.0, 0.0)
colorPt dep ray@(Ray _ dir) bgColor objects lights =
    if (isNothing nearestInt) then bgColor else closure $ shadeColor <+> reflectColor
    where
        shadeColor = foldl (<+>) (0.0,0.0,0.0) (map (shadePt (fromJust nearestInt) dir objects) lights)
        reflectColor = reflectPt (dep-1) (fromJust nearestInt) dir objects lights
        nearestInt = intersect ray objects

rayFigureOut :: Depth -> Scene -> Point3D -> Color
rayFigureOut dep (Scene bgColor (Observer focus _) objects lights) point =
    colorPt dep (createRay focus point) bgColor objects lights

rayTracer :: Depth -> Resolution -> Scene -> Picture
rayTracer dep res scn@(Scene _ (Observer _ dim) _ _) = (rayFigureOut dep scn) . (mapPixelTo3D res dim)
