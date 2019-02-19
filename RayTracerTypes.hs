module RayTracerTypes where

ɛ = 10**(-9)    -- błąd pomiarowy

-- Typ reprezentujący piksel na obrazku
type Point2D = (Int, Int)

-- Wymiary wirtualnej matrycy aparatu (obserwatora), na którą padają promienie ze sceny
-- Format danych: (lewy górny róg (LG), wektor z LG do prawego górnego rogu, wektor z LG do lewego dolnego rogu)
type Dimension = (Point3D, Vector, Vector)

-- Wymiary końcowego obrazka w pikselach: (szerokość, wysokość)
type Resolution = (Int, Int)

-- Ognisko
type Focus = Point3D

type Point3D = (Double, Double, Double)
type Vector = (Double, Double, Double)

data Ray = Ray Point3D Vector

data Object = Sphere Point3D Double             -- (O - X)^2 = R^2
            | Plane Double Double Double Double -- Ax + By + Cz + D = 0

-- Kolor RGB
type Color = (Double, Double, Double)

type BackgroundColor = Color

-- Powierzchnia obiektów
data Texture = Specular Color | Diffuse Color
type TexturedObject = (Object, Texture)

-- Światło
type Intensity = (Double, Double, Double)
data Light = PointLight Point3D Intensity
           | AmbientLight Intensity

-- Obserwator
data Observer = Observer Focus Dimension

-- Głębokość rekursji
type Depth = Int

-- Typ całej sceny (opisuje wszystkie dane potrzebne dla śledzenia promieni),
-- BackgroundColor oznacza kolor tła
data Scene = Scene BackgroundColor Observer [TexturedObject] [Light]

data Intersection = Intersection Double Ray TexturedObject

type Picture = Point2D -> Color

data ParserDescription = ParserDescription Resolution Depth Scene
