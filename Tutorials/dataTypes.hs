module DataTypes where

--data Shape = Circle Float | Parallelogram Float Float | RightTriangle Float Float Float deriving Show

type Measurement = Float
data Shape = Circle Measurement | Parallelogram Measurement Measurement | RightTriangle Measurement Measurement Measurement deriving Show

my_circle = Circle 10
my_para = Parallelogram 5 4
my_tria = RightTriangle 3 4 5

is_round :: Shape -> Bool
is_round (Circle _) = True
is_round _ = False
--is_round (Parallelogram _ _) = False
--is_round (RightTriangle _ _ _) = False

area ::Shape -> Float
area (Circle r) = r*r*3.14
area (Parallelogram b h) = b*h
area (RightTriangle a b c) = 0.5*a*b

data Class = Car | Truck | Motorcycle deriving (Show, Eq)
type Model = String
type Manufacturer = String
data Vehichles = Vehichle Model Manufacturer Class deriving Show

type Database = [Vehichles]

db :: Database
db = [Vehichle "Camaro" "GM" Car,
      Vehichle "F100" "Ford" Truck,
      Vehichle "Mustang" "Ford" Car,
      Vehichle "name" "Yamaha" Motorcycle
     ]

get_manufacturer :: Vehichles -> Manufacturer
get_manufacturer (Vehichle _ manufacturer _) = manufacturer

find_type :: Database -> Class -> [Model]
--find_type [] _ = []
--find_type ((Vehichle model manufacturer class1):xs) findClass
--    | class1 == findClass   = [model] ++ find_type xs findClass
--    | otherwise = find_type xs findClass

find_type db findClass = [model | (Vehichle model manufacturer class1) <- db, class1 == findClass]

add_entry :: Database -> Vehichles -> Database
add_entry db x = db ++ [x]

create_entry :: Database -> Model -> Manufacturer -> Class -> Database
create_entry db model manufacturer class1 = db ++ [(Vehichle model manufacturer class1)]