-- import System.Random
-- import Data.Set (Set, unions, fromList, member)
-- import Data.Map (Map, singleton, elems, (!), insert)
-- import Debug.Trace (trace)

doble :: Num a => a -> a
doble x = x + x

cuadruplo :: Num a => a -> a
cuadruplo x = doble x + doble x

-- signum2 :: (Num a) => a -> a-- error
signum2 n = if n < 0 then -1 else
         if n == 0 then 0 else 1

signum3 n | n < 0 = -1 -- usando guardas
         | n == 0 = 0
         | otherwise = 1

sumaLista :: Num a => [a] -> a
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista xs

-- clasifica :: Show a => [a] -> String
clasifica [ ]       = "La lista esta vacía"
clasifica [x]      = "La lista tiene un elemento: " ++ show x
clasifica [x,y]   = "La lista tiene dos elementos: " ++ show x ++                     " y "++ show y
clasifica (x:y:_) =  "La lista es extensa. Los primeros dos elementos son: " ++ show x ++ " y " ++ show y

sumaVectores :: (Num a, Num b) => (b,a) -> (b,a) -> (b,a)
sumaVectores (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)
-- sumaVectores a b = (fst a + fst b, snd a + snd b)

elemento :: Show a => [a] -> String
elemento [] = "Lista vacía"
elemento lista@(x:xs) = "El primer elemento de " ++ show lista ++ " es " ++ show x

fibonacci :: (Integral a, Num b) => a -> b
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

calcbmi :: (Ord a, Fractional a) => a -> a -> [Char]
calcbmi peso altura | bmi <= 18.5 = "Bajo peso"
                    | bmi <= 25.0 = "Normal"
                    | bmi <= 30.0 = "Sobrepeso"
                    | otherwise = "Obeso"
                    where bmi = peso / altura^2

area_cilindro :: Floating a => a -> a -> a
area_cilindro r h = 
        let lateral = 2 * pi * r * h
            base = pi * r ^ 2
            in lateral + 2 * base

factorial_case n = case n of 0 -> 1
                             _ -> n * factorial_case (n-1)


invierte :: [a] -> [a]
invierte [] = []
invierte (x:xs) = invierte xs ++ [x]

elimina :: Int -> [a] -> [a]
elimina 0 xs = xs
elimina n [] = []
elimina n (_:xs) = elimina (n-1) xs

qsort [] = []
qsort (x:xs) = 
    let smaller = qsort (filter (<=x) xs)
        larger = qsort (filter (>x) xs)
    in smaller ++ [x] ++ larger

-- 
-- Bajo una evaluación impaciente, la definición de min será muy ineficiente (𝑂(𝑛^2)), 
-- pues tendrá que ordenarse primero la lista para luego obtener su primer elemento. 

-- Bajo una evaluación perezosa, la función min es de orden lineal (𝑂(𝑛)), 
-- porque ordIns ubica al menor elemento primero.
ins :: Ord a => a -> [a] -> [a]
ins x [] = [x]
ins x (y:ys) 
    | x <= y = x : y : ys
    | otherwise = y : ins x ys

ordIns :: Ord a => [a] -> [a]
ordIns [] = []
ordIns (x:xs) = ins x (ordIns xs)

min2 xs = head (ordIns xs)
-- OMFG
-- min [9,5,4,3,2] {aplicando min}
-- = head (ordIns [9,5,4,3,2]) {aplicando ordIns}
-- = head (ins 9 (ordIns [5,4,3,2])) {aplicando ordIns}
-- ...
-- = head (ins 9 (ins 5 (ins 4 (ins 3 (ins 2 []))))) {aplicando ins}
-- = head (ins 9 (ins 5 (ins 4 (ins 3 [2])))) {aplicando ins}
-- = head (ins 9 (ins 5 (ins 4 (2 : ins 3 [])))) {aplicando ins}
-- = head (ins 9 (ins 5 (2 : ins 4 (ins 3 [])))) {aplicando ins}
-- = head (ins 9 (2 : ins 5 (ins 4 (ins 3 [])))) {aplicando ins}
-- = head (2 : ins 9 (ins 5 (ins 4 (ins 3 [])))) {aplicando head}
-- = 2

-- 

data Color = Rojo | Amarillo | Verde | Azul | Violeta
data Figura = Circulo Float | Rectangulo Float Float deriving (Show, Eq)



area :: Figura -> Float 
area (Circulo r) = pi * r^2
area (Rectangulo a b) = a*b

data Persona = Persona String String Int String deriving (Show, Eq)

nombre :: Persona -> String
nombre (Persona n _ _ _) = n

apellido :: Persona -> String
apellido (Persona _ a _ _) = a

edad :: Persona -> Int
edad (Persona _ _ e _) = e

data Persona2 = Persona2 {  nombre2 :: String,
                            apellido2 :: String,
                            edad2 :: Int,
                            ocupacion2 :: String} deriving (Show, Eq)

data Car a b c = Car { company :: a , 
                       model :: b , 
                       year :: c } 
                       deriving (Show, Eq) 
                       
data Carro a b c = Carro { companyc :: a , 
                       modelc :: b , 
                       yearc :: c } 
                       deriving (Eq) 

instance (Show a, Show b, Show c) => Show (Carro a b c) where
    show carrusel = "company: " ++ show (companyc carrusel) ++ " model: " ++ show (modelc carrusel) ++ " year: " ++ show (yearc carrusel)


data PCs = DELL | Toshiba | Apple | Microsoft | Vaio deriving (Eq,Ord,Show,Read)


data Arbol a = Vacio | Nodo a (Arbol a) (Arbol a)

contiene :: Eq a => Arbol a -> a -> Bool
contiene Vacio _ = False
contiene (Nodo a b c) e | a == e = True
                        | otherwise = contiene b e || contiene c e
treeelems :: Arbol a -> [a]
treeelems Vacio = []
treeelems (Nodo a b c) = treeelems b ++ [a] ++ treeelems c

type Choice a = [a]

choose :: [a] -> Choice a
choose  xs = xs

fstT (x,y,z) = x

second (x,y,z) = y

lasty (x,y,z) = z

pepe x  y =  [x , y]


test a 
    | True = area pepe 
    | otherwise = 0 
    where pepe = Circulo a



-- main :: IO ()
main = do
    -- pepito <- Rectangulo 1 2
    -- a <- area pepito
    -- print(a)
    print([x| x<-[-8 .. 8]])
    print ( [pepe x y | x<-[1,2,3] , y <- [1,2,3] ] )
    print([ ((fstT x)-1, second x, lasty x ) |x <- [(1,2,3),(2,3,4),(3,4,5)], (fstT x) < 3 ])
    print(treeelems (Nodo 1 (Nodo 2 Vacio Vacio) (Nodo 3 Vacio Vacio) ))
    print(contiene (Nodo 1 (Nodo 2 Vacio Vacio) (Nodo 3 Vacio Vacio) ) 2)
    print(test 1)
    print(DELL)
    print (length [1,2,3,5])
    print ([1,2,3,5]!!1) --index
    print ([1,2,3,5] ++ [6,7,8,9,10]) --concat
    print (head [1,2,3,5]) --head
    print (take 3 [1,2,3,5]) --take
    print (zip [1,2,3,5] [6,7,8,9,10]) --zip... note the missing 10
    print (zip [1,2,3,5] [6,7,8,9,10]) --zip... note the missing 10
    print (1 + 2) 
    print (1 - 2) 
    print (1 * 2) 
    print (1 / 2) 
    -- print (recip 1 2) ??
    print (div 1 2) --division entera
    print (mod 1 2) --division entera
    -- print (1 % 2) -- fail
    print (1 == 2) 
    print (1 /= 2) 
    print (not (1 /= 2)) 
    print (1 < 2) 
    print (1 <= 2) 
    print (1 > 2) 
    print (1 >= 2) 
    print (min 1 2) 
    print (max 1 2) 
    print (abs 5 ) 
    print (abs (-5)) -- () is required!!
    print (negate (-5)) -- () is required!!
    print (negate 5) 
    print (signum 5) 
    print (show (-5)) 
    print (read ("-5")::Int)
    print (read ("-0.5")::Float)
    print (doble 2)
    print (cuadruplo 2)
    print (signum2 2)
    print (signum2 (-2))
    print (signum3 (-2))
    print (signum3 (2))
    print (sumaLista [1,2,3,4,5])
    print (sumaLista [])
    print (clasifica [1,2,3,4,5])
    print (clasifica [1,2])
    print (clasifica [1])
    -- print (clasifica ([])) -- error
    print (sumaVectores (1,1) (2,3)) 
    print (elemento [1,2,3,4,5]) 
    print (take 9 (repeat 0))

    print (fibonacci 1) --error... patrones n+k where k is a constante
    print (fibonacci 5) --error... patrones n+k where k is a constante
    print (fibonacci 10) --error... patrones n+k where k is a constante

    print (calcbmi 60   1.90 ) -- bajo peso
    print (calcbmi 70   1.90 ) -- Normal
    print (calcbmi 90   1.90 ) -- Normal
    print (calcbmi 100  1.90 ) -- sobrepeso
    print (calcbmi 130  1.90 ) -- obeso

    print (area_cilindro 1 1)
    print (pi)
    print (factorial_case 4)
    print (factorial_case 5)

    print (invierte [1,2,3,4,5,6,7,8,9])

    print (elimina 3 [1,2,3,4,5,6,7,8,9])

    print [(x,y)| x <- [1,2,3], y <- [1,2,3], x < y]
    print [x^2| x<-[1,2,3,4]]
    print (take 10 (enumFrom 10))
    print (take 10 (enumFromThen 10 12))
    print (take 10 [2,4..])
    print (take 10 [2..10])
    print (take 10 [2,4..20])
    print (take 10 [2,4..20])

    print (take 27 ['a'..'z'])
    print (take 1000 [' '..'z'])
    print (take 1000 ['\n'..'z'])


    print (map fibonacci [5..10])
    print (map product [[1..2],[4..7]])
    print (product [4..7])
    print (filter (>3) [1,5,3,2,1,6,4,3,2,1])

    print (qsort [1,5,3,2,1,6,4,3,2,1])
    print ((\x y -> x * y) 4 2)
    print (min2 [1,5,3,2,1,6,4,3,0,2,1])
    -- ".......1.4.........2...........5.4.7..8...3....1.9....3..4..2...5.1........8.6..."

    print( map area (map Circulo [1..5]))
    print( area (Circulo 5))
    print( area (Rectangulo 2 5))

    print( Circulo 5)
    print( Rectangulo 2 5)

    print( Persona "Julia" "Sanchez" 30 "Contadora")
    print( edad (Persona "Julia" "Sanchez" 30 "Contadora"))
    print( edad2 (Persona2 "Julia" "Sanchez" 30 "Contadora"))
    print( Persona2 {ocupacion2 = "Contadora", apellido2 = "Sanchez", edad2 = 30, nombre2 = "Julia"})
    print( nombre2 (Persona2 {ocupacion2 = "Contadora", apellido2 = "Sanchez", edad2 = 30, nombre2 = "Julia"}))

    print (Car "Toyota" "Corolla" 1994)
    print (Carro "Toyota" "Corolla" 1994)
    print (Carro "Toyota" "Corolla" 1994 == Carro "Toyota" "Corolla" 1994)
    print(choose [1,2,3] )
 
    -- print(  (x< 2 | x <- choose [s1,2,3,4]))

    -- print(s <- (read ("Rectangulo 4.0 5.0")::Figura))