module Lib where

import Data.Maybe
import Data.Matrix
import Data.Vector 
import Control.Applicative ((<|>))
-- import Data.List

data Nomino =  Nomino {
            valor :: [(Int,Int,Int)]
}

instance Show Nomino where 
    show (Nomino a) =  show (fill a (zero 9 9)  )  



data Sudoku = Sudoku {
                    nominoes :: [Nomino],
                    tablero  :: Matrix Int,
                    posibilidades:: Matrix [Int]

}deriving (Show)



type Choice a = [a]

choose :: [a] -> Choice a
choose xs = xs

fstT (x,y,z) = x

second (x,y,z) = y

lasty (x,y,z) = z


fill:: [(Int,Int,Int)] -> Matrix Int -> Matrix Int 
fill [x] board = setElem (lasty x) (fstT x , second x ) board
fill (x:xs) board = fill xs (setElem (lasty x) (fstT x , second x ) board )

clean_nomino:: [(Int,Int,Int)] -> [(Int,Int,Int)]
clean_nomino [] = []
clean_nomino ((i,j, val ):xs)  | val ==(-1) = [(i,j,0)] Prelude.++ clean_nomino xs   
                                |otherwise = [(i,j,val)] Prelude.++ clean_nomino xs


clean_tablero matrix_actual = mapPos ( \(_,_) val -> (if val ==(-1) then 0 else val)  ) matrix_actual 


someFunc :: IO ()
someFunc = putStrLn "someFunc"



-- printSudoku :: Sudoku -> IO ()
-- printSudoku sudoku = print( fill ((nominoes sudoku) [ [ 0 | x <- [0..8]]| x <- [0..8]] ) )

-- make_sudokus :: [(Nomino,Matrix Int)] -> [Sudoku] 
-- make_sudokus [] = []
-- -- make_sudokus (x:xs) = [ Sudoku { nominoes = fst x , tablero = snd x   }  ]  Prelude.++  make_sudokus xs
-- make_sudokus a = [ make_one (take 9 a) ] Prelude.++ 

my_return:: a -> Choice a
my_return x = choose [x] 

mzero:: Choice a
mzero = choose []

guard :: Bool -> Choice ()
guard True = my_return ()
guard False = mzero

se_logro nomino | nomino == [] =False
                |otherwise = True

sum1 [] = []
sum1 ((i,j,val):xs) = [(i+1,j+1,val)] Prelude.++ sum1 xs 

make_nomino x = Nomino { valor = x}




-- make_board :: [Nomino]  -> IO ()
posibles_tableros::[Nomino] -> [Sudoku]
posibles_tableros nominoes_to_create = do 
    nomino1 <- choose (delete_imposibles (generate_matrix (catMaybes (posibles_pos ((nominoes_to_create)!!0))))(zero 9 9))
    nomino2 <- choose (delete_imposibles (generate_matrix (catMaybes (posibles_pos ((nominoes_to_create)!!1)))) (snd (nomino1)))
    nomino3 <- choose (delete_imposibles (generate_matrix (catMaybes (posibles_pos ((nominoes_to_create)!!2)))) (snd (nomino2)))
    nomino4 <- choose (delete_imposibles (generate_matrix (catMaybes (posibles_pos ((nominoes_to_create)!!3)))) (snd (nomino3)))
    nomino5 <- choose (delete_imposibles (generate_matrix (catMaybes (posibles_pos ((nominoes_to_create)!!4)))) (snd (nomino4))) 
    nomino6 <- choose (delete_imposibles (generate_matrix (catMaybes (posibles_pos ((nominoes_to_create)!!5)))) (snd (nomino5)))
    nomino7 <- choose (delete_imposibles (generate_matrix (catMaybes (posibles_pos ((nominoes_to_create)!!6)))) (snd (nomino6)))
    nomino8 <- choose (delete_imposibles (generate_matrix (catMaybes (posibles_pos ((nominoes_to_create)!!7)))) (snd (nomino7)))
    nomino9 <- choose (delete_imposibles (generate_matrix (catMaybes (posibles_pos ((nominoes_to_create)!!8)))) (snd (nomino8)))
    
    [Sudoku { nominoes =[ Nomino { valor = clean_nomino ( valor (fst nomino1))   },  Nomino {valor = clean_nomino (valor (fst nomino2))}, 
                        Nomino { valor = clean_nomino (valor (fst nomino3))},
                        Nomino {valor = clean_nomino (valor (fst nomino4))},
                        Nomino { valor =  clean_nomino (valor (fst nomino5))},
                        Nomino {valor= clean_nomino (valor (fst nomino6))},
                        Nomino {valor = clean_nomino(valor  (fst nomino7))}, 
                        Nomino {valor =  clean_nomino ( valor(fst nomino8))},
                        Nomino {valor = clean_nomino (valor (fst nomino9))} ] ,
             tablero = ( clean_tablero (snd nomino9)) ,
             posibilidades = ( 
                update_posib [ 
                Nomino { valor = clean_nomino ( valor (fst nomino1))},  
                Nomino {valor = clean_nomino (valor (fst nomino2))}, 
                Nomino { valor = clean_nomino (valor (fst nomino3))},
                Nomino {valor = clean_nomino (valor (fst nomino4))},
                Nomino { valor =  clean_nomino (valor (fst nomino5))},
                Nomino {valor= clean_nomino (valor (fst nomino6))},
                Nomino {valor = clean_nomino(valor  (fst nomino7))}, 
                Nomino {valor =  clean_nomino ( valor(fst nomino8))},
                Nomino {valor = clean_nomino (valor (fst nomino9))}   ] 
                (clean_tablero (snd nomino9))    
                 )
                }
              ]
    
    -- [nomino1,nomino2,nomino3,nomino4,nomino5,nomino6,nomino7,nomino8,nomino9]

generate_matrix :: [Nomino] -> [(Nomino,Matrix Int)]
generate_matrix [] = []
generate_matrix (x:xs)  = [(x,new_matrix)] Prelude.++ generate_matrix xs 
                                        where 
                                            new_matrix = fill ( valor x) (zero 9 9)


delete_imposibles:: [(Nomino,Matrix Int)] -> Matrix Int -> [(Nomino,Matrix Int)]
delete_imposibles  [ ] _ = [] 
delete_imposibles (x:xs) matrix_actual | se_queda = [(fst x , (snd x) + matrix_actual )] Prelude.++ delete_imposibles xs matrix_actual
                                       | otherwise = [] Prelude.++ delete_imposibles xs matrix_actual
                                         where 
                                            se_queda= solapan (valor(fst x)) matrix_actual
  

solapan :: [(Int,Int,Int)] -> Matrix Int -> Bool
solapan  [] _ = True
solapan  ((i ,j ,e):xs)   matrix_actual | (getElem i j matrix_actual ) /= 0  = False 
                                        | otherwise = solapan xs matrix_actual


posibles_pos :: Nomino -> [ Maybe Nomino]
posibles_pos nomino =  [ mover (valor nomino) x y | x<-[-8 .. 8] , y<-[-8 .. 8] ]

mover :: [(Int,Int,Int)] ->Int -> Int ->Maybe Nomino
mover nomino pasox pasoy | valido (valor new_nomino) = Just new_nomino
                         | otherwise = Nothing
                         where
                                new_nomino = try_mover nomino pasox pasoy

valido :: [(Int,Int,Int)] -> Bool
valido nomino = Prelude.length nomino == 9

try_mover ::  [(Int,Int,Int)] -> Int -> Int -> Nomino
try_mover nomino pasox pasoy = Nomino { valor = [((fstT x)+pasox, (second x)+pasoy, lasty x )  | x <-nomino ,  (fstT x) + pasox >= 1 && (fstT x) + pasox <= 9  
                                                                                                                && (second x)+pasoy >= 1 &&(second x)+pasoy <= 9    ]} 

-- todos lo check devuelven true si hay alguien en conflicto
check_row :: Int -> Int  -> Matrix Int -> Bool
check_row  i  val matrix_actual | Data.Vector.elem val (Data.Matrix.getRow i matrix_actual) = True 
                                | otherwise = False


check_column :: Int -> Int   -> Matrix Int -> Bool
check_column  j  val matrix_actual | Data.Vector.elem val (Data.Matrix.getCol j matrix_actual) = True 
                                   | otherwise = False

check_nomino :: [(Int,Int,Int)] -> Int -> Bool
check_nomino [] val = False
check_nomino ((i,j,val_t):xs) val | val_t == val = True  
                                  | otherwise = check_nomino xs val


                        
pertenece_nomino_list :: [Nomino] -> Int -> Int -> Nomino
pertenece_nomino_list (x:xs) i  j = if pertenece_nomino (valor x) i j then x else  (pertenece_nomino_list xs i j)    

pertenece_nomino:: [(Int,Int,Int)] -> Int -> Int -> Bool
pertenece_nomino [] _ _ = False
pertenece_nomino ((i_t,j_t,val):xs) i j = if i == i_t && j == j_t then True else pertenece_nomino xs i j


dame_nomino:: [Nomino] -> (Int,Int,Int) -> Nomino
dame_nomino (x:xs) tripla   = if Prelude.elem  tripla (valor x) then x else (dame_nomino xs tripla)

update_posib :: [Nomino] -> Matrix Int  -> Matrix [Int]
update_posib  actual_nominoes matrix_actual  =   Data.Matrix.fromList 9 9 [ [val | val <- [1..9],( 0 == (getElem i j matrix_actual)   ) &&(val /= (getElem i j matrix_actual)) && (not (check_row i val matrix_actual  ))&& (not (check_column j val matrix_actual  ))&& (not (check_nomino (valor (pertenece_nomino_list actual_nominoes i j )) val ) )  ] | i <-[1..9] , j <- [1..9]  ]
                                                                                                                                                                        
                                                                        -- ]




nextStep :: Sudoku -> (Sudoku,Sudoku)
nextStep sudoku_actual  | count == 2 = ( make_alternaive sudoku_actual  ((getElem i j (posibilidades sudoku_actual ))!!0) i j , make_alternaive sudoku_actual ((getElem i j (posibilidades sudoku_actual ))!!1) i j )
                        | otherwise = (make_alternaive sudoku_actual  ((getElem i j (posibilidades sudoku_actual ))!!0) i j, make_alternaive1 sudoku_actual  ((getElem i j (posibilidades sudoku_actual ))!!0) i j )  
                            where
                                (i,j,count) = find_min (tablero sudoku_actual  ) (posibilidades sudoku_actual)

make_alternaive :: Sudoku -> Int -> Int-> Int->  Sudoku 
make_alternaive sudoku_actual val i j  = Sudoku { nominoes = nomin ,
                                                  tablero = new_tab,
                                                  posibilidades = new_posib
                                                  }
                                        where
                                            nomin = (constr_nominoes (nominoes sudoku_actual ) i j val )
                                            new_tab = (Data.Matrix.setElem val (i,j) (tablero sudoku_actual))
                                            new_posib = update_posib nomin new_tab

make_alternaive1 :: Sudoku -> Int -> Int-> Int-> Sudoku 
make_alternaive1 sudoku_actual val i j = Sudoku {
                                                    nominoes = (nominoes sudoku_actual ),
                                                    tablero = ( tablero sudoku_actual ),
                                                    posibilidades = setElem (del val (Data.Matrix.getElem i j (posibilidades sudoku_actual ) )  ) (i,j) (posibilidades sudoku_actual )

}

del :: Int -> [Int] -> [Int] 
del val (x:xs)   | x == val = [] Prelude.++ xs 
                 | otherwise = [x] Prelude.++ del val xs                                

constr_nominoes (x:xs) i j val | no_esta = [x] Prelude.++  constr_nominoes xs i j val
                             | otherwise = [ Nomino { valor= substitute_nomino (valor x) (i,j,val)} ] Prelude.++ xs
                                where 
                                    no_esta = not(pertenece_nomino (valor x ) i j )
                                    substitute_nomino [] tupla = []
                                    substitute_nomino ((i,j,val):xs) tupla | (i == (fstT tupla ) && j == (second tupla)) = [tupla] Prelude.++ xs  
                                                                           |otherwise = [(i,j,val)] Prelude.++ substitute_nomino xs tupla





find_min :: Matrix Int -> Matrix [Int] -> ( Int ,Int,Int)
find_min tablero_actual posib_matrix = menor_count   [ (i ,j) | i <- [1..9]  , j <- [1..9] , (getElem i j tablero_actual) == 0 ] (10,10, 10)
                                        where 
                                            menor_count [] initial = initial
                                            menor_count ( (t,u):xs ) initial | count < lasty (initial) = menor_count xs (t,u,count) 
                                                                         |otherwise = menor_count xs initial
                                                                         where 
                                                                            count = if cant >1 then cant else 10
                                                                                where 
                                                                                    cant =Prelude.length (getElem t u posib_matrix)



fil_posib :: [Nomino] -> Matrix Int -> Matrix [Int] ->  (Sudoku,Int)
fil_posib figuras tablero_actual posibilidades_list=  (Sudoku { nominoes =figuras_upd , tablero = tablero_upd ,posibilidades = posib_upd     },cant_1)
                                    where
                                        figuras_upd = upd_figs figuras posibilidades_list
                                        tablero_upd =fil_zero_cells (collapse figuras_upd) tablero_actual
                                        posib_upd = update_posib figuras_upd tablero_upd
                                        cant_1 = dame_count (Data.Matrix.toList posib_upd  ) 0
                                        
-- dame_count :: Vector [Int] -> Int -> Int                               
dame_count  [] val = val
dame_count (x:xs) val =  dame_count xs (if Prelude.length x ==1 then val+1 else val )


fil_zero_cells [] tablero_actual = tablero_actual
fil_zero_cells ((i,j,val):xs) tablero_actual = fil_zero_cells xs (Data.Matrix.setElem val (i,j) tablero_actual )


collapse [] = []
collapse (nomino: demas) = valor(nomino) Prelude.++ collapse demas


setNomino  ( (l , m , val): xs )  valu i j | l == i && m ==j = [(i,j,valu)] Prelude.++ xs 
                                          | otherwise = [(l,m,val)] Prelude.++ (setNomino xs valu i j)


upd_figs :: [Nomino] -> Matrix [Int] -> [Nomino]
upd_figs [] _ = []
upd_figs (x:xs) posibilidades_list  = [ Nomino { valor= new_x }]  Prelude.++ upd_figs xs posibilidades_list
                                    where
                                        new_x = upd_fig (valor x)  posibilidades_list

upd_fig [] _ = []
upd_fig ((i,j,val):xs) posibilidades_list   | a == 1 && val == 0  = [(i,j,val_upd)] Prelude.++ upd_fig xs posibilidades_list
                                            | otherwise = [(i,j,val)] Prelude.++ upd_fig xs posibilidades_list
                                            where 
                                                a = Prelude.length (getElem i j posibilidades_list )
                                                val_upd = (getElem i j posibilidades_list)!!0
-- completado ::  Vector Int -> Bool
completado  matrix_actual | Data.Vector.elem 0 matrix_actual = False 
                          | otherwise = True

-- bloqueado ::  Sudoku -> Bool
bloqueado s             | empty_with_no_posib  || valid = True
                        |otherwise = False
                         where 
                            empty_with_no_posib = (Prelude.length  [ (i,j) | 
                                                                        i <- [1..9] , j <- [1..9] , 
                                                                        (Data.Matrix.getElem i j (tablero s  )) == 0 && 
                                                                        (Prelude.length (Data.Matrix.getElem i j (posibilidades s ))) == 0] ) /=0
                            valid = ((Prelude.length (conflic (tablero s) (nominoes s)   )) /= 0)  

conflic matrix_actual actual_nominoes = [ (i,j) |  i <- [1..9] , j <- [1..9]    ,  (0 /= (getElem i j matrix_actual)   ) &&(( (check_row i  (getElem i j  matrix_actual) (setElem 0 (i,j) matrix_actual) ))||   ( (check_column j (getElem i j  matrix_actual) (setElem 0 (i,j) matrix_actual)  ))||  ( (check_nomino (setNomino (valor (pertenece_nomino_list actual_nominoes i j )  ) 0 i j ) (getElem i j  matrix_actual) )) )] 



solve :: Sudoku -> Maybe Sudoku
solve sudoku = solve' (stell_posib sudoku )
    
solve' g
            | bloqueado g = Nothing
            | completado (getMatrixAsVector (tablero g))  = Just g
            | otherwise  = 
                let (sudoku1, sudoku2) = nextStep g
                    in (solve  sudoku1) Control.Applicative.<|> (solve sudoku2)             



stell_posib sudk =  if queden_uno >0  then stell_posib new_sud else new_sud
                    where
                        (new_sud,queden_uno) = fil_posib (nominoes sudk) (tablero sudk) (posibilidades sudk )

