module Main where

import Lib

import Data.Maybe
import Data.Matrix
import System.IO

main :: IO ()
main = do
    
      


    (print ((take 1 (posibles_tableros(map make_nomino (map sum1 [[(0,3,-1),(1,3,-1),(1,4,-1),(1,5,-1),(2,5,-1),(2,6,-1),(2,7,-1),(3,7,-1),(3,8,-1)],[(0,0,3),(0,1,-1),(0,2,-1),(1,0,-1),(1,1,9),(1,2,-1),(2,0,-1),(3,0,-1),(3,1,-1)],[(0,4,9),(0,5,-1),(0,6,-1),(0,7,7),(0,8,-1),(1,6,-1),(1,7,3),(1,8,-1),(2,8,-1)], [(2,1,1),(2,2,3),(2,3,-1),(2,4,7),(3,2,-1),(4,0,-1),(4,1,2),(4,2,-1),(4,3,4)], [(5,0,-1),(5,1,-1),(6,1,-1),(6,2,7),(6,3,-1),(7,3,6),(7,4,1),(7,5,5),(8,5,9)], [(3,3,8),(3,4,4),(3,5,-1),(3,6,6),(4,4,5),(5,2,9),(5,3,7),(5,4,-1),(5,5,1)], [(5,7,-1),(5,8,6),(6,8,1),(7,6,-1),(7,7,-1),(7,8,2),(8,6,-1),(8,7,-1),(8,8,7)], [(4,5,3),(4,6,7),(4,7,-1),(4,8,9),(5,6,-1),(6,4,-1),(6,5,4),(6,6,5),(6,7,-1)], [(6,0,9),(7,0,-1),(7,1,3),(7,2,-1),(8,0,5),(8,1,-1),(8,2,1),(8,3,-1),(8,4,8)]])))!!0)))
    (print (solve(take 1 (posibles_tableros(map make_nomino (map sum1 [[(0,3,-1),(1,3,-1),(1,4,-1),(1,5,-1),(2,5,-1),(2,6,-1),(2,7,-1),(3,7,-1),(3,8,-1)],[(0,0,3),(0,1,-1),(0,2,-1),(1,0,-1),(1,1,9),(1,2,-1),(2,0,-1),(3,0,-1),(3,1,-1)],[(0,4,9),(0,5,-1),(0,6,-1),(0,7,7),(0,8,-1),(1,6,-1),(1,7,3),(1,8,-1),(2,8,-1)], [(2,1,1),(2,2,3),(2,3,-1),(2,4,7),(3,2,-1),(4,0,-1),(4,1,2),(4,2,-1),(4,3,4)], [(5,0,-1),(5,1,-1),(6,1,-1),(6,2,7),(6,3,-1),(7,3,6),(7,4,1),(7,5,5),(8,5,9)], [(3,3,8),(3,4,4),(3,5,-1),(3,6,6),(4,4,5),(5,2,9),(5,3,7),(5,4,-1),(5,5,1)], [(5,7,-1),(5,8,6),(6,8,1),(7,6,-1),(7,7,-1),(7,8,2),(8,6,-1),(8,7,-1),(8,8,7)], [(4,5,3),(4,6,7),(4,7,-1),(4,8,9),(5,6,-1),(6,4,-1),(6,5,4),(6,6,5),(6,7,-1)], [(6,0,9),(7,0,-1),(7,1,3),(7,2,-1),(8,0,5),(8,1,-1),(8,2,1),(8,3,-1),(8,4,8)]])))!!0)))

      
    -- writeFile "posibles.txt" (show(( posibles_tableros [

    --     Nomino { valor = [(0,3,1),(1,3,5),(1,4,6),(1,5,7),(2,5,8),(2,6,4),(2,7,2),(3,7,9),(3,8,3)]},
    --     Nomino { valor = [(0,0,3),(0,1,5),(0,2,8),(1,0,4),(1,1,9),(1,2,2),(2,0,6),(3,0,1),(3,1,7)]},        
    --     Nomino { valor = [(0,4,9),(0,5,6),(0,6,2),(0,7,7),(0,8,4),(1,6,1),(1,7,3),(1,8,8),(2,8,5)]},
    --     Nomino { valor = [(2,1,1),(2,2,3),(2,3,9),(2,4,7),(3,2,5),(4,0,8),(4,1,2),(4,2,6),(4,3,4)]},
    --     Nomino { valor = [(5,0,2),(5,1,4),(6,1,8),(6,2,7),(6,3,3),(7,3,6),(7,4,1),(7,5,5),(8,5,9)]},
    --     Nomino { valor = [(3,3,8),(3,4,4),(3,5,2),(3,6,6),(4,4,5),(5,2,9),(5,3,7),(5,4,3),(5,5,1)]},        
    --     Nomino { valor = [(5,7,5),(5,8,6),(6,8,1),(7,6,9),(7,7,8),(7,8,2),(8,6,3),(8,7,4),(8,8,7)]},
    --     Nomino { valor = [(4,5,3),(4,6,7),(4,7,1),(4,8,9),(5,6,8),(6,4,2),(6,5,4),(6,6,5),(6,7,6)]},        
    --     Nomino { valor = [(6,0,9),(7,0,7),(7,1,3),(7,2,4),(8,0,5),(8,1,6),(8,2,1),(8,3,2),(8,4,8)]}
        
    --      ])))
