-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa1_2019li1g012 where

import LI11920
import System.Random
import Data.List.Split


-- * Testes

-- | Testes unitários da Tarefa 1.
--
-- Cada teste é um triplo (/número de 'Pista's/,/comprimento de cada 'Pista' do 'Mapa'/,/semente de aleatoriedades/).
testesT1 :: [(Int,Int,Int)]
testesT1 = [(3,10,124),(2,10,1),(3,12,45),(1,10,242),(10,3,2),(4,15,432543),(5,20,243),(6,4,42345),(7,10,32433),(8,6,43242),(9,27,2019),(5,20,123)]

-- * Funções pré-definidas da Tarefa 1.

geraAleatorios :: Int -> Int -> [Int]
geraAleatorios n seed = take n (randomRs (0,9) (mkStdGen seed))

-- * Funções principais da Tarefa 1.

gera :: Int -> Int -> Int -> Mapa
gera npistas comprimento semente = if npistas == 0 then [] else confirma (Recta Terra 0) (pecaInicial (Recta Terra 0) (intToPeca ((chunksOf (comprimento*2-2)  (geraAleatorios ((comprimento-1)*npistas*2) semente))) Terra ))

-- | Função que coloca uma Peça (Recta Terra 0) no inicio de cada pista
pecaInicial :: Peca -> Mapa -> Mapa
pecaInicial _ [] = []
pecaInicial (Recta Terra 0) (h:t) = ((Recta Terra 0):h):pecaInicial (Recta Terra 0) t 

confirma :: Peca -> Mapa -> Mapa
confirma (Recta Terra 0) [] = [[Recta Terra 0]]
confirma _ l = l

-- | Função que transforma os ints num mapa de peças

intToPeca :: [[Int]]-> Piso -> Mapa
intToPeca [] _ = [] 
intToPeca ([]:t) piso = intToPeca t piso
intToPeca ((x:y):t) piso = (criaPeca 0 piso (x:y)): intToPeca t piso

-- | Função que faz uma lista de Peças

criaPeca :: Int -> Piso -> [Int] -> [Peca]
criaPeca _ _ [_] = []  
criaPeca altura piso [x,y] = case y of 
                             0 -> [(Rampa pisonovo altura (altura+1))]
                             1 -> [(Rampa pisonovo altura (altura+2))]
                             2 -> if (altura-1) > 0 then [Rampa pisonovo altura (altura-1)] else if (altura == 0 && altura-1 < 0) then [Recta pisonovo altura] else [Rampa pisonovo altura 0]
                             3 -> if (altura-2) > 0 then [Rampa pisonovo altura (altura-2)] else if (altura == 0 && altura-2 < 0) then [Recta pisonovo altura] else [Rampa pisonovo altura 0]
                             4 -> if (altura-3) > 0 then [Rampa pisonovo altura (altura-3)] else if (altura == 0 && altura-3 < 0) then [Recta pisonovo altura] else [Rampa pisonovo altura 0]
                             5 -> if (altura-4) > 0 then [Rampa pisonovo altura (altura-4)] else if (altura == 0 && altura-4 < 0) then [Recta pisonovo altura] else [Rampa pisonovo altura 0]
                             6 -> [(Recta pisonovo altura)]
                             7 -> [(Recta pisonovo altura)]
                             8 -> [(Recta pisonovo altura)]
                             9 -> [(Recta pisonovo altura)] 
                             where pisonovo = (criaPiso x piso)

criaPeca altura piso (x:y:t) = case y of 
                             0 -> (Rampa (criaPiso x piso) altura (altura+1)): criaPeca (altura+1) pisonovo t 
                             1 -> (Rampa (criaPiso x piso) altura (altura+2)): criaPeca (altura+2) pisonovo t
                             2 -> if (altura-1) > 0 then ((Rampa pisonovo altura (altura-1)): criaPeca (altura-1) pisonovo t) else if (altura == 0 && altura-1 < 0) then (Recta pisonovo altura): criaPeca altura pisonovo t else ((Rampa pisonovo altura 0): criaPeca 0 pisonovo t)
                             3 -> if (altura-2) > 0 then ((Rampa pisonovo altura (altura-2)): criaPeca (altura-2) pisonovo t) else if (altura == 0 && altura-2 < 0) then (Recta pisonovo altura): criaPeca altura pisonovo t else ((Rampa pisonovo altura 0): criaPeca 0 pisonovo t)
                             4 -> if (altura-3) > 0 then ((Rampa pisonovo altura (altura-3)): criaPeca (altura-3) pisonovo t) else if (altura == 0 && altura-3 < 0) then (Recta pisonovo altura): criaPeca altura pisonovo t else ((Rampa pisonovo altura 0): criaPeca 0 pisonovo t)
                             5 -> if (altura-4) > 0 then ((Rampa pisonovo altura (altura-4)): criaPeca (altura-4) pisonovo t) else if (altura == 0 && altura-4 < 0) then (Recta pisonovo altura): criaPeca altura pisonovo t else ((Rampa pisonovo altura 0): criaPeca 0 pisonovo t)
                             6 -> (Recta pisonovo altura):criaPeca (altura) pisonovo t
                             7 -> (Recta pisonovo altura):criaPeca (altura) pisonovo t
                             8 -> (Recta pisonovo altura):criaPeca (altura) pisonovo t
                             9 -> (Recta pisonovo altura):criaPeca (altura) pisonovo t
                             where pisonovo = (criaPiso x piso)

-- | Função que cria um Piso correspondente a cada int

criaPiso :: Int -> Piso -> Piso 
criaPiso x piso = case x of
                       0 -> Terra
                       1 -> Terra
                       2 -> Relva
                       3 -> Relva
                       4 -> Lama
                       5 -> Boost
                       6 -> piso 
                       7 -> piso 
                       8 -> piso 
                       9 -> piso 

