-- | Este módulo define funções comuns da Tarefa 3 do trabalho prático.
module Tarefa3_2019li1g012 where

import LI11920
import Constroi
import Tarefa0_2019li1g012
import Tarefa1_2019li1g012
import Tarefa2_2019li1g012
import Data.List 
import Data.List.Split
-- * Testes

-- | Testes unitários da Tarefa 3.
--
-- Cada teste é um 'Mapa'.
testesT3 :: [Mapa]
testesT3 = [gera 4 10 132, gera 3 10 32, gera 1 10 342,gera 2 10 123, gera 4 6 34, gera 2 5 1,gera 1 5 1,gera 3 10 124, gera 4 7 11,[[Recta Terra 0, Recta Terra 0, Recta Boost 0, Recta Boost 0],[Recta Terra 0, Recta Terra 0, Recta Boost 0, Recta Boost 0]]]

-- * Funções principais da Tarefa 3.

-- | Desconstrói um 'Mapa' numa sequência de 'Instrucoes'.
--
-- __NB:__ Uma solução correcta deve retornar uma sequência de 'Instrucoes' tal que, para qualquer mapa válido 'm', executar as instruções '(desconstroi m)' produza o mesmo mapa 'm'.
--
-- __NB:__ Uma boa solução deve representar o 'Mapa' dado no mínimo número de 'Instrucoes', de acordo com a função 'tamanhoInstrucoes'.

-- | O objetivo desta função era comprimir ao máximo o número de instruções que são necessárias para construir um mapa
-- | Para o fazer utilizei dois algoritmos. Um que comparava as peças horizontalmente e outro que as comparava verticalmente.
-- | Na comparação horizontal, não encontrei grandes desafios visto que, caso houvessem peças iguais, simplesmente dava a instrução "Repete x peça"
-- | O maior desafio foi para o algoritmo vertical. De forma a conseguir implementar este algoritmo, inverti a matriz do mapa, de forma a que, na primeira
-- | linha da matriz tivesse todas as peças iniciais de cada pista, na segunda linha as 2 peças de cada pista e por aí em diante.
-- | Fiquei satisfeito com o resultado final desta tarefa pois, apesar de não ter conseguido implementar um terceiro algoritmo, aqueles que implementei funcionam bem.

desconstroi :: Mapa -> Instrucoes
desconstroi [] = []
desconstroi l = repeteHorizontal (matrix2list (transpose (vertAux (divideInst (mapaParaInstrucoes l 0)))))



-- | Esta função serve para passar uma pista para instruções

pistaParaInstrucoes :: Pista -> Int -> Instrucoes
pistaParaInstrucoes [] a = []
pistaParaInstrucoes ((Recta x y):t) a = Anda [a] x: pistaParaInstrucoes t a 
pistaParaInstrucoes ((Rampa x y z):t) a = if z>y then Sobe [a] x (z-y):pistaParaInstrucoes t a else Desce [a] x (y-z):pistaParaInstrucoes t a

-- | Esta função, utilizando a pistaParaInstrucoes, transforma um mapa em instruções

mapaParaInstrucoes :: Mapa -> Int -> Instrucoes
mapaParaInstrucoes [] a = []
mapaParaInstrucoes (h:t) a = pistaParaInstrucoes (tail h) a ++ mapaParaInstrucoes t (a+1) 


-- | Função principal para fazer padrões horizontais

repeteHorizontal :: Instrucoes -> Instrucoes
repeteHorizontal [] = []
repeteHorizontal [x] = [x]
repeteHorizontal ((Repete x [h]):t) = if contaRepsH t h >1 then Repete (x+(contaRepsH (h:t) h)) [h]:repeteHorizontal ((pistaSemReps t) (contaRepsH t h)) else h:repeteHorizontal t
repeteHorizontal (h:t:z) = if (contaRepsH(h:t:z) h)>1 then Repete (contaRepsH (h:t:z) h) [h]:repeteHorizontal (pistaSemReps (h:t:z) (contaRepsH (h:t:z) h)) else h:repeteHorizontal (t:z)

-- 	| Função que conta quantas repetições de uma determinada instrução existem numa pista 

contaRepsH :: Instrucoes -> Instrucao -> Int
contaRepsH [] x = 0
contaRepsH [h] x = if h == x then 1 else 0
contaRepsH (h:t:z) x = if h == x then 1 + contaRepsH(t:z) x else 0 

-- | Função que retira todas as instruções que serão substituidas por uma só instrução Repete no caso horizontal ou com uma instrução para diferentes pistas no caso vertical

pistaSemReps :: Instrucoes -> Int -> Instrucoes
pistaSemReps [] x = []
pistaSemReps l 0 = l
pistaSemReps l x = if length l <= x then [] else pistaSemReps (tail l) (x-1)

-- | Função que passa uma lista de instrução para uma matriz com as pistas na vertical, de forma a ter todos os elementos que se encontram na mesma posição em diferentes pistas estarem na mesma lista

divideInst :: Instrucoes -> [Instrucoes]
divideInst l = transpose (chunksOf (div (tamanhoPistInst l) (numeroPistasInstrucoes l)) l)

-- | Função para fazer padrões verticais

repeteVert :: Instrucoes -> Int -> Instrucoes
repeteVert [] reps = []
repeteVert [x] reps = [x]
repeteVert (h:z:t) reps = if igualV h z then (instrucaoVert h (h:z:t)):repeteVert (pistaSemReps (h:z:t) (contaIguais (h:z:t))) (contaIguais (h:z:t)) else (h:z:t)

-- | Conta quantas instruções iguais existem numa lista

contaIguais :: Instrucoes -> Int
contaIguais [] = 0
contaIguais [x,y] = if igualV x y then 2 else 0
contaIguais [x,y,z] = if igualV x y then 2 + (if igualV y z then 1 else 0) else contaIguais [y,z]
contaIguais (h:z:t) = if igualV h z then 2 + contaIguais (z:t) else contaIguais(z:t)

-- | Junta a função repeteVert para fazer uma matriz com instruções para o padrão vertical

vertAux :: [Instrucoes] -> [Instrucoes]
vertAux [] = []
vertAux ([x]:t) = [x]:vertAux t
vertAux ((h:z:x):t) = if igualV h z then repeteVert (h:z:x) 1: vertAux t else ((h:z:x):t)

-- | Função para atualizar uma instrução de forma a indicar todas as pistas que possuem uma determinada instrução na mesma posição

instrucaoVert :: Instrucao -> Instrucoes -> Instrucao
instrucaoVert (Anda [a] piso) l = (Anda (indicesLista (Anda [a] piso) l 0) piso)
instrucaoVert (Sobe [a] piso int) l = (Sobe (indicesLista (Sobe [a] piso int) l 0) piso int)
instrucaoVert (Desce [a] piso int) l = (Desce (indicesLista (Desce [a] piso int) l 0) piso int)

-- | Encontra todos os indices onde se encontra x instrução numa lista 

indicesLista :: Instrucao -> Instrucoes -> Int -> [Int]
indicesLista h [] a = []
indicesLista h (x:xs) a = if igualV h x then a: indicesLista h xs (a+1) else indicesLista h xs (a+1)

-- | Passa uma matriz de Instrução para Instruções

matrix2list :: [Instrucoes] -> Instrucoes
matrix2list [] = []
matrix2list (h:t) =  h ++ matrix2list t

-- | Dá o tamanho das instruções

tamanhoPistInst :: Instrucoes -> Int
tamanhoPistInst [] = 0 
tamanhoPistInst ((Repete x y):t) = x + tamanhoPistInst t 
tamanhoPistInst ((Anda _ _):t) = 1+ tamanhoPistInst t
tamanhoPistInst ((Sobe _ _ _):t) = 1+ tamanhoPistInst t
tamanhoPistInst ((Desce _ _ _):t) = 1+ tamanhoPistInst t 

-- | Compara 2 elementos de pistas diferentes

igualV :: Instrucao -> Instrucao -> Bool
igualV (Anda _ x) (Anda _ y) = x == y
igualV _ (Anda _ y) = False
igualV (Anda _ x) _ = False
igualV (Sobe _ x y) (Sobe _ z d) = x==z && y==d 
igualV _ (Sobe _ z d) = False
igualV (Sobe _ z d) _ = False
igualV (Desce _ x y) (Desce _ z d) = x == z && y==d
igualV _ (Desce _ z d) = False
igualV (Desce _ x y) _ =False
igualV (Repete x []) y = False
igualV (Repete x [h]) y = igualV h y
