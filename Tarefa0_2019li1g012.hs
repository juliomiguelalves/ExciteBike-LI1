-- | Este mÃ³dulo define funÃ§Ãµes genÃ©ricas sobre vetores e matrizes, que serÃ£o Ãºteis na resoluÃ§Ã£o do trabalho prÃ¡tico.
module Tarefa0_2019li1g012 where

-- * FunÃ§Ãµes nÃ£o-recursivas.

-- | Um ponto a duas dimensÃµes dado num referencial cartesiado (distÃ¢ncias aos eixos vertical e horizontal)
--
-- <<http://li1.lsd.di.uminho.pt/images/cartesiano.png cartesisano>>
-- , ou num referencial polar (distÃ¢ncia Ã  origem e Ã¢ngulo do respectivo vector com o eixo horizontal).
--
-- <<http://li1.lsd.di.uminho.pt/images/polar.png polar>>
data Ponto = Cartesiano Double Double | Polar Double Angulo
             deriving Show
-- | Um Ã¢ngulo em graus.
type Angulo = Double

-- ** FunÃ§Ãµes sobre vetores

-- | Um 'Vetor' na representaÃ§Ã£o escalar Ã© um 'Ponto' em relaÃ§Ã£o Ã  origem.
type Vetor = Ponto 
-- ^ <<http://li1.lsd.di.uminho.pt/images/vetor.png vetor>>

-- *** FunÃ§Ãµes gerais sobre 'Vetor'es.

-- | Soma dois 'Vetor'es.
somaVetores :: Vetor -> Vetor -> Vetor
somaVetores (Polar x1 a1) (Polar x2 a2) = somaVetores (polarCartesiano(Polar x1 a1)) (polarCartesiano(Polar x2 a2))
somaVetores (Polar x1 a1) (Cartesiano x2 y1) = somaVetores (polarCartesiano(Polar x1 a1)) (Cartesiano x2 y1)
somaVetores (Cartesiano x1 y1) (Polar x2 a1) = somaVetores (Cartesiano x1 y1) (polarCartesiano(Polar x2 a1))
somaVetores (Cartesiano x1 y1) (Cartesiano x2 y2) = (Cartesiano (x1+x2) (y1+y2))

-- | Subtrai dois 'Vetor'es.
subtraiVetores :: Vetor -> Vetor -> Vetor
subtraiVetores (Cartesiano x1 y1) (Cartesiano x2 y2) = (Cartesiano (x1-x2) (y1-y2))
--subtraiVetores (Polar x1 a1) (Polar x2 a2) = (Polar (x1*cos(a1)-x2*cos(a2)) (x1*sin(a1)-x2*sin(a2)))

-- | Multiplica um escalar por um 'Vetor'.
multiplicaVetor :: Double -> Vetor -> Vetor
multiplicaVetor a (Cartesiano x1 y1) = (Cartesiano (a*x1) (a*y1))
--multiplicaVetor y (Polar x1 a1) = (Polar (y*x1) a1)

-- ** FunÃ§Ãµes sobre rectas.

-- | Um segmento de reta Ã© definido por dois pontos.
type Reta = (Ponto,Ponto) 

-- | Testar se dois segmentos de reta se intersetam.
--
-- __NB:__ Aplique as equaÃ§Ãµes matemÃ¡ticas bem conhecidas, como explicado por exemplo em <http://www.cs.swan.ac.uk/~cssimon/line_intersection.html>.

intersetam :: Reta -> Reta -> Bool
intersetam ((Cartesiano x1 y1),(Polar x2 a2)) ((Polar x3 y3),(Cartesiano x4 y4)) = intersetam ((Cartesiano x1 y1),(polarCartesiano (Polar x2 a2))) (polarCartesiano(Polar x3 y3),(Cartesiano x4 y4))
intersetam ((Polar x1 y1),(Polar x2 a2)) ((Cartesiano x3 y3),(Cartesiano x4 y4)) = intersetam (polarCartesiano(Polar x1 y1),(polarCartesiano (Polar x2 a2))) ((Cartesiano x3 y3),(Cartesiano x4 y4))
intersetam ((Polar x1 y1),(Cartesiano x2 a2)) ((Polar x3 y3),(Cartesiano x4 y4)) = intersetam (polarCartesiano(Polar x1 y1),(Cartesiano x2 a2)) (polarCartesiano(Polar x3 y3),(Cartesiano x4 y4)) 
intersetam ((Cartesiano x1 y1),(Polar x2 a2)) ((Cartesiano x3 y3),(Polar x4 y4)) = intersetam ((Cartesiano x1 y1),(polarCartesiano (Polar x2 a2))) ((Cartesiano x3 y3),polarCartesiano(Polar x4 y4))
intersetam ((Cartesiano x1 y1),(Polar x2 a2)) ((Cartesiano x3 y3),(Cartesiano x4 y4)) = intersetam ((Cartesiano x1 y1),(polarCartesiano (Polar x2 a2))) ((Cartesiano x3 y3),(Cartesiano x4 y4))
intersetam ((Cartesiano x1 y1),(Cartesiano x2 a2)) ((Polar x3 y3),(Polar x4 y4)) = intersetam ((Cartesiano x1 y1),(Cartesiano x2 a2)) (polarCartesiano(Polar x3 y3),polarCartesiano(Polar x4 y4))
intersetam ((Polar x1 a1),(Polar x2 a2)) ((Polar x3 a3),(Polar x4 a4)) = intersetam ((polarCartesiano (Polar x1 a1)),(polarCartesiano (Polar x2 a2))) ((polarCartesiano (Polar x3 a3)),(polarCartesiano (Polar x4 a4)))
intersetam ((Cartesiano x1 y1),(Cartesiano x2 y2)) ((Cartesiano x3 y3),(Cartesiano x4 y4)) = ((x1+(ta*(x2-x1)))==(x3+(tb*(x4-x3))) && ((y1+ta*(y2-y1))==y3+tb*(y4-y3))) 
                                                                                           where ta =((y3-y4)*(x1-x3)+(x4-x3)*(y1-y3))/((x4-x3)*(y1-y2)-(x1-x2)*(y4-y3))
                                                                                                 tb =((y1-y2)*(x1-x3)+(x2-x1)*(y1-y3))/((x4-x3)*(y1-y2)-(x1-x2)*(y4-y3))


polarCartesiano :: Vetor -> Ponto
polarCartesiano (Polar x1 a1) = (Cartesiano (x1*cos(a1)) (x1*sin(a1)))
polarCartesiano (Cartesiano x y) = (Cartesiano x y)


-- | Calcular o ponto de intersecao entre dois segmentos de reta.
--
-- __NB:__ Aplique as equaÃ§Ãµes matemÃ¡ticas bem conhecidas, como explicado por exemplo em <http://www.cs.swan.ac.uk/~cssimon/line_intersection.html>.
intersecao :: Reta -> Reta -> Ponto
intersecao ((Cartesiano x1 y1),(Cartesiano x2 y2)) ((Cartesiano x3 y3),(Cartesiano x4 y4)) = if intersetam ((Cartesiano x1 y1),(Cartesiano x2 y2)) ((Cartesiano x3 y3),(Cartesiano x4 y4)) then (Cartesiano (x1+ta*(x2-x1)) (y1+ta*(y2-y1))) else error "Não se intersetam"
                                                                                           where ta = (((y3-y4)*(x1-x3) + (x4-x3)*(y1-y3))/((x4-x3)*(y1-y2) - (x1-x2)*(y4-y3)))
                               

-- ** FunÃ§Ãµes sobre listas

-- *** FunÃ§Ãµes gerais sobre listas.
--
-- FunÃ§Ãµes nÃ£o disponÃ­veis no 'Prelude', mas com grande utilidade.

-- | Verifica se o indice pertence Ã  lista.
--
-- __SugestÃ£o:__ use a funÃ§Ã£o 'length' que calcula tamanhos de listas
eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido a l = if (length l)-1 >= a then True else False


-- ** FunÃ§Ãµes sobre matrizes.

-- *** FunÃ§Ãµes gerais sobre matrizes.

-- | A dimensÃ£o de um mapa dada como um par (/nÃºmero de linhas/,/nÃºmero de colunhas/).
type DimensaoMatriz = (Int,Int)

-- | Uma posiÃ§Ã£o numa matriz dada como um par (/linha/,/colunha/).
-- As coordenadas sÃ£o dois nÃºmeros naturais e comeÃ§am com (0,0) no canto superior esquerdo, com as linhas incrementando para baixo e as colunas incrementando para a direita:
--
-- <<http://li1.lsd.di.uminho.pt/images/posicaomatriz.png posicaomatriz>>
type PosicaoMatriz = (Int,Int)

-- | Uma matriz Ã© um conjunto de elementos a duas dimensÃµes.
--
-- Em notaÃ§Ã£o matemÃ¡tica, Ã© geralmente representada por:
--
-- <<https://upload.wikimedia.org/wikipedia/commons/d/d8/Matriz_organizacao.png matriz>>
type Matriz a = [[a]]

-- | Calcula a dimensÃ£o de uma matriz.
--
-- __NB:__ Note que nÃ£o existem matrizes de dimensÃ£o /m * 0/ ou /0 * n/, e que qualquer matriz vazia deve ter dimensÃ£o /0 * 0/.
--
-- __SugestÃ£o:__ relembre a funÃ§Ã£o 'length', referida anteriormente.
dimensaoMatriz :: Matriz a -> DimensaoMatriz
dimensaoMatriz [] = (0,0)
dimensãoMatriz m  = (length m, length(head m))  


-- | Verifica se a posiÃ§Ã£o pertence Ã  matriz.
ePosicaoMatrizValida :: PosicaoMatriz -> Matriz a -> Bool 
ePosicaoMatrizValida (x,y) m = x<=x1 && y<=y1 
                             where (x1,y1)=dimensaoMatriz m

-- * FunÃ§Ãµes recursivas.

-- ** FunÃ§Ãµes sobre Ã¢ngulos

-- | Normaliza um Ã¢ngulo na gama [0..360).
--  Um Ã¢ngulo pode ser usado para representar a rotaÃ§Ã£o
--  que um objecto efectua. Normalizar um Ã¢ngulo na gama [0..360)
--  consiste, intuitivamente, em extrair a orientaÃ§Ã£o do
--  objecto que resulta da aplicaÃ§Ã£o de uma rotaÃ§Ã£o. Por exemplo, Ã© verdade que:
--
-- prop> normalizaAngulo 360 = 0
-- prop> normalizaAngulo 390 = 30
-- prop> normalizaAngulo 720 = 0
-- prop> normalizaAngulo (-30) = 330
normalizaAngulo :: Angulo -> Angulo
normalizaAngulo a = if a >= 90 then normalizaAngulo (a-90) else if (a<=(-90)) then normalizaAngulo (a+90) else a
-- ** FunÃ§Ãµes sobre listas.

-- | Devolve o elemento num dado Ã­ndice de uma lista.
--
-- __SugestÃ£o:__ NÃ£o use a funÃ§Ã£o (!!) :: [a] -> Int -> a :-)
encontraIndiceLista :: Int -> [a] -> a
encontraIndiceLista 0 (h:t) = h
encontraIndiceLista a (h:t) = encontraIndiceLista (a-1) t
encontraIndiceLista _ _ = error "Indice inválido"


-- | Modifica um elemento num dado Ã­ndice.
--
-- __NB:__ Devolve a prÃ³pria lista se o elemento nÃ£o existir.
atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista 0 x (h:t) = (x:t)
atualizaIndiceLista x a (h:t) = h: atualizaIndiceLista (x-1) a t
atualizaIndiceLista _ _ [] = []


-- ** FunÃ§Ãµes sobre matrizes.

-- | Devolve o elemento numa dada 'Posicao' de uma 'Matriz'.
encontraPosicaoMatriz :: PosicaoMatriz -> Matriz a -> a
encontraPosicaoMatriz (0,y) (h:t) = encontraIndiceLista y h
encontraPosicaoMatriz (x,y) (h:t) = encontraPosicaoMatriz (x-1,y) t 
encontraPosicaoMatriz _ _ = error "Posição inválida"

-- | Modifica um elemento numa dada 'Posicao'
--
-- __NB:__ Devolve a prÃ³pria 'Matriz' se o elemento nÃ£o existir.
atualizaPosicaoMatriz :: PosicaoMatriz -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz _ _ [] = []
atualizaPosicaoMatriz (0,y) a (h:t) | ePosicaoMatrizValida (0,y) (h:t) = atualizaIndiceLista y a h : t
                                    | otherwise = (h:t)
atualizaPosicaoMatriz (x,y) a (h:t) | ePosicaoMatrizValida (x,y) (h:t) = h: atualizaPosicaoMatriz (x-1,y) a t
                                    | otherwise = (h:t)    

