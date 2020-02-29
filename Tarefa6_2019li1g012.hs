-- | Este módulo define funções comuns da Tarefa 6 do trabalho prático.
module Tarefa6_2019li1g012 where

import LI11920
import Tarefa0_2019li1g012
import Tarefa1_2019li1g012
import Tarefa2_2019li1g012
import Tarefa3_2019li1g012
import Tarefa4_2019li1g012

-- | Para esta tarefa, havia o desafio de criar um bot que se desviasse de peças que o tornariam mais lento (como por exemplo a cola e a lama)
-- | , que disparasse cola contra os outros jogadores, que não morresse na transição Ar-Chão e que chegasse primeiro à meta que todos os outros.
-- | Comecei por definir uma função que fazia o jogador se movimentasse para a direita ou para a esquerda, conforme a sua inclinação.
-- | Em segundo lugar, precisava que o jogador, caso estivesse no chão e não estivesse a acelerar, acelerasse de forma a mover-se para a frente.
-- | De seguida, fiz a função que faz o jogador disparar cola caso tenha jogadores atrás de si, na mesma pista e a uma distância inferior a 2 blocos.
-- | Por fim, foi feita a função que faz o jogador mudar de pista, que examina todas as peças que se encontram à distância do jogador.
-- | Na conclusão desta tarefa, observei que as duas primeiras funcionalidades que tentei estabelecer funcionam bem. 
-- | No entanto, a funcionalidade de disparar e a funcionalidade de mover o jogador entre as diferentes pistas não estão eficazes pois o jogador apenas as efetuavam no inicio de cada jogo.
          

          
-- * Funções principais da Tarefa 6.

-- | Define um ro'bot' capaz de jogar autonomamente o jogo.
bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
bot num e@(Estado m js) | (estaAr (encontraJogador num js)) && (abs((inclinacao m (pistaDoJogador (encontraJogador num js)) (distDoJog num js)) - abs(incJogador num js)) > 45) = decideMov (incJogador num js)
                        | chaoFalse (encontraJogador num js) = Just Acelera
                        | temBalas (encontraJogador num js) &&  jogAtras (encontraJogador num js) js = Just Dispara
                        | otherwise = paraMelhorPista m (encontraJogador num js)

-- | Função que indica se um jogador tem balas

temBalas :: Jogador -> Bool
temBalas (Jogador pis dist vel cola estJog) = cola > 0

-- | Funçao que indica se o jogador está a acelerar

chaoFalse :: Jogador -> Bool
chaoFalse (Jogador pis dist vel cola (Chao False)) = True
chaoFalse (Jogador pis dist vel cola _) = False

-- | Função que diz se existe algum jogador atrás

jogAtras :: Jogador -> [Jogador] -> Bool
jogAtras (Jogador pis dist vel cola estJog) js = temAlgumJogAtras pis dist js

temAlgumJogAtras :: Int -> Double -> [Jogador] -> Bool
temAlgumJogAtras pis dist [] = False
temAlgumJogAtras pis dist js@((Jogador pis1 dist1 vel cola estJog):t) = ((contaJogAtras dist js >=2) && (contaJogPis pis js)>=2) 

contaJogAtras :: Double -> [Jogador] -> Int
contaJogAtras dist [] = 0
contaJogAtras dist ((Jogador pis dist1 vel cola estJog):t) = if dist-dist1<=2.0 && dist-dist1 >= 0.0 then 1+contaJogAtras dist t else contaJogAtras dist t

contaJogPis :: Int -> [Jogador] -> Int
contaJogPis pis [] = 0
contaJogPis pis ((Jogador pis1 dist vel cola estJog):t) = if pis==pis1 then 1+contaJogPis pis t else contaJogPis pis t


-- | Função que decide para que lado o jogador se deve mover no ar de forma a não morrer

decideMov :: Double -> Maybe Jogada
decideMov x | x > 0 = Just (Movimenta D)
            | x < 0 = Just (Movimenta E)
            | otherwise = Nothing

-- | Função que dá a inclinação do Jogador no ar

incJogador :: Int -> [Jogador] -> Double
incJogador 0 (h:t) = incl h
incJogador x (h:t) = incJogador (x-1) t

incl :: Jogador -> Double
incl (Jogador pis dist vel cola (Ar alt inc grav)) = inc
incl (Jogador pis dist vel cola _) = 0

-- | Função que indica a distância a que o jogador se encontra

distDoJog :: Int -> [Jogador] -> Double
distDoJog 0 (h:t) = distancia h
distDoJog x (h:t) = distDoJog (x-1) t 

distancia :: Jogador -> Double
distancia (Jogador pis dist vel cola estJog) = dist

-- | Função que diz qual a indica qual a melhor pista para a qual o jogador deve ir

paraMelhorPista :: Mapa -> Jogador -> Maybe Jogada
paraMelhorPista m (Jogador pis dist vel cola estJog) = vaiMelhor pis dist m

qualMelhor :: Int -> Double -> Mapa -> Double
qualMelhor pis dist (h:t) = melhor (melhorPeca dist (h:t))

vaiMelhor :: Int -> Double -> Mapa -> Maybe Jogada
vaiMelhor pis dist m | daIndice (qualMelhor pis dist m) (melhorPeca dist m) == pis = Just Acelera
                     | daIndice (qualMelhor pis dist m) (melhorPeca dist m) > pis = Just (Movimenta B)
                     | daIndice (qualMelhor pis dist m) (melhorPeca dist m) < pis = Just (Movimenta C)


melhor :: [Double] -> Double
melhor [h] = h
melhor (h:z:t) = if h<=z then melhor (h:t) else melhor (z:t)


daIndice :: Double -> [Double] -> Int
daIndice x (h:t) = if x == h then 0 else 1 + daIndice x t


melhorPeca :: Double -> Mapa -> [Double]
melhorPeca _ [] = []
melhorPeca dist (h:t) =  pecaSeg dist h : melhorPeca dist t

pecaSeg :: Double -> Pista -> Double
pecaSeg dist (h:t:z) = if floor dist == 0 then vePeca t else pecaSeg (dist-1) (t:z)
pecaSeg dist [h] = vePeca h

vePeca :: Peca -> Double
vePeca (Rampa piso x y) = case piso of
                                Terra -> 0.25
                                Relva -> 0.75 
                                Lama -> 1.5 
                                Boost -> -0.5 
                                Cola -> 3
vePeca (Recta piso x) = case piso of
                                Terra -> 0.25
                                Relva -> 0.75 
                                Lama -> 1.5 
                                Boost -> -0.5 
                                Cola -> 3                               

-- | Função que dá o tamanho de um mapa

distMap :: Mapa -> Int
distMap (h:t) = tamanho h

tamanho :: Pista -> Int
tamanho [] = 0
tamanho (h:t) = 1+ tamanho t

                 
