-- | Este módulo define funções comuns da Tarefa 4 do trabalho prático.
module Tarefa4_2019li1g012 where

import LI11920
import Tarefa3_2019li1g012
import Tarefa2_2019li1g012
import Tarefa1_2019li1g012

-- * Testes
-- | Testes unitários da Tarefa 4.
--
-- Cada teste é um par (/tempo/,/'Mapa'/,/'Jogador'/).
testesT4 :: [(Double,Mapa,Jogador)]
testesT4 = [ (1.0,gera 2 5 1,Jogador {pistaJogador = 1, distanciaJogador = 1.5, velocidadeJogador = 0.0, colaJogador = 0, estadoJogador = Chao {aceleraJogador = False}})
            ,(1.0,gera 2 5 1,Jogador {pistaJogador = 1, distanciaJogador = 1.5, velocidadeJogador = 2.0, colaJogador = 0, estadoJogador = Chao {aceleraJogador = True}})
            ,(4.0, gera 2 5 1,Jogador {pistaJogador = 1, distanciaJogador = 3.0, velocidadeJogador = 2.0, colaJogador = 0, estadoJogador = Chao {aceleraJogador = True}})
            ,(2.4, gera 2 5 342,Jogador {pistaJogador = 0, distanciaJogador = 2.3, velocidadeJogador = 1.0, colaJogador = 0, estadoJogador = Chao {aceleraJogador = True}})
            ,(2.4, gera 2 5 342,Jogador {pistaJogador = 0, distanciaJogador = 2.3, velocidadeJogador = 0.0, colaJogador = 0, estadoJogador = Morto {timeoutJogador = 1.0}})
            ,(2.4, gera 2 5 342,Jogador {pistaJogador = 0, distanciaJogador = 2.3, velocidadeJogador = 1.0, colaJogador = 0, estadoJogador = Ar {alturaJogador = 2.0,inclinacaoJogador = 23.0, gravidadeJogador= 1.0}})
            ,(2.4, gera 2 5 342,Jogador {pistaJogador = 0, distanciaJogador = 2.3, velocidadeJogador = 1.0, colaJogador = 0, estadoJogador = Ar {alturaJogador = 2.0,inclinacaoJogador = 45.1, gravidadeJogador= 1.0}})
            ,(2.4, gera 2 5 342,Jogador {pistaJogador = 0, distanciaJogador = 2.3, velocidadeJogador = 0.0, colaJogador = 0, estadoJogador = Ar {alturaJogador = 2.0,inclinacaoJogador = 45.1, gravidadeJogador= 1.0}})
            ,(2.4, gera 2 5 342,Jogador {pistaJogador = 0, distanciaJogador = 2.3, velocidadeJogador = 0.0, colaJogador = 0, estadoJogador = Ar {alturaJogador = 2.0,inclinacaoJogador = 23.0, gravidadeJogador= 1.0}})
            ,(0.0,gera 2 5 1,Jogador {pistaJogador = 1, distanciaJogador = 1.5, velocidadeJogador = 0.0, colaJogador = 0, estadoJogador = Chao {aceleraJogador = True}})
            ,(5.0,gera 2 5 1,Jogador {pistaJogador = 1, distanciaJogador = 1.5, velocidadeJogador = 0.0, colaJogador = 0, estadoJogador = Chao {aceleraJogador = True}})]

-- * Funções principais da Tarefa 4.

-- | Avança o estado de um 'Jogador' um 'passo' em frente, durante um determinado período de tempo.

passo :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após um 'passo'.
passo t m j = move t m (acelera t m j)

-- | Altera a velocidade de um 'Jogador', durante um determinado período de tempo.
acelera :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após acelerar.
acelera x mapa (Jogador pis dist vel cola estJog)= if estaChao (Jogador pis dist vel cola estJog) then (Jogador pis dist (daVelocidadeChao x mapa pis dist vel estJog) cola estJog)
                                                      else if (estaAr (Jogador pis dist vel cola estJog))
                                                        then (Jogador pis dist (daVelocidadeAr x vel) cola ( daGravidade x estJog))
                                                        else (Jogador pis dist vel cola estJog)

-- | Dá a velocidade do jogador no ar

daVelocidadeAr :: Double -> Double  -> Double
daVelocidadeAr tempo vel = vel-(0.125*vel*tempo)

-- | Dá a gravidade do jogador 

daGravidade :: Double -> EstadoJogador -> EstadoJogador
daGravidade tempo (Ar alt inc grav) = Ar alt inc (grav+tempo)

-- | Dá a velocidade do jogador conforme os diferentes pisos

daVelocidadeChao :: Double -> Mapa -> Int -> Double -> Double -> EstadoJogador -> Double
daVelocidadeChao tempo mapa pis dist vel estJog = case (daPisoMapa mapa pis dist) of
                                              Terra -> if vel+(fromIntegral(acelMota vel estJog) -0.25*vel)*tempo <0.0 then 0.0 else vel+(fromIntegral(acelMota vel estJog) -0.25*vel)*tempo
                                              Relva -> if vel+(fromIntegral(acelMota vel estJog) -0.75*vel)*tempo <0.0 then 0.0 else vel+(fromIntegral(acelMota vel estJog) -0.75*vel)*tempo 
                                              Lama  -> if vel+(fromIntegral(acelMota vel estJog) -1.50*vel)*tempo <0.0 then 0.0 else vel+(fromIntegral(acelMota vel estJog) -1.50*vel)*tempo 
                                              Boost -> if vel+(fromIntegral(acelMota vel estJog) +0.5*vel)*tempo < 0.0 then 0.0 else vel+(fromIntegral(acelMota vel estJog) +0.5*vel)*tempo
                                              Cola  -> if vel+(fromIntegral(acelMota vel estJog) -3.0*vel)*tempo < 0.0 then 0.0 else vel+(fromIntegral(acelMota vel estJog) -3.0*vel)*tempo


-- | Função que devolve o piso que se encontra na pista x e à distância y 

daPisoMapa :: Mapa -> Int -> Double -> Piso
daPisoMapa [h] pis dist = daPisoPista dist h
daPisoMapa (h:t) pis dist = if pis == 0 then daPisoPista dist h else daPisoMapa t (pis-1) dist

-- | Função que dá o piso à distância y de uma pista

daPisoPista :: Double -> Pista -> Piso
daPisoPista x [h] = daPisoPeca h
daPisoPista x (h:t) = if floor x== 0 then daPisoPeca h else daPisoPista (x-1.0) t 

-- | Dá o piso que uma peça possui

daPisoPeca :: Peca -> Piso
daPisoPeca (Recta piso int) = piso
daPisoPeca (Rampa piso int1 int2) = piso

-- | Função que diz se a mota pode acelerar ou não

acelMota :: Double -> EstadoJogador -> Int
acelMota vel (Chao y) = if vel<2 && y==True then 1 else 0
acelMota vel _ = 0

-- | Altera a posição de 'Jogador', durante um determinado período de tempo.
move :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após se movimentar.
move tempo mapa (Jogador pis dist vel cola (Morto x)) = if x - tempo > fromIntegral 0 then (Jogador pis dist vel cola (Morto (x-tempo))) else (Jogador pis dist 0.0 cola (Chao False))
move tempo mapa (Jogador pis dist vel cola (Chao x )) = if daAltura (daPista mapa pis) dist == altPecaAnterior mapa pis dist then (Jogador pis novadist vel cola (Chao x))
                                                          else if  dentroPeca dist && (inclinacao mapa pis (if dist-fromIntegral (floor dist) == 0.0 then dist-0.1 else dist) <= inclinacao mapa pis (if dist-fromIntegral (floor dist) == 0.0 then dist else fromIntegral (ceiling dist)))
                                                            then (Jogador pis novadist vel cola (Chao x))
                                                             else (Jogador pis novadist vel cola (Ar (altPecaAnterior mapa pis dist) (inclinacao mapa pis dist) (fromIntegral 0) ))
                                                             where novadist=if vel == fromIntegral 0 then dist else if dist -fromIntegral (floor dist) == 0.0 then dist+1.0 else fromIntegral (ceiling dist)
move tempo mapa (Jogador pis dist vel cola (Ar altura inc grav)) = if (vel== fromIntegral 0) && inc-inclinacao mapa pis dist >= fromIntegral 45 then (Jogador pis dist vel cola (Morto 1.0))
                                                                    else if altura >= fromIntegral 0 && dentroPeca dist
                                                                    then (Jogador pis novadist vel cola (if (alturaNova altura vel (grauRad inc) grav tempo) <= altPecaAnterior mapa pis dist then (Chao False) else (Ar (alturaNova altura vel (grauRad inc) grav tempo) inc grav)))
                                                                    else if inc - inclinacao mapa pis dist >= fromIntegral 45 
                                                                        then (Jogador pis novadist (fromIntegral 0) cola (Morto 1.0)) 
                                                                        else (Jogador pis novadist vel cola (Chao False))
                                                                        where novadist=if vel == fromIntegral 0 then dist else if dist -fromIntegral (floor dist) == 0.0 then dist+1.0 else fromIntegral (ceiling dist)


-- | Função que dá a altura de um jogador

alturaNova :: Double -> Double -> Double -> Double -> Double -> Double
alturaNova altura vel inc grav tempo =sin(inc)*(vel*tempo)

-- | Função que dá a altura da peça anterior

altPecaAnterior :: Mapa -> Int -> Double -> Double
altPecaAnterior mapa pis dist = daAltura (daPista mapa pis) dist

-- | Função que dá a altura de uma peça

daAltura :: Pista -> Double -> Double
daAltura [] x = fromIntegral 0
daAltura ((Recta piso x):t) dist = if floor dist == 0 then fromIntegral x else daAltura t (dist-1.0)
daAltura ((Rampa piso x y):t) dist = if floor dist == 0 then (if x > y then fromIntegral x else fromIntegral y) else daAltura t (dist-1.0) 

-- | Função para ver se o jogador está dentro da peça

dentroPeca :: Double -> Bool
dentroPeca x = x-fromIntegral(floor x) /= fromIntegral 0 

-- | Função que dá a inclinacao de uma peca à distância y e no piso x

inclinacao :: Mapa -> Int -> Double -> Double
inclinacao mapa pis dist = inclinacaoPista (daPista mapa pis) dist


-- | Função que dá a inclinação de uma peça à distância y

inclinacaoPista :: Pista -> Double -> Double
inclinacaoPista [h] x = inclinacaoPeca h
inclinacaoPista (h:t) x = if floor x == 0 then inclinacaoPeca h else inclinacaoPista t (x-1.0)

-- | Função que dá a inclinação de uma peça

inclinacaoPeca :: Peca -> Double
inclinacaoPeca (Recta piso x) = fromIntegral 0
inclinacaoPeca (Rampa piso x y) = if y>x then radGrau (atan (fromIntegral y)) else -(radGrau (atan(fromIntegral x)))

-- | Função que converte graus para radianos
grauRad :: Double -> Double
grauRad x = (x*pi)/180