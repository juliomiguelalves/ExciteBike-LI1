-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa2_2019li1g012 where

import LI11920
import Tarefa1_2019li1g012
import Tarefa0_2019li1g012

-- * Testes

-- | Testes unitários da Tarefa 2.
--
-- Cada teste é um triplo (/identificador do 'Jogador'/,/'Jogada' a efetuar/,/'Estado' anterior/).
testesT2 :: [(Int,Jogada,Estado)]
testesT2 = [(1,Dispara,Estado (gera 2 5 1) [j1,j2])
            ,(1,Movimenta C,Estado (gera 3 10 1) [j1,j2,j3])
            ,(0,Movimenta C,Estado (gera 3 10 1) [j1,j2,j3])
            ,(2,Movimenta B,Estado (gera 4 5 3) [j1,j2,j4,j3])
            ,(0,Movimenta D,Estado (gera 3 5 42) [j3])
            ,(0,Movimenta D,Estado [[Recta Terra 0, Rampa Terra 0 2, Rampa Terra 2 3, Rampa Terra 3 1]] [j5])
            ,(0,Movimenta E,Estado [[Recta Terra 0, Rampa Terra 0 2, Rampa Terra 2 3, Rampa Terra 3 1]] [j5])
            ,(0,Movimenta D,Estado [[Recta Terra 0, Rampa Terra 0 2, Rampa Terra 2 3, Rampa Terra 3 1]] [jteste])
            ,(0,Movimenta E,Estado [[Recta Terra 0, Rampa Terra 0 2, Rampa Terra 2 3, Rampa Terra 3 1]] [jteste])
            ,(0,Movimenta D,Estado [[Recta Terra 0, Rampa Terra 0 2, Rampa Terra 2 3, Rampa Terra 3 1]] [jteste1])
            ,(0,Movimenta E,Estado [[Recta Terra 0, Rampa Terra 0 2, Rampa Terra 2 3, Rampa Terra 3 1]] [jteste1])
            ,(0,Dispara,Estado (gera 1 5 1) [j6])
            ,(0,Dispara,Estado (gera 1 5 1) [j7])
            ,(0,Dispara,Estado (gera 1 5 1) [j8])
            ,(0,Movimenta C,Estado (gera 1 5 1) [j6])
            ,(0,Movimenta B,Estado (gera 1 5 1) [j6])
            ,(0,Acelera,Estado (gera 1 5 1) [j8])
            ,(0,Desacelera,Estado (gera 1 5 1) [j8])
            ,(1,Movimenta C,Estado (gera 3 10 1) [j1,j9,j3])
            ,(0,Movimenta B,Estado (gera 1 5 1) [j6])
            ,(0,Movimenta D,Estado (gera 1 5 1) [j10])
            ,(0,Movimenta E,Estado (gera 1 5 1) [j10])]


jteste :: Jogador
jteste = Jogador 0 2.0 4.0 1 (Ar 4.0 (90.0) 0)


jteste1 :: Jogador
jteste1 = Jogador 0 2.0 4.0 1 (Ar 4.0 (90.0) 0)

j1::Jogador
j1 = Jogador 1 3.0 1.0 4 (Chao True)

j2:: Jogador
j2 = Jogador 0 4.0 1.0 0 (Chao False)

j3 :: Jogador
j3 = Jogador 2 2.0 0.0 1 (Ar 1.0 90 0)

j4 :: Jogador
j4 = Jogador 2 0.0 0.0 1 (Chao False)

j5 :: Jogador
j5 = Jogador 0 2.0 4.0 1 (Ar 4.0 (-90.0) 0)

j6 :: Jogador
j6 = Jogador 0 0.0 0.0 5 (Chao True)

j7 :: Jogador
j7 = Jogador 0 0.0 0.0 5 (Morto 1.0)

j8 :: Jogador
j8 = Jogador 0 3.0 0.0 5 (Chao False)

j9 :: Jogador
j9 = Jogador 0 4.0 1.0 0 (Chao False)

j10 :: Jogador
j10 = Jogador 0 2.0 1.0 0 (Ar 1.0 45.0 0)

-- * Funções principais da Tarefa 2.

-- | Efetua uma jogada.

jogada :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
       -> Jogada -- ^ A 'Jogada' a efetuar.
       -> Estado -- ^ O 'Estado' anterior.
       -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada.
jogada numJogador (Movimenta dir) e = if ((estaVivo(encontraJogador numJogador (jogadoresEstado e))) && (estaChao (encontraJogador numJogador (jogadoresEstado e)))) 
                                             then e{ jogadoresEstado = (atualizaIndiceLista numJogador (mEstado e (Movimenta dir) (pistaJogadorNova (mapaEstado e) (encontraJogador numJogador (jogadoresEstado e))) (encontraJogador numJogador (jogadoresEstado e))) (jogadoresEstado e))}
                                              else if ((estaVivo(encontraJogador numJogador (jogadoresEstado e)) && (estaAr (encontraJogador numJogador (jogadoresEstado e)))))
                                                then e{ jogadoresEstado = (atualizaIndiceLista numJogador (rodaJogador (Movimenta dir) (encontraJogador numJogador (jogadoresEstado e))) (jogadoresEstado e))}
                                                else e

jogada numJogador (Acelera) e = if ((estaVivo(encontraJogador numJogador (jogadoresEstado e))) && (estaChao (encontraJogador numJogador (jogadoresEstado e))))
                                  then e {jogadoresEstado = atualizaIndiceLista numJogador (aceleraDesacelera (Acelera) (encontraJogador numJogador (jogadoresEstado e))) (jogadoresEstado e)} 
                                   else e     

jogada numJogador (Desacelera) e = if ((estaVivo(encontraJogador numJogador (jogadoresEstado e))) && (estaChao (encontraJogador numJogador (jogadoresEstado e))))
                                    then e {jogadoresEstado = atualizaIndiceLista numJogador (aceleraDesacelera (Desacelera) (encontraJogador numJogador (jogadoresEstado e))) (jogadoresEstado e)} 
                                     else e     

jogada numJogador (Dispara) e = if (estaChao (encontraJogador numJogador (jogadoresEstado e))) && (temMunicoes (encontraJogador numJogador (jogadoresEstado e))) && (notPrimeiraPeca (encontraJogador numJogador (jogadoresEstado e)))
                                then e{mapaEstado = atualizaIndiceLista (pistaDoJogador (encontraJogador numJogador (jogadoresEstado e))) (pistaNova (daDistJog (encontraJogador numJogador (jogadoresEstado e))) (daPista (mapaEstado e) (pistaDoJogador (encontraJogador numJogador (jogadoresEstado e))))) (mapaEstado e) , jogadoresEstado = atualizaIndiceLista numJogador (dispara (Dispara) (encontraJogador numJogador (jogadoresEstado e))) (jogadoresEstado e)}
                                 else e

-- | Funções para a jogada de movimento

mudaDir :: Peca -> Estado -> Jogada -> Jogador -> Jogador
mudaDir peca e (Movimenta C) (Jogador pis dist vel cola estJog) = if (pis > 0 ) 
                                                                then (Jogador (pis-1) dist vel cola (compara C estJog peca e (pis) dist)) 
                                                                 else (Jogador pis dist vel cola estJog)
mudaDir peca e (Movimenta B) (Jogador pis dist vel cola estJog) = if (pis < (numeroPistas (mapaEstado e))-1) 
                                                               then (Jogador (pis+1) dist vel cola (compara B estJog peca e (pis) dist)) 
                                                                 else (Jogador pis dist vel cola estJog)
mudaDir _ _ _ j = j


compara :: Direcao ->  EstadoJogador -> Peca -> Estado -> Int -> Double -> EstadoJogador
compara dir estJog (Recta x alt1) e pis dist =  if (fromIntegral alt1) >encontraPistaJogador pis dist (mapaEstado e) then (Ar (encontraPistaJogador pis dist (mapaEstado e)) (45.0) 0) else estJog
compara dir estJog (Rampa x alt1 alt2) e pis dist =  if (encontraPistaJogador (if dir == C then pis-1 else if dir == B then pis+1 else pis) dist (mapaEstado e)) >((encontraPistaJogador pis dist (mapaEstado e))) then (Ar ((dist-fromIntegral(floor dist))+((encontraPistaJogador pis dist (mapaEstado e)))) (45.0) 0) else estJog


numeroPistas :: Mapa-> Int
numeroPistas [] = 0
numeroPistas (h:t) = 1+ numeroPistas t

rodaJogador :: Jogada -> Jogador -> Jogador
rodaJogador (Movimenta D) (Jogador pis dist vel cola estJog) = (Jogador pis dist vel cola (novaInclinacao (Movimenta D) estJog))
rodaJogador (Movimenta E) (Jogador pis dist vel cola estJog) = (Jogador pis dist vel cola (novaInclinacao (Movimenta E) estJog))
rodaJogador _ j = j

novaInclinacao :: Jogada -> EstadoJogador -> EstadoJogador
novaInclinacao (Movimenta D) (Ar alt inc grav) = if inc==(fromIntegral (-90)) then (Ar alt inc grav) else if inc-15.0 < (fromIntegral (-90))
                                                  then (Ar alt (normalizaAngulo (inc-15.0)) grav)
                                                    else (Ar alt (inc-15.0) grav)
novaInclinacao (Movimenta E) (Ar alt inc grav) = if inc== (fromIntegral 90) then (Ar alt inc grav) else if inc+15.0 > (fromIntegral 90)
                                                   then (Ar alt (normalizaAngulo (inc+15.0)) grav)
                                                    else (Ar alt (inc+15.0) grav)

mEstado :: Estado -> Jogada -> Pista -> Jogador -> Jogador
mEstado e x [] j = j
mEstado e (Movimenta dir) ((Recta y alt1):z) (Jogador pis dist vel cola estJog)= if (dir == B && pis==(numeroPistas(mapaEstado e))-1) || dir == C && pis == 0 then (Jogador pis dist vel cola estJog) else 
                                                                                                      if (abs (checkPistas (Recta y alt1) (if dir == C then (pis-1) else if dir== B then (pis+1) else pis) dist  e)<= abs (0.2))
                                                                                                      then (mudaDir (Recta y alt1) e (Movimenta dir) (Jogador pis dist vel cola estJog))
                                                                                                        else if (fromIntegral alt1 < (encontraPistaJogador (if dir == C then (pis-1) else if dir== B then (pis+1) else pis) dist (mapaEstado e)))
                                                                                                          then (Jogador (if dir == C then (pis-1) else if dir== B then (pis+1) else pis) dist vel cola (compara dir estJog (Recta y alt1) e pis dist))
                                                                                                            else (Jogador pis dist vel cola (Morto 1.0))
mEstado e (Movimenta dir) ((Rampa y alt1 alt2):z) (Jogador pis dist vel cola estJog)=  if (dir == B && pis==(numeroPistas(mapaEstado e))-1) || dir == C && pis == 0 then (Jogador pis dist vel cola estJog) else 
                                                                                                if (abs (checkPistas (Rampa y alt1 alt2) (if dir == C then (pis-1) else if dir== B then (pis+1) else pis) dist e) <= abs (0.2))
                                                                                                           then (mudaDir (Rampa y alt1 alt2) e (Movimenta dir) (Jogador pis dist vel cola estJog))
                                                                                                              else if (((((dist-fromIntegral (floor dist)))*(fromIntegral (if alt2>alt1 then alt2-alt1 else alt1-alt2)))) <(encontraPistaJogador (if dir == C then (pis-1) else if dir== B then (pis+1) else pis) dist (mapaEstado e)))
                                                                                                                then (Jogador (if dir == C then (pis-1) else if dir== B then (pis+1) else pis) dist vel cola (compara dir estJog (Rampa y alt1 alt2) e pis dist))
                                                                                                                  else (Jogador pis dist vel cola (Morto 1.0))


checkPistas :: Peca -> Int -> Double -> Estado -> Double
checkPistas (Recta y alt1) pis dist e = fromIntegral alt1 - (encontraPistaJogador pis dist (mapaEstado e))
checkPistas (Rampa x alt1 alt2) pis dist e = (((( (dist-fromIntegral (floor dist)))*(fromIntegral (if alt2>alt1 then alt2-alt1 else alt1-alt2)))+(fromIntegral (if alt2>alt1 then alt1 else alt2))) - (encontraPistaJogador pis dist (mapaEstado e)))

encontraPistaJogador :: Int -> Double -> Mapa -> Double
encontraPistaJogador x y [h] = daAltJog y h
encontraPistaJogador x y (h:t) = if x == 0 then daAltJog y h else encontraPistaJogador (x-1) y t 

daAltJog :: Double -> Pista -> Double
daAltJog x [(Recta pis alt1)] = (x-fromIntegral(floor x))+fromIntegral alt1 
daAltJog x [(Rampa pis alt1 alt2)] = ((((x-fromIntegral (floor x)))*(fromIntegral (if alt2>alt1 then alt2-alt1 else alt1-alt2))))
daAltJog x ((Recta pis alt1):t) = if (floor x) == 0 then (x-fromIntegral(floor x))+fromIntegral alt1 else daAltJog (x-1.0) t
daAltJog x ((Rampa pis alt1 alt2):t) = if (floor x) == 0 then ((((x-fromIntegral (floor x)))*(fromIntegral (if alt2>alt1 then alt2-alt1 else alt1-alt2)))) else daAltJog (x-1.0) t

novoEstJog :: Peca -> Double -> EstadoJogador -> EstadoJogador
novoEstJog (Rampa x alt1 alt2) dist (Chao y) = if alt1<alt2
                                           then (Ar (fromIntegral(alt2)*(dist-fromIntegral(floor dist))+fromIntegral(alt1))  (radGrau ((atan (fromIntegral alt2)))) 0) 
                                            else (Ar ((fromIntegral(alt1)*(dist-fromIntegral(floor dist))+fromIntegral(alt2))) (radGrau (atan ((1.0/(fromIntegral alt1))))) 0)  
novoEstJog (Recta x alt1) dist (Chao y) = (Ar ((fromIntegral alt1)+(dist-fromIntegral(floor dist))) (45.0) 0)  


radGrau :: Double -> Double
radGrau x = normalizaAngulo (x*(180.0/pi))  

-- |Funções para a velocidade (Se acelera ou desacelera)

aceleraDesacelera :: Jogada -> Jogador -> Jogador
aceleraDesacelera (Acelera) (Jogador pis dist vel cola estJog) = (Jogador pis dist vel cola (speed (Acelera) estJog))
aceleraDesacelera (Desacelera) (Jogador pis dist vel cola estJog) = (Jogador pis dist vel cola (speed (Desacelera) estJog))

speed :: Jogada -> EstadoJogador -> EstadoJogador
speed (Acelera) (Chao False) = (Chao True)
speed (Acelera) (Chao True) = (Chao True)
speed (Desacelera) (Chao True) = (Chao False)
speed (Desacelera) (Chao False) = (Chao False)

-- | Funções para os disparos

dispara :: Jogada -> Jogador -> Jogador
dispara (Dispara) (Jogador pis dist vel cola estJog) = (Jogador pis dist vel (cola-1) estJog)

daDistJog :: Jogador -> Int
daDistJog (Jogador pis dist vel cola estJog) = floor(dist)-1

pistaNova :: Int -> Pista -> Pista
pistaNova x [] = []
pistaNova x (h:t) = if x == 0 then mudaPiso h:t else h:pistaNova (x-1) t

mudaPiso :: Peca -> Peca
mudaPiso (Rampa x alt1 alt2) = (Rampa (Cola) alt1 alt2)
mudaPiso (Recta x alt1) = (Recta (Cola) alt1)

temMunicoes :: Jogador -> Bool
temMunicoes (Jogador pis dist vel cola estJog) = cola>0

pistaJogadorNova :: Mapa -> Jogador -> Pista
pistaJogadorNova [] jog = []
pistaJogadorNova map (Jogador pis dist vel cola estJog) = daPista map pis

daPista :: Mapa -> Int -> Pista
daPista [] x = []
daPista (h:t) x = if x == 0 then h else daPista t (x-1)


pistaDoJogador :: Jogador -> Int
pistaDoJogador (Jogador pis dist vel cola estJog) = pis


-- | Funções auxiliadores das diferentes jogadas


notPrimeiraPeca :: Jogador -> Bool
notPrimeiraPeca (Jogador pis dist vel cola estJog) = abs(dist) > 0

encontraJogador :: Int -> [Jogador] -> Jogador
encontraJogador 0 (h:t) = h
encontraJogador a (h:t) = encontraJogador (a-1) t

estaChao :: Jogador -> Bool
estaChao (Jogador pis dist vel cola estJog) = (estJog == (Chao True)) || (estJog == (Chao False)) 

estaVivo :: Jogador -> Bool
estaVivo (Jogador pis dist vel cola (Morto x)) = False
estaVivo jog = True

estaAr :: Jogador -> Bool
estaAr (Jogador pis dist vel cola estJog) = aux estJog

aux :: EstadoJogador -> Bool
aux (Ar alt inc grav) = True
aux _ = False



