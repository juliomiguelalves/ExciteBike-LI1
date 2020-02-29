module Main where

import LI11920
import System.Random
import Tarefa0_2019li1g012
import Tarefa1_2019li1g012
import Tarefa2_2019li1g012
import Tarefa3_2019li1g012
import Tarefa4_2019li1g012
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy


-- | O objetivo desta função era desenvolver a parte gráfica do jogo, de forma a que este se tornasse o mais apelativo possível.
-- | Para o fazer, tentei criar um jogo esteticamente simples e que conseguisse representar da melhor forma todos os possíveis estados do jogo.
-- | Por não saber utilizar ferramentas de edição de imagem, tentei simplificar ao máximo aquilo que iria pôr no jogo. Desta forma,
-- | as únicas imagens carregadas são as motas e o background. As peças são todas feitas através do Gloss.        
-- | Na conclusão desta tarefa, consegui fazer com que o jogador andasse para a frente e para as diferentes pistas e que os seus disparos sejam representados.
-- | No entanto, o movimento do jogador não está muito bem conseguido pois ele "salta" entre as diferentes peças e não se move "normalmente" e também não é representado
-- | corretamente quando o jogador se encontra no Ar.

-- | Mapa que será apresentado

mapaJogo :: Mapa
mapaJogo = gera 4 5 1

-- | Jogador 1

jogador0 :: Jogador
jogador0 = (Jogador 0 0.0 0.0 4 (Chao False))

-- | Jogador 2

jogador1 :: Jogador
jogador1 = (Jogador 1 0.0 0.0 4 (Chao False))

-- | Jogador 3

jogador2 :: Jogador
jogador2 = (Jogador 2 0.0 0.0 4 (Chao False))

-- | Jogador 4

jogador3 :: Jogador
jogador3 = (Jogador 3 0.0 0.0 4 (Chao False))

-- | Imagem que produz uma rampa a descer

irampadesce :: Picture
irampadesce = Polygon [(0,0),(200,0),(0,200),(0,0)]

-- | Imagem que produz uma rampa a subir

irampasobe :: Picture
irampasobe = Polygon [(0,0),(200,0),(200,200),(0,0)]

-- | Imagem de uma recta

irecta :: Picture
irecta = Polygon [(0,0),(200,0),(200,200),(0,200),(0,0)]

-- | Cor da relva

corRelva :: Color
corRelva = green

-- | Cor da terra

corTerra :: Color
corTerra = makeColor 0.61 0.46 0.33 1

-- | Cor da cola

corCola :: Color
corCola = makeColor 0.46 0.61 0.13 1

-- | Cor da lama

corLama :: Color
corLama = makeColor 0.61 0.40 0.22 1

-- | Cor do boost

corBoost :: Color
corBoost = red

-- | Largura em que o mapa é começado a desenhar

width :: Float
width = (-800)

-- | Altura em que o mapa é começado a desenhar

height :: Float
height = 300

--| Estado Inicial do jogo que utiliza o mapa gerado e os jogadores que serão necessários num máximo de 4 

estadoInicial :: Estado
estadoInicial = (Estado mapaJogo (quantosjog mapaJogo [jogador0,jogador1,jogador2,jogador3]))

-- | Função que dá quantos jogadores serão necessários para o mapa em questão

quantosjog :: Mapa -> [Jogador] -> [Jogador]
quantosjog [] _ = []
quantosjog (h:t) l =  take (npistas (h:t)) l

-- | Função que diz quantas pistas tem o mapa

npistas :: Mapa -> Int
npistas [] = 0
npistas (h:t) = 1+npistas t

-- | Função utilizada para desenhar o mapa e os jogadores

desenhaEstado :: Estado -> [Picture] -> [Picture]
desenhaEstado (Estado m j@((Jogador pis dist vel cola estJog):t)) texturas = [Scale 1.6 1.4 (head texturas)] ++ desenhaMapa m width height texturas ++ desenhaJogadores 1 e texturas
                                                                         where e = (Estado m ((Jogador pis dist vel cola estJog):t))

-- | Função que desenha os jogadores

desenhaJogadores :: Int->Estado -> [Picture] -> [Picture]
desenhaJogadores x (Estado m (h:t)) images = desenhaJogador x m h images : desenhaJogadores (x+1) (Estado m t) images
desenhaJogadores x (Estado m []) _  = [blank]

-- | Função que desenha um jogador

desenhaJogador :: Int -> Mapa ->Jogador -> [Picture] -> Picture
desenhaJogador x m j@(Jogador pis dist vel cola estJog) images = if estaAr j then Translate ((p0 dist)+40) ((p1 pis)+80) (Pictures[Rotate (-(realToFrac (inclJog j))) (Scale 3 3 (encontraIndiceLista x images))])
                                                               else if pecaMapa m pis dist && dist < (tamanhoPistaMapa m) 
                                                                then if rampadesce m pis dist 
                                                                 then Pictures[Translate ((p0 dist)+40) ((p1 pis)+80) (Pictures[Rotate (-(realToFrac (inclinacao m pis dist))) (Scale 3 3 (encontraIndiceLista x images))])]
                                                                  else Pictures[Translate (p0 dist) ((p1 pis)-100) (Pictures[Rotate (-(realToFrac (inclinacao m pis dist))) (Scale 3 3 (encontraIndiceLista x images))])]
                                                                   else if dist < (tamanhoPistaMapa m) 
                                                                    then Pictures[Translate (p0 dist) ((p1 pis)-100) (Scale 3 3 (encontraIndiceLista x images))]
                                                                     else Pictures[Translate (p0 (tamanhoPistaMapa m)) ((p1 pis)-100) (Scale 3 3 (encontraIndiceLista x images))]

-- | Função que dá a inclinação de um jogador

inclJog :: Jogador -> Double
inclJog (Jogador pis dist vel cola (Ar alt inc grav)) = inc

-- | Função que percorre o mapa para ver se uma peça é rampa ou não


pecaMapa :: Mapa -> Int -> Double -> Bool
pecaMapa [h] x y = pecaPista h y
pecaMapa (h:t) 0 y = pecaPista h y 
pecaMapa (h:t) x y = pecaMapa t (x-1) y 

-- | Função que percorre uma pista para ver se uma peça é rampa 

pecaPista :: Pista -> Double -> Bool
pecaPista [h] x= eRampa h
pecaPista (h:t) 0 = eRampa h 
pecaPista (h:t) x = pecaPista t (x-1.0) 

-- | Função que diz se uma peça é rampa ou não

eRampa :: Peca -> Bool
eRampa (Rampa _ _ _) = True
eRampa _ = False

-- | Função que percorre o mapa para ver se uma peça é uma rampa a descer ou não

rampadesce :: Mapa -> Int -> Double -> Bool
rampadesce [h] pis dist = simOuNao h dist
rampadesce (h:t) 0 dist = simOuNao h dist
rampadesce (h:t) pis dist = rampadesce t (pis-1) dist

-- | Função que diz se uma rampa é a descer ou não

simOuNao :: Pista -> Double -> Bool
simOuNao [(Rampa piso alt1 alt2)] x = alt1>alt2
simOuNao ((Rampa piso alt1 alt2):t) 0 = alt1> alt2
simOuNao (h:t) x = simOuNao t (x-1)

-- | Função que nos dá o comprimento do mapa

tamanhoPistaMapa :: Mapa -> Double
tamanhoPistaMapa (h:t) = tamanhoPista h
tamanhoPistaMapa [] = 0

tamanhoPista :: Pista -> Double
tamanhoPista (h:t) = 1+tamanhoPista t
tamanhoPista [] = 0


-- | Valor do lado de cada peça

l :: Float
l = 200

-- | Funções que dão a posição de um jogador no mapa

p0 :: Double -> Float
p0 x = width + ((realToFrac x)*l)

p1 :: Int -> Float
p1 y = height - ((((realToFrac y)-1)*l)+50)

-- | Função que desenha o mapa

desenhaMapa ::Mapa -> Float -> Float -> [Picture] -> [Picture]
desenhaMapa (h:t) x y images = desenhaPista h x y (listFromTo 4 7 images) ++ (desenhaMapa t (x) (y-200) (listFromTo 4 7 images))
desenhaMapa _ _ _ _ = []

-- | Função que pega nos valores de uma lista compreendidos entre 2 indíces

listFromTo :: Int -> Int -> [a] -> [a]
listFromTo 0 f l = take (f+1) l
listFromTo i f (h:t) = listFromTo (i-1) (f-1) t 
listFromTo _ _ _ = []

-- | Função que desenha uma pista

desenhaPista :: Pista -> Float -> Float -> [Picture] -> [Picture]
desenhaPista (h:t) x y images = desenhaPeca h x y images:desenhaPista t (x+200) y images
desenhaPista [] x y images = []

-- | Função que desenha uma peça

desenhaPeca :: Peca -> Float -> Float -> [Picture] -> Picture
desenhaPeca (Recta piso alt) x y images = case piso of 
                                        Relva -> Pictures[Translate x y (Color corRelva irecta)]
                                        Terra -> Pictures[Translate x y (Color corTerra irecta)]
                                        Cola  -> Pictures[Translate x y (Color corCola  irecta)]
                                        Lama  -> Pictures[Translate x y (Color corLama  irecta)]
                                        Boost -> Pictures[Translate x y (Color corBoost irecta)]
desenhaPeca (Rampa piso alt1 alt2) x y images =  case piso of 
                                      Relva -> if alt2>alt1 then Pictures[Translate x y (Color corRelva irampasobe)] else Pictures[Translate x y (Color corRelva irampadesce)]
                                      Terra -> if alt2>alt1 then Pictures[Translate x y (Color corTerra irampasobe)] else Pictures[Translate x y (Color corTerra irampadesce)]
                                      Cola  -> if alt2>alt1 then Pictures[Translate x y (Color corCola  irampasobe)] else Pictures[Translate x y (Color corCola  irampadesce)]
                                      Lama  -> if alt2>alt1 then Pictures[Translate x y (Color corLama  irampasobe)] else Pictures[Translate x y (Color corLama  irampadesce)]
                                      Boost -> if alt2>alt1 then Pictures[Translate x y (Color corBoost irampasobe)] else Pictures[Translate x y (Color corBoost irampadesce)]

-- | Função que possui os comandos para cada ação de cada jogador

reageEvento :: Event -> Estado -> Estado
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) e =  jogada 0 (Movimenta C) e
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) e =  jogada 0 (Movimenta B) e
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) e =  jogada 0 (Movimenta D) e
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) e =  jogada 0 (Movimenta E) e
reageEvento (EventKey (Char 'm') Down _ _) e =  jogada 0 (Acelera) e
reageEvento (EventKey (Char 'n') Down _ _) e =  jogada 0 (Desacelera) e
reageEvento (EventKey (SpecialKey KeySpace) Down _ _) e = jogada 0 (Dispara) e
reageEvento (EventKey (Char '1') Down _ _) e =  jogada 1 (Movimenta C) e
reageEvento (EventKey (Char '2') Down _ _) e =  jogada 1 (Movimenta B) e
reageEvento (EventKey (Char '3') Down _ _) e =  jogada 1 (Movimenta D) e
reageEvento (EventKey (Char '4') Down _ _) e =  jogada 1 (Movimenta E) e
reageEvento (EventKey (Char '5') Down _ _) e =  jogada 1 (Acelera) e
reageEvento (EventKey (Char '6') Down _ _) e =  jogada 1 (Desacelera) e
reageEvento (EventKey (Char '7') Down _ _) e =  jogada 1 (Dispara) e
reageEvento (EventKey (Char 'w') Down _ _) e =  jogada 2 (Movimenta C) e
reageEvento (EventKey (Char 's') Down _ _) e =  jogada 2 (Movimenta B) e
reageEvento (EventKey (Char 'd') Down _ _) e =  jogada 2 (Movimenta D) e
reageEvento (EventKey (Char 'a') Down _ _) e =  jogada 2 (Movimenta E) e
reageEvento (EventKey (Char 'f') Down _ _) e =  jogada 2 (Acelera) e
reageEvento (EventKey (Char 'g') Down _ _) e =  jogada 2 (Desacelera) e
reageEvento (EventKey (Char 'h') Down _ _) e =  jogada 2 (Dispara) e
reageEvento (EventKey (Char 'i') Down _ _) e =  jogada 3 (Movimenta C) e
reageEvento (EventKey (Char 'k') Down _ _) e =  jogada 3 (Movimenta B) e
reageEvento (EventKey (Char 'l') Down _ _) e =  jogada 3 (Movimenta D) e
reageEvento (EventKey (Char 'j') Down _ _) e =  jogada 3 (Movimenta E) e
reageEvento (EventKey (Char 'o') Down _ _) e =  jogada 3 (Acelera) e
reageEvento (EventKey (Char 'p') Down _ _) e =  jogada 3 (Desacelera) e
reageEvento (EventKey (Char 'y') Down _ _) e =  jogada 3 (Dispara) e
reageEvento _ s = s -- ignora qualquer outro evento


-- | Função que dita o que acontece durante o tempo aos jogadores

reageTempo :: Float -> Estado -> Estado
reageTempo n (Estado m []) = (Estado m [])
reageTempo n (Estado m [j0]) = (Estado m [passo (realToFrac n) m jogador0])
reageTempo n (Estado m [j0,j1]) = (Estado m [passo (realToFrac n) m j0,passo (realToFrac n) m j1])
reageTempo n (Estado m [j0,j1,j2]) = (Estado m [passo (realToFrac n) m j0,passo (realToFrac n) m j1,passo (realToFrac n) m j2])   
reageTempo n (Estado m [j0,j1,j2,j3]) = (Estado m [passo (realToFrac n) m j0,passo (realToFrac n) m j1,passo (realToFrac n) m j2,passo (realToFrac n) m j3])


-- * Estado Gloss

type EstadoGloss = (Estado,[Picture],(Float,Float))

-- | Função que passa uma lista d e imagens para um EstadoGloss

estadoGlossInicial :: [Picture] -> EstadoGloss
estadoGlossInicial images = (estadoInicial,images,(0,1900))

-- | Função que desenha um EstadoGloss

desenhaEstadoGloss :: EstadoGloss -> Picture
desenhaEstadoGloss (e,imagens,k) = Pictures[Pictures(desenhaEstado e imagens)]

-- | Função que atualiza o EstadoGloss conforme um evento

reageEventoGloss :: Event -> EstadoGloss -> EstadoGloss
reageEventoGloss ev (e,imagens,k) = (reageEvento ev e,imagens,k)

-- | Função que atualiza um EstadoGloss conforme um tempo

reageTempoGloss :: Float -> EstadoGloss -> EstadoGloss
reageTempoGloss t (e,imagens,k) = (reageTempo t e,imagens,k)

-- | Frame Rate

fr :: Int
fr = 1

-- | Função que diz o tipo de janela em que o jogo vai correr

dm :: Display
dm = FullScreen

main :: IO ()
main = do 
    Just fundo <- loadJuicy "images/maxresdefault2.jpg"
    Just mtAzul <- loadJuicy "images/mota_azul.png"
    Just mtVerde <- loadJuicy "images/mota_verde.png"
    Just mtAmarela <- loadJuicy "images/mota_amarela.png"
    Just mtVermelha <- loadJuicy "images/mota_vermelha.png"
    play dm                       -- janela onde irá correr o jogo
        (greyN 0.5)               -- côr do fundo da janela
        fr                        -- frame rate
        (estadoGlossInicial [fundo,mtAzul,mtVerde,mtAmarela,mtVermelha,irampadesce,irampasobe,irecta]) -- estado inicial
        desenhaEstadoGloss        -- desenha o estado do jogo
        reageEventoGloss          -- reage a um evento
        reageTempoGloss           -- reage ao passar do tempo
