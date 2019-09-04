{--
PROGRAMAÇÃO FUNCIONAL - JOGO: PONG

ATIVIDADE:
1. Estender o programa Pong trabalhado na aula de quinta feira 22/08 adicionando ao mundo duas "raquetes". Cada raquete é representada por um retângulo, um posicionado no lado esquerdo da janela e outro no lado direito. A bola, além de rebater nas paredes superior e inferior, pode também rebater nas raquetes. Teste o programa "lançando" a bola com diversos ângulos.

2. Estender Pong adicionando funcionalidade para mover as raquetes. A raquete esquerda deverá ser controlada com as teclas 'w' e 's'. Se a tecla 'w' estiver apertada a raquete deverá se mover na direção superior, enquanto que se a tecla 's' estiver apertada a raquete se moverá na direção inferior. Se nenhuma de ambas teclas estiver apertada, a raquete ficará parada na posição em que se encontra. De forma similar, a raquete da direita será controlada pelas teclas 'o' e 'l', para subir e descer, respectivamente. Teste o programa, brincando com seu colega. Permita que seu programa seja testado com diversos ângulos para o movimento da bola.

A FAZER:
OK - Aumentar a velocidade em 25% a cada 10 batidas na raquete
- Controlar o angulo de batida na raquete: 80º, 40º, 0º, -40º, -80º
- A cada 40 batidas, colocar obstaculos na tela
OK - Limitar a posicao das raquetes as paredes superior e inferior
OK - A raquete deve se movimentar continuamente ao apertar a tecla parar ao soltar
--}
module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

width, height, offset :: Int
width = 600
height = 600
offset = 100

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black

pictureBall :: Picture
pictureBall = color ballColor $ circleSolid ballSize

ballColor = dark red
ballSize = 10

widthF, heightF, offsetF :: Float
widthF = fromIntegral(width) - 300
heightF = fromIntegral(height) - 300
offsetF = 100

-- velocidade inicial da bola
velIni = 50

-- frames por segundo
fps = 24

-- ajuste na posicao das barreiras (obstáculos)
x = -50

-- tamanho dos muros da parte superior e inferior
wallHeigth = 10
wallWidth = widthF - 30

-- tamanho das raquetes
wallHeigthAside = 100
wallWidthAside = 10

-- em quantas batidas mudará a velocidade da bola em 25%
batidasVelocidade = 10

-- percentual de velocidade a ser aumentada
velPerc = 1.25

-- em quantas batidas os obstáculos surgirão na tela
batidasObstaculo = 40

-- tamanho dos obstaculos - altura e largura
obstaculoHeight = 20
obstaculoWidth = 10

data BouncingBall = Ball
  { posX :: Float         -- posição x da bola.
  , posY :: Float         -- posição y da bola.
  , velX :: Float         -- velocidade sobre o eixo x. 
  , velY :: Float         -- velocidade sobre o eixo y. 
  , player1 :: Float      -- posicao do player 1
  , player2 :: Float      -- posicao do player 2
  , velP1 :: Float        -- velocidade do player 1 (direcao)
  , velP2 :: Float        -- velocidade do player 2 (direcao) 
  , contBatObst :: Int    -- contador para exibicao dos obstaculos
  , contBatVel :: Int     -- contador para aumento da velocidade
  , totalObstaculos :: Float -- total de obstaculos exibidos
  } deriving Show


-- Estado inicial
initialState = Ball { posX = -widthF/2 + ballSize
                      , posY = 0
                      , velX = velIni
                      , velY = -velIni
                      , player1 = 0
                      , player2 = 0
                      , velP1 = 0
                      , velP2 = 0
                      , contBatObst = 0
                      , contBatVel = 0
                      , totalObstaculos = 0
                     }

toInt :: Float -> Int
toInt x = round x

main :: IO ()  
main = play window background fps initialState render handleKeys update

-- renderizacao do jogo
render :: BouncingBall -> Picture
render ball = pictures [ picBall, walls, mkPaddle yellow (-widthF/2) $ player1 ball, mkPaddle orange (widthF/2) $ player2 ball, obstaculos $ totalObstaculos ball ]
 where
  picBall = translate (posX ball) (posY ball) pictureBall
  wall offset = translate 0 offset $ color wallColor $ rectangleSolid wallWidth wallHeigth
  wallColor = greyN 1
  walls = pictures [wall (widthF/2), wall (-widthF/2)] 

-- renderizacao dos obstaculos
obstaculos :: Float -> Picture
obstaculos 0 = pictures [ ]
obstaculos n = pictures [ obst (heightF/2 + (n*x)), obstaculos (n-1) ] 

-- auxiliar da renderizado dos obstaculos
obst :: Float -> Picture
obst offset = translate 0 offset $ color blue $ rectangleSolid obstaculoWidth obstaculoHeight

--  uma raquete de uma determinada borda e deslocamento vertical.
mkPaddle :: Color -> Float -> Float -> Picture
mkPaddle col x y = pictures
 [ translate x y $ color col $ rectangleSolid wallWidthAside wallHeigthAside
 , translate x y $ color col $ rectangleSolid wallWidthAside wallHeigthAside
 ] 

-- atualizacao
update :: Float -> BouncingBall -> BouncingBall
update t = moveBall t . wallBounce . checkBatidas . checkObstaculos . obstaculoBounce

-- calcula a quantidade de batidas nas raquetes para exibicao dos obstaculos
checkObstaculos :: BouncingBall -> BouncingBall
checkObstaculos ball = ball { totalObstaculos = t, contBatObst = c }
 where
  c
   | (contBatObst ball >= batidasObstaculo) = 0
   | otherwise = contBatObst ball
  t
   | (totalObstaculos ball < 5) && (contBatObst ball > 0) && (mod (contBatObst ball)  batidasObstaculo == 0) = totalObstaculos ball + 1
   | otherwise = totalObstaculos ball

-- verifica a quantidade de batidas nas raquetes para aumentar a velocidade da bola
checkBatidas :: BouncingBall -> BouncingBall
checkBatidas ball = ball { velY = fst v, velX = snd v, contBatVel = c }
 where
  c
   | (contBatVel ball >= batidasVelocidade) = 0
   | otherwise = contBatVel ball
  v
   | (contBatVel ball > 0) && (mod (contBatVel ball)  batidasVelocidade == 0) = (velY ball * velPerc, velX ball * velPerc)
   | otherwise = (velY ball, velX ball)

moveBall :: Float -> BouncingBall -> BouncingBall
moveBall t ball = ball { posX = posX ball + velX ball * t
                       , posY = posY ball + velY ball * t
                       , player1 = player1 ball + velP1 ball * t
                       , player2 = player2 ball + velP2 ball * t
                       }

wallBounce :: BouncingBall -> BouncingBall
wallBounce ball = ball { velY = vy, velX = raquete , velP1 = v1, velP2 = v2, player1 = p1, player2 = p2, contBatObst = contObs, contBatVel = contVel }
  where
    vy
     | upColision || downColision  = - velY ball
     | otherwise = velY ball
    raquete
     | raqLeft || raqRight = - velX ball
     | otherwise = velX ball
    v1
     | (raqUpColision $ player1 ball) || (raqDownColision $ player1 ball) = 0
     | otherwise = velP1 ball
    v2
     | (raqUpColision $ player2 ball) || (raqDownColision $ player2 ball) = 0
     | otherwise = velP2 ball
    p1
     | (velP1 ball <= 0)  && (raqUpColision $ player1 ball) = player1 ball - 1
     | (velP1 ball <= 0)  && (raqDownColision $ player1 ball) = player1 ball + 1
     | otherwise = player1 ball
    p2
     | (velP2 ball <= 0)  && (raqUpColision $ player2 ball) = player2 ball - 1
     | (velP2 ball <= 0)  && (raqDownColision $ player2 ball) = player2 ball + 1
     | otherwise = player2 ball
    contObs
     | raqLeft || raqRight = contBatObst ball + 1
     | otherwise = contBatObst ball
    contVel
     | raqLeft || raqRight = contBatVel ball + 1
     | otherwise = contBatVel ball

    -- centroL = (posY ball <= (player1 ball/3  + wallHeigthAside)) && (posY ball >= (player1 ball/3  - wallHeigthAside)) && (posX ball + ballSize/2 <= -widthF/2 + wallWidthAside)
    -- centroR = False

    upColision = posY ball + ballSize/2 >= heightF/2 - wallHeigth
    downColision = posY ball - ballSize/2 <= -heightF/2 + wallHeigth

    raqLeft = (posY ball <= (player1 ball  + wallHeigthAside/2)) && (posY ball >= (player1 ball  - wallHeigthAside/2)) && (posX ball + ballSize/2 <= -widthF/2 + wallWidthAside)
    raqRight = (posY ball <= (player2 ball  + wallHeigthAside/2)) && (posY ball >= (player2 ball  - wallHeigthAside/2)) && (posX ball + ballSize/2 >= widthF/2 - wallWidthAside)

obstaculoBounce :: BouncingBall -> BouncingBall
obstaculoBounce ball = ball { velX = vx }
  where
    vx
     | obstColision (totalObstaculos ball) (posY ball) (posX ball) = - velX ball
     | otherwise = velX ball

obstColision :: Float -> Float -> Float -> Bool
obstColision 0 _ _ = False
obstColision n pBallY pBallX = (pBallY <= (heightF/2 + (n*x) + obstaculoHeight/2)) && (pBallY >= (heightF/2 + (n*x) - obstaculoHeight/2)) && ((pBallX <= obstaculoWidth) && (pBallX >= - obstaculoWidth)) || obstColision (n-1) pBallY pBallX

-- && (pBallX + ballSize/2 == 0)
-- (heightF/2 + (n*x))

-- limite superior das raquetes
raqUpColision :: Float -> Bool
raqUpColision pos = pos + wallHeigthAside/2 >= heightF/2

-- limite inferior das raquetes
raqDownColision :: Float -> Bool
raqDownColision pos = pos - wallHeigthAside/2 <= -heightF/2

-- quantidade de passos que movimentam as raquetes
passos :: Float
passos = 80

handleKeys :: Event -> BouncingBall -> BouncingBall
-- reiniciar a posicao da bola
handleKeys (EventKey (Char 'h') _ _ _) ball = ball {  posX = 0, posY = 0, velX = velIni, velY = -velIni, totalObstaculos = 0 }
-- jogador 2 - UP
handleKeys (EventKey (Char 'o') Down _ _) ball = ball { velP2 = velP2 ball + passos}
handleKeys (EventKey (Char 'o') Up _ _) ball = ball { velP2 = 0 }
-- jogador 2 - DOWN
handleKeys (EventKey (Char 'l') Down _ _) ball = ball { velP2 = velP2 ball - passos }
handleKeys (EventKey (Char 'l') Up _ _) ball = ball { velP2 = 0 }
-- jogador 1 - UP
handleKeys (EventKey (Char 'w') Down _ _) ball = ball { velP1 = velP1 ball + passos }
handleKeys (EventKey (Char 'w') Up _ _) ball = ball { velP1 = 0 }
-- jogador 1 - DOWN
handleKeys (EventKey (Char 's') Down _ _) ball = ball { velP1 = velP1 ball - passos }
handleKeys (EventKey (Char 's') Up _ _) ball = ball { velP1 = 0 }
handleKeys _ ball = ball


-----------------------------------------------------------------------------------------------------------------------------------------------------------

-- handleKeys (EventKey (Char 'h') _ _ _) ball = ball {  posX = 0, posY = 0, velX = 150, velY = -150 }
-- -- jogador 2 - UP
-- handleKeys (EventKey (Char 'o') Down _ _) ball = if raqUpColision $ player2 ball then ball { player2 = player2 ball + 0 } else ball { player2 = player2 ball + passos }
-- handleKeys (EventKey (Char 'o') Up _ _) ball = ball { player2 = player2 ball + 0 }
-- -- jogador 2 - DOWN
-- handleKeys (EventKey (Char 'l') Down _ _) ball = if raqDownColision $ player2 ball then ball { player2 = player2 ball + 0 } else ball { player2 = player2 ball - passos }
-- handleKeys (EventKey (Char 'l') Up _ _) ball = ball { player2 = player2 ball + 0 }
-- -- jogador 1 - UP
-- handleKeys (EventKey (Char 'w') Down _ _) ball = if raqUpColision $ player1 ball then ball { player1 = player1 ball + 0 } else ball { player1 = player1 ball + passos }
-- handleKeys (EventKey (Char 'w') Up _ _) ball = ball { player1 = player1 ball + 0 }
-- -- jogador 1 - DOWN
-- handleKeys (EventKey (Char 's') Down _ _) ball = if raqDownColision $ player1 ball then ball { player1 = player1 ball + 0 } else ball { player1 = player1 ball - passos }
-- handleKeys (EventKey (Char 's') Up _ _) ball = ball { player1 = player1 ball + 0 }
-- handleKeys _ ball = ball
-- 
-- -- limite superior das raquetes
-- raqUpColision :: Float -> Bool
-- raqUpColision pos = pos + wallHeigthAside/2 >= heightF/2
-- 
-- -- limite inferior das raquetes
-- raqDownColision :: Float -> Bool
-- raqDownColision pos = pos - wallHeigthAside/2 <= -heightF/2
