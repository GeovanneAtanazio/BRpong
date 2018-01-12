import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Simulate

-- Estrutura de dados do estado do jogo 
data BRpong = Game { 
                     point :: (Float, Float),
                     vector :: (Float, Float),
                     playerUSER :: (Float,Float),
                     playerCPU :: (Float,Float) 
                     } | GameOver String deriving Show

-- Taxa de atualização
fps :: Int
fps = 60
-- Raio da circunferencia da bola
radius :: Float
radius = 15
-- Limite do campo menos o raio 
limit :: Float
limit = 300 - radius 
-- Cor de fundo
backgroung :: Color 
backgroung = black
-- Janela
window :: Display 
window = InWindow "BRpong" (600,600) (10,10) 

-- Estado para iniciar o Jogo
initialization :: BRpong
initialization = Game { point = (0,0),
                        vector = (300,30),
                        playerUSER = (0,0),
                        playerCPU = (0,1) }

-- Desenha a bola, as paredes e os jogadores 
draw :: BRpong -> Picture
draw (GameOver s) = scale 0.3 0.3 . translate (-300) 0
     . color red . text $ s
draw game = pictures [ ball,
                       wallLong (0) (300), wallLong (0) (-300),
                       player (280) (playerUSER game), player (-280) (playerCPU game),
                       wallSmall (300) (195), wallSmall (-300) (195), wallSmall (300) (-195), wallSmall (-300) (-195)]
                       where
                            -- bola
                            ball = uncurry translate (point game) (color white (circleSolid (radius)))
                            -- paredes grandes
                            wallLong :: Float -> Float -> Picture
                            wallLong x y = pictures [translate (x) (y) (color red (rectangleSolid 600 20))]
                            -- paredes pequenas
                            wallSmall :: Float -> Float -> Picture
                            wallSmall x y = pictures [translate (x) (y) (color red (rectangleSolid 20 210))]
                            -- jogador
                            player :: Float -> (Float,Float) -> Picture
                            player x (y,_) = pictures [translate (x) (y) (color white $ rectangleSolid 10 70)]

-- Move a bola e o PlayerCPu detectando as colisões com as paredes 
moveCollision :: Float -> BRpong -> BRpong
moveCollision _ (GameOver s) = GameOver s
moveCollision seconds game = game { point = (x',y'), 
                                    vector = (vx',vy'),
                                    playerCPU = (yC+d,d) -- movimentar o playerCPU
                                  }
                                    where
                                         (yC,d1) = playerCPU game
                                         (x,y) = point game
                                         (vx, vy) = vector game
                                         -- d é a velocidade do playerCPU
                                         d = if (yC+45>155) then -1
                                            else if (yC-45< -155) then 1
                                            else d1
                                         (x',vx') = clip x vx (limit) 
                                         (y',vy') = clip y vy (limit) 
                                         clip h vh max | h' > max = (max, -vh)
                                                       | h' < -max = (-max, -vh)
                                                       |otherwise = (h',vh)
                                                       where
                                                            h' = h + seconds*vh

-- Detecta a colisão com os jogadores e retorna um valor booleano
playerDetect :: (Float, Float) -> BRpong -> Bool
playerDetect (x, y) game = userPlayer || cpuPlayer
        where
            (pCpu,_) = playerCPU game
            (pUser,_) = playerUSER game          
            cpuPlayer = ((x - radius < -280)&&((y-radius < pCpu+45) && (y+radius > pCpu-45)))
            userPlayer = ((x + radius > 280)&&((y-radius < pUser+45) && (y+radius > pUser-45)))
-- Muda a direção do vetor caso detecte colisões com os jogadores 
playerCollision :: BRpong -> BRpong
playerCollision (GameOver s) = GameOver s
playerCollision game = game {vector = (vx', vy)}
        where
            (vx, vy) = vector game
            vx' = if  (playerDetect (point game) game)
                then (-vx)
                else (vx)

--Movimenta o jogador do usuario
movePlayer:: Event -> BRpong -> BRpong
movePlayer (EventKey (SpecialKey KeyUp)_ _ _) game = game {playerUSER = y'}
                where 
                    y = playerUSER game
                    y' = if (y < 245)
                        then (y + 15)
                        else (y)
movePlayer (EventKey (SpecialKey KeyDown)_ _ _) game = game {playerUSER = y'}
                where 
                    y = playerUSER game
                    y' = if (y > -245)
                        then (y - 15)
                        else (y)
movePlayer _ game = game

-- (Verifica se a bola passou do gol
goal :: BRpong -> BRpong
goal (GameOver s) = GameOver s
goal game = q
    where
        (x,y) = point game
        userWin = ((x-radius <= -300)&&(y+radius < 90)&&(y-radius > -90))   
        cpuWin = ((x+radius >= 300)&&(y+radius < 90)&&(y-radius > -90))
        q = if cpuWin then (GameOver "Voce perdeu!")
            else if userWin then (GameOver "Voce ganhou!")
            else game

-- Atualiza a posição da bola
update ::  Float -> BRpong -> BRpong
update seconds = playerCollision . moveCollision seconds . goal

-- Metodo main executável
main :: IO () 
main = play window backgroung fps initialization draw movePlayer update