import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Simulate
 
-- (FUNCIONANDO) Dados do jogo 
data BRpong = Game { point :: (Float, Float),  
                     vector :: (Float, Float),   
                     playerUSER :: Float,         
                     playerCPU :: Float } deriving Show 
-- (FUNCIONANDO) Dados inciais do jogo 
initialization :: BRpong
initialization = Game { point = (0,0),
                        vector = (300,20),
                        playerUSER = 0,
                        playerCPU = 0 }
-- (FUNCIONANDO) Desenha a bola, as paredes e os jogadores 
draw :: BRpong -> Picture 
draw game = pictures [ ball,
                       wallLong (0) (300), wallLong (0) (-300),
                       player (260) (playerUSER game), player (-260) (playerCPU game),
                       wallSmall (300) (217), wallSmall (-300) (217), wallSmall (300) (-217), wallSmall (-300) (-217)]
                       where
                            -- bola
                            ball = uncurry translate (point game) (color white (circleSolid 10))
                            -- paredes grandes
                            wallLong :: Float -> Float -> Picture
                            wallLong x y = pictures [translate (x) (y) (color red (rectangleSolid 600 14))]
                            -- paredes pequenas
                            wallSmall :: Float -> Float -> Picture
                            wallSmall x y = pictures [translate (x) (y) (color red (rectangleSolid 20 180))]
                            -- jogador
                            player :: Float -> Float -> Picture
                            player x y = pictures [translate (x) (y) (color white $ rectangleSolid 10 70)]
-- (FUNCIONANDO) Move a bola e detecta colisões com as paredes 
moveCollision :: Float -> BRpong -> BRpong
moveCollision seconds game = game { point = (x',y'), 
                                    vector = (vx',vy')}
                                    where
                                         (x,y) = point game
                                         (vx, vy) = vector game
                                         (x',vx') = clip x vx (290) -- 290 corresponde ao limite do campo menos o raio da bola
                                         (y',vy') = clip y vy (290) -- 290 corresponde ao limite do campo menos o raio da bola
                                         clip h vh max | h' > max = (max, -vh)
                                                       | h' < -max = (-max, -vh)
                                                       |otherwise = (h',vh)
                                                       where
                                                            h' = h + seconds*vh
-- (ACHO QUE NÃO) Detecta os jogadores 
playerDetect :: (Float, Float) -> Float -> BRpong -> Bool
playerDetect (x, y) radius game = userPlayer || cpuPlayer
        where
            pCpu = playerCPU game
            pUser = playerUSER game          
            cpuPlayer = ((x - radius < -fromIntegral 260)&&((y < pCpu) && (y > pCpu)))
            userPlayer = ((x - radius > fromIntegral 260)&&((y < pUser) && (y > pUser)))
-- (NÃO FUNCIONA) Detecta colisões com os jogadores 
playerCollision :: BRpong -> BRpong
playerCollision game = game {vector = (vx', vy')}
        where
            radius = 10
            (vx, vy) = vector game
            vx' = if  (playerDetect (point game) radius game)
                then (-vx)
                else (vx)
            vy' = if (playerDetect (point game) radius game)
                then (-vy)
                else (vy)
-- (FUNCIONANDO) Movimenta o jogador do usuario
movePlayer:: Event -> BRpong -> BRpong
movePlayer (EventKey (SpecialKey KeyUp)_ _ _) game = game {playerUSER = y'}
                where 
                    y = playerUSER game
                    y' = if (y < 155)
                        then (y + 15)
                        else (y)
movePlayer (EventKey (SpecialKey KeyDown)_ _ _) game = game {playerUSER = y'}
                where 
                    y = playerUSER game
                    y' = if (y > -155)
                        then (y - 15)
                        else (y)
movePlayer _ game = game
-- (NÃO SEI) Verifica se a bola passou do gol
goal :: BRpong -> Bool
goal game = cpuWin || userWin
    where
        (x,y) = point game
        cpuWin = ((x-10 <= -fromIntegral 300)&&(y > 80)&&(y < -80))    
        userWin = ((x-10 <= fromIntegral 300)&&(y > 80)&&(y < -80))
 -- (NÃO SEI) Informa se o gol foi válido       
referee :: BRpong -> BRpong
referee game = game {vector = (vx',vy')} 
    where
        (vx,vy) = vector game
        vx'= if (goal game)
            then 0
            else vx
        vy'= if (goal game)
            then 0
            else vy
-- (FUNCIONANDO) Atualiza a posição da bola
update ::  Float -> BRpong -> BRpong
update seconds= playerCollision . moveCollision seconds . referee
-- (FUNCIONANDO) Janela
window :: Display 
window = InWindow "BRpong" (800,800) (10,10) 
-- (FUNCIONANDO) Cor de fundo
backgroung :: Color 
backgroung = black
-- (FUNCIONANDO) Taxa de atualização
fps :: Int
fps = 300


main :: IO () 
--main = simulate window backgroung fps initialization draw update
main = play window backgroung fps initialization draw movePlayer update
