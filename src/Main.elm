module ChargeShooting where

import Text (..)
import Mouse
import Signal (..)
import Graphics.Element (Element, layers, container, middle)
import Graphics.Collage (..)
import Color (..)
import Time (Time, fps, every, millisecond)
import List
import List ((::))
import Random

-- Input
type alias Position = (Int, Int)
type alias Input = {pos:Position, isDown:Bool, time:Time}

input : Signal Input
input = sampleOn (fps 30) (Input <~ Mouse.position ~ Mouse.isDown ~ every millisecond)

-- Model
width = 320
height = 480
initialLimitTime = 60
type alias Object a = { a | x:Float, y:Float, vx:Float, vy:Float, size:Float }
type alias Player = Object { isLive:Bool }
type alias Bullet = Object { level:Int, color:Color }
type alias Enemy = Object { hp:Int }
type alias Effect = Object { color:Color }
type alias Game =
  { player:Player
  , bullets:List Bullet
  , enemies:List Enemy
  , effects:List Effect
  , frame:Int
  , score:Int
  , pastTime:Int
  , isGameOver:Bool
  , isBomb:Bool
  , bgm:String
  , limitTime:Int
  }

initialPlayer =
  { x = width / 2
  , y = height - 20
  , vx = 0
  , vy = 0
  , size = 5
  , isLive=True }

initialGame =
  { player=initialPlayer
  , bullets = []
  , enemies=[]
  , effects=[]
  , frame=0
  , score=0
  , pastTime=0
  , isGameOver=False
  , isBomb=False
  , bgm="BGM"
  , limitTime=initialLimitTime
  }

--Update
step : Input -> Game -> Game
step i g =
  if g.isGameOver
    then if i.isDown then initialGame else g
    else stepPlayGame i g

stepPlayGame : Input -> Game -> Game
stepPlayGame i = update i << generateObject i << moveObject i << collisionObject i << isGameOver

isGameOver : Game -> Game
isGameOver ({player, pastTime, limitTime} as g)=
  { g | isGameOver <- (not player.isLive) || limitTime <= pastTime }

update : Input -> Game -> Game
update i ({player, bullets, enemies, effects} as g) =
  let
    newPlayer = chargePlayer i player
    newBullets = List.filter (\b -> b.level > 0) bullets
    newEnemies = List.filter (\e -> e.hp > 0) enemies
    newEffects =
      List.map (\e -> { e | size <- e.size - 1 })
        (List.filter (\e -> e.size > 0) effects)
    newScore = g.score + ((List.length enemies) - (List.length newEnemies))
  in
    { g |
      player <- newPlayer
    , bullets <- newBullets
    , enemies <- newEnemies
    , effects <- newEffects
    , frame <- g.frame + 1
    , score <- newScore
    , pastTime <- g.pastTime + (if g.frame % 30 == 0 then 1 else 0)
    , isBomb <- 0 < ((List.length enemies) - (List.length newEnemies))
    , limitTime <- initialLimitTime + (g.score // 5)
    }

chargePlayer i p =
  { p | size <- p.size + 1}

-- collision
collisionObject : Input -> Game -> Game
collisionObject i ({player, bullets, enemies, effects} as g) =
  let
    isHit o o' = (o.x - o'.x)^2 + (o.y - o'.y)^2 < (o.size + o'.size)^2
    isPlayerHitted = List.any (isHit player) enemies
    p = { player | isLive <- (player.isLive && (not isPlayerHitted)) }
    (newBullets, newEffects) =
      List.foldr (\b (bs, es) ->
        if List.any (isHit b) enemies
          then ({ b | level <- b.level - 1} :: bs,  (generateEffect b.x b.y b.color (b.level * 10)) :: es)
          else (b::bs, es)
        ) ([], effects) bullets
    newEnemies =
      List.map (\e -> if List.any (isHit e) bullets then { e | hp <- e.hp - 1} else e) enemies
  in
    { g | player <- p, bullets <- newBullets, enemies <- newEnemies, effects <- newEffects }

---Generate
generateEffect : Float -> Float -> Color -> Int -> Effect
generateEffect x y c size =
  { x=x, y=y, vx=0, vy=0, size=toFloat size, color=c }

generateObject : Input -> Game -> Game
generateObject i ({player, bullets, enemies, frame} as g) =
  let
    (newPlayer, newBullets) = generateBullet i (player, bullets)
    newEnemies = if frame % 10 == 0 then List.append (generateEnemy ((frame // 1000)+1) i) enemies else enemies
  in
    { g | player <- newPlayer, bullets <- newBullets, enemies <- newEnemies }

generateEnemy : Int -> Input -> List Enemy
generateEnemy n {time} =
  let
    seed = Random.initialSeed (round time)
    getX _ (_, s) = Random.generate (Random.float 0 width) s
    xs = List.map fst (List.scanl getX (width/2, seed) (List.repeat n 0))
  in
    List.map (\x -> { x= x, y=0, vx=0, vy=3, size=10, hp=2 }) (List.tail xs)

generateBullet : Input -> (Player, List Bullet) -> (Player, List Bullet)
generateBullet {isDown, time} (p, bs) =
  if isDown then (p, bs)
  else
    if | p.size > 25 -> ({p | size <- p.size - 5}, (makeBullet time p.x p.y 3) :: bs)
       | p.size > 20 -> ({p | size <- p.size - 15}, (makeBullet time p.x p.y 1) :: bs)
       | otherwise -> (p, bs)

makeBullet : Float -> Float -> Float -> Int -> Bullet
makeBullet time x y l =
  let
    f l h = (fst (Random.generate (Random.float l h) (Random.initialSeed (round time))))
    vx = if l==1 then 0 else f -5 5
    c = if l==1 then black else hsla (degrees (f 0 360)) 0.9 0.6 0.7
  in
    { x=x, y=y, level=l, vx=vx, vy=-5, size=(l * 3), color=c }

---Move
moveObject :Input -> Game -> Game
moveObject i ({player, bullets, enemies} as g) =
  { g | player <- movePlayer i player
  , bullets <- moveBullets bullets
  , enemies <- moveEnemies enemies
  }

movePlayer : Input -> Player -> Player
movePlayer i p =
  let
    trunc limit x =
      if | x < 0 -> 0
         | limit < x -> limit
         | otherwise -> x
    nx = trunc width <| toFloat <| fst <| i.pos
    ny = trunc height <| toFloat <| snd <| i.pos
  in
    { p | x <- nx  , y <- ny }

moveEnemies : List Enemy -> List Enemy
moveEnemies es =
  let
    move e = { e | x <- e.x + e.vx, vx <- (cos <| (pi * (e.y * 2 /height))), y <- e.y + e.vy }
    inArea e = {--0 <= e.x && e.x <= width &&--} 0 <= e.y && e.y <= height
  in
    List.filter inArea (List.map move es)

moveBullets : List Bullet -> List Bullet
moveBullets bs =
  let
    moveBullet b =
      let
        isReflect limit x vx = x + vx < 0 || limit < x + vx
        nextVY =
          if isReflect height b.y b.vy
            then -b.vy
            else b.vy
        nextVX =
          if isReflect width b.x b.vx
            then -b.vx
            else b.vx
        nextLevel = if (isReflect height b.y b.vy) || (isReflect width b.x b.vx) then b.level - 1 else b.level
      in
        {b | x <- b.x + nextVX, vx <- nextVX, y <- b.y + nextVY, vy <- nextVY, level <- nextLevel}
  in
    (List.map moveBullet bs)

--View
display : Game -> Element
display ({player, bullets, enemies, effects} as g) =
  if g.isGameOver
  then container width height middle <| centered <| fromString <| "GameOver\nyour score is " ++ toString g.score
  else
    layers
    [ collage width height
        [ effectsForm effects
        , playerForm player
        , bulletsForm bullets
        , enemiesForm enemies
        ]
    , plainText ("score:" ++ toString g.score)
    , plainText ("\ntime:" ++ (toString g.pastTime) ++ "/" ++ (toString g.limitTime))
    ]

effectsForm : List Effect -> Form
effectsForm es =
  let
    toForm {x, y, size, color} = ngon 6 size |> (outlined <| solid color) |> move (x - (width/2), (height/2) - y)
  in
    group (List.map toForm es)

playerForm : Player -> Form
playerForm player =
  circle player.size |> filled black |> move (player.x - (width/2), (height/2) - player.y)

bulletsForm : List Bullet -> Form
bulletsForm bs =
  let
    toForm {x, y, level, color} = ngon 6 (toFloat (level * 3)) |> filled color |> move (x - (width/2), (height/2) - y)
  in
    group (List.map toForm bs)

enemiesForm : List Enemy -> Form
enemiesForm es =
  let
    toForm {x, y, size} = square size |> filled blue |> move (x - (width/2), (height/2) - y)
  in
    group (List.map toForm es)

game = foldp step initialGame input

main = map display game

port jsPlayMusic : Signal String
port jsPlayMusic = .bgm <~ game
port jsPlayBombSound : Signal Bool
port jsPlayBombSound = .isBomb <~ game
