module ChargeShooting where

import Text (..)
import Mouse
import Signal (..)
import Graphics.Element (Element, layers, container, middle, image, opacity)
import Graphics.Collage (..)
import Color (..)
import Time
import Time (Time, every, millisecond)
import List
import List ((::))
import Random
import Keyboard
import Char
import Debug

-- Global Setting
fps = 30
width = 320
height = 480
gaugeMax = 500

-- Input
type alias Position = (Int, Int)
type alias Input = {pos:Position, isDown:Bool, reset:Bool, time:Time}

input : Signal Input
input = sampleOn (Time.fps fps) <|
  Input <~ Mouse.position ~ Mouse.isDown ~ Keyboard.isDown (Char.toCode 'r') ~ every millisecond

-- Model
type alias Object a = { a | x:Float, y:Float, vx:Float, vy:Float, size:Float }
type alias Player = Object { isLive:Bool }
type alias Bullet = Object { level:Int, color:Color }
type alias Enemy = Object { hp:Int }
type alias Effect = Object { color:Color }
type alias Coin = Object {}
type alias Game =
  { player:Player
  , bullets:List Bullet
  , enemies:List Enemy
  , effects:List Effect
  , coins:List Coin
  , frame:Int
  , score:Int
  , isGameOver:Bool
  , isBomb:Bool
  , bgm:String
  , isGet:Bool
  , comboGauge:Int
  }

initialPlayer =
  { x=width / 2
  , y=height - 20
  , vx=0
  , vy=0
  , size=2
  , isLive=True }

initialGame =
  { player=initialPlayer
  , bullets=[]
  , enemies=[]
  , effects=[]
  , coins=[]
  , frame=0
  , score=0
  , isGameOver=False
  , isBomb=False
  , bgm="BGM"
  , isGet=False
  , comboGauge=0
  }

--Update
step : Input -> Game -> Game
step i g =
  if g.isGameOver
    then if i.reset then initialGame else soundReset g
    else stepPlayGame i g

soundReset : Game -> Game
soundReset g = { g | isBomb <- False, isGet <- False }

stepPlayGame : Input -> Game -> Game
stepPlayGame i = update i << generateObject i << moveObjects i << collisionObject i << isGameOver

isGameOver : Game -> Game
isGameOver ({player} as g)=
  { g | isGameOver <- (not player.isLive) }

update : Input -> Game -> Game
update i ({player, bullets, enemies, coins, effects, comboGauge} as g) =
  let
    newPlayer = chargePlayer i player
    newBullets = List.filter (\b -> b.level > 0) bullets
    (newEnemies, generatedCoins) =
      let
        f e (es, cs) =
          if e.hp > 0
            then (e::es, cs)
            else (es, generateCoin e.x e.y :: cs)
      in
        List.foldl f ([], coins) enemies
    newCoins =
      let
        ratio = 60 - (player.size - 60)
        canDraw = player.size > 40
        vx' c =
          if canDraw
            then (player.x - c.x)/ratio
            else 0
        vy' c =
          if canDraw
            then (player.y - c.y)/ratio
            else 0
        f c = { c | vx <- vx' c, vy <- vy' c }
      in
        List.map f generatedCoins
    newEffects =
      List.map (\e -> { e | size <- e.size - 1 })
        (List.filter (\e -> e.size > 0) effects)
    killedEnemyNum = (List.length enemies) - (List.length newEnemies)
    newComboGauge =
      let
        t = comboGauge - 1
      in
        if t < 0 then 0 else t
  in
    { g |
      player <- newPlayer
    , bullets <- newBullets
    , enemies <- newEnemies
    , effects <- newEffects
    , coins <- newCoins
    , frame <- g.frame + 1
    , isBomb <- 0 < killedEnemyNum
    , comboGauge <- newComboGauge
    }

chargePlayer i p =
  { p | size <- p.size + 1}

-- collision
collisionObject : Input -> Game -> Game
collisionObject i ({player, bullets, enemies, effects, coins, score, comboGauge} as g) =
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
    newCoins =
      List.filter (not << isHit player) coins
    gotCoins = List.length coins - List.length newCoins
    newScore = score + (gotCoins * (calcMgn comboGauge))
    newComboGauge =
      let
        t = comboGauge + (gotCoins * 10)
      in
        if gaugeMax < t then gaugeMax else t
  in
    { g | player <- p, bullets <- newBullets, enemies <- newEnemies, effects <- newEffects
    , coins <- newCoins, score <- newScore, isGet <- newScore > score, comboGauge <- newComboGauge }

calcMgn : Int -> Int
calcMgn g =
  if 100 <= g then 10 else (g // 10) + 1

---Generate
generateEffect : Float -> Float -> Color -> Int -> Effect
generateEffect x y c size =
  { x=x, y=y, vx=0, vy=0, size=toFloat size, color=c }

generateObject : Input -> Game -> Game
generateObject i ({player, bullets, enemies, frame} as g) =
  let
    (newPlayer, newBullets) = generateBullet i (player, bullets)
    newEnemyNum = frame // 1000 + 1
    newEnemies =
      if frame % (fps // 3) == 0
        then List.append (generateEnemy newEnemyNum i) enemies
        else enemies
  in
    { g | player <- newPlayer, bullets <- newBullets, enemies <- newEnemies }

genRandomFloat : Float -> Float -> Random.Seed -> (Float, Random.Seed)
genRandomFloat low high s =
  Random.generate (Random.float low high) s

generateEnemy : Int -> Input -> List Enemy
generateEnemy n {time} =
  let
    seed = Random.initialSeed (round time)
    getX _ (_, s) = genRandomFloat 0 width s
    xs = List.tail <| List.map fst <| List.scanl getX (0, seed) (List.repeat n 0)
  in
    List.map (\x -> { x=x, y=0, vx=0, vy=3, size=5, hp=2 }) xs

generateCoin : Float -> Float -> Coin
generateCoin x y = { x=x, y=y, vx=0, vy=0, size=5 }

generateBullet : Input -> (Player, List Bullet) -> (Player, List Bullet)
generateBullet {isDown, time} (p, bs) =
  if isDown then (p, bs)
  else
    if | p.size > 22 -> ({p | size <- p.size - 5}, (makeBullet time p.x p.y 3) :: bs)
       | p.size > 17 -> ({p | size <- p.size - 15}, (makeBullet time p.x p.y 1) :: bs)
       | otherwise -> (p, bs)

makeBullet : Float -> Float -> Float -> Int -> Bullet
makeBullet time x y l =
  let
    gen = Random.generate (Random.float 0 1)
    (a, s) = gen (Random.initialSeed (round time))
    (b, _) = gen s
    g = sqrt (-2 * (logBase e a)) * cos (2 * pi * b)
    f l h = fst <| genRandomFloat l h (Random.initialSeed (round time))
    vx = if l==1 then 0 else g * 3
    c = if l==1 then black else hsla (degrees (f 0 360)) 0.9 0.6 0.7
  in
    { x=x, y=y, level=l, vx=vx, vy=-5, size=(l * 3), color=c }

---Move
moveObjects : Input -> Game -> Game
moveObjects i ({player, bullets, enemies, coins} as g) =
  { g | player <- movePlayer i player
  , bullets <- moveBullets bullets
  , enemies <- moveEnemies enemies
  , coins <- List.map moveObject coins
  }

moveObject : Object a -> Object a
moveObject o =
  { o | x <- o.x + o.vx, y <- o.y + o.vy }

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

inRange : number -> number -> Bool
inRange limit x = 0 <= x && x <= limit

moveEnemies : List Enemy -> List Enemy
moveEnemies es =
  let
    updateVY e = { e | vx <- (cos <| (pi * (e.y * 2 /height))) }
    move = updateVY << moveObject
  in
    List.filter (\e -> inRange height e.y) (List.map move es)

moveBullets : List Bullet -> List Bullet
moveBullets bs =
  let
    moveBullet b =
      let
        isReflect limit x vx = x + vx < 0 || limit < x + vx
        isReflectY = isReflect height b.y b.vy
        isReflectX = isReflect width b.x b.vx
        nextVY = if isReflectY then -b.vy else b.vy
        nextVX = if isReflectX then -b.vx else b.vx
        nextLevel = b.level - (if isReflectY || isReflectX then 1 else 0)
        b' = {b | vx <- nextVX, vy <- nextVY, level <- nextLevel}
      in
        moveObject b'
  in
    (List.map moveBullet bs)

--View
display : Game -> Element
display ({player, bullets, enemies, effects, coins, comboGauge} as g) =
  if g.isGameOver
  then gameOverScene g
  else
    layers
    [ background comboGauge
    , collage width height
        [ effectsForm effects
        , coinsForm coins
        , enemiesForm enemies
        , playerForm player
        , bulletsForm bullets
        , gaugeForm comboGauge
        ]
    , plainText ("score:" ++ toString g.score)
    ]

gameOverScene : Game -> Element
gameOverScene g =
  layers
    [ background g.comboGauge
    , container width height middle <| centered <| fromString <|
        "GameOver\nyour score is " ++ toString g.score ++ "\n\n\"r\" : restart"
    ]

background : Int -> Element
background gauge =
  let
    g = toFloat gauge
    range = gaugeMax - 100
    s1 = range / 3
    s2 = range / 3
    s3 = range / 4
    f l h g = if | g < 100 + l -> 0
                 | 100 + h < g -> 1
                 | otherwise -> (g - 100) / h
    a1 = f 0 s1 g
    a2 = f s1 (s1 + s2) g
    a3 = f (s1 + s2) (s1 + s2 + s3) g
  in
    layers
    [ image width height "../img/base.png"
    , image width height "../img/b1.png" |> opacity (Debug.watch "a1" a1)
    , image width height "../img/b2.png" |> opacity (Debug.watch "a2" a2)
    , image width height "../img/b3.png" |> opacity (Debug.watch "a3" a3)
    ]

moveForm : Float -> Float -> Form -> Form
moveForm x y f = move (x - (width/2), (height/2) - y) f

effectsForm : List Effect -> Form
effectsForm es =
  let
    toForm {x, y, size, color} = ngon 6 size |> outlined (solid color) |> moveForm x y
  in
    group (List.map toForm es)

playerForm : Player -> Form
playerForm player =
  circle (player.size + 3) |> filled black |> moveForm player.x player.y

bulletsForm : List Bullet -> Form
bulletsForm bs =
  let
    toForm {x, y, level, color} = ngon 6 (toFloat (level * 3)) |> filled color |> moveForm x y
  in
    group (List.map toForm bs)

enemiesForm : List Enemy -> Form
enemiesForm es =
  let
    toForm {x, y, size} = square (size * 2) |> filled blue |> moveForm x y
  in
    group (List.map toForm es)

coinsForm : List Coin -> Form
coinsForm cs =
  let
    toForm {x, y, size} = circle size |> filled (rgb 255 210 90) |> moveForm x y
  in
    group (List.map toForm cs)

gaugeForm : Int -> Form
gaugeForm gauge =
  let
    width = if gauge > 100 then 200 else toFloat gauge * 2
    -- 得点倍率
    mgn = calcMgn gauge
  in
    group [ ("x " ++ toString mgn) |> plainText |> toForm |> moveForm 100 10
          , rect width 12 |> filled yellow |> moveForm (120 + width/2) 10
          ]

game : Signal Game
game = foldp step initialGame input

main : Signal Element
main = map display game

port jsPlayMusic : Signal String
port jsPlayMusic = .bgm <~ game
port jsPlayBombSound : Signal Bool
port jsPlayBombSound = .isBomb <~ game
port jsPlayGetSound : Signal Bool
port jsPlayGetSound = .isGet <~ game
