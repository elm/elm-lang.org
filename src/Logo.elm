module Logo exposing
  ( Model
  , start
  , view
  , setPattern
  , step
  , perturb
  , isMoving
  , Pattern
  , logo
  , heart
  , bird
  , child
  , house
  , cat
  , camel
  )


import Cycle
import Html
import Svg exposing (Svg, polygon, svg, g)
import Svg.Attributes exposing (fill, height, points, transform, viewBox)



-- MODEL


type alias Model =
  { tb1 : Shape
  , tb2 : Shape
  , tm  : Shape
  , sqr : Shape
  , par : Shape
  , ts1 : Shape
  , ts2 : Shape
  }


type Shape
  = Static Float Float Float
  | Moving Float Float Float Float Float Float Float Float Float


start : Model
start =
  { tb1 = Static 0 -210 0
  , tb2 = Static -210 0 90
  , tm  = Static 207 207 45
  , sqr = Static 150 0 0
  , par = Static -89 239 0
  , ts1 = Static 0 106 180
  , ts2 = Static 256 -150 270
  }



-- VIEW


view : List (Html.Attribute msg) -> Model -> Html.Html msg
view attrs model =
  svg
    (viewBox "-600 -600 1200 1200" :: attrs)
    [ g [ transform "scale(1 -1)"
        ]
        [ viewShape model.tb1 triangleBig
        , viewShape model.tb2 triangleBig
        , viewShape model.tm  triangleMedium
        , viewShape model.sqr square
        , viewShape model.par parallelogram
        , viewShape model.ts1 triangleSmall
        , viewShape model.ts2 triangleSmall
        ]
    ]


viewShape : Shape -> String -> Svg msg
viewShape shape coordinates =
  case shape of
    Static x y a             -> viewShapeHelp x y a coordinates
    Moving _ _ _ x y a _ _ _ -> viewShapeHelp x y a coordinates


viewShapeHelp : Float -> Float -> Float -> String -> Svg msg
viewShapeHelp x y a coordinates =
  polygon
    [ fill "currentColor"
    , points coordinates
    , transform <|
        "translate(" ++ String.fromFloat x ++ " " ++ String.fromFloat y
        ++ ") rotate(" ++ String.fromFloat -a ++ ")"
    ]
    []


triangleBig    = "-280,-90 0,190 280,-90" --    /\    -- 396x396x560
triangleMedium = "-198,-66 0,132 198,-66" --   /  \   -- 280x280x396
triangleSmall  = "-130,-44 0,86  130,-44" --  /____\  -- 184x184260
square         = "-130,0 0,-130 130,0 0,130"          -- 184x184
parallelogram  = "-191,61 69,61 191,-61 -69,-61"



-- SET PATTERN


setPattern : Pattern -> Model -> Model
setPattern target model =
  { tb1 = setShape target.tb1 model.tb1
  , tb2 = setShape target.tb2 model.tb2
  , tm  = setShape target.tm  model.tm
  , sqr = setShape target.sqr model.sqr
  , par = setShape target.par model.par
  , ts1 = setShape target.ts1 model.ts1
  , ts2 = setShape target.ts2 model.ts2
  }


setShape : Target -> Shape -> Shape
setShape {tx,ty,ta} shape =
  case shape of
    Static x y a ->
      Moving 0 0 0 x y a tx ty ta

    Moving vx vy va x y a _ _ _ ->
      Moving vx vy va x y a tx ty ta



-- STEP


step : Float -> Model -> Model
step timeDelta model =
  let
    dt = timeDelta / 1000
  in
  { tb1 = stepShape dt model.tb1
  , tb2 = stepShape dt model.tb2
  , tm  = stepShape dt model.tm
  , sqr = stepShape dt model.sqr
  , par = stepShape dt model.par
  , ts1 = stepShape dt model.ts1
  , ts2 = stepShape dt model.ts2
  }


stepShape : Float -> Shape -> Shape
stepShape dt shape =
  case shape of
    Static _ _ _ ->
      shape

    Moving vx vy va x y a tx ty ta ->
      let
        tt = toNearestAngle a ta
        ax = pstiffness * (tx - x) - pdamping * vx
        ay = pstiffness * (ty - y) - pdamping * vy
        at = astiffness * (tt - a) - adamping * va
        nvx = vx + ax * dt
        nvy = vy + ay * dt
        nva = va + at * dt
        nx = x + nvx * dt
        ny = y + nvy * dt
        na = a + nva * dt
        dx = abs (tx - nx)
        dy = abs (ty - ny)
        da = abs (tt - na)
      in
      if dx < 1 && dy < 1 && da < 1 && abs nvx < 0.6 && abs nvy < 0.6 && abs nva < 0.6 then
        Static tx ty (normalize ta)
      else
        Moving nvx nvy nva nx ny na tx ty ta


toNearestAngle : Float -> Float -> Float
toNearestAngle current target =
  let
    distance = target - current
  in
  if distance < -180 then
    target + 360
  else if distance > 180 then
    target - 360
  else
    target


normalize : Float -> Float
normalize angle =
  if angle < 0 then
    angle + 360
  else if 360 < angle then
    angle - 360
  else
    angle


pstiffness = 220
pdamping = 10

astiffness = 200
adamping = 10



-- PERTURB


-- TODO figure out how to get angle to center of mass involved
--
perturb : Float -> Float -> Float -> Float -> Float -> Model -> Model
perturb timeDelta x y dx dy model =
  if timeDelta > 40 then
    model
  else
    let
      dt = max 1 timeDelta / 40000
    in
    { tb1 = perturbShape dt x y dx dy model.tb1
    , tb2 = perturbShape dt x y dx dy model.tb2
    , tm  = perturbShape dt x y dx dy model.tm
    , sqr = perturbShape dt x y dx dy model.sqr
    , par = perturbShape dt x y dx dy model.par
    , ts1 = perturbShape dt x y dx dy model.ts1
    , ts2 = perturbShape dt x y dx dy model.ts2
    }


perturbShape : Float -> Float -> Float -> Float -> Float -> Shape -> Shape
perturbShape dt mx my dx dy shape =
  case shape of
    Static x y a ->
      let
        rx = mx * 10 - x / 120
        ry = my * 10 - y / 120
        r2 = max 0.5 (rx * rx + ry * ry)
        vx = dx / (dt * r2)
        vy = dy / (dt * r2)
      in
      Moving vx vy 0 x y a x y a

    Moving vx vy va x y a tx ty ta ->
      let
        rx = mx * 10 - x / 120
        ry = my * 10 - y / 120
        r2 = max 0.5 (rx * rx + ry * ry)
        nvx = vx + dx / (dt * r2)
        nvy = vy + dy / (dt * r2)
      in
      Moving nvx nvy va x y a tx ty ta



-- IS MOVING?


isMoving : Model -> Bool
isMoving model =
  isMovingShape model.tb1
  || isMovingShape model.tb2
  || isMovingShape model.tm
  || isMovingShape model.sqr
  || isMovingShape model.par
  || isMovingShape model.ts1
  || isMovingShape model.ts2


isMovingShape : Shape -> Bool
isMovingShape shape =
  case shape of
    Static _ _ _ -> False
    Moving _ _ _ _ _ _ _ _ _ -> True



-- PATTERNS


type alias Pattern =
  { tb1 : Target
  , tb2 : Target
  , tm  : Target
  , sqr : Target
  , par : Target
  , ts1 : Target
  , ts2 : Target
  }


type alias Target =
  { tx : Float
  , ty : Float
  , ta : Float
  }


logo : Pattern
logo =
  { tb1 = Target 0 -210 0
  , tb2 = Target -210 0 90
  , tm  = Target 207 207 45
  , sqr = Target 150 0 0
  , par = Target -89 239 0
  , ts1 = Target 0 106 180
  , ts2 = Target 256 -150 270
  }


heart : Pattern
heart =
  { tb1 = Target -160 120 0
  , tb2 = Target 150 -90 180
  , tm  = Target -270 -93 45
  , sqr = Target 0 -300 0
  , par = Target 231 91 0
  , ts1 = Target 150 224 0
  , ts2 = Target -106 -150 90
  }


bird : Pattern
bird =
  { tb1 = Target -296 166 45
  , tb2 = Target 0 40 225
  , tm  = Target 200 136 270
  , sqr = Target -42 -212 45
  , par = Target -138 -424 135
  , ts1 = Target 139 -181 315
  , ts2 = Target 352 214 225
  }


child : Pattern
child =
  { tb1 = Target -88 -46 135
  , tb2 = Target 208 86 -45
  , tm  = Target 120 -300 0
  , sqr = Target 104 352 36
  , par = Target -140 -300 315
  , ts1 = Target -404 -380 315
  , ts2 = Target 328 -434 180
  }


house : Pattern
house =
  { tb1 = Target 0 -250 0
  , tb2 = Target 96 54 0
  , tm  = Target -218 -152 315
  , sqr = Target -106 266 45
  , par = Target -212 56 315
  , ts1 = Target 162 -104 180
  , ts2 = Target 264 -206 270
  }


cat : Pattern
cat =
  { tb1 = Target -40 -120 90
  , tb2 = Target 20 -420 135
  , tm  = Target -226 -38 270
  , sqr = Target -220 276 0
  , par = Target 350 -462 315
  , ts1 = Target -320 428 90
  , ts2 = Target -120 428 270
  }


camel : Pattern
camel =
  { tb1 = Target -250 -256 315
  , tb2 = Target 100 -260 270
  , tm  = Target -190 -30 0
  , sqr = Target 40 40 0
  , par = Target 278 40 90
  , ts1 = Target 262 276 90
  , ts2 = Target 366 380 180
  }
