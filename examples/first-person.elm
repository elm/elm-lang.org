-- Walk around in 3D space using the keyboard.
--
-- Dependencies:
--   elm install elm-explorations/linear-algebra
--   elm install elm-explorations/webgl
--
-- Try adding the ability to crouch or to land on top of the crate!
--


import Browser
import Browser.Dom as Dom
import Browser.Events as E
import Html exposing (Html, p, text, div)
import Html.Attributes exposing (width, height, style)
import Json.Decode as D
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Task
import WebGL
import WebGL.Texture as Texture



-- MAIN


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = \msg model -> (update msg model, Cmd.none)
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { keys : Keys
  , width : Float
  , height : Float
  , person : Person
  , texture : Maybe Texture.Texture
  }


type alias Keys =
  { up : Bool
  , left : Bool
  , down : Bool
  , right : Bool
  , space : Bool
  }


type alias Person =
  { position : Vec3
  , velocity : Vec3
  }


init : () -> ( Model, Cmd Msg )
init _ =
  ( { keys = noKeys
    , width = 400
    , height = 400
    , person = Person (vec3 0 eyeLevel -10) (vec3 0 0 0)
    , texture = Nothing
    }
  , Cmd.batch
      [ Task.attempt GotTexture (Texture.load "/assets/wood-crate.jpg")
      , Task.perform (\{viewport} -> Resized viewport.width viewport.height) Dom.getViewport
      ]
  )


eyeLevel : Float
eyeLevel =
  2


noKeys : Keys
noKeys =
  Keys False False False False False



-- UPDATE


type Msg
  = GotTexture (Result Texture.Error Texture.Texture)
  | KeyChanged Bool String
  | TimeDelta Float
  | Resized Float Float
  | VisibilityChanged E.Visibility


update : Msg -> Model -> Model
update msg model =
  case msg of
    GotTexture result ->
      { model | texture = Result.toMaybe result }

    KeyChanged isDown key ->
      { model | keys = updateKeys isDown key model.keys }

    TimeDelta dt ->
      { model | person = updatePerson dt model.keys model.person }

    Resized width height ->
      { model
          | width = width
          , height = height
      }

    VisibilityChanged _ ->
      { model | keys = noKeys }


updateKeys : Bool -> String -> Keys -> Keys
updateKeys isDown key keys =
  case key of
    " "          -> { keys | space = isDown }
    "ArrowUp"    -> { keys | up    = isDown }
    "ArrowLeft"  -> { keys | left  = isDown }
    "ArrowDown"  -> { keys | down  = isDown }
    "ArrowRight" -> { keys | right = isDown }
    _            -> keys


updatePerson : Float -> Keys -> Person -> Person
updatePerson dt keys person =
  let
    velocity = stepVelocity dt keys person
    position = Vec3.add person.position (Vec3.scale (dt / 500) velocity)
  in
  if Vec3.getY position < eyeLevel then
    { position = Vec3.setY eyeLevel position
    , velocity = Vec3.setY 0 velocity
    }
  else
    { position = position
    , velocity = velocity
    }


stepVelocity : Float -> Keys -> Person -> Vec3
stepVelocity dt { left, right, up, down, space } person =
  if Vec3.getY person.position > eyeLevel then
    Vec3.setY (Vec3.getY person.velocity - dt / 250) person.velocity
  else
    let
      toV positive negative =
        (if positive then 1 else 0) - (if negative then 1 else 0)
    in
    vec3 (toV left right) (if space then 2 else 0) (toV up down)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ E.onResize (\w h -> Resized (toFloat w) (toFloat h))
    , E.onKeyUp (D.map (KeyChanged False) (D.field "key" D.string))
    , E.onKeyDown (D.map (KeyChanged True) (D.field "key" D.string))
    , E.onAnimationFrameDelta TimeDelta
    , E.onVisibilityChange VisibilityChanged
    ]



-- VIEW


view : Model -> Html Msg
view model =
  let
    entities =
      case model.texture of
        Nothing ->
          []

        Just texture ->
          [ viewCrate model.width model.height model.person texture ]
  in
  div
    [ style "position" "absolute"
    , style "left" "0"
    , style "top" "0"
    , style "width" (String.fromFloat model.width ++ "px")
    , style "height" (String.fromFloat model.height ++ "px")
    ]
    [ WebGL.toHtmlWith [ WebGL.depth 1 ]
        [ style "display" "block"
        , width (round model.width)
        , height (round model.height)
        ]
        entities
    , keyboardInstructions model.keys
    ]


viewCrate : Float -> Float -> Person -> Texture.Texture -> WebGL.Entity
viewCrate width height person texture =
  let
    perspective =
      Mat4.mul
        (Mat4.makePerspective 45 (width / height) 0.01 100)
        (Mat4.makeLookAt person.position (Vec3.add person.position Vec3.k) Vec3.j)
  in
  WebGL.entity vertexShader fragmentShader crate
    { texture = texture
    , perspective = perspective
    }


keyboardInstructions : Keys -> Html msg
keyboardInstructions keys =
  div
    [ style "position" "absolute"
    , style "font-family" "monospace"
    , style "color" "white"
    , style "text-align" "center"
    , style "left" "20px"
    , style "right" "20px"
    , style "top" "20px"
    ]
    [ p [] [ text "Walk around with a first person perspective." ]
    , p [] [ text "Arrows keys to move, space bar to jump." ]
    ]



-- MESH


type alias Vertex =
  { position : Vec3
  , coord : Vec2
  }


crate : WebGL.Mesh Vertex
crate =
  WebGL.triangles <| List.concatMap rotatedSquare <|
    [ (0, 0)
    , (90, 0)
    , (180, 0)
    , (270, 0)
    , (0, 90)
    , (0, -90)
    ]


rotatedSquare : (Float, Float) -> List (Vertex, Vertex, Vertex)
rotatedSquare ( angleXZ, angleYZ ) =
  let
    transformMat =
      Mat4.mul
        (Mat4.makeRotate (degrees angleXZ) Vec3.j)
        (Mat4.makeRotate (degrees angleYZ) Vec3.i)

    transform vertex =
      { vertex
          | position =
              Mat4.transform transformMat vertex.position
      }

    transformTriangle (a, b, c) =
      (transform a, transform b, transform c)
  in
  List.map transformTriangle square


square : List ( Vertex, Vertex, Vertex )
square =
  let
    topLeft     = Vertex (vec3 -1  1  1) (vec2 0 1)
    topRight    = Vertex (vec3  1  1  1) (vec2 1 1)
    bottomLeft  = Vertex (vec3 -1 -1  1) (vec2 0 0)
    bottomRight = Vertex (vec3  1 -1  1) (vec2 1 0)
  in
  [ ( topLeft, topRight, bottomLeft )
  , ( bottomLeft, topRight, bottomRight )
  ]



-- SHADERS


type alias Uniforms =
  { texture : Texture.Texture
  , perspective : Mat4
  }


vertexShader : WebGL.Shader Vertex Uniforms { vcoord : Vec2 }
vertexShader =
  [glsl|
    attribute vec3 position;
    attribute vec2 coord;
    uniform mat4 perspective;
    varying vec2 vcoord;

    void main () {
      gl_Position = perspective * vec4(position, 1.0);
      vcoord = coord;
    }
  |]


fragmentShader : WebGL.Shader {} Uniforms { vcoord : Vec2 }
fragmentShader =
  [glsl|
    precision mediump float;
    uniform sampler2D texture;
    varying vec2 vcoord;

    void main () {
      gl_FragColor = texture2D(texture, vcoord);
    }
  |]
