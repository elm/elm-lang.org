-- Thwomp looks at your mouse. What is it up to?
--
-- Dependencies:
--   elm install elm/json
--   elm install elm-explorations/linear-algebra
--   elm install elm-explorations/webgl
--
-- Thanks to The PaperNES Guy for the texture:
--   https://the-papernes-guy.deviantart.com/art/Thwomps-Thwomps-Thwomps-186879685


import Browser
import Browser.Dom as Dom
import Browser.Events as E
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Json.Decode as D
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Result
import Task
import WebGL
import WebGL.Texture as Texture



-- MAIN


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = \msg model -> ( update msg model, Cmd.none )
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { width : Float
  , height : Float
  , x : Float
  , y : Float
  , side : Maybe Texture.Texture
  , face : Maybe Texture.Texture
  }


init : () -> ( Model, Cmd Msg )
init _ =
  ( { width = 0
    , height = 0
    , x = 0
    , y = 0
    , face = Nothing
    , side = Nothing
    }
  , Cmd.batch
      [ Task.perform GotViewport Dom.getViewport
      , Task.attempt GotFace (Texture.loadWith options "https://elm-lang.org/assets/thwomp-face.jpg")
      , Task.attempt GotSide (Texture.loadWith options "https://elm-lang.org/assets/thwomp-side.jpg")
      ]
  )


options : Texture.Options
options =
  { magnify = Texture.nearest
  , minify = Texture.nearest
  , horizontalWrap = Texture.repeat
  , verticalWrap = Texture.repeat
  , flipY = True
  }



-- UPDATE


type Msg
  = GotFace (Result Texture.Error Texture.Texture)
  | GotSide (Result Texture.Error Texture.Texture)
  | GotViewport Dom.Viewport
  | Resized Int Int
  | MouseMoved Float Float


update : Msg -> Model -> Model
update msg model =
  case msg of
    GotFace result ->
      { model
          | face = Result.toMaybe result
      }

    GotSide result ->
      { model
          | side = Result.toMaybe result
      }

    GotViewport { viewport } ->
      { model
          | width = viewport.width
          , height = viewport.height
      }

    Resized width height ->
      { model
          | width = toFloat width
          , height = toFloat height
      }

    MouseMoved x y ->
      { model
          | x = x
          , y = y
      }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ E.onResize Resized
    , E.onMouseMove decodeMovement
    ]


decodeMovement : D.Decoder Msg
decodeMovement =
  D.map2 MouseMoved
    (D.field "pageX" D.float)
    (D.field "pageY" D.float)



-- VIEW


view : Model -> Html Msg
view model =
  case Maybe.map2 Tuple.pair model.face model.side of
    Nothing ->
      Html.text "Loading textures..."

    Just (face, side) ->
      let
        perspective =
          toPerspective model.x model.y model.width model.height
      in
      WebGL.toHtml
        [ style "display" "block"
        , style "position" "absolute"
        , style "left" "0"
        , style "top" "0"
        , width (round model.width)
        , height (round model.height)
        ]
        [ WebGL.entity vertexShader fragmentShader faceMesh
            { perspective = perspective
            , texture = face
            }
        , WebGL.entity vertexShader fragmentShader sidesMesh
            { perspective = perspective
            , texture = side
            }
        ]


toPerspective : Float -> Float -> Float -> Float -> Mat4
toPerspective x y width height =
  let
    eye =
      Vec3.scale 6 <| Vec3.normalize <|
        vec3 (0.5 - x / width) (y / height - 0.5) 1
  in
  Mat4.mul
    (Mat4.makePerspective 45 (width / height) 0.01 100)
    (Mat4.makeLookAt eye (vec3 0 0 0) Vec3.j)



-- MESHES


type alias Vertex =
  { position : Vec3
  , coord : Vec2
  }


faceMesh : WebGL.Mesh Vertex
faceMesh =
  WebGL.triangles square


sidesMesh : WebGL.Mesh Vertex
sidesMesh =
  WebGL.triangles <| List.concatMap rotatedSquare <|
    [ (90, 0)
    , (180, 0)
    , (270, 0)
    , (0, 90)
    , (0, 270)
    ]


rotatedSquare : (Float, Float) -> List (Vertex, Vertex, Vertex)
rotatedSquare (angleXZ, angleYZ) =
  let
    transformMat =
      Mat4.mul
        (Mat4.makeRotate (degrees angleXZ) Vec3.j)
        (Mat4.makeRotate (degrees angleYZ) Vec3.i)

    transform vertex =
      { vertex | position = Mat4.transform transformMat vertex.position }

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
  [ (topLeft, topRight, bottomLeft)
  , (bottomLeft, topRight, bottomRight)
  ]



-- SHADERS


type alias Uniforms =
  { perspective : Mat4
  , texture : Texture.Texture
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
      vcoord = coord.xy;
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
