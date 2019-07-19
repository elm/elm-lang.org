-- Demonstrate how to load textures and put them on a cube.
--
-- Dependencies:
--   elm install elm-explorations/linear-algebra
--   elm install elm-explorations/webgl
--

import Browser
import Browser.Events as E
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Result
import Task
import WebGL
import WebGL.Texture as Texture



-- MAIN


main =
  Browser.element
    { init = init
    , view = view
    , update = \msg model -> (update msg model, Cmd.none)
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { angle : Float
  , texture : Maybe Texture.Texture
  }


init : () -> (Model, Cmd Msg)
init () =
  ( { angle = 0
    , texture = Nothing
    }
  , Task.attempt GotTexture (Texture.load "https://elm-lang.org/assets/wood-crate.jpg")
  )



-- UPDATE


type Msg
  = TimeDelta Float
  | GotTexture (Result Texture.Error Texture.Texture)


update : Msg -> Model -> Model
update msg model =
  case msg of
    TimeDelta dt ->
      { model | angle = model.angle + dt / 5000 }

    GotTexture result ->
      { model | texture = Result.toMaybe result }




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  E.onAnimationFrameDelta TimeDelta



-- VIEW


view : Model -> Html Msg
view model =
  case model.texture of
    Nothing ->
      Html.text "Loading texture..."

    Just texture ->
      WebGL.toHtml
        [ width 400, height 400, style "display" "block"
        ]
        [ WebGL.entity vertexShader fragmentShader crateMesh (toUniforms model.angle texture)
        ]



-- UNIFORMS


type alias Uniforms =
  { rotation : Mat4
  , perspective : Mat4
  , camera : Mat4
  , texture : Texture.Texture
  }


toUniforms : Float -> Texture.Texture -> Uniforms
toUniforms angle texture =
  { rotation =
      Mat4.mul
        (Mat4.makeRotate (3 * angle) (vec3 0 1 0))
        (Mat4.makeRotate (2 * angle) (vec3 1 0 0))
  , perspective = perspective
  , camera = camera
  , texture = texture
  }


perspective : Mat4
perspective =
  Mat4.makePerspective 45 1 0.01 100


camera : Mat4
camera =
  Mat4.makeLookAt (vec3 0 0 5) (vec3 0 0 0) (vec3 0 1 0)



-- MESH


type alias Vertex =
  { position : Vec3
  , coord : Vec2
  }


crateMesh : WebGL.Mesh Vertex
crateMesh =
  WebGL.triangles <| List.concatMap rotatedSquare <|
    [ (0, 0)
    , (90, 0)
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


vertexShader : WebGL.Shader Vertex Uniforms { vcoord : Vec2 }
vertexShader =
  [glsl|
    attribute vec3 position;
    attribute vec2 coord;
    uniform mat4 perspective;
    uniform mat4 camera;
    uniform mat4 rotation;
    varying vec2 vcoord;

    void main () {
        gl_Position = perspective * camera * rotation * vec4(position, 1.0);
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
