-- Try adding the ability to crouch or to land on top of the crate.

import Graphics.Element exposing (..)
import Http exposing (..)
import Keyboard
import List
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Vector3 as V3
import Math.Matrix4 exposing (..)
import Text exposing (..)
import Time exposing (..)
import WebGL exposing (..)
import Window


-- MODEL

type alias Person =
    { position : Vec3
    , velocity : Vec3
    }


type alias Inputs =
    ( Bool, {x:Int, y:Int}, Float )


eyeLevel : Float
eyeLevel = 2


defaultPerson : Person
defaultPerson =
    { position = vec3 0 eyeLevel -10
    , velocity = vec3 0 0 0
    }


-- UPDATE

update : Inputs -> Person -> Person
update (isJumping, directions, dt) person =
    person
        |> walk directions
        |> jump isJumping
        |> gravity dt
        |> physics dt


walk : { x:Int, y:Int } -> Person -> Person
walk directions person =
    if getY person.position > eyeLevel
      then person
      else
        let vx = toFloat -directions.x
            vz = toFloat  directions.y
        in
            { person |
                velocity <- vec3 vx (getY person.velocity) vz
            }


jump : Bool -> Person -> Person
jump isJumping person =
    if not isJumping || getY person.position > eyeLevel
      then person
      else
        let (vx,_,vz) = toTuple person.velocity
        in
            { person |
                velocity <- vec3 vx 2 vz
            }


physics : Float -> Person -> Person
physics dt person =
    let position = person.position `add` V3.scale dt person.velocity
        (x,y,z) = toTuple position
    in
        { person |
            position <-
                if y < eyeLevel then vec3 x eyeLevel z else position
        }


gravity : Float -> Person -> Person
gravity dt person =
    if getY person.position <= eyeLevel
      then person
      else
        let v = toRecord person.velocity
        in
            { person |
                velocity <- vec3 v.x (v.y - 2 * dt) v.z
            }


-- SIGNALS

main : Varying Element
main =
    let person = Signal.foldp update defaultPerson inputs
        entities =
            Varying.map2 world
                (loadTexture "/texture/woodCrate.jpg")
                (Varying.map2 perspective Window.dimensions person)
    in
        Varying.map2 view Window.dimensions entities


inputs : Varying Inputs
inputs =
    let dt = Varying.map (\t -> t/500) (fps 25)
    in
        Varying.map3 (,,) Keyboard.space Keyboard.arrows dt
          |> Signal.sampleOn dt


-- VIEW

perspective : (Int,Int) -> Person -> Mat4
perspective (w,h) person =
    mul (makePerspective 45 (toFloat w / toFloat h) 0.01 100)
        (makeLookAt person.position (person.position `add` k) j)


view : (Int,Int) -> List Entity -> Element
view (w,h) entities =
    layers
        [ webgl (w,h) entities
        , container w 100 position message
        ]


position = midLeftAt (absolute 40) (relative 0.5)


message : Element
message =
    plainText <|
        "Walk around with a first person perspective.\n"
        ++ "Arrows keys to move, space bar to jump."


world : Response Texture -> Mat4 -> List Entity
world response perspective =
    case response of
        Waiting     -> []
        Failure _ _ -> []
        Success tex ->
            [entity vertexShader fragmentShader crate { crate=tex, perspective=perspective }]


-- Define the mesh for a crate

type alias Vertex =
    { position:Vec3, coord:Vec3 }


crate : List (Triangle Vertex)
crate =
    List.concatMap rotatedFace [ (0,0), (90,0), (180,0), (270,0), (0,90), (0,-90) ]


rotatedFace : (Float,Float) -> List (Triangle Vertex)
rotatedFace (angleXZ,angleYZ) =
    let x = makeRotate (degrees angleXZ) j
        y = makeRotate (degrees angleYZ) i
        t = x `mul` y
    in
        List.map (map (\v -> {v | position <- transform t v.position })) face


face : List (Triangle Vertex)
face =
    let topLeft     = Vertex (vec3 -1  1 1) (vec3 0 1 0)
        topRight    = Vertex (vec3  1  1 1) (vec3 1 1 0)
        bottomLeft  = Vertex (vec3 -1 -1 1) (vec3 0 0 0)
        bottomRight = Vertex (vec3  1 -1 1) (vec3 1 0 0)
    in
        [ (topLeft,topRight,bottomLeft)
        , (bottomLeft,topRight,bottomRight)
        ]


-- Shaders

vertexShader : Shader { position:Vec3, coord:Vec3 } { u | perspective:Mat4 } { vcoord:Vec2 }
vertexShader = [glsl|

attribute vec3 position;
attribute vec3 coord;
uniform mat4 perspective;
varying vec2 vcoord;

void main () {
  gl_Position = perspective * vec4(position, 1.0);
  vcoord = coord.xy;
}

|]

fragmentShader : Shader {} { u | crate:Texture } { vcoord:Vec2 }
fragmentShader = [glsl|

precision mediump float;
uniform sampler2D crate;
varying vec2 vcoord;

void main () {
  gl_FragColor = texture2D(crate, vcoord);
}

|]

--}