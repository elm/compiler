
module Box where

import Math.Vector3 (..)
import Math.Matrix4 (..)
import Graphics.WebGL (Triangle, zipTriangle, Shader, Model, model, webgl)
import Window(dimensions)

-- Create a cube in which each point has a color

type ColorPoint = { color:Vec3, point:Vec3 }

face : Color -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> [Triangle ColorPoint]
face color a b c d =
  let toV3 color =
        let c = toRgb color
        in  v3 (toFloat c.red / 255) (toFloat c.green / 255) (toFloat c.blue / 255)
      p = ColorPoint (toV3 color)
  in
      [ (p a, p b, p c), (p c, p d, p a) ]

cube : [Triangle ColorPoint]
cube =
  let rft = v3  1  1  1   -- right, front, top
      lft = v3 -1  1  1   -- left,  front, top
      lbt = v3 -1 -1  1
      rbt = v3  1 -1  1
      rbb = v3  1 -1 -1
      rfb = v3  1  1 -1
      lfb = v3 -1  1 -1
      lbb = v3 -1 -1 -1
  in
      concat [ face green  rft rfb rbb rbt   -- right
             , face blue   rft rfb lfb lft   -- front
             , face purple lft lfb lbb lbt   -- left
             , face orange rbt rbb lbb lbt   -- back
             ]

-- Create the scene

main : Signal Element
main = webgl (400,400) <~ lift scene angle

angle : Signal Float
angle = foldp (\dt theta -> theta + dt / 5000) 0 (fps 25)

scene : Float -> [Model]
scene angle =
    [ model vertexShader fragmentShader cube (uniforms angle) ]

uniforms : Float -> { rotation:Mat4, perspective:Mat4, camera:Mat4, shade:Float }
uniforms t =
    { rotation = mul (makeRotate (3*t) (v3 0 1 0)) (makeRotate (2*t) (v3 1 0 0))
    , perspective = makePerspective 45 1 0.01 100
    , camera = makeLookAt (v3 0 0 5) (v3 0 0 0) (v3 0 1 0)
    , shade = 0.8
    }

-- Shaders

vertexShader : Shader { attr | point:Vec3, color:Vec3 }
                      { unif | rotation:Mat4, perspective:Mat4, camera:Mat4 }
                      { vcolor:Vec3 }
vertexShader = [glShader|

attribute vec3 point;
attribute vec3 color;
uniform mat4 perspective;
uniform mat4 camera;
uniform mat4 rotation;
varying vec3 vcolor;
void main () {
    gl_Position = perspective * camera * rotation * vec4(point, 1.0);
    vcolor = color;
}

|]

fragmentShader : Shader {} { u | shade:Float } { vcolor:Vec3 }
fragmentShader = [glShader|

precision mediump float;
uniform float shade;
varying vec3 vcolor;
void main () {
    gl_FragColor = shade * vec4(vcolor, 1.0);
}

|]
