
import open Color
import open MJS

import Graphics.WebGL (GLContext, Vertex, Triangle, Mesh, Scene, makeGL, bufferMesh, transform, mesh, webgl)

-- Setup the GLContext

gl : Signal GLContext
gl = makeGL 400 400

-- Define what our geometry looks like

top = v3 0 1 0
left = v3 -1 -1 0
right = v3 1 -1 0

geometry : Triangle
geometry = (Vertex top red, Vertex left blue, Vertex right green)

-- Bind our geometry to a mesh in our GL gl

geoMesh : Signal Mesh
geoMesh = lift (bufferMesh [geometry]) gl

-- Build two helper transforms

perspective : M4x4
perspective = m4x4makePerspective 45 1 0.01 100

camera : Float -> M4x4
camera r = 
  let eye = v3 0 0 r
      center = v3 0 0 0
      up = v3 0 1 0
  in m4x4makeLookAt eye center up

-- Build the scene with extra model rotate transforms

scene : Float -> Mesh -> Scene
scene angle m = 
    let xAxis = v3 1 0 0
        yAxis = v3 0 1 0
        rotationX = m4x4makeRotate (angle / 2) xAxis
        rotationY = m4x4makeRotate (angle / 3) yAxis
    in transform perspective [
        transform (camera <| 5 + 3 * sin angle) [
        transform rotationX [
        transform rotationY [
        mesh m ]]]]

-- Standard accumulator and wiring it up with main

angle : Signal Float
angle = foldp (\_ a -> a + 0.1) 0 (fps 25)

main : Signal Element
main = lift2 webgl gl (lift2 scene angle geoMesh)

