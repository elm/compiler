
import open Color
import open Graphics.WebGL
import open MJS

-- Setup the GL context

context : Signal GLContext
context = glContext 400 400

-- Define what our picture looks like

top = v3 0 1 0
left = v3 -1 -1 0
right = v3 1 -1 0

triangle : GLTriangle
triangle = GLTriangle (GLPoint top red) (GLPoint left blue) (GLPoint right green)

-- Bind our picture to a mesh in our GL context

triMesh : Signal Mesh
triMesh = lift (bindMesh [triangle]) context

-- Given an angle and a Mesh, what does the scene look like?

scene : Float -> Mesh -> Scene
scene angle mesh = 
    let xAxis = v3 1 0 0
        yAxis = v3 0 1 0
        rotationX = m4x4makeRotate (angle / 2) xAxis
        rotationY = m4x4makeRotate (angle / 3) yAxis
    in Node rotationX [Node rotationY [Leaf mesh]]

-- Standard accumulator and wiring it up with main

angle : Signal Float
angle = foldp (\_ a -> a + 0.1) 0 (fps 25)

main : Signal Element
main = lift2 renderGL context (lift2 scene angle triMesh)
