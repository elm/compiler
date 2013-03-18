-- Paths and Shapes.
-- These structures are purely geometric forms, no colors or styles.
-- Could be the basis of some cool geometry stuff :)

module Graphics.Geometry where

import List

type Path = [(Float,Float)]

path ps = ps
segment p1 p2 = [p1,p2]

type Shape = [(Float,Float)]

polygon points = points

rect w h = [ (0-w/2,0-h/2), (0-w/2,h/2), (w/2,h/2), (w/2,0-h/2) ]

oval w h =
  let n = 50
      f i = (w/2 * Math.cos (2*Math.pi/n * i), h/2 * Math.sin (2*Math.pi/n * i))
  in  map f [0..n-1]

circle r = oval (2*r) (2*r)

ngon n r =
  let m = toFloat n
      f i = ( r * Math.cos (2*Math.pi/m * i), r * Math.sin (2*Math.pi/m * i))
  in  map f [0..n-1]
