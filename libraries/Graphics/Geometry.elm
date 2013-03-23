-- Paths and Shapes.
-- These structures are purely geometric forms, no colors or styles.
-- Could be the basis of some cool geometry stuff :)

module Graphics.Geometry where

import List
import Native.Utils (toFloat)

type Path = [(Float,Float)]

path ps = ps
segment p1 p2 = [p1,p2]

type Shape = [(Float,Float)]

polygon points = points

rect w h = [ (0-w/2,0-h/2), (0-w/2,h/2), (w/2,h/2), (w/2,0-h/2) ]

oval w h =
  let n = 50
      t = 2 * Math.PI / n
      hw = w/2
      hh = h/2
      f i = (hw * Math.cos (t*i), hh * Math.sin (t*i))
  in  map f [0..n-1]

circle r = oval (2*r) (2*r)

ngon n r =
  let m = toFloat n
      t = 2 * Math.PI / m
      f i = ( r * Math.cos (t*i), r * Math.sin (t*i) )
  in  map f [0..n-1]
