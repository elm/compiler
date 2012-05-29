niceBlue   = rgb    0   (1/3) (2/3)
clearGreen = rgba (1/9) (8/9) (3/9) (1/2)

scene (x,y) (w,h) =
  collage w h [ filled niceBlue . rotate ((x+y)/1000) $ ngon 4 100 (200,200)
              , filled clearGreen $ ngon 5 30 (x,y)
              ]

main = lift2 scene Mouse.position Window.dimensions