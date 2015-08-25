module AnimateBench where

import Time exposing (Time)


{-| Main Spring Type.
A spring's behavior is defined by its stiffness and damping parameters.
-}
type alias Spring a =
  { stiffness   : Float
  , damping     : Float
  , position    : a
  , velocity    : a
  , destination : a
  }
  

defaultSpring  = 
  Spring 1.0 0.5 0.0 1.0 100.0

  
epsilon = 0.0001

  
{-| Animate a spring given a framerate.

    animate framerate spring
-}
animate : Time -> Spring Float -> Spring Float
animate fpms spring =
  let
      frameRate = fpms / 1000

      fspring = -spring.stiffness * (spring.position - spring.destination)

      fdamper = -spring.damping  * spring.velocity

      a = fspring + fdamper

      newV = spring.velocity + a * frameRate
      newX = spring.position + newV * frameRate
  in
      if
          abs (newV - spring.velocity) < epsilon && abs (newX - spring.position) < epsilon
      then
          { spring | position <- spring.destination
                   , velocity <- 0
          }
      else
          { spring | position <- newX
                   , velocity <- newV
          }  

          

runAnimation : List Float -> Spring Float
runAnimation timeSteps = 
  List.foldr animate defaultSpring timeSteps