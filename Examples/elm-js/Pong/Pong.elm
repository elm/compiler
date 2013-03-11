
{-----  Overview  ------------------------------------------------------

  This game displays some of the strengths of Functional Reactive
  Programming (FRP). By the end of this file we will have written an
  entire GUI/game without any imperative code! No global mutable state,
  no flipping pixels, no destructive updates. In fact, Elm disallows all
  of these things at the language level. So good design and safe coding
  practices are a requirement, not just self-inforced suggestions.

  This code neatly divides Pong into three major parts: modeling the
  game, updating the game, and viewing the game. It may be helpful to
  think of it as a functional variation on the Model-View-Controller
  paradigm.

  The code for Pong is structured as follows:

    1. MODEL:
       First we need to define Pong. We do this by modelling Pong with
       simple data structures. We need two categories of model:
         - Inputs to the game. For Pong, this is keyboard input from
           users and clock-ticks from the frame rate manager.
         - A model of the game itself: paddles, ball, score, etc.
           Without a model of the game we would have nothing to update
           or display!
       These models are the basis for the next two sections, holding
       all of the information about Pong that we will need.

    2. UPDATE:
       When new inputs come in, we need to update the current state
       of the game. Without updates, this version of Pong would be very
       very boring! This section defines a number of 'step functions'
       that step the game forward based on our inputs. By separating
       this from the model and display code, we can change how the game
       works (how it steps forward) without changing anything else: the
       underlying model and the display code need not be touched.

    3. VIEW:
       Finally, we need a display function that defines the user's view
       of the game. This code is separate from the game logic, so like
       the update logic, it can be modified without affecting any other
       part of the program. We can also define many different views
       of the same underlying model. In Pong there's not much need for
       this, but as your model becomes more complex this may be very
       useful!

  If you would like to make a game or larger application in Elm, use
  this structure! Maybe even use this file as a starting point for
  playing around with your own ideas.

  Let's get started!

-----------------------------------------------------------------------}


module Pong where

import JavaScript


-- Set the frames per second (FPS) to 60, calculate the deltas (the
-- difference between the two latest times, the amount of time since
-- the last frame), and convert the time into a number of seconds.

delta = lift inSeconds (fps 60)


------------------------------------------------------------------------
------                  Modelling User Input                      ------
------------------------------------------------------------------------

-- During gameplay, all keyboard input is about the position of the
-- two paddles. So the keyboard input can be reduced to two directions,
-- each represented by an integer in {-1,0,1}. Furthermore, the SPACE
-- key is used to start the game between rounds, so we also need a
-- boolean value to represent whether it is pressed.

data KeyInput = KeyInput Bool Int Int

defaultKeyInput = KeyInput False 0 0

keyInput = lift3 KeyInput Keyboard.space
                          (lift .y Keyboard.wasd)
                          (lift .y Keyboard.arrows)


------------------------------------------------------------------------
------                  Combining all inputs                      ------
------------------------------------------------------------------------

-- The inputs to this game include a timestep (which we extracted from
-- JavaScript) and the keyboard input from the users.

data Input = Input Float KeyInput

-- Combine both kinds of inputs and filter out keyboard events. We only
-- want the game to refresh on clock-ticks, not key presses too.

input = sampleOn delta (lift2 Input delta keyInput)



------------------------------------------------------------------------
------            Modelling Pong / a State Machine                ------
------------------------------------------------------------------------

-- Pong has two obvious components: the ball and two paddles.

data Paddle = Paddle Float                      -- y-position
data Ball   = Ball (Float,Float) (Float,Float)  -- position and velocity

-- But we also want to keep track of the current score and whether
-- the ball is currently in play. This will allow us to have rounds
-- of play rather than just having the ball move around continuously.

data Score = Score Int Int
data State = Play | BetweenRounds

-- Together, this information makes up the state of the game. We model
-- Pong by using the inputs (defined above) to update the state of the
-- game!

data GameState = GameState State Score Ball Paddle Paddle


-- I have chosen to parameterize the size of the board, so it can
-- be changed with minimal effort.

gameWidth  = 600
gameHeight = 400
halfWidth  = gameWidth  / 2
halfHeight = gameHeight / 2


-- Before we can update anything, we must first define the default
-- configuration of the game. In our case we want to start between
-- rounds with a score of zero to zero.

defaultGame = GameState BetweenRounds
                        (Score 0 0)
                        (Ball (halfWidth, halfHeight) (150,150))
                        (Paddle halfHeight)
                        (Paddle halfHeight)



------------------------------------------------------------------------
------               Stepping from State to State                 ------
------------------------------------------------------------------------

-- Now to step the game from one state to another. We can break this up
-- into smaller components.

-- First, we define a step function for updating the position of
-- paddles. It depends on our timestep and a desired direction (given
-- by keyboard input).

stepPaddle delta dir (Paddle y) =
  Paddle $ clamp 20 (gameHeight-20) (y - toFloat dir * 200 * delta)


-- We must also step the ball forward. This is more complicated due to
-- the many kinds of collisions that can happen. All together, this
-- function figures out the new velocity of the ball based on 
-- collisions with the top and bottom borders and collisions with the
-- paddles. This new velocity is used to calculate a new position.

-- This function also determines whether a point has been scored and
-- who receives the point. Thus, its output is a new Ball and points
-- to be added to each player.

makePositive n = if n > 0 then n else 0-n
makeNegative n = if n > 0 then 0-n else n
within epsilon n x = x > n - epsilon && x < n + epsilon

stepVelocity velocity lowerCollision upperCollision =
  if lowerCollision then makePositive velocity else
  if upperCollision then makeNegative velocity else velocity

stepBall delta (Ball (x,y) (vx,vy)) (Paddle y1) (Paddle y2) =
  let hitPaddle1 = within 20 y1 y && within 8 25 x
      hitPaddle2 = within 20 y2 y && within 8 (gameWidth - 25) x
      vx' = stepVelocity vx hitPaddle1 hitPaddle2
      vy' = stepVelocity vy (y < 7) (y > gameHeight - 7)
      scored = x > gameWidth || x < 0
      x' = if scored then halfWidth  else x + vx' * delta
      y' = if scored then halfHeight else y + vy' * delta
  in ( Ball (x',y') (vx',vy')
     , if x > gameWidth then 1 else 0
     , if x < 0         then 1 else 0 )


-- Finally, we define a step function for the entire game. This steps from state to
-- state based on the inputs to the game.

stepGame (Input delta (KeyInput space dir1 dir2))
         (GameState state (Score s1 s2) ball paddle1 paddle2) =
  let (ball',s1',s2') = if state == Play then stepBall delta ball paddle1 paddle2
                                         else (ball, 0, 0)
      state' = case state of
                 Play -> if s1' /= s2' then BetweenRounds else state
                 BetweenRounds -> if space then Play else state
  in  GameState state'
                (Score (s1+s1') (s2+s2'))
                ball'
                (stepPaddle delta dir1 paddle1)
                (stepPaddle delta dir2 paddle2)


-- Now we put it all together. We have a signal of inputs that changes whenever there
-- is a clock tick. This input signal carries the all the information we need about
-- the keyboard. We also have a step function that steps from one game-state to the
-- next based on some inputs.

-- The `gameState` signal steps forward every time a new input comes in. It starts
-- as the default game and progresses based on user behavior.

gameState = foldp stepGame defaultGame input



------------------------------------------------------------------------
------                    Displaying the Game                     ------
------------------------------------------------------------------------

-- These functions take a GameState and turn it into something a user
-- can see and understand. It is totally independent of how the game
-- updates, it only needs to know the current game state. This allows us
-- to change how the game looks without changing any of the logic of the
-- game.


-- This function displays the current score and directions.

scoreBoard w inPlay p1 p2 =
  let code = text . monospace . toText
      stack top bottom = flow down [ code " ", code top, code bottom ]
      msg = width w . centeredText . monospace $ toText "Press SPACE to begin"
      board = flow right [ stack "W" "S", spacer 20 1
                         , text . Text.height 4 . toText $ show p1 ++ "    " ++ show p2
                         , spacer 20 1, stack "&uarr;" "&darr;" ]
      score = container w (heightOf board) midTop board
  in  if inPlay then score else score `above` msg


-- This function displays the entire GameState.

display (w,h) (GameState state (Score p1 p2) (Ball pos _) (Paddle y1) (Paddle y2)) =
  layers
    [ let pongGreen = rgb 60 100 60 in
      container w h middle $ collage gameWidth gameHeight
        [ filled pongGreen (rect gameWidth gameHeight (halfWidth,halfHeight))
        , filled white (oval 15 15 pos)                    -- ball
        , filled white (rect 10 40 (            20, y1))   -- first paddle
        , filled white (rect 10 40 (gameWidth - 20, y2))   -- second paddle
        ]
    , scoreBoard w (state == Play) p1 p2
    ]

-- We can now define a view of the game (a signal of Elements) that changes
-- as the GameState changes. This is what the users will see.

main = lift2 display Window.dimensions gameState
