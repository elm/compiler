
module GameSkeleton where



{-- Part 1: Model the user input ----------------------------------------------

What information do you need to represent all relevant user input?

Task: Redefine `UserInput` to include all of the information you need.
      Redifene `userInput` to be a signal that correctly models the user
      input as described by `UserInput`.

------------------------------------------------------------------------------}

data UserInput = UserInput

userInput = constant UserInput

data Input = Input Float UserInput



{-- Part 2: Model the game ----------------------------------------------------

What information do you need to represent the entire game?

Tasks: Redefine `GameState` to represent your particular game.
       Redefine `defaultGame` to represent your initial game state.

For example, if you want to represent many objects that just have a position,
your GameState might just be a list of coordinates and your default game might
be an empty list (no objects at the start):

    data GameState = GameState [(Float,Float)]
    defaultGame = GameState []

------------------------------------------------------------------------------}

data GameState = GameState

defaultGame = GameState



{-- Part 3: Update the game ---------------------------------------------------

How does the game step from one state to another based on user input?

Task: redefine `stepGame` to use the UserInput and GameState
      you defined in parts 1 and 2. Maybe use some helper functions
      to break up the work, stepping smaller parts of the game.

------------------------------------------------------------------------------}

stepGame (Input delta (UserInput)) (GameState) = GameState



{-- Part 4: Display the game --------------------------------------------------

How should the GameState be displayed to the user?

Task: redefine `display` to use the GameState you defined in part 2.

------------------------------------------------------------------------------}

display (w,h) gameState = asText gameState



{-- That's all folks! ---------------------------------------------------------

The following code puts it all together and show it on screen.

------------------------------------------------------------------------------}

delta = fps 60
input = sampleOn delta (lift2 Input delta userInput)

gameState = foldp stepGame defaultGame input

main = lift2 display Window.dimensions gameState
