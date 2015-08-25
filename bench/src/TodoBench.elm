module TodoBench where

import String

---- MODEL ----

-- The full application state of our todo app.
type alias Model =
    { tasks : List Task
    , field : String
    , uid : Int
    , visibility : String
    }


type alias Task =
    { description : String
    , completed : Bool
    , editing : Bool
    , id : Int
    }


newTask : String -> Int -> Task
newTask desc id =
    { description = desc
    , completed = False
    , editing = False
    , id = id
    }


emptyModel : Model
emptyModel =
    { tasks = []
    , visibility = "All"
    , field = ""
    , uid = 0
    }


---- UPDATE ----

-- A description of the kinds of actions that can be performed on the model of
-- our application. See the following post for more info on this pattern and
-- some alternatives: http://elm-lang.org/learn/Architecture.elm
type Action
    = NoOp
    | UpdateField String
    | EditingTask Int Bool
    | UpdateTask Int String
    | Add
    | Delete Int
    | DeleteComplete
    | Check Int Bool
    | CheckAll Bool
    | ChangeVisibility String


-- How we update our Model on a given Action?
update : Action -> Model -> Model
update action model =
    case action of
      NoOp -> model

      Add ->
          { model |
              uid <- model.uid + 1,
              field <- "",
              tasks <-
                  if String.isEmpty model.field
                    then model.tasks
                    else model.tasks ++ [newTask model.field model.uid]
          }

      UpdateField str ->
          { model | field <- str }

      EditingTask id isEditing ->
          let updateTask t = if t.id == id then { t | editing <- isEditing } else t
          in
              { model | tasks <- List.map updateTask model.tasks }

      UpdateTask id task ->
          let updateTask t = if t.id == id then { t | description <- task } else t
          in
              { model | tasks <- List.map updateTask model.tasks }

      Delete id ->
          { model | tasks <- List.filter (\t -> t.id /= id) model.tasks }

      DeleteComplete ->
          { model | tasks <- List.filter (not << .completed) model.tasks }

      Check id isCompleted ->
          let updateTask t = if t.id == id then { t | completed <- isCompleted } else t
          in
              { model | tasks <- List.map updateTask model.tasks }

      CheckAll isCompleted ->
          let updateTask t = { t | completed <- isCompleted }
          in
              { model | tasks <- List.map updateTask model.tasks }

      ChangeVisibility visibility ->
          { model | visibility <- visibility }
          
testActions = 
  List.concat
  [ List.concatMap --Add 50 tasks 
      (\i -> [UpdateField <| "My task " ++ toString i, Add]) [1 .. 50]
  , [CheckAll True] --Check them all
  , [CheckAll False] --Uncheck them all
  , List.concatMap --Alter the text of even numbered tasks
      (\i -> [EditingTask i True, UpdateTask i "New Task text", Add] ) <| List.filter (\x -> x % 2 == 0 ) [1 .. 50]
  --Check and uncheck some more tasks
  , [Check 5 True, Check 7 True, Check 9 True, Check 13 True, Check 17 True, Check 23 True, Check 37 True]
  , [Check 7 False, Check 13 False, Check 23 False, Check 37 False]
  , [DeleteComplete] --Delete our checked tasks
  , [Delete 7, Delete 23] --Delete a few more tasks
  , List.concatMap --Add a few more tasks
      (\i -> [UpdateField <| "Second batch task " ++ toString i, Add]) [51 .. 80]
  , List.map --Check off and delete every other task
      (\i -> Check i True ) <| List.filter (\x -> x % 2 == 0 ) [1 .. 80]
  , [DeleteComplete]
  , [CheckAll True, DeleteComplete] --Check and delete the remaining tasks
  ]

  
evalActions : List Action -> Model
evalActions actions = 
  List.foldr update emptyModel actions