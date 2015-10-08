module Realistic.Todo (benchmark) where
{-| Source: https://github.com/evancz/elm-todomvc
-}

import Benchmark
import String


benchmark : Benchmark.Benchmark
benchmark =
  Benchmark.suite "todomvc business logic"
    [ run "type in 5 todos, each 15 characters long" typingActions
    , run "do everything" batchActions
    ]


run : String -> List Action -> Benchmark.Benchmark
run description actions =
  Benchmark.test
    (description ++ " - " ++ toString (List.length actions) ++ " actions")
    (\_ -> List.foldl update emptyModel actions)


typingActions =
  let
    enter i =
      List.map (\n -> UpdateField (String.repeat n "X")) [1..15]
      ++ [ Add ]
  in
    List.concatMap enter [1..5]


batchActions =
  let
    addTask i =
      [ UpdateField ("My task " ++ toString i)
      , Add
      ]

    editTask i =
      [ EditingTask i True
      , UpdateTask i "New Task text"
      , EditingTask i False
      ]

    check i =
      Check i True

    uncheck i =
      Check i False

    isEven i =
      i % 2 == 0
  in
    List.concat
      [ List.concatMap addTask [1..50]
      , [CheckAll True, CheckAll False]
      , List.concatMap editTask (List.filter isEven [1 .. 50])
      , List.map check [5, 7, 9, 13, 17, 23, 37]
      , List.map uncheck [7, 13, 23, 37]
      , [DeleteComplete, Delete 7, Delete 23]
      , List.concatMap addTask [51..80]
      , List.map check (List.filter isEven [51..80])
      , [DeleteComplete, CheckAll True, DeleteComplete]
      ]



-- MODEL


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



-- UPDATE


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


update : Action -> Model -> Model
update action model =
    case action of
      NoOp -> model

      Add ->
          { model |
              uid = model.uid + 1,
              field = "",
              tasks =
                  if String.isEmpty model.field
                    then model.tasks
                    else model.tasks ++ [newTask model.field model.uid]
          }

      UpdateField str ->
          { model | field = str }

      EditingTask id isEditing ->
          let updateTask t = if t.id == id then { t | editing = isEditing } else t
          in
              { model | tasks = List.map updateTask model.tasks }

      UpdateTask id task ->
          let updateTask t = if t.id == id then { t | description = task } else t
          in
              { model | tasks = List.map updateTask model.tasks }

      Delete id ->
          { model | tasks = List.filter (\t -> t.id /= id) model.tasks }

      DeleteComplete ->
          { model | tasks = List.filter (not << .completed) model.tasks }

      Check id isCompleted ->
          let updateTask t = if t.id == id then { t | completed = isCompleted } else t
          in
              { model | tasks = List.map updateTask model.tasks }

      CheckAll isCompleted ->
          let updateTask t = { t | completed = isCompleted }
          in
              { model | tasks = List.map updateTask model.tasks }

      ChangeVisibility visibility ->
          { model | visibility = visibility }

