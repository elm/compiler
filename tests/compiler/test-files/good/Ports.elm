
-- incoming
port userID : String
port signal : Signal Int
port tuple : (Float,Bool)
port array : List Int
port record : { x:Float, y:Float }

-- outgoing
port fortyTwo : Int
port fortyTwo = 42

port time : Signal Float
port time = every second

port students : Signal (List {name:String, age:Int})
port students = constant []