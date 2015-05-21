
-- incoming
port userID : String
port number : Int
port tuple : (Float,Bool)
port array : List Int
port record : { x:Float, y:Float }

-- outgoing
port fortyTwo : Int
port fortyTwo =
    42


port time : Float
port time =
    3.14

port students : List { name:String, age:Int }
port students =
    [ { name = "Tom", age = 42 } ]