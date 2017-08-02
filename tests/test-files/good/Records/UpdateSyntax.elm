{-
    A test for allowing an arbitrary expression as the first part of the record
    update syntax.
-}


type alias SubModel =  { counter : Int, other: Int }
type alias Model = { subModel : SubModel }

identityModel : Model -> Model
identityModel model = model

resetSubModel : Model -> SubModel
resetSubModel model =
    { model.subModel | counter = 0 , other = 1 }


termCall : Model -> Model
termCall model =
    { (identityModel model) | subModel = { counter = 1, other = 1} }

functionCall : Model -> Model
functionCall model =
    { identityModel model | subModel = { counter = 1, other = 1} }


emptyRecord = {}


(||) : Bool -> Bool -> Bool
(||) left right = if left then True else right

checkOrStillWorks = False || True
