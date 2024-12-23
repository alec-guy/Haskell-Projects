module Main exposing (..)

import Http exposing (..)
import Json.Decode as D  exposing (..)
import Json.Encode as E exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Browser 



main : Program () Model Msg 
main = Browser.element 
       { init = init
       , view = view 
       , update = update
       , subscriptions = subscriptions
       }

initModel = { argumentFrontend = []
            , loading = False
            , argumentBackend = {validity = "", cellContent = [], vars = [], varAssignments = []}
            , argumentFrontendUnParsed = ""
            }
init _    = (initModel,Cmd.none)

type alias Model = 
                 {argumentFrontend  : List String
                 ,loading           : Bool 
                 ,argumentBackend   : Argument
                 ,argumentFrontendUnParsed : String
                 }

view : Model -> Html Msg 
view model = div 
             [style "text-align" "center", class "main-div"] 
             [h1 [] [text "Truth Table Generator"]
             , br [] []
             ,textarea [onInput <| GetArgument] []
             , br [] []
             , text <| if model.loading then "loading..." else ""
             , br [] []
             , makeTable model.argumentFrontend model.argumentBackend
             , br [] []
             , button [onClick Submit] [text "submit"]
             , br [] []
             , br [] []
             , text model.argumentBackend.validity
             ]

------------------------
-- Making table
makeTable : List String -> Argument -> Html Msg 
makeTable argfrontend argbackend = 
      let vars = argbackend.vars 
      in 
      table 
      [class "truth-table"] 
      (
      [ tr 
        [] 
        (makeTableHeaders (vars ++ (argfrontend)))
      ] 
      ++ 
      (makeTableRows argbackend)
      )
     
makeTableHeaders : List String -> List (Html Msg)
makeTableHeaders headers = 
    case headers of 
     []        -> [] 
     (h :: hs) -> (th [class "table-header"] [text h]) :: (makeTableHeaders hs)

makeTableRows : Argument -> List (Html Msg)
makeTableRows arg = 
       let myTuple = (arg.varAssignments, arg.cellContent)
       in case (List.isEmpty (Tuple.first myTuple) && List.isEmpty (Tuple.second myTuple)) of 
           True   -> [] 
           False  -> 
            case myTuple of 
             (varAssignmentss, cellContents) -> 
                tr [class "table-row"] ((makeDataVar varAssignmentss) ++ (makeDataContent cellContents))
                :: 
                (makeTableRows <| {arg | varAssignments = (Maybe.withDefault []) <| (List.tail arg.varAssignments)
                                  ,cellContent = (Maybe.withDefault []) <| (List.tail arg.cellContent)
                                  }
                )
makeDataVar : List String -> List (Html Msg)
makeDataVar l = 
   case l of 
    (oneone :: onetwo) -> 
       case String.toList oneone of 
         (b :: bs) -> 
           (td [class "table-data"] [text <| String.fromChar b]) :: (makeDataVar ((String.fromList bs) :: onetwo))
         []        -> []
    []                -> [] 
makeDataContent : List (String, String) -> List (Html Msg)
makeDataContent l = 
    case l of 
     (t :: ts) -> 
         case t of 
          (premiseEvals, conclusionEval) -> 
               case String.toList premiseEvals of 
                 (b :: bs) -> 
                  (td [class "table-data"] [text <| String.fromChar b]) :: (makeDataContent <| ((String.fromList bs, conclusionEval) :: ts))
                 [] -> [(td [class "table-data"] [text conclusionEval])]
     []        -> [] 
-------------------
type Msg = GotArgument (Result Http.Error Argument)
         | GetArgument String 
         | Submit

type alias Argument = {validity : String, cellContent : List (String, String), vars : List String, varAssignments : List String}

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of 
   (GotArgument r) -> 
       case r of 
        (Ok a)   -> ({model | argumentBackend = a}, Cmd.none)
        (Err _ ) -> (model, Cmd.none)
   (GetArgument s) -> 
       ({model | argumentFrontendUnParsed = s}, Cmd.none)
   Submit         ->  
        ({model | loading = True, argumentFrontend = (String.lines model.argumentFrontendUnParsed)}, sendArgument model)
   
----------

sendArgument : Model -> Cmd Msg 
sendArgument model = post 
                   {url    = "/PropLogic"
                   ,body   = jsonBody (E.object [("argToParse", E.string model.argumentFrontendUnParsed)])
                   ,expect = expectJson GotArgument argumentDecoder  
                   }
argumentDecoder : Decoder Argument 
argumentDecoder = 
   map4 Argument
   (D.field "validity" D.string)
   (D.field "cellContent" (D.list (D.map2 Tuple.pair 
        (D.index 0 D.string)
        (D.index 1 D.string))))
   (D.field "vars" (D.list D.string))
   (D.field "varAssignments" (D.list D.string))
-- The only thing i used AI for was argumentDecoder and that's it

subscriptions _ = Sub.none