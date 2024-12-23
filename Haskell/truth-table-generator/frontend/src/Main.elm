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
            , argumentBackend = {validity = "", cellContent = []}
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
             [style "text-align" "center", style "background-color" "red"] 
             [h4 [] [text "Truth Table Generator"]
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
      table 
      [class "truth-table"] 
      (
      [ tr 
        [] 
        (makeTableHeaders argfrontend)
      ] 
      ++ 
      (makeTableRows argbackend)
      )
     
makeTableHeaders : List String -> List (Html Msg)
makeTableHeaders headers = 
    case headers of 
     []        -> [] 
     (h :: hs) -> (th [] [text h]) :: (makeTableHeaders hs)
makeTableRows : Argument -> List (Html Msg)
makeTableRows arg = 
        case arg.cellContent of 
         [] -> [] 
         (firstRow :: otherRows) -> 
              tr [] (makeData firstRow) :: (makeTableRows {arg | cellContent = otherRows})
makeData : (String, String) -> List (Html Msg) 
makeData t = 
      case t of 
       (premises, conclusionEval) -> 
               case String.toList premises of 
                [] -> [td [] [text conclusionEval]]
                (firstPremise :: otherPremises) -> 
                    (td [] [text <| String.fromChar <| firstPremise]) 
                    :: 
                    (makeData ((String.fromList otherPremises), conclusionEval))
       

          

-----------------------------------------------------------------------------

-------------------
type Msg = GotArgument (Result Http.Error Argument)
         | GetArgument String 
         | Submit

type alias Argument = {validity : String, cellContent : List (String, String)}

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
   map2 Argument
   (D.field "validity" D.string)
   (D.field "cellContent" (D.list (D.map2 Tuple.pair 
        (D.index 0 D.string)
        (D.index 1 D.string))))
-- The only thing i used AI for was argumentDecoder and that's it

subscriptions _ = Sub.none