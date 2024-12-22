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
      [id "truth-table"] 
      (
      [ tr 
        [] 
        (makeTableHeaders argfrontend)
      ] 
      ++ 
      (makeTableRows 1 argbackend)
      )
     
makeTableHeaders : List String -> List (Html Msg)
makeTableHeaders headers = 
    case headers of 
     []        -> [] 
     (h :: hs) -> (th [] [text h]) :: (makeTableHeaders hs)

makeTableRows : Int -> Argument -> List (Html Msg)
makeTableRows i argum = 
    case i > (List.length argum.cellContent) of 
     True  -> [] 
     False -> (tr 
               [style "border-bottom" "1px solid #ddd"
               ]  
               (makeTableData argum.cellContent)
              )
              ::
              (makeTableRows (i + 1) argum)

makeTableData : List (String, String) -> List (Html Msg)
makeTableData l = 
   case l of 
    [] -> [] 
    (t :: ts) -> (g t) ++ (makeTableData ts)

g : (String, String) -> List (Html Msg)
g t = case (String.toList <| Tuple.first t) of 
         []         -> [td [] [text <| Tuple.second t]]
         (b :: bs)  -> [td [] [text <| String.fromChar <| b]] ++  (g (String.fromList bs, Tuple.second t))
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