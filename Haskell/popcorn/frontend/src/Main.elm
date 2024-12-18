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

initModel = {loading = False, numberPopcorn = "", receivedPopcorn = False, popcorn = ""}

init _    = (initModel,Cmd.none)

type alias Model = { loading         : Bool 
                   , numberPopcorn   : String
                   , receivedPopcorn : Bool 
                   , popcorn         : String 
                   }
view : Model -> Html Msg 
view model = div 
             [style "background-color" "yellow"
             ,style "text-align" "center"
             ] 
             [h1 [] [text "Popcorn Getter"]
             ,br [] []
             ,text "Enter popcorn ex: 10"
             ,br [] []
             ,input [onInput Popcorn,  maxlength 2] []
             ,br [] []
             ,button [onClick SubmitPop] [text "submit"]
             ,br [] []
             ,if model.loading then text "loading" else text ""
             ,br [] []
             ,div [] (popcornToImage model.popcorn)
             ]

poppedImage : Html Msg 
poppedImage = img [src "/popped"] []
popcornToImage : String -> List (Html Msg)

popcornToImage s = 
      case String.toList s of 
       [] -> []
       (c :: cs) -> 
         case c of 
          'p' -> poppedImage :: (popcornToImage <| String.fromList cs)
          'h' -> text "h" :: (popcornToImage <| String.fromList cs)
          'u' -> text "h" :: (popcornToImage <| String.fromList cs)
          _   -> text "" :: ((popcornToImage <| String.fromList cs))
-------------------
type Msg = Popcorn String
         | GotPopcorn (Result Http.Error String)
         | SubmitPop 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of 
   Popcorn s -> ({model | loading = False, numberPopcorn = s, popcorn = ""}, Cmd.none)
   GotPopcorn result -> 
         case result of 
          Err _  -> ({model | loading = False, receivedPopcorn = False, popcorn = "", numberPopcorn = ""}, Cmd.none)
          Ok r   -> ({model | loading = False, receivedPopcorn = True, popcorn = r, numberPopcorn = ""}, Cmd.none)
   SubmitPop -> ({model | loading = True, popcorn = "", numberPopcorn = ""}, if String.toInt model.numberPopcorn == Nothing then Cmd.none else postPopCorn model)
   
----------

postPopCorn : Model -> Cmd Msg 
postPopCorn model = post 
                  {url    = "/popcorn"
                  ,body   = jsonBody (E.string model.numberPopcorn)
                  ,expect = expectJson GotPopcorn D.string 
                  }

subscriptions _ = Sub.none