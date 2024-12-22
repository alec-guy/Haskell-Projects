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

initModel = 
init _    = (initModel,Cmd.none)

type alias Model = 

view : Model -> Html Msg 
view model = 




-------------------
type Msg = 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of 
   
----------

sendArgument : Model -> Cmd Msg 
sendArgument model = post 
                   {url    = "/PropLogic"
                   ,body   = jsonBody (E.string model.argument)
                   ,expect = expectJson GotArgument argumentDecoder  
                   }
argumentDecoder : Decoder Argument 
argumentDecoder = 

subscriptions _ = Sub.none