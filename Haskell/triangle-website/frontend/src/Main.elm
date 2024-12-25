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


init _    = (initModel,Cmd.none)

view = 

update = 

subscriptions = Sub.none