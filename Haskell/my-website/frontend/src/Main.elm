module Main exposing (..)

import Http exposing (..)
import Json.Decode as D  exposing (..)
import Json.Encode as E exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Browser 
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)

main : Program () Model Msg 
main = Browser.element 
       { init = init
       , view = view 
       , update = update
       , subscriptions = subscriptions
       }
type Model = LoginPage 
           | GotUser User
type alias User = { username : Maybe String
                  , password : Maybe String
                  }
type Msg   = Username String 
           | Password String

update : Msg -> Model -> Html Msg 
update msg model = 
   case msg of 
    Username u -> 
      case model of 
       LoginPage    -> 
        (GotUser { username = Just u, password = Nothing}, Cmd.none)
       GotUser user -> 
          case (user.username, user.password) of 
            (Just x, Just y) -> 
               (model, sendUserToServer user)
            _   -> (GotUser <| {user | username = Just u}, Cmd.none)
      
    
view : Model -> Html Msg 
view model = 
  case model of 
   LoginPage -> loginpage 

loginpage : Html Msg 
loginpage = 
   div 
   [] 
   [ label 
      [] 
       [Html.text "Enter Username"
       ,input
        [onInput Username] 
        []
       ]
    ,label 
     [] 
     [Html.text "Enter Password"
     ,input 
      [onInput Password]
      []
     ]
   ] 
update : Msg - Model -> Html Msg