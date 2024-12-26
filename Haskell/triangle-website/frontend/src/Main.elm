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

initModel = { displayBaseAndHeight = False
            , displayThreeSides = False
            , displaySAS = False
            , base       = Nothing 
            , height     = Nothing 
            }
init _    = (initModel ,Cmd.none)
type Msg = BaseAndHeight Bool 
         | ThreeSides Bool 
         | SAS Bool 
         | Base String
         | Height String 

type alias Model = { displayBaseAndHeight : Bool 
                   , displayThreeSides    : Bool 
                   , displaySAS           : Bool 
                   , base                 : Maybe Int 
                   , height               : Maybe Int 
                   }
viewDisplayBaseAndHeight : Html Msg 
viewDisplayBaseAndHeight = 
       Html.form 
       [] 
       [
         input [onInput Base] []
       , input [onInput Height] []
       ]
view : Model -> Html Msg 
view model = 
   div 
   [] 
   [h1 [] [Html.text "My SVG"]
   ,mySvg
   ,br [] []
   ,Html.select
    [
    ] 
    [option
     [onClick BaseAndHeight] 
     [Html.text "base and height"] 
    ,option
     [onClick ThreeSides] 
     [Html.text "three sides (SSS)"]
    ,option 
     [onClick SAS] 
     [Html.text "two sides + angle between (SAS)"]
    ] 
   , if model.displayBaseAndHeight 
     then viewDisplayBaseAndHeight
     else Html.text ""
   
   ]
------------------------------------------
update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of 
   (BaseAndHeight b) -> 
        ({model | displayBaseAndHeight = b}, Cmd.none)
   (ThreeSides b)    -> 
      ({model | displayThreeSides = b},Cmd.none)
   (SAS b)           -> 
      ({model | displaySAS = b}, Cmd.none)
   (Base s)          -> 
     ({model | base = String.toInt s}, Cmd.none)-- if (not <| String.isEmpty model.height) && (not <| String.isEmpty model.height))
   (Height s)        -> 
    ({model | height = String.toInt s}, Cmd.none)

subscriptions _ = Sub.none


------------------------------------------------
----         SVG ----------------
--========================----------
mySvg : Html Msg 
mySvg = 
   S.svg 
   [SA.version "1.1"
   ,SA.width "300"
   ,SA.height "200"
   ,SA.xlinkHref "http://www.w3.org/2000/svg"
   ] 
   [S.rect 
    [SA.width "100%"
    ,SA.height "100%"
    ,SA.fill "red"
    ] 
    [
    ]
   ,S.circle 
    [SA.cx "150"
    ,SA.cy "100"
    ,SA.r "80"
    ,SA.fill "green"
    ]
    []
   ,S.text_ 
    [SA.x "150"
    ,SA.y "125"
    ,SA.fontSize "60"
    ,SA.textAnchor "middle"
    ,SA.fill "white"
    ] 
    [
     S.text "SVG"
    ]
   ]