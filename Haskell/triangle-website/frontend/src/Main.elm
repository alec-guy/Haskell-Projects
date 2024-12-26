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
radioSelection : Model -> Html Msg 
radioSelection model = 
      Html.form []
     [ Html.label 
         [] 
         [
             input 
               [ Html.Attributes.type_ "radio"
               , Html.Attributes.name "base-and-height"
               , checked <| model.displayBaseAndHeight
               , onClick <| BaseAndHeight <| not <| model.displayBaseAndHeight
               ] 
               []
         , Html.text "base and height"
         ] 
    , Html.label []
       [
           input 
            [ Html.Attributes.type_ "radio"
            , Html.Attributes.name "three-sides"
            , checked <| model.displayThreeSides
            , onClick <| ThreeSides <| not <| model.displayThreeSides 
            ] 
            []
       , Html.text "SSS"
       ]
    , Html.label [] 
       [
           input  
            [ Html.Attributes.type_ "radio"
            , Html.Attributes.name "SAS"
            , onClick <| SAS <| not <| model.displaySAS 
            , checked <| model.displaySAS
            ] 
            []
       ,Html.text "SAS"
       ]
    ]
view : Model -> Html Msg 
view model = 
   div 
   [Html.Attributes.style "text-align" "center"] 
   [div [] [radioSelection model]
   , if model.displayBaseAndHeight 
     then viewDisplayBaseAndHeight model 
     else Html.text ""
   ]
viewDisplayBaseAndHeight : Model -> Html Msg 
viewDisplayBaseAndHeight model = 
      div 
      []
      [
      Html.form [] 
      [input [onInput Base] []
      ,input [onInput Height] []
      ]
      ,svgBox <| [mkTriangleBsHeight model.base model.height]
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
mkTriangleBsHeight : Maybe Int -> Maybe Int -> Svg Msg 
mkTriangleBsHeight b h =           
         let triangle = "M 150 150 H 180 V 50 L 150 150 "
         in
         S.g
         [] 
         [ S.path
            [SA.d triangle
            ,SA.stroke "black"
            ,SA.fill "none"
            ]
            []
         , S.text_ [SA.x "150", SA.y "160"] 
           [S.text <| ("Base = " ++ ((\i -> if i == "0" then "" else i) <| String.fromInt <|  (Maybe.withDefault 0) <| b))]
         , S.text_ [SA.x "185", SA.y "100"] 
           [S.text <| ("Height = " ++ ((\i -> if i == "0" then "" else i) <| String.fromInt <|  (Maybe.withDefault 0) <| h))]
         ]
svgBox : List (Svg Msg) -> Html Msg 
svgBox list = 
   S.svg 
   [
    SA.width "300", SA.height "350"
    , SA.version "1.1"
    ,SA.xlinkHref "http://www.w3.org/2000/svg"
   ]
   list
------------------------------------------------
----         SVG ----------------
--========================----------
