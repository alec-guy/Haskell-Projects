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
            , sss1       = Nothing 
            , sss2       = Nothing 
            , sss3       = Nothing 
            }
init _    = (initModel ,Cmd.none)
type Msg = BaseAndHeight Bool 
         | ThreeSides Bool 
         | SAS Bool 
         | Base String
         | Height String 
         | SSS1 String 
         | SSS2 String 
         | SSS3 String

type alias Model = { displayBaseAndHeight : Bool 
                   , displayThreeSides    : Bool 
                   , displaySAS           : Bool 
                   , base                 : Maybe Int 
                   , height               : Maybe Int 
                   , sss1                 : Maybe Int 
                   , sss2                 : Maybe Int 
                   , sss3                 : Maybe Int
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
     else if model.displayThreeSides 
          then viewDisplayThreeSides model 
          else Html.text ""
   ]
viewDisplayBaseAndHeight : Model -> Html Msg 
viewDisplayBaseAndHeight model = 
      div 
      []
      [
      Html.form [] 
      [ label [] [input [onInput Base] [], Html.text "Enter Base"]
      , label [] [input [onInput Height] [], Html.text "Enter Height"]
      ]
      ,svgBox <| [mkTriangleBsHeight model.base model.height]
      ]
viewDisplayThreeSides : Model -> Html Msg 
viewDisplayThreeSides model = 
           div 
           [Html.Attributes.style "text-align" "center"] 
           [
            Html.form [] 
            [ label [] [input [onInput SSS1] [], Html.text "Enter side a"] 
            , label [] [input [onInput SSS2] [], Html.text "Enter side b"] 
            , label [] [input [onInput SSS3] [], Html.text "Enter side c"]
            , svgBox [mkTriangleSSS model]
            ]
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
   (SSS1 s )         -> 
      ({model | sss1 = String.toInt s}, Cmd.none)
   (SSS2 s )         -> 
      ({model | sss2 = String.toInt s}, Cmd.none)
   (SSS3 s )         -> 
      ({model | sss3 = String.toInt s}, Cmd.none)

subscriptions _ = Sub.none

mkTriangleSSS : Model -> Svg Msg 
mkTriangleSSS model = 
     let triangle = "M 150 150 H 180 M 170 50 L 150 150 M 170 50 L 180 150 "
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
        [S.text <| ("a = " ++ ((\i -> if i == "0" then "" else i) <| String.fromInt <|  (Maybe.withDefault 0) <| model.sss1))]
      , S.text_ 
        [SA.x "185", SA.y "100"] 
        [S.text <| ("b = " ++ ((\i -> if i == "0" then "" else i) <| String.fromInt <|  (Maybe.withDefault 0) <| model.sss2))]
      , S.text_
        [SA.x "50", SA.y "100"] 
        [S.text <| ("c = " ++ ((\i -> if i == "0" then "" else i) <| String.fromInt <|  (Maybe.withDefault 0) <| model.sss3))]
      ]
      
mkTriangleBsHeight : Maybe Int -> Maybe Int -> Svg Msg 
mkTriangleBsHeight b h  =           
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
         , let maybeArea = case (b, h) of 
                            (Just b1, Just h2) -> 
                                 S.text_ [SA.x "150", SA.y "200"] 
                                 [S.text <| ("Area = " ++ (String.fromFloat <| (toFloat b1) * (toFloat h2) * (1 / 2)))]
                            _ -> S.text_ [] []
           in maybeArea

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
