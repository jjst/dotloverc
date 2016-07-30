module Cell exposing (..)

import Html exposing (Html, div, button)
import Html.App as App
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes as SA
import Svg.Attributes exposing (..)



main =
    App.beginnerProgram
        { model = init
        , update = update
        , view = view
        }

-- Model

type alias Rect = { 
    x : Int,
    y : Int,
    width : Int,
    height : Int
}

type alias InteractiveElement = { coords: Rect, action: Action -> Model -> Model }

interactiveElements =
    [ { coords = { x = 0, y = 0, width = 300, height = 300 }, action = (\action model -> { model | infoText = "Clicked on first rec" }) }
    , { coords = { x = 400, y = 0, width = 50, height = 50 }, action = (\action model -> { model | infoText = "Clicked on second rec" }) }
    ]


init = { currentAction = Look , infoText = "" }

type alias Model = 
    { currentAction: Action 
    , infoText : String
    }

type Action
   = Look
   | Move
   | Take

--init : Model
--init = On

-- Update
type Msg
    = ChangeAction Action
    | ExecuteAction InteractiveElement


update : Msg -> Model -> Model
update message model = 
    case message of
        ChangeAction action -> { model | currentAction = action }
        ExecuteAction element -> element.action model.currentAction model


-- View

view : Model -> Html Msg
view model =
    div [] [ svg [viewBox "0 0 800 600", width "800px"] [(svgView model)]
           , div [] 
               [ text (model.infoText ++ " --  Current action is " ++ (toString model.currentAction))
               , button [ onClick (ChangeAction Look) ] [ text "Look!" ]
               , button [ onClick (ChangeAction Move) ] [ text "Move" ]
               , button [ onClick (ChangeAction Take) ] [ text "Take" ]
               ]
           ]


svgViewInteractiveElement : InteractiveElement -> Svg Msg
svgViewInteractiveElement ({coords, action} as e) = 
    let
        x_ = toString coords.x
        y_ = toString coords.y
        h = toString coords.height
        w = toString coords.width
    in
    rect [ x x_, y y_, height h, width w, SA.style "fill:red", onClick (ExecuteAction e) ] []

svgView : Model -> Svg Msg
svgView model = 
    let
        rects = List.map svgViewInteractiveElement interactiveElements
    in
        g [] ([ image [ xlinkHref "img/street_small.jpg", x "0", y "0", height "416", width "800" ] [] ] ++ rects)


