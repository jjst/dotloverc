module Scene exposing (..)

import String
import Html exposing (Html, div, button, img)
import Html.Attributes as HA
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
type EntityKind = Simple | Location | Item { name: String }

type alias Entity = {
    hitbox : Rect,
    description : String,
    kind : EntityKind
}

type alias Model =
    { currentAction: Action
    , entities: List Entity
    , infoText : String
    , inventory: List Item
    }

type alias Item = String

streetEntities =
    [ { hitbox = { x = 0, y = 0, width = 300, height = 300 }, description = "apartment", kind = Location }
    , { hitbox = { x = 400, y = 0, width = 50, height = 50 }, description = "sky", kind = Item { name = "sky" } }
    ]

init = {
   currentAction = Look
   , entities = streetEntities
   , infoText = ""
   , inventory = []
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
    | ExecuteAction Entity


update : Msg -> Model -> Model
update message model =
    case message of
        ChangeAction action ->
            { model | currentAction = action }

        ExecuteAction entity ->
            case model.currentAction of
                Look ->
                    { model | infoText = entity.description }

                Take ->
                    case entity.kind of
                        Item { name } ->
                            { model
                                | inventory = (name :: model.inventory)
                                , entities = List.filter (\e -> e /= entity) model.entities
                                , infoText = ("You have acquired " ++ name ++ "!")
                            }
                        _ ->
                            { model | infoText = "You can't take that." }
                _ ->
                    model

-- View

renderButton : Action -> Action -> Html Msg
renderButton currentAction a =
    let
        classes = HA.classList [ ("selected", a == currentAction) ]
    in
        button [ onClick (ChangeAction a), classes ] [ text (toString a) ]

view : Model -> Html Msg
view ({inventory, currentAction, infoText} as model) =
    let
        cursor =
            case currentAction of
                Move -> "s-resize"
                Take -> "grab"
                Look -> "zoom-in"
        buttons = List.map (renderButton currentAction) [Look, Move, Take]
        actionPane =
            div [ id "left" ]
                [ div [ class "menutitle" ] [ text "Actions" ]
                , div [ id "actionbuttons" ] buttons
                , div [ class "infoText" ] [ text infoText ]
                ]
        mainPane =
            div [ id "middle", HA.style [("cursor", cursor)] ]
                [ ]
        inventoryPane =
            div [ id "right" ]
                [ div [ class "menutitle" ] [ text "Inventory" ]
                ]
    in
       div [ HA.id "container" ] [ actionPane, mainPane, inventoryPane ]
   {--
        div [  ] [ svg [viewBox "0 0 800 600", width "800px"] [(svgView model)]
               , div [] [ text ("Inventory: " ++ (if List.isEmpty inventory then "(empty)" else (String.join " ⚫ " inventory))) ]
               ]
               --}


svgViewEntity : Entity -> Svg Msg
svgViewEntity ({hitbox} as e) =
    let
        x_ = toString hitbox.x
        y_ = toString hitbox.y
        w = toString hitbox.width
        h = toString hitbox.height
    in
    rect [ x x_, y y_, height h, width w, SA.class "entity debug", onClick (ExecuteAction e) ] []

svgView : Model -> Svg Msg
svgView model =
    let
        rects = List.map svgViewEntity model.entities
    in
        g [] ([ image [ xlinkHref "img/rc_workshop.jpg", x "0", y "0", height "768", width "1024" ] [] ] ++ rects)


