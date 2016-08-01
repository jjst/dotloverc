module Scene exposing (..)

import List
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

type EntityKind = Simple | Portal Location | Item { name: String }

type alias Entity = {
    hitbox : Rect,
    description : String,
    kind : EntityKind
}

type Location
    = Apartment
    | ApartmentStreet
    | RCStreet
    | RCWorkshop

type alias LocationProperties =
    { imagePath : String
    , entities : List Entity
    , location : Location
    }

-- Entity should be constrained to kind == Item
takeItemFromLocation : Entity -> LocationProperties -> LocationProperties
takeItemFromLocation entity props =
    { props | entities = List.filter (\e -> e /= entity) props.entities }

type alias Model =
    { currentAction: Action
    , currentLocation: LocationProperties
    , otherLocations: List LocationProperties
    , infoText : String
    , inventory: List InventoryItem
    }

changeLocation : Location -> Model -> Model
changeLocation location ({currentLocation, otherLocations} as model) =
    case otherLocations |> List.filter (\e -> e.location == location) |> List.head of
        Just nextLocation ->
            { model
                | currentLocation = nextLocation
                , otherLocations = (currentLocation :: otherLocations) |> List.filter (\e -> e.location /= location)
            }
        Nothing -> { model | infoText = "Developer Error: portal to unknown location => " ++ (toString location) }

type alias InventoryItem = String

-- Items: Key Fob(Locks access to RC), Journal(Required to use the computer)
-- Portal to the street
apartment =
    { location = Apartment
    , imagePath = "img/apartment.jpg"
    , entities =
        [
            { kind = Portal ApartmentStreet
            , hitbox = { x = 0, y = 0, width = 100, height = 1080 }
            , description = "A door that leads into the street."
            }

            , { kind = Item { name = "journal" }
            , hitbox = { x = 400, y = 800, width = 50, height = 50 }
            , description = "A Journal."
            }

            , { kind = Item { name = "keyfob" }
            , hitbox = { x = 500, y = 900, width = 50, height = 50 }
            , description = "A grey plastic device attached to a keyring."
            }
        ]
    }

-- Portals: Apartment
apartmentStreet =
    { location = ApartmentStreet
    , imagePath = "img/apartment_street.jpg"
    , entities =
        [
            { kind = Portal Apartment
            , hitbox = { x = 0, y = 0, width = 100, height = 1080 }
            , description = "The door into your apartment."
            }

            , { kind = Portal RCStreet
            , hitbox = { x = 980, y = 0, width = 100, height = 1080 }
            , description = "A street that leads away from your apartment."
            }
        ]
    }

-- Simple: RCEntrance (Replaced with a portal when used with key fob)
rcStreet =
    { location = RCStreet
    , imagePath = "img/rc_street.jpg"
    , entities =
        [
            { kind = Item { name = "planks" }
            , hitbox = { x = 980, y = 0, width = 100, height = 1080 }
            , description = "Some loose planks covering a door."
            }

            , { kind = Portal ApartmentStreet
            , hitbox = { x = 0, y = 0, width = 100, height = 1080 }
            , description = "A street that leads back towards your apartment."
            }
        ]
    }

-- Simple: Computer
rcWorkshop =
    { location = RCWorkshop
    , imagePath = "img/rc_workshop.jpg"
    , entities =
        [
        ]
    }


init = {
   currentAction = Look
   , currentLocation = apartment
   , otherLocations = [apartmentStreet, rcStreet, rcWorkshop]
   , infoText = "You wake up all alone, and all your friends are dead. Welcome to the game!"
   , inventory = [ "A banana", "5 dollars" ]
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
                                , currentLocation = takeItemFromLocation entity model.currentLocation
                                , infoText = ("You have acquired " ++ name ++ "!")
                            }
                        _ ->
                            { model | infoText = "You can't take that." }
                Move ->
                    case entity.kind of
                        Portal location ->
                            changeLocation location model
                        _ ->
                            { model | infoText = "You can't walk there." }

-- View

renderActionButton : Action -> Action -> Html Msg
renderActionButton currentAction a =
    let
        classes = HA.classList [ ("selected", a == currentAction) ]
    in
        button [ onClick (ChangeAction a), classes ] [ text (toString a) ]

renderInventoryItem : InventoryItem -> Html Msg
renderInventoryItem item =
    div [ class "inventoryitem" ] [ button [] [ text item ] ]


view : Model -> Html Msg
view ({inventory, currentAction, infoText} as model) =
    let
        cursor =
            case currentAction of
                Move -> "s-resize"
                Take -> "grab"
                Look -> "zoom-in"
        actionButtons = List.map (renderActionButton currentAction) [Look, Move, Take]
        inventoryItems =
            if List.isEmpty inventory then
               [ div [ class "inventoryempty" ] [ text "(empty)" ] ]
            else
               List.map renderInventoryItem inventory
        entityRects = List.map svgViewEntity model.currentLocation.entities
        sceneView =
            g [] ([ image [ xlinkHref model.currentLocation.imagePath, x "0", y "0", height "1080", width "1080" ] [] ] ++ entityRects)
        actionPane =
            div [ id "left" ]
                [ div [ class "menutitle" ] [ text "Actions" ]
                , div [ id "actionbuttons" ] actionButtons
                , div [ class "infotext" ] [ text infoText ]
                ]
        mainPane =
            div [ id "middle", HA.style [("cursor", cursor)] ]
                [ svg [ viewBox "0 0 1080 1080" ] [ sceneView ] ]
        inventoryPane =
            div [ id "right" ]
                [ div [ class "menutitle" ] [ text "Inventory" ]
                , div [ class "inventory" ] inventoryItems
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
