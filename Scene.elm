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

init = {
   currentAction = Look
   , currentLocation = apartment
   , otherLocations = [apartmentStreet, rcStreet, rcWorkshop]
   , infoText = "You wake up all alone, and all your friends are dead. Welcome to the game!"
   , inventory = []
   }

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

type InventoryItem
    = Diary
    | Keyfob
    | Crowbar

type EntityKind
    = Simple
    | Portal Location
    -- Should be InventoryItem
    | Item InventoryItem
    | Replaceable
        { replacedWith: Entity
        , requiredItem: InventoryItem }

type alias Entity =
    { hitbox : Rect
    , description : String
    , imagePath : Maybe String
    , kind : EntityKind
    }

portalTo : Location -> { description: String, hitbox: Rect } -> Entity
portalTo location entity =
    { kind = Portal location
    , description = entity.description
    , hitbox = entity.hitbox
    , imagePath = Nothing
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

replaceEntity : Entity -> Entity -> LocationProperties -> LocationProperties
replaceEntity entity newEntity location =
    { location | entities = (newEntity :: location.entities) |> List.filter (\e -> e /= entity) }

removeItem : InventoryItem -> List InventoryItem -> List InventoryItem
removeItem item list = list |> List.filter (\i -> i /= item)

useItem : InventoryItem -> Entity -> Model -> Model
useItem item entity ({inventory, currentLocation} as model) =
    let
        doesntDoAnything =
            { model
            | infoText = "It doesn't do anything."
            , currentAction = Look
        }
    in
    case entity.kind of
        Replaceable {replacedWith, requiredItem} ->
            if item == requiredItem then
                { model
                | inventory = removeItem item model.inventory
                , currentLocation = replaceEntity entity replacedWith currentLocation
                , currentAction = Look
                }

            else
                doesntDoAnything
        _ ->
            doesntDoAnything

-- Items: Key Fob(Locks access to RC), Diary(Required to use the computer)
-- Portal to the street
apartment =
    { location = Apartment
    , imagePath = "apartment.jpg"
    , entities =
        [
            portalTo ApartmentStreet
                { hitbox = { x = 245, y = 0, width = 225, height = 475 }
                , description = "A door that leads into the street."
                }

            , { kind = Item Diary
            , hitbox = { x = 641, y = 879, width = 187, height = 137 }
            , description = "A Diary."
            , imagePath = Just "items/diary.png"
            }

            , { kind = Item Keyfob
            , hitbox = { x = 500, y = 900, width = 50, height = 50 }
            , description = "A grey plastic device attached to a keyring."
            , imagePath = Nothing
            }
        ]
    }

-- Portals: Apartment
apartmentStreet =
    { location = ApartmentStreet
    , imagePath = "apartment_street.jpg"
    , entities =
        [
            portalTo Apartment
                { hitbox = { x = 0, y = 0, width = 100, height = 1080 }
                , description = "The door into your apartment."
                }

            , portalTo RCStreet
                { hitbox = { x = 980, y = 0, width = 100, height = 1080 }
                , description = "A street that leads away from your apartment."
                }
        ]
    }

planks =
    { kind = Replaceable
        { replacedWith = lockedRCDoor
        -- Declare these as types
        , requiredItem = Crowbar
        }

    , hitbox = { x = 980, y = 0, width = 100, height = 1080 }
    , description = "Some loose planks covering a door."
    , imagePath = Nothing
    }

lockedRCDoor =
    { kind = Replaceable
        { replacedWith = portalIntoRC
        , requiredItem = Keyfob
        }
    , hitbox = { x = 1000, y = 0, width = 80, height = 1080 }
    , description = "A locked door."
    , imagePath = Nothing
    }

portalIntoRC =
    portalTo RCWorkshop
        { hitbox = { x = 1020, y = 0, width = 60, height = 1080 }
        , description = "An open Door."
        }

-- Simple: RCEntrance (Replaced with a portal when used with key fob)
rcStreet =
    { location = RCStreet
    , imagePath = "rc_street.jpg"
    , entities =
        [
            { kind = Item Crowbar
            , hitbox = { x = 800, y = 880, width = 50, height = 100 }
            , description = "A well blacksmithed sturdy steel crowbar."
            , imagePath = Nothing
            }

            , planks

            , portalTo ApartmentStreet
                { hitbox = { x = 0, y = 0, width = 100, height = 1080 }
                , description = "A street that leads back towards your apartment."
                }
        ]
    }

computer =
    { kind = Simple
    , hitbox = { x = 730, y = 568, width = 306, height = 312 }
    , description = "Access to the computer is locked."
    , imagePath = Nothing
    }

lockedComputer =
    { kind = Replaceable
        { replacedWith = computer
        , requiredItem = Diary
        }
    , hitbox = { x = 730, y = 568, width = 306, height = 312 }
    , description = "The computer is unlocked."
    , imagePath = Just "items/lockscreen.png"
    }

-- Simple: Computer
rcWorkshop =
    { location = RCWorkshop
    , imagePath = "rc_workshop.jpg"
    , entities =
        [ lockedComputer ]
    }

type Action
   = Look
   | Move
   | Take
   | Use InventoryItem

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
                        Item item ->
                            { model
                                | inventory = (item :: model.inventory)
                                , currentLocation = takeItemFromLocation entity model.currentLocation
                                , infoText = ("You have acquired " ++ (toString item) ++ "!")
                            }
                        _ ->
                            { model | infoText = "You can't take that." }

                Move ->
                    case entity.kind of
                        Portal location ->
                            changeLocation location model
                        _ ->
                            { model | infoText = "You can't walk there." }

                Use item ->
                    useItem item entity model

-- View

renderActionButton : Action -> Action -> Html Msg
renderActionButton currentAction a =
    let
        classes = HA.classList [ ("selected", a == currentAction) ]
    in
        button [ onClick (ChangeAction a), classes ] [ text (toString a) ]

renderInventoryItem : Action -> InventoryItem -> Html Msg
renderInventoryItem action item =
    let
        itemButton = button [ onClick (ChangeAction (Use item)) ] [ text (toString item) ]
        cssClasses = "inventoryitem" ::
            case action of
                Use selectedItem -> (if item == selectedItem then ["selected"] else [])
                _ -> []
    in
    div [ class (cssClasses |> String.join " ") ] [ itemButton ]

-- Returns a CSS class that represents the current action
actionClass : Action -> String
actionClass action =
    "action-" ++ case action of
        Use _ -> "use"
        _ -> toString action |> String.toLower


view : Model -> Html Msg
view ({inventory, currentAction, infoText} as model) =
    let
        actionButtons = List.map (renderActionButton currentAction) [Look, Move, Take]
        inventoryItems =
            if List.isEmpty inventory then
                [ div [ class "inventoryempty" ] [ text "(empty)" ] ]
            else
                inventory |> List.map (renderInventoryItem currentAction)
        entityRects = List.map svgViewEntity model.currentLocation.entities
        sceneView =
            g [] ([ image [ xlinkHref ("img/scenes/" ++ model.currentLocation.imagePath), x "0", y "0", height "1080", width "1080" ] [] ] ++ entityRects)
        actionPane =
            div [ id "left" ]
                [ div [ class "menutitle" ] [ text "Actions" ]
                , div [ id "actionbuttons" ] actionButtons
                , div [ class "infotext" ] [ text infoText ]
                ]
        mainPane =
            div [ id "middle", HA.class (actionClass currentAction) ]
                [ svg [ viewBox "0 0 1080 1080" ] [ sceneView ] ]
        inventoryPane =
            div [ id "right" ]
                [ div [ class "menutitle" ] [ text "Inventory" ]
                , div [ class "inventory" ] inventoryItems
                ]
    in
       div [ HA.id "container" ] [ actionPane, mainPane, inventoryPane ]


svgViewEntity : Entity -> Svg Msg
svgViewEntity ({hitbox, imagePath} as entity) =
    let
        attributes =
            [ x (toString hitbox.x)
            , y (toString hitbox.y)
            , height (toString hitbox.height)
            , width (toString hitbox.width)
            , SA.class ([ "entity", (toString entity.kind) |> String.toLower, "debug" ] |> String.join " ")
            , onClick (ExecuteAction entity)
            ]
    in
       case imagePath of
           Just path ->
               image (xlinkHref ("img/" ++ path) :: attributes) []
           Nothing ->
               rect attributes []
