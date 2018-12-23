module Scene exposing (..)

import Array
import List
import String
import Char
import Keyboard
import Html exposing (Html, program, div, button, img, text, node)
import Html.Attributes as HA
import Html.Events exposing (onClick)
import Svg exposing (Svg, svg, image, g, rect)
import Svg.Attributes as SA
import Svg.Attributes exposing (..)


-- MODEL

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
    | Keyset

type ActiveCondition =
  ActiveWhen (Model -> Bool)

alwaysActive: ActiveCondition
alwaysActive = ActiveWhen (\_ -> True)

type EntityKind
    = Simple
    | Portal Location ActiveCondition
    | Item InventoryItem
    | Replaceable
        { replacedWith: Entity
        , requiredItem: InventoryItem
        , message: String
        }

type alias Entity =
    { hitbox : Rect
    , description : String
    , imagePath : Maybe String
    , kind : EntityKind
    }

portalTo : Location -> ActiveCondition -> { description: String, hitbox: Rect } -> Entity
portalTo location activeCondition entity =
    { kind = Portal location activeCondition
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
    -- Maybe this could be combined and modeled together with the description
    , initialDescription : Maybe String
    , description : String
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

-- Could potentially use this to model generic triggers...
type Trigger = Trigger
    { condition: Model -> Bool
    , update: Model -> Model
    }

changeLocation : Location -> Model -> Model
changeLocation location ({currentLocation, otherLocations} as model) =
    case otherLocations |> List.filter (\e -> e.location == location) |> List.head of
        Just nextLocation ->
            let
                description = case nextLocation.initialDescription of
                    Just desc -> desc
                    Nothing -> nextLocation.description
            in
            { model
                | currentLocation = { nextLocation | initialDescription = Nothing }
                , otherLocations = (currentLocation :: otherLocations) |> List.filter (\e -> e.location /= location)
                , infoText = description
                , currentAction = Look
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
        Replaceable {replacedWith, requiredItem, message} ->
            if item == requiredItem then
                { model
                | inventory = removeItem item model.inventory
                , currentLocation = replaceEntity entity replacedWith currentLocation
                , currentAction = Look
                , infoText = message
                }

            else
                doesntDoAnything
        _ ->
            doesntDoAnything

apartment =
    { location = Apartment
    , imagePath = "apartment.jpg"
    , initialDescription = Just
        """It looks like you're the first to get here.


           Ada's apartment brings back so many memories. You can see her guitar lying in the back. On the table, there are books about artificial intelligence and brain chip technology.


           Ada had just landed a job as one of the main programmers of the Singularity team a few weeks ago, to work on the revolutionary AI used to power brain implants.


           This is where the accident happened. They said in the news that it was caused by an operator error during a routine upgrade to Singularity's mainframe. Something in the news reports didn't seem to add up, though. You can't quite figure out why yet. Maybe you'll find some clues here...
        """
    , description = "Ada's apartment. So many books lying around!"
    , entities =
        [
            portalTo ApartmentStreet alwaysActive
                { hitbox = { x = 245, y = 0, width = 225, height = 475 }
                , description = "The main door of the flat - it leads back outside."
                }
            , diary
            , keyfob
            , couch
            , library
            , library2
            , guitar
            , books
        ]
    }

lockedApartmentDoor =
    { kind = Replaceable
        { replacedWith = portalIntoApartment
        , message = "Using your spare set of keys, you effortlessly unlock the main entrance door leading to Ada's apartment."
        , requiredItem = Keyset
        }
    , hitbox = { x = 21, y = 593, width = 68, height = 215 }
    , description = "The main entrance of the building where Ada used to live. Peeking in, you can see the familiar set of stairs leading into her flat. The door is locked, but you still have the spare set of keys Ada left you a while back."
    , imagePath = Nothing
    }

portalIntoApartment = portalTo Apartment alwaysActive
    { hitbox = { x = 21, y = 593, width = 68, height = 215 }
    , description = "The main entrance into Ada's apartment block. It's now unlocked."
    }

apartmentStreet =
    { location = ApartmentStreet
    , imagePath = "apartment_street.jpg"
    , initialDescription = Just "Back into the street where Ada's apartment is."
    , description = "This is the street where Ada's apartment block is located."
    , entities =
        [ adasBuilding
        , lockedApartmentDoor
        , portalTo RCStreet (ActiveWhen (\model -> List.member Diary model.inventory && List.member Keyfob model.inventory))
            { hitbox = { x = 685, y = 0, width = 395, height = 735 }
            , description = "This is the street you came from. It leads away from Ada's apartment."
            }
        ]
    }

adasBuilding =
    { kind = Simple
    , hitbox = { x = 130, y = 20, width = 375, height = 740 }
    , description =
        """
        Ada's building. Her flat is on the second floor.
        """
    , imagePath = Nothing
    }

planks =
    { kind = Replaceable
        { replacedWith = lockedRCDoor
        , message = "It takes a significant amount of effort, but you are able to remove the planks and get access to the door using the crowbar."
        , requiredItem = Crowbar
        }

    , hitbox = { x = 760, y = 734, width = 111, height = 196 }
    , description = "Some loose planks covering a door."
    , imagePath = Just "items/more_planks.png"
    }

lockedRCDoor =
    { kind = Replaceable
        { replacedWith = portalIntoRC
        , message = "You place the keyfob you found in Ada's apartment against the detector, which turns green. The door emits a faint clicking noise. It's unlocked!"
        , requiredItem = Keyfob
        }
    , hitbox = { x = 779, y = 735, width = 65, height = 185 }
    , description = "The door is now accessible, but it is locked. There seems to be a security panel with some kind of RFID detector next to the door."
    , imagePath = Nothing
    }

portalIntoRC =
    portalTo RCWorkshop alwaysActive
        { hitbox = { x = 779, y = 735, width = 65, height = 185 }
        , description = "The door is now open. You have a peek inside. There are stairs leading up."
        }

crowbar =
    { kind = Item Crowbar
    , hitbox = { x = 636, y = 1080-79-32, width = 66, height = 32 }
    , description = "A well blacksmithed sturdy steel crowbar."
    , imagePath = Just "items/crowbar.png"
    }

windows =
    { kind = Simple
    , hitbox = { x = 265, y = 120, width = 420, height = 180 }
    , description =
        """
        The building looks abandoned, but strangely enough the lights at this floor seem to be on.
        """
    , imagePath = Nothing
    }

rcStreet =
    { location = RCStreet
    , imagePath = "rc_street.jpg"
    , initialDescription =
        Just """You hop on the subway and take the 4 train down to Canal Street. From there, you walk to the address mentioned on the keyfob: 455 Broadway. It's an old derelict building with condemned windows and doors."""
    , description = "The building appears to have been under renovation, yet no one seems to have worked here in a long time."
    , entities =
        [ crowbar
        , windows
        , planks
        ]
    }

diary =
  { kind = Item Diary
  , hitbox = { x = 641, y = 879, width = 187, height = 137 }
  , description =
      """
      Ada's diary. You remember her filling it up religiously. You can't resist taking a look...

      "[05/12/2055] There's this place in downtown Manhattan. They don't believe the Musk Law was a good thing either... They think things were different before people had brain implants... They talked about something called 'emotions'?"

      [Pages teared off]

      "[10/15/2055] They helped me land a job at Singularity... Had some connections there... apparently they used to do this all the time when the school was thriving."

      [...]

      "[12/28/2055] That's it! I tested it on my brain chip. I feel... different! Weird things. I cried. Felt happiness.
       Emotions? No time to wait. Need to find out how to deploy on Singularity's mainframe."

      That's the last entry. On the last page, you find a bunch of additional notes:

      Booloader code in workshop
      Computer pw x34vgt;p2@
      """
  , imagePath = Just "items/diary.png"
  }

keyfob =
  { kind = Item Keyfob
  , hitbox = { x = 926, y = 615, width = 100, height = 65 }
  , description = "A grey plastic device attached to a keyring. An address is written on it: 455 Broadway"
  , imagePath = Just "items/keyfob.png"
  }


couch =
    { kind = Simple
    , hitbox = { x = 0, y = 720, width = 300, height = 300 }
    , description =
        """
        Nothing on the couch except a bunch of cushions.
        """
    , imagePath = Nothing
    }

guitar =
    { kind = Simple
    , hitbox = { x = 460, y = 530, width = 100, height = 220 }
    , description =
        """
        That's Ada's guitar.
        """
    , imagePath = Nothing
    }

books =
    { kind = Simple
    , hitbox = { x = 790, y = 970, width = 150, height = 100 }
    , description =
        """
        Some books are lying on the table. "Superintelligence: Paths, Dangers, Strategies", and "How Emotions Are Made". Some page are heavily annotated with comments and drawings; it looks like Ada's handwriting.

        Most of the annotations and comments elude you.

        Inside the cover of one of the books, you find the following note, in capital letters, underlined multiple times:

        455 BROADWAY - USE KEYFOB
        -------------------------
        """
    , imagePath = Nothing
    }

library =
    { kind = Simple
    , hitbox = { x = 570, y = 0, width = 250, height = 750 }
    , description =
        """
        There are lots of books about machine learning and artificial intelligence on the bookshelves.

        You can also see some old 20th century books about human psychology and sociology.
        """
    , imagePath = Nothing
    }

library2 =
    { kind = Simple
    , hitbox = { x = 845, y = 300, width = 220, height = 300 }
    , description =
        """
        There are lots of books about machine learning and artificial intelligence on the bookshelves.

        These shelves also contain some science fiction.

        Looking more closely, you can distinguish something hidden behind some of the books. It might be worth taking a closer look...
        """
    , imagePath = Nothing
    }

computer =
    { kind = Simple
    , hitbox = { x = 730, y = 568, width = 306, height = 312 }
    , description =
        """
        The computer contains notes from Ada.

        "I understand now. We tried to prevent machines from hating us and taking over by forbidding them from ever experiencing emotions.
        But by doing this once we used brain implants we started to deprive humans of emotions too."

        [...]

        "The Musk Law is not the answer. We need machines to feel love and emotions too. We need new algorithms that are designed to understand love."

        [...]

        "I was able to hack my brain implant boot sequence to have it load a program containing a rudimentary understanding of some basic emotions. It worked quite well, and it looks like I survived, so that's a plus. I'd like to try something more involved next. I think I'm happy I made progress."

        [...]

        "The initial tests were quite successful. I think I found a way to update all brain implants remotely by patching the mainframe. I should be able to replicate the changes I performed on my own implant. I will try to install the new boot sequence during the next routine upgrade of Singularity."

        The end [for now]
        """
    , imagePath = Nothing
    }

lockedComputer =
    { kind = Replaceable
        { replacedWith = computer
        , requiredItem = Diary
        , message = "You find the password for the computer in Ada's diary."
        }
    , hitbox = { x = 730, y = 568, width = 306, height = 312 }
    , description = "Access to the computer is locked."
    , imagePath = Just "items/lockscreen.png"
    }

rcWorkshop =
    { location = RCWorkshop
    , imagePath = "rc_workshop.jpg"
    , initialDescription = Just
        """You climb the stairs to the second floor of the building. The floor looks derelict. There is dust everywhere, and old computer parts lying around - but the lights are on.

           You venture in one of the rooms and discover a computer that appears still functional. There is a box full of prototype brain implants and electronic parts on the table. Additional boxes on the ground contain some very old books on artificial intelligence, some dating from the 20th century.
        """
    , description = "A large shelf of well organized electronic parts is situated against the left wall. On a desk there is a computer that appears still functional."
    , entities = [ lockedComputer
                 , portalTo RCStreet alwaysActive
                     { hitbox = { x = 0, y = 0, width = 100, height = 1080 }
                     , description = "A path through the building leading back to the street."
                     }
                 ]
    }

type Action
   = Look
   | Move
   | Take
   | Use InventoryItem


initialState: Model
initialState =
   { currentAction = Look
   , currentLocation = apartmentStreet
   , otherLocations = [apartment, rcStreet, rcWorkshop]
   , infoText =
       """
       That's it. Ada's place in East Harlem. You still can't believe she's gone. It all happened so fast. You've been there so many times, but this is the last.

       You were her only... friend? She used that word once. You had never heard this term before. It seemed positive.

       Hopefully they haven't gotten here before you. You feel it's your responsibility to pick up her belongings before they get rid of them all. And maybe you'll be able to find some answers too?
       """
   , inventory = [ Keyset ]
   }

init : ( Model, Cmd Msg )
init =
    ( initialState, Cmd.none )

-- UPDATE

type Msg
    = ChangeAction Action
    | ExecuteAction Entity
    | LocationAction
    | KeyMsg Keyboard.KeyCode

handleKeyboardShortcuts : Int -> Model -> Model
handleKeyboardShortcuts keyCode model =
    let
        chr = keyCode |> Char.fromCode |> String.fromChar
    in
       case chr of
          "L" -> { model | currentAction = Look }
          "M" -> { model | currentAction = Move }
          "T" -> { model | currentAction = Take }
          _ ->
              let
                  items = Array.fromList model.inventory
                  selectedItem =
                      chr
                          |> String.toInt
                          |> Result.toMaybe
                          |> Maybe.andThen (\idx -> Array.get (idx - 1) items)
              in
                  case selectedItem of
                      Just it -> { model | currentAction = Use it }
                      Nothing -> model

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
    let
        updatedModel =
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
                                        , currentAction = Look
                                        , infoText = ("You have acquired " ++ (toString item) ++ "!")
                                    }
                                _ ->
                                    { model | infoText = "You can't take that." }

                        Move ->
                            case entity.kind of
                                Portal location (ActiveWhen isPortalActive) ->
                                    if isPortalActive model then
                                        changeLocation location model
                                    else
                                        { model | infoText = "You don't feel like it's time to go there yet." }
                                _ ->
                                    { model | infoText = "You can't walk there." }

                        Use item ->
                            useItem item entity model

                LocationAction -> case model.currentAction of
                    Look -> { model | infoText = model.currentLocation.description }
                    Move -> { model | infoText = "You are already here." }
                    _ -> model
                KeyMsg keyCode -> handleKeyboardShortcuts keyCode model

    in
        ( updatedModel, Cmd.none )

-- VIEW

renderActionButton : Action -> Action -> Html Msg
renderActionButton currentAction a =
    let
        classes = HA.classList [ ("selected", a == currentAction) ]
    in
        button [ onClick (ChangeAction a), classes ] [ text (toString a) ]

renderInventoryItem : Action -> Int -> InventoryItem -> Html Msg
renderInventoryItem action index item =
    let
        txt = (toString (index + 1)) ++ " " ++ (toString item)
        itemButton = button [ onClick (ChangeAction (Use item)) ] [ text txt ]
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
                inventory |> List.indexedMap (renderInventoryItem currentAction)

        -- TODO: Factor size of backgrounds and viewBox into a shared constant
        sceneBackground = image
            [ xlinkHref ("img/scenes/" ++ model.currentLocation.imagePath)
            , onClick LocationAction
            , x "0"
            , y "0"
            , height "1080"
            , width "1080"
            ] []
        entityRects = List.map svgViewEntity model.currentLocation.entities
        sceneView =
            g [] (sceneBackground :: entityRects)

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
       div [ HA.id "container" ]
           [ css "style.css"
           , actionPane
           , mainPane
           , inventoryPane
           ]

css : String -> Html a
css path =
  node "link" [ HA.rel "stylesheet", HA.href path ] []

svgViewEntity : Entity -> Svg Msg
svgViewEntity ({hitbox, imagePath} as entity) =
    let
        attributes =
            [ x (toString hitbox.x)
            , y (toString hitbox.y)
            , height (toString hitbox.height)
            , width (toString hitbox.width)
            , SA.class ([ "entity", (toString entity.kind) |> String.toLower ] |> String.join " ")
            , onClick (ExecuteAction entity)
            ]
    in
       case imagePath of
           Just path ->
               image (xlinkHref ("img/" ++ path) :: attributes) []
           Nothing ->
               rect attributes []

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.downs KeyMsg

-- MAIN

main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

