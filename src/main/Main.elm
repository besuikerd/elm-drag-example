module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import Event
import Drag
import Store
import DictSet
import Debug
import Update.Extra as Update

main : Program Never
main = App.program
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  }

type alias Model =
  { offset: Int
  , daysShown: Int
  , store : Store.Model
  , drag: Drag.Model Event.Model
  , height: Int
  }

type Msg
  = NoOp
  | ChangeDay Int
  | DragMsg (Drag.Msg Event.Model)
  | StoreMsg Store.Msg


dummyEvents : List Event.Model
dummyEvents =
  [ Event.Model 0 "Event1" 3 0.2 0.3
  , Event.Model 1 "Event2" 11 0.2 0.3
  , Event.Model 2 "Event3" 12 0.2 0.3
  , Event.Model 3 "Event4" 9 0.2 0.3
  , Event.Model 4 "Event5" 7 0.2 0.3
  ]

init: (Model, Cmd Msg)
init =
  let
    (store, storeCmd) = Store.init
    events' = DictSet.union (DictSet.fromList (DictSet.ord store.events) dummyEvents) store.events
    store' = {store | events = events'}
    (drag, dragCmd) = Drag.init
  in
    ( Model
        8
        5
        store'
        drag
        800
    , Cmd.batch
        [ Cmd.map StoreMsg storeCmd
        , Cmd.map DragMsg dragCmd
        ]
    )

subscriptions : Model -> Sub Msg
subscriptions model =
  let
    dragSubscriptions = Drag.subscriptions model.drag
  in
    Sub.batch
      [ Sub.map DragMsg dragSubscriptions
      ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)
    ChangeDay day ->

      case Drag.draggedItem model.drag of
        Just event ->
          let
            event' = { event | day = day }
            drag' = Drag.updateItem model.drag event'
          in
            ({ model | drag = drag' }, Cmd.none)
              |> Update.andThen update (StoreMsg <| Store.UpdateEvent event')
        Nothing ->
          (model, Cmd.none)
    DragMsg msg ->
      let
        (drag', cmd', event) = Drag.update msg model.drag
        model' = { model | drag = drag' }
        (model'', cmd'') = onDragEvent event model'
      in
        (model'', Cmd.batch [Cmd.map DragMsg cmd', cmd''])
    StoreMsg msg ->
      let
        (store', cmd') = Store.update msg model.store
      in
        { model | store = store' } ! [Cmd.map StoreMsg cmd']


onDragEvent : Drag.Event Event.Model -> Model -> (Model, Cmd Msg)
onDragEvent event model =
  case event of
    Drag.OnMove position event ->
      let
        offset' = Drag.calculateOffsetWithinBounds model.drag event.offset model.height event.amount
        event' = { event | offset = offset' }
        (store', storeMsg) = Store.update (Store.UpdateEvent event') model.store
      in
        {model | store = store' } ! [Cmd.map StoreMsg storeMsg]
    _ -> model ! []

view : Model -> Html Msg
view model =
  let
    eventsAtOffset offset = DictSet.values <| DictSet.filter (.day >> (==) offset) model.store.events
    dayRange = [model.offset .. model.offset + model.daysShown]
  in
    div
      [ class "schedule"
      , style
          [ ("height", toString model.height ++ "px")
          , ("webkitUserSelect", "none")
          , ("webkitTouchCallout", "none")
          , ("kHtmlUserSelect", "none")
          , ("mozUserSelect", "none")
          , ("mskitUserSelect", "none")
          , ("userSelect", "none")
          ]
      , attribute "unselectable" "on"
      ]
      (
        List.map
          (\offset -> viewDay offset (eventsAtOffset offset))
          dayRange
      )


viewDay : Int -> List Event.Model -> Html Msg
viewDay offset events =
  div
    [ class "day"
    , style
        [ ("position", "relative")
        , ("float", "left")
        , ("width", "200px")
        , ("height", "100%")
        , ("marginRight", "10px")
        , ("backgroundColor", "red")
        ]
    , onMouseEnter <| ChangeDay offset
    ]
    (List.map eventContainer events)

eventContainer : Event.Model -> Html Msg
eventContainer event =
  div
    [ class "event"
    , style
        [ ("position", "absolute")
        , ("top", toString (event.offset * 100) ++ "%")
        , ("height", toString (event.amount * 100) ++ "%")
        , ("width", "100%")
        , ("backgroundColor", "blue")
        ]
    , onMouseDown (Drag.start DragMsg event)
    ]
    [Event.view event]
