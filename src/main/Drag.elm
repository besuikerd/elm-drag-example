module Drag exposing (..)

import Mouse

type alias Model item =
  { dragState : DragState item
  , mousePosition : Mouse.Position
  }

type alias DragState item = Maybe
  { item : item
  , startPosition : Mouse.Position
  }

type Msg item
  = DragStart item
  | Move Mouse.Position
  | DragEnd

type Event item
  = NoOp
  | OnDragStart
  | OnMove Mouse.Position item
  | OnDragEnd

init : (Model item, Cmd (Msg item))
init = (
  Model
    Nothing
    (Mouse.Position 0 0)
  , Cmd.none
  )

subscriptions : Model item -> Sub (Msg item)
subscriptions model =
  let
    ups = Maybe.withDefault
      Sub.none
      (Maybe.map (always (Mouse.ups <| always DragEnd)) model.dragState)
  in
    Sub.batch
      [ ups
      , Mouse.moves Move
      ]


update :  Msg item -> Model item -> (Model item, Cmd (Msg item), Event item)
update msg model =
  case msg of
    DragStart item ->
      let
        dragState = Just { item = item, startPosition = model.mousePosition }
      in
        ( { model | dragState = dragState }
        , Cmd.none
        , OnDragStart
        )
    Move position ->
      let
        event = case model.dragState of
          Just { item } ->
            OnMove position item
          Nothing ->
            NoOp
      in
        ( { model | mousePosition = position }
        , Cmd.none
        , event
        )
    DragEnd ->
        ( { model | dragState = Nothing }
        , Cmd.none
        , OnDragEnd
        )

start : (Msg item -> msg) -> item -> msg
start tagger item = DragStart item |> tagger

draggedItem : Model item -> Maybe item
draggedItem model = Maybe.map .item model.dragState

updateItem : Model item -> item -> Model item
updateItem model item =
  let dragState' =
    Maybe.map (\dragState -> {dragState | item = item}) model.dragState
  in
    { model | dragState = dragState' }


type alias StartOffset = Float
type alias ContainerSize = Int
type alias ElementSize = Float

calculateOffsetWithinBounds : Model item -> StartOffset -> ContainerSize -> ElementSize -> Float
calculateOffsetWithinBounds { dragState, mousePosition }  startOffset containerSize elementSize =
  case dragState of
    Just dragState ->
      let
        dy =  mousePosition.y - dragState.startPosition.y
        diffY = (toFloat dy) / (toFloat containerSize)
        offset' = startOffset + diffY
      in
        Basics.max 0 (Basics.min  (1 - elementSize) offset')
    Nothing ->
      0
