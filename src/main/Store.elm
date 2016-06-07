module Store exposing (..)

import DictSet exposing (DictSet)
import Event

type alias Model =
  { events: DictSet Int Event.Model
  }

type Msg =
  UpdateEvent Event.Model

init : (Model, Cmd Msg)
init =
  ( Model
      (DictSet.empty Event.ord)
  , Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateEvent event ->
      { model | events = DictSet.update event (always <| Just event) model.events } ! []
