module Event exposing (..)

import Html exposing (..)

type alias Model =
  { id: Int
  , name: String
  , day: Int
  , offset: Float
  , amount: Float
  }

ord : Model -> Int
ord = .id

view : Model -> Html a
view model =
  div
    []
    [ text model.name
    ]
