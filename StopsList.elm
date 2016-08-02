module StopsList exposing (..)

import Html exposing (..)
import Html.App as App
import List
import Stop


type alias Model =
    { stops : List IndexedStop
    , uid : Int
    }


type alias IndexedStop =
    { id : Int
    , model : Stop.Model
    }


initialModel : Model
initialModel =
    { stops = []
    , uid = 0
    }


type Msg
    = Add
    | Remove
    | Modify Int Stop.Msg


update : Msg -> Model -> Model
update message ({ stops, uid } as model) =
    case message of
        Add ->
            { model
                | stops = stops ++ [ IndexedStop uid Stop.initialModel ]
                , uid = uid + 1
            }

        Remove ->
            { model | stops = List.drop 1 stops }

        Modify id msg ->
            { model | stops = List.map (updateHelp id msg) stops }


updateHelp : Int -> Stop.Msg -> IndexedStop -> IndexedStop
updateHelp targetId msg { id, model } =
    IndexedStop id
        (if targetId == id then
            fst (Stop.update msg model)
         else
            model
        )


view : Model -> Html Msg
view model =
    let
        stops =
            List.map viewIndexedStop model.stops
    in
        div []
            stops


viewIndexedStop : IndexedStop -> Html Msg
viewIndexedStop { id, model } =
    App.map (Modify id) (Stop.view model)
