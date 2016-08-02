module Main exposing (..)

import Html exposing (..)
import Html.App as App exposing (..)
import Html.Events exposing (onClick)
import Signup
import Routes
import Stop


--


main : Program Never
main =
    App.program { init = init, subscriptions = subscriptions, view = view, update = update }



--


type alias Model =
    { signUp : Signup.Model
    , stops : List IndexedStop
    , stopsUid : Int
    , page : Routes.Model
    }


type alias IndexedStop =
    { id : Int
    , model : Stop.Model
    }


type Page
    = SignUpPage
    | LoginPage
    | HomePage



--


initialModel : Model
initialModel =
    { signUp = Signup.init
    , stops = []
    , stopsUid = 0
    , page = Routes.model
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel
    , Cmd.none
    )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--


type Msg
    = Modify Int Stop.Msg
    | Insert
    | Remove


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Modify id msg ->
            ( { model | stops = List.map (updateHelp id msg) model.stops }
            , Cmd.none
            )

        Insert ->
            ( { model
                | stops = model.stops ++ [ IndexedStop model.stopsUid Stop.initialModel ]
                , stopsUid = model.stopsUid + 1
              }
            , Cmd.none
            )

        Remove ->
            ( { model
                | stops = List.drop 1 model.stops
              }
            , Cmd.none
            )


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

        add =
            button [ onClick Insert ] [ text "Add" ]

        remove =
            button [ onClick Remove ] [ text "Remove" ]
    in
        div []
            [ add
            , remove
            , div
                []
                stops
            ]


viewIndexedStop : IndexedStop -> Html Msg
viewIndexedStop { id, model } =
    App.map (Modify id) (Stop.view model)
