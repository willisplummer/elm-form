module Main exposing (..)

import Html exposing (..)
import Html.App as App exposing (..)
import Html.Events exposing (onClick)
import Signup
import Routes
import StopsList


--


main : Program Never
main =
    App.program { init = init, subscriptions = subscriptions, view = view, update = update }



--


type alias Model =
    { signUp : Signup.Model
    , stopsList : StopsList.Model
    , page : Routes.Model
    }


type Page
    = SignUpPage
    | LoginPage
    | HomePage



--


init : ( Model, Cmd Msg )
init =
    let
        ( list, listMsgs ) =
            StopsList.init
    in
        ( { signUp = Signup.init, stopsList = list, page = Routes.model }
        , Cmd.batch
            [ Cmd.map StopsList listMsgs
            ]
        )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--


type Msg
    = StopsList StopsList.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StopsList subMsg ->
            let
                ( list, listCmds ) =
                    StopsList.update subMsg model.stopsList
            in
                ( { model | stopsList = list }
                , Cmd.map StopsList listCmds
                )


view : Model -> Html Msg
view model =
    App.map (\msg -> StopsList msg) (StopsList.view model.stopsList)
