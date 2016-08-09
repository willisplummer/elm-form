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


initialModel : Model
initialModel =
    { signUp = Signup.init
    , stopsList = StopsList.initialModel
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
    = StopsList StopsList.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StopsList msg ->
            ( { model | stopsList = StopsList.update msg model.stopsList }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    App.map (\msg -> StopsList msg) (StopsList.view model.stopsList)
