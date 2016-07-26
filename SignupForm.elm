module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Json
import Task


main : Program Never
main =
    App.program { init = init, subscriptions = subscriptions, view = view, update = update }



-- MODEL


type alias Model =
    { email : String
    , password : String
    , passwordAgain : String
    , response : String
    , errors : Errors
    , page : Page
    , hide : Bool
    }


type alias Errors =
    { email : String
    , password : String
    }


type Page
    = Login
    | Home


initialErrors : Errors
initialErrors =
    Errors "" ""


model : Model
model =
    Model "" "" "" "" initialErrors Login True


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )



-- UPDATE


type Msg
    = Email String
    | Password String
    | PasswordAgain String
    | Validate
    | SuccessToMessage String
    | FailureToMessage Http.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Email email ->
            ( { model
                | email = email
              }
            , Cmd.none
            )

        Password password ->
            ( { model
                | password = password
              }
            , Cmd.none
            )

        PasswordAgain passwordAgain ->
            ( { model
                | passwordAgain = passwordAgain
              }
            , Cmd.none
            )

        Validate ->
            validateSubmission model

        SuccessToMessage response ->
            ( { model
                | response = response
                , page = Home
              }
            , Cmd.none
            )

        FailureToMessage response ->
            ( { model
                | response = "ERROR!"
              }
            , Cmd.none
            )


validateSubmission : Model -> ( Model, Cmd Msg )
validateSubmission model =
    let
        newErrors =
            { email =
                if model.email == "" then
                    "Enter an email address"
                else
                    ""
            , password =
                if model.password == "" then
                    "Enter a password!"
                else if model.passwordAgain == "" then
                    "Please re-enter your password!"
                else if model.password /= model.passwordAgain then
                    "Passwords don't match"
                else
                    ""
            }
    in
        if newErrors.email == "" && newErrors.password == "" then
            ( { model | errors = newErrors }, submitData model )
        else
            ( { model | errors = newErrors }, Cmd.none )


areEmpty : String -> Bool
areEmpty error =
    error == ""



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


submitData : Model -> Cmd Msg
submitData model =
    let
        url =
            "http://jsonplaceholder.typicode.com/users/1"
    in
        Task.perform FailureToMessage SuccessToMessage (Http.get decodeResponse url)


decodeResponse : Json.Decoder String
decodeResponse =
    Json.at [ "email" ] Json.string



-- VIEW


view : Model -> Html Msg
view model =
    case model.page of
        Login ->
            renderLogin model

        Home ->
            renderHome model


renderLogin : Model -> Html Msg
renderLogin model =
    div []
        [ input [ type' "text", placeholder "Email", onInput Email ] []
        , div [ class "validation-error", style [ ( "color", "red" ) ] ] [ text model.errors.email ]
        , input [ type' "password", placeholder "Password", onInput Password ] []
        , input [ type' "password", placeholder "Re-enter Password", onInput PasswordAgain ] []
        , div [ class "validation-error", style [ ( "color", "red" ) ] ] [ text model.errors.password ]
        , button [ onClick Validate ] [ text "Submit" ]
        , div [ class "response" ] [ text model.response ]
        ]


renderHome : Model -> Html Msg
renderHome model =
    div []
        [ img [ src "http://i.ndtvimg.com/i/2015-07/minion-gabbar_470x471_51437144751.jpg" ] []
        , div [] [ text ("welcome home, " ++ model.email ++ ", it's been a minute.. also your password is " ++ model.password) ]
        ]
