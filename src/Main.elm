port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Navigation
import LeaderBoard exposing (..)
import Login exposing (..)
import Runner exposing (..)


-- model


type alias Model =
    { page : Page
    , leaderBoard : LeaderBoard.Model
    , login : Login.Model
    , runner : Runner.Model
    , token : Maybe String
    , loggedIn : Bool
    }


type Page
    = NotFound
    | LoginPage
    | LeaderBoardPage
    | RunnerPage


authPages : List Page
authPages =
    [ RunnerPage ]


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    let
        page =
            hashToPage location.hash

        loggedIn =
            flags.token /= Nothing

        ( updatedPage, cmd ) =
            authRedirect page loggedIn

        ( leaderBoardInitModel, leaderBoardCmd ) =
            LeaderBoard.init

        ( loginInitModel, loginCmd ) =
            Login.init

        ( runnerInitModel, runnerCmd ) =
            Runner.init

        initModel =
            { page = updatedPage
            , leaderBoard = leaderBoardInitModel
            , login = loginInitModel
            , runner = runnerInitModel
            , token = flags.token
            , loggedIn = loggedIn
            }

        cmds =
            Cmd.batch
                [ Cmd.map LeaderBoardMsg leaderBoardCmd
                , Cmd.map LoginMsg loginCmd
                , Cmd.map RunnerMsg runnerCmd
                , cmd
                ]
    in
        ( initModel, cmds )



-- update


type Msg
    = Navigate Page
    | ChangePage Page
    | LeaderBoardMsg LeaderBoard.Msg
    | LoginMsg Login.Msg
    | RunnerMsg Runner.Msg
    | Logout


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Navigate page ->
            ( { model | page = page }, Navigation.newUrl <| pageToHash page )

        ChangePage page ->
            let
                ( updatePage, cmd ) =
                    authRedirect page model.loggedIn
            in
                ( { model | page = updatePage }, cmd )

        LeaderBoardMsg msg ->
            let
                ( leaderBoardModel, cmd ) =
                    LeaderBoard.update msg model.leaderBoard
            in
                ( { model | leaderBoard = leaderBoardModel }
                , Cmd.map LeaderBoardMsg cmd
                )

        LoginMsg msg ->
            let
                ( loginModel, cmd, token ) =
                    Login.update msg model.login

                loggedIn =
                    token /= Nothing

                saveTokenCmd =
                    case token of
                        Just jwt ->
                            saveToken jwt

                        Nothing ->
                            Cmd.none
            in
                ( { model
                    | login = loginModel
                    , token = token
                    , loggedIn = loggedIn
                  }
                , Cmd.batch
                    [ Cmd.map LoginMsg cmd
                    , saveTokenCmd
                    ]
                )

        Logout ->
            ( { model
                | loggedIn = False
                , token = Nothing
              }
            , Cmd.batch
                [ deleteToken ()
                , Navigation.modifyUrl <| pageToHash LeaderBoardPage
                ]
            )

        RunnerMsg msg ->
            let
                ( runnerModel, cmd ) =
                    Runner.update (Maybe.withDefault "" model.token) msg model.runner
            in
                ( { model | runner = runnerModel }
                , Cmd.map RunnerMsg cmd
                )


authForPage : Page -> Bool -> Bool
authForPage page loggedIn =
    loggedIn || not (List.member page authPages)


authRedirect : Page -> Bool -> ( Page, Cmd Msg )
authRedirect page loggedIn =
    if authForPage page loggedIn then
        ( page, Cmd.none )
    else
        ( LoginPage, Navigation.modifyUrl <| pageToHash LoginPage )



-- view


view : Model -> Html Msg
view model =
    let
        page =
            case model.page of
                LeaderBoardPage ->
                    Html.map LeaderBoardMsg <| LeaderBoard.view model.leaderBoard

                LoginPage ->
                    Html.map LoginMsg <| Login.view model.login

                RunnerPage ->
                    Html.map RunnerMsg <| Runner.view model.runner

                NotFound ->
                    div [ class "main" ]
                        [ h1 []
                            [ text "Page Not Found!" ]
                        ]
    in
        div []
            [ pageHeader model
            , page
            ]


addRunnerView : Model -> Html Msg
addRunnerView model =
    if model.loggedIn then
        a [ onClick (Navigate RunnerPage) ] [ text "Add runner" ]
    else
        text ""


pageHeader : Model -> Html Msg
pageHeader model =
    header []
        [ a [ onClick (Navigate LeaderBoardPage) ] [ text "Race Results" ]
        , ul []
            [ li []
                [ addRunnerView model
                ]
            ]
        , ul []
            [ li []
                [ if model.loggedIn then
                    a [ onClick (Logout) ] [ text "Logout" ]
                  else
                    a [ onClick (Navigate LoginPage) ] [ text "Login" ]
                ]
            ]
        ]



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        leaderBoardSub =
            LeaderBoard.subscriptions model.leaderBoard
    in
        Sub.batch
            [ Sub.map LeaderBoardMsg leaderBoardSub ]


hashToPage : String -> Page
hashToPage hash =
    case hash of
        "#/" ->
            LeaderBoardPage

        "" ->
            LeaderBoardPage

        "#/login" ->
            LoginPage

        "#/add" ->
            RunnerPage

        _ ->
            NotFound


pageToHash : Page -> String
pageToHash page =
    case page of
        LeaderBoardPage ->
            "#/"

        LoginPage ->
            "#/login"

        RunnerPage ->
            "#/add"

        NotFound ->
            "#notfound"


locationToMsg : Navigation.Location -> Msg
locationToMsg location =
    location.hash
        |> hashToPage
        |> ChangePage


type alias Flags =
    { token : Maybe String }


main : Program Flags Model Msg
main =
    Navigation.programWithFlags locationToMsg
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


port saveToken : String -> Cmd msg


port deleteToken : () -> Cmd msg
