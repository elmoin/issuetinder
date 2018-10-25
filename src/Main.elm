module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, img, li, text, ul)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D
import Swiper exposing (SwipeEvent, SwipingState, onSwipeEvents)



---- MODEL ----


type alias Model =
    { issuesIWantToWorkOn : List Issue
    , issuesIDontWantToWorkOn : List Issue
    , currentIssue : Maybe Issue
    , issuesLeft : List Issue
    , swipingState : SwipingState
    }


init : ( Model, Cmd Msg )
init =
    ( { issuesIWantToWorkOn = []
      , issuesIDontWantToWorkOn = []
      , currentIssue = Nothing
      , issuesLeft = []
      , swipingState = Swiper.initialSwipingState
      }
    , fetchIssues
    )


fetchIssues =
    Http.get "https://api.github.com/repos/elm/compiler/issues" (D.list issueDecoder)
        |> Http.send IssuesFetched


issueDecoder =
    D.map2 Issue
        (D.field "title" D.string)
        (D.field "number" D.int)


type alias Issue =
    { title : String
    , number : Int
    }



---- UPDATE ----


type Msg
    = IssuesFetched (Result Http.Error (List Issue))
    | Swiped Issue SwipeEvent


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IssuesFetched (Ok issues) ->
            case issues of
                [] ->
                    ( { model | currentIssue = Nothing, issuesLeft = [] }, Cmd.none )

                first :: rest ->
                    ( { model | currentIssue = Just first, issuesLeft = rest }, Cmd.none )

        IssuesFetched (Err err) ->
            ( model, Cmd.none )

        Swiped issue swipeEvent ->
            let
                ( swipingState, swipedLeft ) =
                    Swiper.hasSwipedLeft swipeEvent model.swipingState

                ( _, swipedRight ) =
                    Swiper.hasSwipedRight swipeEvent model.swipingState
            in
            if swipedLeft then
                blorgh model issue (\updatedModel -> { updatedModel | issuesIWantToWorkOn = issue :: model.issuesIWantToWorkOn })

            else if swipedRight then
                blorgh model issue (\updatedModel -> { updatedModel | issuesIDontWantToWorkOn = issue :: model.issuesIDontWantToWorkOn })

            else
                ( { model | swipingState = swipingState }, Cmd.none )


blorgh model issue updateModel =
    let
        nextIssue =
            List.head model.issuesLeft
    in
    ( updateModel
        { model
            | issuesLeft = List.tail model.issuesLeft |> Maybe.withDefault []
            , currentIssue = nextIssue
        }
    , Cmd.none
    )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        issueView issue =
            div [ style "text-align" "left" ]
                [ div (onSwipeEvents <| Swiped issue) [ text <| String.fromInt issue.number ++ " - " ++ issue.title ] ]

        listView =
            div [ style "display" "flex", style "word-break" "break-all" ]
                [ div []
                    [ text "like"
                    , ul [ style "text-align" "left" ] <| List.map (\i -> li [] [ text i.title ]) model.issuesIWantToWorkOn
                    ]
                , div []
                    [ text "dislike"
                    , ul [ style "text-align" "left" ] <| List.map (\i -> li [] [ text i.title ]) model.issuesIDontWantToWorkOn
                    ]
                ]
    in
    case model.currentIssue of
        Just issue ->
            div [] [ issueView issue, listView ]

        Nothing ->
            div [] [ text "no issues left", listView ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
