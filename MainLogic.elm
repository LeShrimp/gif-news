module MainLogic
  ( init
  , view
  , update
  ) where

import MiniScraper exposing (scrapeTexts)
import Giphy exposing (getTranslateGif)

import Effects exposing (Never, Effects, task)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task exposing (..)
import String exposing (words, join)
import Char exposing (..)

type alias Model =
  { headlines : List String
  , index : Int
  -- Matching phrase to image url
  , gifUrls : List (String, String)
  }

type Action
  = NewHeadlines (Maybe (List String))
  | NextGifUrl (String, Maybe String) String
  | NextHeadline

fetchHeadlines : Effects Action
fetchHeadlines =
  scrapeTexts "http://edition.cnn.com/" ".cd__headline .cd__headline-text"
    |> Task.toMaybe
    |> Task.map NewHeadlines
    |> Effects.task


fetchNextGifUrl : String -> Effects Action
fetchNextGifUrl remainigHeadline =
  let
    firstTwoWords =
      String.join " " (List.take 2 (words remainigHeadline))

    remainigPhrase =
      String.join " " (List.drop 2 (words remainigHeadline))
  in
    getTranslateGif firstTwoWords
      |> Task.toMaybe
      |> Task.map (\maybeUrl -> NextGifUrl (firstTwoWords, maybeUrl) remainigPhrase)
      |> Effects.task

init : (Model, Effects Action)
init =
  ( Model [] 0 []
  , fetchHeadlines
  )

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NewHeadlines maybeHeadlines ->
      let
        newModel =
          {model | headlines = (Maybe.withDefault model.headlines maybeHeadlines), index = 0}
      in
        ( newModel
        , fetchNextGifUrl (cleanHeadline <| currentHeadline newModel)
        )

    NextGifUrl (phrase, maybeUrl) remainingHeadline ->
      let
        nextEffect =
          if remainingHeadline == "" then
            Effects.none
          else
            fetchNextGifUrl remainingHeadline

      in
        case maybeUrl of
          Just url ->
            ( { model | gifUrls = model.gifUrls `List.append` [(phrase, url)] }
            , nextEffect
            )

          Nothing ->
            ( { model | gifUrls = model.gifUrls `List.append` [(phrase, "")] }
            , nextEffect
            )

    NextHeadline ->
      let
        newModel =
          {model | index = (model.index + 1) % (List.length model.headlines), gifUrls = []}
      in
        ( newModel
        , fetchNextGifUrl (cleanHeadline <| currentHeadline newModel)
        )

cleanHeadline : String -> String
cleanHeadline headline =
  words headline
    |> List.map String.toLower
    |> List.map (String.filter Char.isLower)
    |> List.filter (\s -> String.length s > 3)
    |> String.join " "


currentHeadline : Model -> String
currentHeadline model =
  List.drop model.index model.headlines
  |> List.head
  |> Maybe.withDefault ""


view : Signal.Address Action -> Model -> Html
view address model =
  let
    renderImages =
      List.map (\(s, url) -> img [src url] []) model.gifUrls
  in
    div [onClick address NextHeadline]
      [ h1 [] [text (currentHeadline model)]
      , div [] renderImages
      ]
