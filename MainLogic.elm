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
  , gifUrls : List String
  }

type Action
  = NewHeadlines (Maybe (List String))
  | NewGifUrls (Maybe (List String))
  | Next

fetchHeadlines : Effects Action
fetchHeadlines =
  scrapeTexts "http://edition.cnn.com/" ".cd__headline .cd__headline-text"
    |> Task.toMaybe
    |> Task.map NewHeadlines
    |> Effects.task


getTranslateGifNoFail : String -> Task x String
getTranslateGifNoFail phrase =
  getTranslateGif phrase `onError` (\_ -> succeed "")

fetchGifUrls : String -> Effects Action
fetchGifUrls headline =
  List.map getTranslateGifNoFail (headlineToPhrases headline)
    |> Task.sequence
    |> Task.toMaybe
    |> Task.map NewGifUrls
    |> Effects.task


{-
fetchGifUrls : String -> Effects Action
fetchGifUrls headline =
  fetchGifUrlsForListOfPhrases (headlineToPhrases headline)
-}

headlineToPhrases : String -> List String
headlineToPhrases headline =
  words headline
    |> List.map String.toLower
    |> List.map (String.filter Char.isLower)
    |> List.filter (\s -> String.length s > 3)
    |> groups 2
    |> List.map (join " ")

groups : Int -> List a -> List (List a)
groups n xs =
  case xs of
    [] ->
      []
    xs ->
      (List.take n xs) :: (groups n (List.drop n xs))

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
        , fetchGifUrls (currentHeadline newModel)
        )

    NewGifUrls gifUrls ->
      ( {model | gifUrls = (Maybe.withDefault model.gifUrls gifUrls) }
      , Effects.none
      )

    Next ->
      let
        newModel =
          {model | index = (model.index + 1) % (List.length model.headlines), gifUrls = []}
      in
        ( newModel
        , fetchGifUrls (currentHeadline newModel)
        )

currentHeadline : Model -> String
currentHeadline model =
  List.drop model.index model.headlines
  |> List.head
  |> Maybe.withDefault ""

view : Signal.Address Action -> Model -> Html
view address model =
  let
    renderImages =
      List.map (\url -> img [src url] []) model.gifUrls
  in
    div [onClick address Next]
      [ h1 [] [text (currentHeadline model)]
      , div [] renderImages
      ]
