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

type alias Model =
  { headlines : List String
  , index : Int
  , gifUrl : String
  }

type Action
  = NewHeadlines (Maybe (List String))
  | NewGifUrl (Maybe (String))
  | Next

fetchHeadlines : Effects Action
fetchHeadlines =
  scrapeTexts "http://edition.cnn.com/" ".cn--idx-1 .cd__headline .cd__headline-text"
    |> Task.toMaybe
    |> Task.map NewHeadlines
    |> Effects.task

fetchGifUrls : String -> Effects Action
fetchGifUrls phrase =
  getTranslateGif phrase
    |> Task.toMaybe
    |> Task.map NewGifUrl
    |> Effects.task

init : (Model, Effects Action)
init =
  ( Model [] 0 ""
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

    NewGifUrl gifUrl ->
      ( {model | gifUrl = (Maybe.withDefault model.gifUrl gifUrl) }
      , Effects.none
      )

    Next ->
      ( {model | index = (model.index + 1) % (List.length model.headlines)}
      , Effects.none
      )

currentHeadline : Model -> String
currentHeadline model =
  List.drop model.index model.headlines
  |> List.head
  |> Maybe.withDefault ""

view : Signal.Address Action -> Model -> Html
view address model =
  div [onClick address Next]
    [ img [src model.gifUrl] []
    , text (currentHeadline model)
    ]
