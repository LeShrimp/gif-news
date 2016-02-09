module Giphy
  ( getRandomGif
  , getTranslateGif
  ) where

import Json.Decode as Json
import Http exposing (..)
import Task exposing (..)

(=>) = (,)

type alias ErrorMessage = String

getRandomGif : String -> Task ErrorMessage String
getRandomGif topic =
  Http.get decodeRandomUrl (randomUrl topic) `onError` errorToErrorMessage

getTranslateGif : String -> Task ErrorMessage String
getTranslateGif phrase =
  Http.get decodeTranslateUrl (translateUrl phrase) `onError` errorToErrorMessage

errorToErrorMessage : Error -> Task ErrorMessage a
errorToErrorMessage error =
  Task.fail
    <| case error of
        Timeout -> "Connection timed out"
        NetworkError -> "Network error"
        UnexpectedPayload str -> "UnexpectedPayload: " ++ str
        BadResponse int str -> "Bad response: " ++ (toString int) ++ ", " ++ str

randomUrl : String -> String
randomUrl topic =
  Http.url "http://api.giphy.com/v1/gifs/random"
    [ "api_key" => "dc6zaTOxFJmzC"
    , "tag" => topic
    ]

decodeRandomUrl : Json.Decoder String
decodeRandomUrl =
  Json.at ["data", "image_url"] Json.string

translateUrl : String -> String
translateUrl phrase =
  Http.url "http://api.giphy.com/v1/gifs/translate"
    [ "api_key" => "dc6zaTOxFJmzC"
    , "s" => phrase
    ]

decodeTranslateUrl : Json.Decoder String
decodeTranslateUrl =
  Json.at ["data", "images", "fixed_height", "url"] Json.string
