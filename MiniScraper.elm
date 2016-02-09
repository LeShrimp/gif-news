module MiniScraper
  (scrapeTexts
  ,fetchHeadlines
  ) where

import Json.Encode exposing (..)
import Json.Decode exposing (..)
import Http exposing (..)
import Task exposing (..)

type alias ErrorMessage = String

(=>) = (,)

fetchHeadlines : Task ErrorMessage (List String)
fetchHeadlines =
  scrapeTexts "http://edition.cnn.com/" ".cn--idx-1 .cd__headline .cd__headline-text"

scrapeTexts : String -> String -> Task ErrorMessage (List String)
scrapeTexts url selector =
  (postRequest url selector) `Task.onError` rawErrorToErrorMessage `Task.andThen` decodeResponse

decodeResponse : Response -> Task ErrorMessage (List String)
decodeResponse response =
  let
    successDecoder =
      "result" := (Json.Decode.list Json.Decode.string)
    errorDecoder =
      "error" := Json.Decode.string

    decodeResponseText str =
      case Json.Decode.decodeString successDecoder str of
        Ok r -> Task.succeed r
        Err _ -> tryDecodeError str

    tryDecodeError str =
      case Json.Decode.decodeString errorDecoder str of
        Ok e -> Task.fail e
        Err _ -> Task.fail ("Unable to decode response: " ++ str)

  in
    if 200 <= response.status && response.status < 300 then
        case response.value of
          Text str ->
            decodeResponseText str
          _ ->
            Task.fail "Response body is a blob, expecting a string."
    else
        Task.fail <| toString response.status ++ " " ++ response.statusText

rawErrorToErrorMessage : RawError -> Task ErrorMessage Response
rawErrorToErrorMessage error =
  case error of
    RawTimeout -> Task.fail "Connection timed out"
    RawNetworkError -> Task.fail "Network error"

postRequest : String -> String -> Task RawError Response
postRequest url selector =
  send defaultSettings
    { verb = "POST"
    , headers = ["Content-Type" => "application/json"]
    , url = "http://www.thereisbeauty.de/mini-scraper/"
    , body = Http.string (requestBody url selector)
    }

requestBody : String -> String -> String
requestBody url selector =
  encode 0
  <| object
    ["url" => Json.Encode.string url
    ,"selector" => Json.Encode.string selector
    ]
