import Graphics.Element exposing (show)
import Json.Encode exposing (..)
import Json.Decode exposing (..)
import Http exposing (..)
import Task exposing (..)

(=>) = (,)

main =
  Signal.map show headlines.signal

headlines : Signal.Mailbox (List String)
headlines =
  Signal.mailbox []

port run : Task Error ()
port run =
  fetchHeadlines `Task.andThen` Signal.send headlines.address

fetchHeadlines : Task Error (List String)
fetchHeadlines =
  let
    decoder =
      "result" := (Json.Decode.list Json.Decode.string)
  in
    fromJson decoder postRequest

postRequest : Task RawError Response
postRequest =
  send defaultSettings
    { verb = "POST"
    , headers = ["Content-Type" => "application/json"]
    , url = "http://www.thereisbeauty.de/mini-scraper/"
    , body = Http.string requestBody
    }

requestBody : String
requestBody =
  encode 0
  <| object
    ["url" => Json.Encode.string "http://edition.cnn.com/"
    ,"selector" => Json.Encode.string ".cn--idx-1 .cd__headline .cd__headline-text"
    ]
