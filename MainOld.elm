import Graphics.Element exposing (show)
import Html exposing (..)
import Html.Attributes exposing (style)
import Task exposing (..)
import Signal exposing (..)

import MiniScraper exposing (fetchHeadlines)
import Giphy exposing (getTranslateGif)

(=>) = (,)

main =
  Signal.map show gifUrl.signal

gifUrl : Signal.Mailbox String
gifUrl =
  Signal.mailbox ""

port run : Task String ()
port run =
  getTranslateGif "cat"
    `andThen` (Signal.send gifUrl.address)

view : Signal.Address Action -> Model -> Html
view address model =
  div [ style [ "width" => "200px" ] ]
    [ h2 [headerStyle] [text model.topic]
    , div [imgStyle model.gifUrl] []
    , button [ onClick address RequestMore ] [ text "More Please!" ]
    ]


headerStyle : Attribute
headerStyle =
  style
    [ "width" => "200px"
    , "text-align" => "center"
    ]


imgStyle : String -> Attribute
imgStyle url =
  style
    [ "display" => "inline-block"
    , "width" => "200px"
    , "height" => "200px"
    , "background-position" => "center center"
    , "background-size" => "cover"
    , "background-image" => ("url('" ++ url ++ "')")
    ]

{--
port run : Task x ()
port run =
  fetchHeadlines
    `andThen` (Signal.send headlines.address)
    `onError` (Signal.send headlines.address << (\msg -> ["Error: " ++ msg]))

-}
