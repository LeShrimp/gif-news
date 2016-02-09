import MainLogic exposing (init, view, update)

import StartApp exposing (..)
import Task exposing (..)
import Effects exposing (..)

app =
    start { init = init, view = view, update = update, inputs = [] }

main =
    app.html

port tasks : Signal (Task.Task Never ())
port tasks =
    app.tasks
