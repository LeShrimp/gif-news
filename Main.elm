import StartApp exposing (..)
import MainLogic exposing (init, view, update)

app =
    start { init = init, view = view, update = update, inputs = [] }

main =
    app.html

port tasks : Signal (Task.Task Never ())
port tasks =
    app.tasks
