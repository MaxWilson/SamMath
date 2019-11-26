module App.View

open Update
open View

open Elmish
open Elmish.Debug
open Elmish.HMR

// App
Program.mkProgram init update view
//|> Program.toNavigable (parseHash pageParser) urlUpdate
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withSubscription(fun model ->
    Cmd.ofSub(fun dispatch ->
        Browser.Dom.document.onkeydown <- fun e ->
            let key = e.key.ToUpperInvariant()
            if e.keyCode = 13. then
                dispatch Complete
            elif e.keyCode = 8. then
                dispatch Backspace
            else dispatch (AnswerKey key)
        )
    )
|> Program.withReactBatched "main"
|> Program.run
