module View

open Elmish
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

open Fable
open Fable.React
open Fable.React.Props

open Common
open ViewElement
open Model

type SettingChange =
    | Sound of SoundState
    | FeedbackDuration of int
type Message =
    | Reset
    | ToggleOptions
    | AnswerKey of string
    | Backspace
    | Complete
    | Setting of SettingChange
    | UserMessage of {| color:string; msg: string |} option

let onClick f x = OnClick <| fun _ -> f x
let btn label attrs = button attrs [str label]

let ofEnum x = x |> List.map (fun v -> v, v.ToString())
let ofBool = [true, "On"; false, "Off"]

// minor perf opt: pre-curry all the settings. Not necessary, just seeing if it can be done elegantly.
let adapt (label, currentValueGetter, modelCommand, options) model d = ViewElement.setting label options (currentValueGetter model) (fun v -> d (modelCommand v |> Setting))
let changeTo wrap (ChangeValue v) = wrap v
let settingsControls = [
    // hey, this getter/setter pattern kind of reminds me of lenses!
    ("Sound", (fun (s:Settings) -> s.sound), changeTo Sound, [On, "On"; Off, "Off"; BombOnly, "Bomb"; CheerOnly, "Cheers"]) |> adapt
    ("Feedback duration", (fun s -> s.feedbackDuration), changeTo FeedbackDuration, [0, "None"; 300, "Short"; 1000, "Medium"; 5000, "Long"]) |> adapt
    ]

let viewOptions (settings:Settings) dispatch =
    let setting label currentValue msg options =
        div[][
            text[ClassName "optionLabel"][str label]
            span[ClassName "optionSpan"][
                for (label, value) in options ->
                    button[ClassName (if value = currentValue then "option selected" else "option");
                            OnClick (thunk1 dispatch (Setting <| msg value))][str label]
                ]
            ]
    div [ClassName "optionsDisplay"] [
        for setting in settingsControls do
            yield setting settings dispatch
        yield button [ClassName "optionDoneButton"; OnClick (thunk1 dispatch ToggleOptions)][unbox "OK"]
        ]

let view (g:Game) dispatch =
    div [ClassName "ui"](
        div[ClassName "header"][
            btn "Reset" [onClick dispatch Reset]
            btn "Options" [onClick dispatch ToggleOptions]
            ]
        ::
        if g.showOptions then
            [viewOptions g.settings dispatch]
        else [
            h3[ClassName "scoreDisplay"][str <| sprintf "Score: %d" g.score]

            (if g.messageToUser.IsSome then
                div[ClassName "numDisplay"; Style[Color g.messageToUser.Value.color]][str g.messageToUser.Value.msg]
             else
                div[ClassName "numDisplay"][str (sprintf "%s = %s" (Game.CurrentProblem g) (if g.currentAnswer = "" then "??" else g.currentAnswer))])
            div[ClassName "keyList"][
                let maybeDispatch = if g.messageToUser.IsSome then ignore else dispatch
                let keyButton label msg = btn label [onClick maybeDispatch msg]
                yield! [for x in 1..9 -> keyButton (x.ToString()) (AnswerKey (x.ToString()))]
                yield keyButton "Back" Backspace
                yield keyButton "0" (AnswerKey "0")
                yield keyButton "-" (AnswerKey "-")
                yield keyButton "ENTER" Complete
                ]
            ]
        )
