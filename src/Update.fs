module Update

open Elmish
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Common
open Model
open View

let init _ = Game.Fresh(), Cmd.none

module Sounds =
    open Browser.Types
    [<Emit("new Audio($0)")>]
    let sound file : HTMLAudioElement = jsNative
    let private cheers = [|
                    sound("1_person_cheering-Jett_Rifkin-1851518140.mp3")
                    sound("Cheer1.m4a")
                    sound("Cheer2.m4a")
                    sound("Cheer4.m4a")
                    sound("Cheer5.m4a")
                    sound("Cheer6.m4a")
                    |]
    let play (sound: HTMLAudioElement) =
        if sound.ended then sound.play()
        else
            sound.currentTime <- 0.
            sound.play()
    let cheer() =
        chooseRandom cheers |> play
    let bomb =
        let s = sound("Grenade Explosion-SoundBible.com-2100581469.mp3")
        fun () -> play s
open Sounds

[<Emit("setTimeout($1, $0)")>]
let setTimeout ms callback = jsNative

let update msg model =
    match msg with
    | UserMessage msg ->
        { model with messageToUser = msg }, Cmd.none
    | ToggleOptions ->
        let showOptions = not model.showOptions
        if not showOptions then // done button was hit, so persist settings
            let encode = Thoth.Json.Encode.Auto.toString(1, model.settings)
            Browser.WebStorage.localStorage.["settings"] <- encode
        { model with showOptions = showOptions }, Cmd.none
    | Reset -> { Game.Fresh() with showOptions = model.showOptions }, Cmd.none
    | AnswerKey k ->
        if k = "-" || (System.Int32.TryParse(k) |> fst) then
            { model with currentAnswer = model.currentAnswer + k }, Cmd.none
        else model, Cmd.none
    | Backspace ->
        { model with currentAnswer = model.currentAnswer.Substring(0, max 0 (model.currentAnswer.Length - 1)) }, Cmd.none
    | Complete ->
        match model.messageToUser with
        | Some _ -> { model with messageToUser = None}, Cmd.Empty
        | None ->
            match System.Int32.TryParse model.currentAnswer with
            | true, ans ->
                let (Equation(lhs, rhs, env)) = model.problem
                let currentAnswer = sprintf "%s = %d" (renderTerm 0 lhs) ans
                let stake = (abs (eval (defaultArg env 0) lhs))
                let feedback x =
                    match model.settings.feedbackDuration with
                    | 0 -> { x with messageToUser = None }, Cmd.none
                    | n -> x, Cmd.ofSub(fun dispatch -> setTimeout n (thunk1 dispatch (UserMessage None)))
                if ans = eval (defaultArg env 0) rhs then
                    match model.settings.sound with
                    | On | CheerOnly -> cheer()
                    | _ -> ()
                    { model with currentAnswer = ""; problem = generate(); messageToUser = Some {| color = "Green"; msg = currentAnswer |}; score = model.score + stake * 100 } |> feedback
                else
                    match model.settings.sound with
                    | On | BombOnly -> bomb()
                    | _ -> ()
                    { model with currentAnswer = ""; messageToUser = Some {| color = "Red"; msg = currentAnswer |}; score = model.score - stake * 100 } |> feedback
            | _ -> model, Cmd.none
    | Setting msg ->
        let settings = model.settings
        let settings' =
            match msg with
            | SettingChange.Sound v -> { settings with sound = v }
            | SettingChange.FeedbackDuration v -> { settings with feedbackDuration = v }
        match msg with
        | _ ->
            { model with settings = settings'; }, Cmd.none
