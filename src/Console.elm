module Console exposing
    ( Cmd
    , Log(..)
    , Model
    , Msg
    , advanced
    , basic
    , batch
    , log
    )

import Browser exposing (Document)
import Browser.Dom as Dom
import Browser.Events as BE
import Console.Internal exposing (Cmd, Msg(..))
import Css as C exposing (Color, Style)
import Css.Colors exposing (..)
import Css.Global as CG
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Json.Decode as D
import Process
import Task
import Time


type alias Model model =
    Console.Internal.Model Log model


type alias Msg msg =
    Console.Internal.Msg Log msg


type alias Cmd msg =
    Console.Internal.Cmd Log msg


type Log
    = In String
    | Out String


basic :
    { init : ( model, Cmd () )
    , process : Log -> model -> ( model, Cmd () )
    }
    -> Program () (Model model) (Msg ())
basic { init, process } =
    Browser.document
        { init =
            \_ ->
                let
                    ( model, cmd ) =
                        init
                in
                ( init_ model
                , cmd
                )
        , update = update_ process <| \_ model -> ( model, Cmd.none )
        , subscriptions = \_ -> BE.onResize Resize
        , view = view
        }


advanced :
    { init : flags -> ( model, Cmd msg )
    , process : Log -> model -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    }
    -> Program flags (Model model) (Msg msg)
advanced { init, process, update, subscriptions } =
    Browser.document
        { init =
            \flags ->
                let
                    ( model, cmd ) =
                        init flags
                in
                ( init_ model
                , cmd
                )
        , update = update_ process update
        , subscriptions =
            \model ->
                Sub.batch
                    [ Sub.map UpdateMsg <| subscriptions model.model
                    , BE.onResize Resize
                    ]
        , view = view
        }


log : String -> Cmd msg
log str =
    Task.perform NewLog <| Task.succeed <| Out str


batch : List String -> Cmd msg
batch logs =
    log <| String.join "\n" logs



--INTERNAL


init_ : model -> Model model
init_ model =
    { logs = []
    , currentInput = ""
    , model = model
    , time = 0
    }



-- UPDATE


update_ :
    (Log -> model -> ( model, Cmd msg ))
    -> (msg -> model -> ( model, Cmd msg ))
    -> Msg msg
    -> Model model
    -> ( Model model, Cmd msg )
update_ process update msg model =
    case msg of
        NewLog log_ ->
            let
                ( newModel, cmd ) =
                    process log_ model.model
            in
            ( { model
                | logs = List.take 500 <| log_ :: model.logs
                , model = newModel
                , currentInput = ""
              }
            , -- all this nonsense is required to log the messages in real time as opposed to waiting for the entire computation finish
              Time.now
                |> Task.andThen
                    (\time ->
                        if Time.posixToMillis time - model.time > 1000 // 30 then
                            Process.sleep 0
                                |> Task.map (\_ -> ( Just time, cmd ))

                        else
                            Task.succeed ( Nothing, cmd )
                    )
                |> Task.perform RunCmd
            )

        InputChanged str ->
            ( { model | currentInput = str }, Cmd.none )

        UpdateMsg msg_ ->
            let
                ( newModel, cmd ) =
                    update msg_ model.model
            in
            ( { model | model = newModel }, cmd )

        RunCmd ( time, cmd ) ->
            case time of
                Just posix ->
                    ( { model | time = Time.posixToMillis posix }
                    , Cmd.batch [ cmd, scrollToBottom ]
                    )

                Nothing ->
                    ( model, cmd )

        Resize _ _ ->
            ( model, scrollToBottom )

        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model a -> Document (Msg msg)
view model =
    { title = "Elm Console"
    , body =
        [ H.div []
            (model.logs
                |> List.reverse
                |> List.map
                    (logToString
                        >> H.text
                        >> List.singleton
                        >> H.pre []
                    )
            )
        , H.div [ A.css [ C.displayFlex ] ]
            [ H.div
                [ A.css
                    [ font
                    , C.position C.absolute
                    ]
                ]
                [ H.text ">" ]
            , H.input
                [ A.value model.currentInput
                , A.autofocus True
                , A.css
                    [ C.backgroundColor background
                    , C.color foreground
                    , font
                    , C.borderStyle C.none
                    , C.padding C.zero
                    , C.flex <| C.int 1
                    , C.focus [ C.outline C.none ]
                    ]
                , E.onInput InputChanged
                , E.on "keydown" <|
                    (D.field "key" D.string
                        |> D.andThen
                            (\key ->
                                if key == "Enter" then
                                    D.succeed <| NewLog <| In model.currentInput

                                else
                                    D.fail ""
                            )
                    )
                ]
                []
            ]
        , CG.global
            [ CG.body
                [ C.backgroundColor background
                , C.color foreground
                , font

                --, C.whiteSpace C.preWrap
                ]
            , CG.each [ CG.pre, CG.input ]
                [ C.margin C.zero
                , C.marginLeft <| C.px <| fontSize * 0.75
                ]
            ]
        ]
            |> List.map H.toUnstyled
    }


background : Color
background =
    black


foreground : Color
foreground =
    white


font : Style
font =
    C.batch
        [ C.fontSize <| C.px fontSize
        , C.fontFamily C.monospace
        ]


fontSize : Float
fontSize =
    18


scrollToBottom : Cmd msg
scrollToBottom =
    Dom.getViewport
        |> Task.andThen
            (\{ scene } ->
                Dom.setViewport 0 scene.height
            )
        |> Task.perform (\_ -> NoOp)


logToString : Log -> String
logToString log_ =
    let
        logStr =
            case log_ of
                In str ->
                    str

                Out str ->
                    str
    in
    if String.isEmpty logStr then
        "\n"

    else
        logStr
