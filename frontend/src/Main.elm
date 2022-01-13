port module Main exposing (main)

import Browser
import Dict
import Html exposing (Html, button, div, h1, input, p, span, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http exposing (request)
import Json.Decode as Decode exposing (Decoder, Error(..))
import Json.Decode.Field as Field
import Json.Encode as E
import String exposing (left)


type alias Flags =
    ()


main : Program Flags Model Msg
main =
    Browser.element
        { init = \_ -> ( init, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { source : String, extensions : String, coreResult : Maybe (Result Http.Error CoreResult) }


type alias CoreRequest =
    { source : String, extensions : String }


encode : CoreRequest -> E.Value
encode req =
    E.dict identity
        E.string
        (Dict.fromList
            [ ( "source", req.source )
            , ( "extensions", req.extensions )
            ]
        )


port renderLatex : { target : String, tex : String } -> Cmd msg


type Msg
    = RequestCore
    | CoreReceived (Result Http.Error CoreResult)
    | SourceChanged String
    | ExtensionChanged String


init : Model
init =
    { source = "module Blah where\n  f x = x"
    , extensions = ""
    , coreResult = Nothing
    }


requestCore : CoreRequest -> Cmd Msg
requestCore req =
    Http.post
        { url = "http://localhost:3030/core"
        , body = Http.jsonBody (encode req)
        , expect = Http.expectJson CoreReceived decodeCoreResult
        }


decodeCoreResult : Decoder CoreResult
decodeCoreResult =
    Field.require "tag" Decode.string <|
        \tag ->
            case tag of
                "Success" ->
                    Field.require "contents" decodeModule <| \mod -> Decode.succeed (Success mod)

                "Error" ->
                    Field.require "contents" Decode.string <|
                        \err ->
                            Decode.succeed (Error err)

                _ ->
                    Decode.fail "unexpected tag decoding coreResult"


type CoreResult
    = Success Module
    | Error String


decodeModule : Decoder Module
decodeModule =
    Field.require "name" Decode.string <|
        \name ->
            Field.require "main_binds" (Decode.list decodeBindgroup) <|
                \binds ->
                    Field.require "special_binds" (Decode.list decodeBindgroup) <|
                        \specialBinds ->
                            Field.require "types" (Decode.succeed ()) <|
                                \types ->
                                    Decode.succeed
                                        { name = name
                                        , mainBinds = binds
                                        , specialBinds = specialBinds
                                        , types = types
                                        }


type alias Module =
    { name : String
    , mainBinds : List Bindgroup
    , specialBinds : List Bindgroup
    , types : TypeEnv
    }


type alias TypeEnv =
    ()


decodeBindgroup : Decoder Bindgroup
decodeBindgroup =
    Field.require "tag" Decode.string <|
        \tag ->
            case tag of
                "Binding" ->
                    Field.require "contents" decodeBind <|
                        \b ->
                            Decode.succeed (Binding b)

                "RecursiveBinding" ->
                    Field.require "contents" (Decode.list decodeBind) <|
                        \bs ->
                            Decode.succeed (RecursiveBinding bs)

                _ ->
                    Decode.fail "unexpected tag decoding bind group"


type Bindgroup
    = Binding Bind
    | RecursiveBinding (List Bind)


decodeBind : Decoder Bind
decodeBind =
    Field.require "bindName" Decode.string <|
        \n ->
            Field.require "bindType" Decode.string <|
                \t ->
                    Field.require "bindBody" Decode.string <|
                        \bod ->
                            Decode.succeed { bindName = n, bindType = t, bindBody = bod }


type alias Bind =
    { bindName : String
    , bindType : String
    , bindBody : String
    }


interpretCoreErr : Http.Error -> String
interpretCoreErr err =
    case err of
        Http.BadStatus i ->
            String.fromInt i

        Http.BadBody s ->
            s

        Http.BadUrl s ->
            s

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "NetworkError"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestCore ->
            ( model, requestCore { source = model.source, extensions = model.extensions } )

        CoreReceived res ->
            ( { model | coreResult = Just res }, renderLatex {target = "eq", tex = "$ = $"} )

        SourceChanged text ->
            ( { model | source = text }, Cmd.none )

        ExtensionChanged text ->
            ( { model | extensions = text }, Cmd.none )


view : Model -> Html Msg
view { source, extensions, coreResult } =
    div []
        [ div [ class "flex flex-row width-100" ]
            [ div [ class "w-1/3 flex-grow flex-col" ]
                [ textarea [ class "w-full font-mono resize-none border-r-1 border-black p-5", cols 40, rows 10, value source, onInput SourceChanged ] []
                , input [ class "w-full font-mono border-t-1", placeholder "Language extensions...", value extensions, onInput ExtensionChanged ] []
                ]
            , button [ class "w-8 hover:text-blue-500", onClick RequestCore ] [ div [ class "text-lg font-bold m-2" ] [ text "»" ] ]
            , div [ class "w-2/3 flex-grow border-l-1 border-black p-5 flex flex-row", id "KaTeX" ] [ viewCore coreResult ]
            ]
        , div [] [ text source, text extensions ]
        ]


viewCore : Maybe (Result Http.Error CoreResult) -> Html Msg
viewCore x =
    div [ class "prose prose-stone prose-xl flex-auto font-serif" ]
        (case x of
            Just res ->
                case res of
                    Ok coreRes ->
                        case coreRes of
                            Success core ->
                                h1 [ class "text-center mt-5" ] [ text core.name ]
                                    :: List.map viewBinding core.mainBinds

                            Error coreErr ->
                                [ p [] [ text coreErr ] ]

                    Err err ->
                        [ p [] [ text (interpretCoreErr err) ]
                        ]

            Nothing ->
                []
        )


viewBinding : Bindgroup -> Html Msg
viewBinding bb =
    case bb of
        Binding b ->
            div []
                [ span [ class "font-bold" ] [ text b.bindName ]
                , span [] [text " $\\leftarrow$ "]
                , span [] [ text <| "$" ++ b.bindBody ++"$"]
                ]

        RecursiveBinding bs ->
            Debug.todo ""
