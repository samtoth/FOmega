port module Main exposing (main)

import Browser

import Html exposing (Html, button, div, text, input, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

import Dict

import Http

import Json.Encode as E
import Http exposing (request)

type alias Flags = ()

main : Program Flags Model Msg
main = Browser.element { init = \_ -> ( init, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

type alias Model = {source: String, extensions: String}

type alias CoreRequest = Model
encode : CoreRequest -> E.Value
encode req =
  E.dict identity E.string
    (Dict.fromList [ ("source", req.source),
    ("extensions", req.extensions)
    ])

port renderLatex : {target: String, tex: String} -> Cmd msg

type alias CoreResult = Result Http.Error String

type Msg =   RequestCore
           | CoreReceived CoreResult
           | SourceChanged String
           | ExtensionChanged String


init : Model
init = {
    source = "module Blah where\n  f x = x",
    extensions =  ""
    }

requestCore : CoreRequest -> Cmd Msg
requestCore req = Http.post
    { 
     l url = "http://localhost:3030/core"
    , body = Http.jsonBody (encode req)
    , expect = Http.expectString CoreReceived
    }

interpretCoreErr : Http.Error -> String
interpretCoreErr err = case err of
  Http.BadStatus i       -> String.fromInt i 
  Http.BadBody s         -> s
  Http.BadUrl s          -> s
  Http.Timeout           -> "Timeout"
  Http.NetworkError      -> "NetworkError"


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    RequestCore       -> (model, requestCore (Debug.log "model req: " model))
    CoreReceived res -> case res of
        Ok latex -> (model, renderLatex {target = "KaTeX", tex = latex}) 
        Err e -> (model, renderLatex {target = "KaTeX", tex = ("ERROR: " ++ interpretCoreErr e)})
    SourceChanged text  -> ({ model | source = text }, Cmd.none)
    ExtensionChanged text -> ( {model | extensions = text}, Cmd.none)

view : Model -> Html Msg
view {source, extensions} = 
  div [] [div [class "flex flex-row width-100"]
    [  
      div [class "w-1/2 flex-grow flex-col" ] [
     textarea [class "w-full font-mono resize-none border-r-1 border-black p-5", cols 40, rows 10, value source, onInput SourceChanged] []
     ,input [class "w-full font-mono border-t-1", placeholder "Language extensions...", value extensions, onInput ExtensionChanged] []
      ]
    ,button [class "w-8 hover:text-blue-500", onClick RequestCore] [ div [class "text-lg font-bold m-2"] [text "»" ]]
    ,div [class "w-1/2 flex-grow border-l-1 border-black p-5", id "KaTeX"] []
    ],
    div [] [text source, text extensions]]
