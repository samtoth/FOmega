module Main exposing (main)

import Browser

import Html exposing (Html, button, div, text, input, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

import Http

import TLWND as TW


type alias Flags = ()

main = Browser.element { init = \() -> ( init, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

type alias Model = {source: String, currentCore: String}

type alias CoreResult = Result Http.Error String

type Msg =   RequestCore
           | CoreReceived CoreResult
           | InputChange String


init : Model
init = {
    source = "module Blah where\n  f x = x",
    currentCore =  ""
    }

requestCore : String -> Cmd Msg
requestCore s = Http.post
    { 
    url = "http://localhost:3030/core"
    , body = Http.stringBody "text/plain;charset=utf-8" s
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
    RequestCore       -> (model, requestCore model.source)
    CoreReceived res -> (case res of
        Ok svg -> {model | currentCore = svg} 
        Err e -> {model | currentCore = "ERROR: " ++ interpretCoreErr e},
        Cmd.none)
    InputChange text  -> ({ model | source = text }, Cmd.none)

view : Model -> Html Msg
view {source, currentCore} = 
  div [class "flex flex-row width-100"]
    [  
     textarea [class "w-1/2 flex-grow font-mono resize-none border-r-1 border-black p-5", cols 40, rows 10, value source, onInput InputChange] []
    ,button [class "w-8 hover:text-blue-500", onClick RequestCore] [ div [class "text-lg font-bold m-2"] [text "»" ]]
    ,div [class "w-1/2 flex-grow border-l-1 border-black p-5"] [text currentCore]
    ]
