port module Demo exposing (..)

import Html exposing (..)
import Html.App exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import HtmlParser exposing (..)

port init : (String -> msg) -> Sub msg

port input : (String -> msg) -> Sub msg

port parse : ({} -> msg) -> Sub msg


main : Program Never
main =
  program
    { init = initialModel ! []
    , update = \msg model -> (update msg model) ! []
    , view = view
    , subscriptions = \_ ->
        Sub.batch
          [ init Init
          , input Input
          , parse (always Show)
          ]
    }


type alias Model a =
  { src : String
  , dest : List (Html a)
  }


initialModel : Model a
initialModel =
  { src = ""
  , dest = [ text "" ]
  }


type Msg
  = Init String
  | Input String
  | Show


update : Msg -> Model a -> Model a
update msg model =
  case msg of
    Init s ->
      { model | src = s } |> show

    Input s ->
      { model | src = s }

    Show ->
      model |> show


show : Model a -> Model a
show model =
  { model |
    dest =
      case HtmlParser.parse model.src of
        Ok nodes ->
          List.map toVirtualDom nodes

        Err e ->
          [ text (toString e) ]
  }


view : Model Msg -> Html Msg
view model =
  div []
    [ h1 [ style [] ] [ text "HtmlParser DEMO" ]
    , p []
      [ text "Press Ctrl+S to show result ("
      , a [ href "https://github.com/jinjor/elm-html-parser" ] [ text "Source" ]
      , text ")"
      ]
    , div [ style [("display", "flex")] ]
      [ div [ id "editor", style [("height", "500px"), ("width", "50%"), ("border", "solid 1px #aaa")] ] [ text """<h1 style="color:#3a6">Hello!</h1>"""]
      , div [ style [("min-height", "500px"), ("flex-grow", "1"), ("border", "solid 1px #aaa")] ] model.dest
      ]
    ]


toVirtualDom : Node -> Html msg
toVirtualDom node =
  case node of
    Element name attrs children ->
      Html.node name (List.map toAttribute attrs) (List.map toVirtualDom children)

    Text s ->
      text s

    Comment _ ->
      text ""


toAttribute : (String, String) -> Attribute msg
toAttribute (name, value) =
  attribute name value
