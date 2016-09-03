module HtmlParser.Search exposing
  ( getElementsByTagName, getElementsByClassName
  , traverse
  )

import HtmlParser exposing (Node(..))
import String
import Dict exposing (Dict)


getElementsByTagName : String -> List Node -> List Node
getElementsByTagName tagName nodes =
  let
    targetTagName =
      String.toLower tagName

    match tagName _ =
      tagName == targetTagName
  in
    findAllElements match nodes


getElementsByClassName : String -> List Node -> List Node
getElementsByClassName targetClassName nodes =
  let
    match _ attrs =
      matchesToClass targetClassName attrs
  in
    findAllElements match nodes


getElementById : String -> List Node -> Maybe Node
getElementById targetId nodes =
  let
    match _ attrs =
      matchesToId targetId attrs
  in
    findElement match nodes


createIndex : List Node -> Dict String Node
createIndex nodes =
  let
    f dict node =
      case node of
        Element tagName attrs children ->
          case getAttribute "id" attrs of
            Just id ->
              (Dict.insert id node dict, False)

            Nothing ->
              (dict, False)

        _ ->
          (dict, False)
  in
    traverse f Dict.empty nodes



findAllElements : (String -> List (String, String) -> Bool) -> List Node -> List Node
findAllElements match nodes =
  let
    f results node =
      case node of
        Element tagName attrs children ->
          if match tagName attrs then
            (results ++ (node :: findAllElements match children), False)
          else
            (results ++ findAllElements match children, False)

        _ ->
          (results, False)
  in
    traverse f [] nodes


findElement : (String -> List (String, String) -> Bool) -> List Node -> Maybe Node
findElement match nodes =
  let
    f _ node =
      case node of
        Element tagName attrs children ->
          if match tagName attrs then
            (Just node, True)
          else
            case findElement match children of
              Nothing ->
                (Nothing, False)

              x ->
                (x, True)

        _ ->
          (Nothing, False)
  in
    traverse f Nothing nodes


matchesToClass : String -> List (String, String) -> Bool
matchesToClass targetClassName attrs =
  case getAttribute "class" attrs of
    Nothing ->
      False

    Just value ->
      List.member targetClassName (String.words value)


matchesToId : String -> List (String, String) -> Bool
matchesToId targetId attrs =
  getAttribute "id" attrs == Just targetId


getAttribute : String -> List (String, String) -> Maybe String
getAttribute targetName attrs =
  case attrs of
    [] ->
      Nothing

    (name, value) :: tail ->
      if name == targetName then
        Just value
      else
        getAttribute targetName tail


traverse : (a -> Node -> (a, Bool)) -> a -> List Node -> a
traverse f a nodes =
  case nodes of
    [] -> a

    node :: tail ->
      case f a node of
        (a, True) -> a

        (a, False) ->
          traverse f a tail
