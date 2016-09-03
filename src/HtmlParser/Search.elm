module HtmlParser.Search exposing
  ( getElementById, getElementsByTagName, getElementsByClassName
  , createIndex, createTagIndex, createClassIndex
  , findElement, findElements
  , traverse
  , filterMapElements
  , getId, getClassList
  )


import HtmlParser exposing (Node(..), Attributes)
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
    findElements match nodes


getElementsByClassName : String -> List Node -> List Node
getElementsByClassName targetClassName nodes =
  let
    match _ attrs =
      matchesToClass targetClassName attrs
  in
    findElements match nodes


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
              (Dict.union (Dict.insert id node dict) (createIndex children), False)

            Nothing ->
              (dict, False)

        _ ->
          (dict, False)
  in
    traverse f Dict.empty nodes


createTagIndex : List Node -> Dict String (List Node)
createTagIndex nodes =
  let
    f dict node =
      case node of
        Element tagName attrs children ->
          ( mergeListDict (updateTagDict node tagName dict) (createTagIndex children)
          , False
          )

        _ ->
          (dict, False)
  in
    traverse f Dict.empty nodes


createClassIndex : List Node -> Dict String (List Node)
createClassIndex nodes =
  let
    f dict node =
      case node of
        Element tagName attrs children ->
          ( mergeListDict
              (List.foldl (updateClassDict node) dict (getClassList attrs))
              (createClassIndex children)
          , False
          )

        _ ->
          (dict, False)
  in
    traverse f Dict.empty nodes


updateTagDict : Node -> String -> Dict String (List Node) -> Dict String (List Node)
updateTagDict node tagName dict =
  updateListDict tagName node dict


updateClassDict : Node -> String -> Dict String (List Node) -> Dict String (List Node)
updateClassDict node class dict =
  updateListDict class node dict


mergeListDict : Dict comparable (List v) -> Dict comparable (List v) -> Dict comparable (List v)
mergeListDict d1 d2 =
  Dict.foldl
    (\k2 v2 d1 ->
      Dict.update
        k2
        (\v1 -> case v1 of
          Just list ->
            Just (v2 ++ list)

          Nothing ->
            Just v2
        )
        d1
    )
    d1
    d2


updateListDict : comparable -> v -> Dict comparable (List v) -> Dict comparable (List v)
updateListDict key value dict =
  Dict.update
    key
    (\v -> case v of
      Just list ->
        Just (value :: list)

      Nothing ->
        Just [ value ]
    )
    dict


findElements : (String -> Attributes -> Bool) -> List Node -> List Node
findElements match nodes =
  let
    f results node =
      case node of
        Element tagName attrs children ->
          if match tagName attrs then
            (results ++ (node :: findElements match children), False)
          else
            (results ++ findElements match children, False)

        _ ->
          (results, False)
  in
    traverse f [] nodes


findElement : (String -> Attributes -> Bool) -> List Node -> Maybe Node
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


matchesToClass : String -> Attributes -> Bool
matchesToClass targetClassName attrs =
  List.member targetClassName (getClassList attrs)


matchesToId : String -> Attributes -> Bool
matchesToId targetId attrs =
  getAttribute "id" attrs == Just targetId


getClassList : Attributes -> List String
getClassList attrs =
  case getAttribute "class" attrs of
    Nothing ->
      []

    Just value ->
      String.words value


getId : Attributes -> Maybe String
getId attrs =
  getAttribute "id" attrs


getAttribute : String -> Attributes -> Maybe String
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


filterMapElements : (String -> Attributes -> List Node -> Maybe b) -> List Node -> List b
filterMapElements f nodes =
  List.filterMap (\node ->
    case node of
      Element tagName attrs children ->
        f tagName attrs children

      _ ->
        Nothing
  ) nodes
