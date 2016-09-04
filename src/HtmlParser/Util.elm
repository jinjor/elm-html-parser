module HtmlParser.Util exposing
  ( getElementById, getElementsByTagName, getElementsByClassName
  , createIdDict, createTagDict, createClassDict
  , findElement, findElements
  , filterMapElements
  , getValue, getId, getClassList
  , textContent
  , toVirtualDom
  )

{-|
# Query
@docs getElementById, getElementsByTagName, getElementsByClassName

# Optimize
@docs createIdDict, createTagDict, createClassDict

# Custom Query
@docs findElement, findElements

# Mapping
@docs filterMapElements

# Attributes
@docs getValue, getId, getClassList

# Get Content
@docs textContent
-}

import HtmlParser exposing (Node(..), Attributes)
import String
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)

{-|

-}
getElementById : String -> List Node -> Maybe Node
getElementById targetId nodes =
  findElement (\_ attrs -> matchesToId targetId attrs) nodes


{-|

-}
getElementsByTagName : String -> List Node -> List Node
getElementsByTagName tagName nodes =
  let
    targetTagName =
      String.toLower tagName

    match tagName _ =
      tagName == targetTagName
  in
    findElements match nodes


{-|

-}
getElementsByClassName : String -> List Node -> List Node
getElementsByClassName targetClassName nodes =
  findElements (\_ attrs -> matchesToClass targetClassName attrs) nodes


matchesToId : String -> Attributes -> Bool
matchesToId targetId attrs =
  getValue "id" attrs == Just targetId


matchesToClass : String -> Attributes -> Bool
matchesToClass targetClassName attrs =
  List.member targetClassName (getClassList attrs)


{-|

-}
createIdDict : List Node -> Dict String Node
createIdDict nodes =
  let
    f node dict =
      case node of
        Element tagName attrs children ->
          case getValue "id" attrs of
            Just id ->
              Dict.union (Dict.insert id node dict) (createIdDict children)

            Nothing ->
              Dict.union dict (createIdDict children)

        _ ->
          dict
  in
    List.foldl f Dict.empty nodes


{-|

-}
createTagDict : List Node -> Dict String (List Node)
createTagDict nodes =
  let
    f node dict =
      case node of
        Element tagName attrs children ->
          updateTagDict node tagName (mergeListDict (createTagDict children) dict)

        _ ->
          dict
  in
    List.foldr f Dict.empty nodes


{-|

-}
createClassDict : List Node -> Dict String (List Node)
createClassDict nodes =
  let
    f node dict =
      case node of
        Element tagName attrs children ->
          List.foldl
            (updateClassDict node)
            (mergeListDict (createClassDict children) dict)
            (getClassList attrs)

        _ ->
          dict
  in
    List.foldr f Dict.empty nodes


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
            Just (list ++ v2)

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


{-|

-}
findElement : (String -> Attributes -> Bool) -> List Node -> Maybe Node
findElement match nodes =
  let
    f node _ =
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
    foldlWithBreak f Nothing nodes


{-|

-}
findElements : (String -> Attributes -> Bool) -> List Node -> List Node
findElements match nodes =
  let
    f node results =
      case node of
        Element tagName attrs children ->
          if match tagName attrs then
            results ++ (node :: findElements match children)
          else
            results ++ findElements match children

        _ ->
          results
  in
    List.foldl f [] nodes


foldlWithBreak : (a -> b -> (b, Bool)) -> b -> List a -> b
foldlWithBreak f b list =
  case list of
    [] -> b

    a :: tail ->
      case f a b of
        (b, True) -> b

        (b, False) ->
          foldlWithBreak f b tail


{-|

-}
filterMapElements : (String -> Attributes -> List Node -> Maybe b) -> List Node -> List b
filterMapElements f nodes =
  List.filterMap (\node ->
    case node of
      Element tagName attrs children ->
        f tagName attrs children

      _ ->
        Nothing
  ) nodes


{-|

-}
getValue : String -> Attributes -> Maybe String
getValue targetName attrs =
  case attrs of
    [] ->
      Nothing

    (name, value) :: tail ->
      if name == targetName then
        Just value
      else
        getValue targetName tail


{-|

-}
getId : Attributes -> Maybe String
getId attrs =
  getValue "id" attrs


{-|

-}
getClassList : Attributes -> List String
getClassList attrs =
  case getValue "class" attrs of
    Nothing ->
      []

    Just value ->
      String.words value


{-|

-}
textContent : List Node -> String
textContent nodes =
  String.join "" (List.map textContentEach nodes)


textContentEach : Node -> String
textContentEach node =
  case node of
    Element _ _ children ->
      textContent children

    Text s ->
      s

    Comment s ->
      ""


{-|

-}
toVirtualDom : List Node -> List (Html msg)
toVirtualDom nodes =
  List.map toVirtualDomEach nodes


toVirtualDomEach : Node -> Html msg
toVirtualDomEach node =
  case node of
    Element name attrs children ->
      Html.node name (List.map toAttribute attrs) (toVirtualDom children)

    Text s ->
      text s

    Comment _ ->
      text ""


toAttribute : (String, String) -> Attribute msg
toAttribute (name, value) =
  attribute name value
