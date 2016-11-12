module HtmlParser.Util exposing
  ( getElementById, getElementsByTagName, getElementsByClassName
  , createIdDict, createTagDict, createClassDict
  , findElement, findElements
  , mapElements, filterElements, filterMapElements
  , getValue, getId, getClassList
  , textContent
  , toVirtualDom, toVirtualDomSvg
  )

{-| Utility functions that may help you digging into the contents.

```elm
table = """
  <table border=0 cellpadding=0 cellspacing=0 width=216 style='border-collapse:
   collapse;width:162pt'>
  <!--StartFragment-->
   <col width=72 span=3 style='width:54pt'>
   <tr height=18 style='height:13.5pt'>
    <td height=18 align=right width=72 style='height:13.5pt;width:54pt'>1</td>
    <td align=right width=72 style='width:54pt'>2</td>
    <td align=right width=72 style='width:54pt'>3</td>
   </tr>
   <tr height=18 style='height:13.5pt'>
    <td height=18 class=xl69 align=right style='height:13.5pt'>2</td>
    <td class=xl66 align=right>3</td>
    <td align=right>4</td>
   </tr>
  <!--EndFragment-->
  </table>
"""

( parse table
  |> getElementsByTagName "tr"
  |> mapElements
    (\_ _ innerTr ->
      innerTr
        |> mapElements (\_ _ innerTd -> textContent innerTd)
        |> String.join "\t"
        |> String.trim
    )
  |> String.join "\n"
) == "1\t2\t3\n2\t3\t4"
```

# Query
@docs getElementById, getElementsByTagName, getElementsByClassName

# Optimize
@docs createIdDict, createTagDict, createClassDict

# Custom Query
@docs findElement, findElements

# Mapping
@docs mapElements, filterElements, filterMapElements

# Attributes
@docs getValue, getId, getClassList

# Get Content
@docs textContent

# Virtual DOM
@docs toVirtualDom, toVirtualDomSvg
-}

import HtmlParser exposing (Node(..), Attributes)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import VirtualDom
import Svg exposing (Svg)
import Svg.Attributes


{-| Returns a element by its ID. This function returns a list but it contains at most one value.

Note: This function internally traverses all nodes until the target node is found. For faster access, use createIdDict.
-}
getElementById : String -> List Node -> List Node
getElementById targetId nodes =
  findElement (\_ attrs -> matchesToId targetId attrs) nodes


{-| Returns elements with the given tag name.
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


{-| Returns all child elements which have all of the given class names.
-}
getElementsByClassName : List String -> List Node -> List Node
getElementsByClassName targetClassNames nodes =
  findElements (\_ attrs -> matchesToClass targetClassNames attrs) nodes


matchesToId : String -> Attributes -> Bool
matchesToId targetId attrs =
  getValue "id" attrs == Just targetId


matchesToClass : List String -> Attributes -> Bool
matchesToClass targetClassNames attrs =
  List.all (flip List.member (getClassList attrs)) targetClassNames


{-| Creates a dictionaty for faster access by ID.
-}
createIdDict : List Node -> Dict String (List Node)
createIdDict nodes =
  let
    f node dict =
      case node of
        Element tagName attrs children ->
          ( case getValue "id" attrs of
              Just id -> updateIdDict node id
              Nothing -> identity
          )
          (mergeListDict (createIdDict children) dict)

        _ ->
          dict
  in
    List.foldl f Dict.empty nodes


{-| Creates a dictionaty for faster access by tag name.
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


{-| Creates a dictionaty for faster access by single class.
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


updateIdDict : Node -> String -> Dict String (List Node) -> Dict String (List Node)
updateIdDict node id dict =
  updateListDict id node dict


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


{-| Find one element that satisfies the given condition.
-}
findElement : (String -> Attributes -> Bool) -> List Node -> List Node
findElement match nodes =
  let
    f node _ =
      case node of
        Element tagName attrs children ->
          if match tagName attrs then
            ([node], True)
          else
            case findElement match children of
              [] ->
                ([], False)

              x ->
                (x, True)

        _ ->
          ([], False)
  in
    foldlWithBreak f [] nodes


{-| Find elements that satisfies the given condition.
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


{-| Apply a function to every element of a nodes.
-}
mapElements : (String -> Attributes -> List Node -> b) -> List Node -> List b
mapElements f nodes =
  List.filterMap (\node ->
    case node of
      Element tagName attrs children ->
        Just (f tagName attrs children)

      _ ->
        Nothing
  ) nodes


{-| Keep only elements that satisfy the predicate.
-}
filterElements : (String -> Attributes -> List Node -> Bool) -> List Node -> List Node
filterElements f nodes =
  List.filter (\node ->
    case node of
      Element tagName attrs children ->
        f tagName attrs children

      _ ->
        False
  ) nodes


{-| Apply a function that may succeed to all values in the nodes, but only keep the successes.
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


{-| Returns a value from attributes with the given name.
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


{-| Returns the ID value from attributes.
-}
getId : Attributes -> Maybe String
getId attrs =
  getValue "id" attrs


{-| Returns the class value from attributes in form of list.
-}
getClassList : Attributes -> List String
getClassList attrs =
  case getValue "class" attrs of
    Nothing ->
      []

    Just value ->
      String.words value


{-| Returns the text content of a node and its descendants.
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


{-| Converts nodes to virtual dom nodes.
-}
toVirtualDom : List Node -> List (Html msg)
toVirtualDom nodes =
  List.map toVirtualDomEach nodes


toVirtualDomEach : Node -> Html msg
toVirtualDomEach node =
  case node of
    Element name attrs children ->
      if name == "svg" then
        toVirtualDomSvgEach node
      else
        Html.node name (List.map toAttribute attrs) (toVirtualDom children)

    Text s ->
      text s

    Comment _ ->
      text ""


toAttribute : (String, String) -> Attribute msg
toAttribute (name, value) =
  attribute name value


{-| Converts nodes to virtual dom SVG nodes.

Note: If node list contains `<svg>` tag, you can use `toVirtualDom` instead.
Otherwise, use this function.

```elm
svg : Svg msg
svg =
  parse """<circle cx="40" cy="40" r="24" style="stroke:#006600; fill:#00cc00"/>"""
  |> toVirtualDomSvg
  |> Svg.svg []
```

-}
toVirtualDomSvg : List Node -> List (Svg msg)
toVirtualDomSvg nodes =
  List.map toVirtualDomSvgEach nodes


toVirtualDomSvgEach : Node -> Svg msg
toVirtualDomSvgEach node =
  case node of
    Element name attrs children ->
      Svg.node name (List.map toSvgAttribute attrs) (toVirtualDomSvg children)

    Text s ->
      text s

    Comment _ ->
      text ""


toSvgAttribute : (String, String) -> Attribute msg
toSvgAttribute (name, value) =
  if String.startsWith "xlink:" name then
    VirtualDom.attributeNS "http://www.w3.org/1999/xlink" name value
  else if String.startsWith "xml:" name then
    VirtualDom.attributeNS "http://www.w3.org/XML/1998/namespace" name value
  else
    VirtualDom.attribute name value
