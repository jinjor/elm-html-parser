module HtmlParser.Util exposing
  ( getElementById, getElementsByTagName, getElementsByClassName
  , createIdDict, createTagDict, createClassDict
  , findElement, findElements
  , mapElements, filterElements, filterMapElements
  , getValue, getId, getClassList
  , textContent
  , toVirtualDom
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
@docs toVirtualDom
-}

import HtmlParser exposing (Node(..), Attributes)
import String
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
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
      Html.node name (List.map toAttribute attrs) (toVirtualDom children)

    Text s ->
      text s

    Comment _ ->
      text ""


toAttribute : (String, String) -> Attribute msg
toAttribute (name, value) =
  attribute name value

{-| Converts nodes to virtual dom SVG nodes
-}
toVirtualDomSvg : List HtmlParser.Node -> List (Svg msg)
toVirtualDomSvg nodes =
  List.map toVirtualDomSvgEach nodes

toVirtualDomSvgEach : HtmlParser.Node -> Svg msg
toVirtualDomSvgEach node =
  case node of
    HtmlParser.Element name attrs children ->
      Svg.node name (List.filterMap toSvgAttribute attrs) (toVirtualDomSvg children)

    HtmlParser.Text s ->
      text s

    HtmlParser.Comment _ ->
      text ""

toSvgAttribute : (String, String) -> Maybe (Attribute msg)
toSvgAttribute (name, value) =
  case name of
    "accent-height" -> Just <| Svg.Attributes.accentHeight value
    "accelerate" -> Just <| Svg.Attributes.accelerate value
    "accumulate" -> Just <| Svg.Attributes.accumulate value
    "additive" -> Just <| Svg.Attributes.additive value
    "alphabetic" -> Just <| Svg.Attributes.alphabetic value
    "allowReorder" -> Just <| Svg.Attributes.allowReorder value
    "amplitude" -> Just <| Svg.Attributes.amplitude value
    "arabic-form" -> Just <| Svg.Attributes.arabicForm value
    "ascent" -> Just <| Svg.Attributes.ascent value
    "attributeName" -> Just <| Svg.Attributes.attributeName value
    "attributeType" -> Just <| Svg.Attributes.attributeType value
    "autoReverse" -> Just <| Svg.Attributes.autoReverse value
    "azimuth" -> Just <| Svg.Attributes.azimuth value
    "baseFrequency" -> Just <| Svg.Attributes.baseFrequency value
    "baseProfile" -> Just <| Svg.Attributes.baseProfile value
    "bbox" -> Just <| Svg.Attributes.bbox value
    "begin" -> Just <| Svg.Attributes.begin value
    "bias" -> Just <| Svg.Attributes.bias value
    "by" -> Just <| Svg.Attributes.by value
    "calcMode" -> Just <| Svg.Attributes.calcMode value
    "cap-height" -> Just <| Svg.Attributes.capHeight value
    "class" -> Just <| Svg.Attributes.class value
    "clipPathUnits" -> Just <| Svg.Attributes.clipPathUnits value
    "contentScriptType" -> Just <| Svg.Attributes.contentScriptType value
    "contentStyleType" -> Just <| Svg.Attributes.contentStyleType value
    "cx" -> Just <| Svg.Attributes.cx value
    "cy" -> Just <| Svg.Attributes.cy value
    "d" -> Just <| Svg.Attributes.d value
    "decelerate" -> Just <| Svg.Attributes.decelerate value
    "descent" -> Just <| Svg.Attributes.descent value
    "diffuseConstant" -> Just <| Svg.Attributes.diffuseConstant value
    "divisor" -> Just <| Svg.Attributes.divisor value
    "dur" -> Just <| Svg.Attributes.dur value
    "dx" -> Just <| Svg.Attributes.dx value
    "dy" -> Just <| Svg.Attributes.dy value
    "edgeMode" -> Just <| Svg.Attributes.edgeMode value
    "elevation" -> Just <| Svg.Attributes.elevation value
    "end" -> Just <| Svg.Attributes.end value
    "exponent" -> Just <| Svg.Attributes.exponent value
    "externalResourcesRequired" -> Just <| Svg.Attributes.externalResourcesRequired value
    "filterRes" -> Just <| Svg.Attributes.filterRes value
    "filterUnits" -> Just <| Svg.Attributes.filterUnits value
    "format" -> Just <| Svg.Attributes.format value
    "from" -> Just <| Svg.Attributes.from value
    "fx" -> Just <| Svg.Attributes.fx value
    "fy" -> Just <| Svg.Attributes.fy value
    "g1" -> Just <| Svg.Attributes.g1 value
    "g2" -> Just <| Svg.Attributes.g2 value
    "glyph-name" -> Just <| Svg.Attributes.glyphName value
    "glyphRef" -> Just <| Svg.Attributes.glyphRef value
    "gradientTransform" -> Just <| Svg.Attributes.gradientTransform value
    "gradientUnits" -> Just <| Svg.Attributes.gradientUnits value
    "hanging" -> Just <| Svg.Attributes.hanging value
    "height" -> Just <| Svg.Attributes.height value
    "horiz-adv-x" -> Just <| Svg.Attributes.horizAdvX value
    "horiz-origin-x" -> Just <| Svg.Attributes.horizOriginX value
    "horiz-origin-y" -> Just <| Svg.Attributes.horizOriginY value
    "id" -> Just <| Svg.Attributes.id value
    "ideographic" -> Just <| Svg.Attributes.ideographic value
    "in'" -> Just <| Svg.Attributes.in' value
    "in2" -> Just <| Svg.Attributes.in2 value
    "intercept" -> Just <| Svg.Attributes.intercept value
    "k" -> Just <| Svg.Attributes.k value
    "k1" -> Just <| Svg.Attributes.k1 value
    "k2" -> Just <| Svg.Attributes.k2 value
    "k3" -> Just <| Svg.Attributes.k3 value
    "k4" -> Just <| Svg.Attributes.k4 value
    "kernelMatrix" -> Just <| Svg.Attributes.kernelMatrix value
    "kernelUnitLength" -> Just <| Svg.Attributes.kernelUnitLength value
    "keyPoints" -> Just <| Svg.Attributes.keyPoints value
    "keySplines" -> Just <| Svg.Attributes.keySplines value
    "keyTimes" -> Just <| Svg.Attributes.keyTimes value
    "lang" -> Just <| Svg.Attributes.lang value
    "lengthAdjust" -> Just <| Svg.Attributes.lengthAdjust value
    "limitingConeAngle" -> Just <| Svg.Attributes.limitingConeAngle value
    "local" -> Just <| Svg.Attributes.local value
    "markerHeight" -> Just <| Svg.Attributes.markerHeight value
    "markerUnits" -> Just <| Svg.Attributes.markerUnits value
    "markerWidth" -> Just <| Svg.Attributes.markerWidth value
    "maskContentUnits" -> Just <| Svg.Attributes.maskContentUnits value
    "maskUnits" -> Just <| Svg.Attributes.maskUnits value
    "mathematical" -> Just <| Svg.Attributes.mathematical value
    "max" -> Just <| Svg.Attributes.max value
    "media" -> Just <| Svg.Attributes.media value
    "method" -> Just <| Svg.Attributes.method value
    "min" -> Just <| Svg.Attributes.min value
    "mode" -> Just <| Svg.Attributes.mode value
    "name" -> Just <| Svg.Attributes.name value
    "numOctaves" -> Just <| Svg.Attributes.numOctaves value
    "offset" -> Just <| Svg.Attributes.offset value
    "operator" -> Just <| Svg.Attributes.operator value
    "order" -> Just <| Svg.Attributes.order value
    "orient" -> Just <| Svg.Attributes.orient value
    "orientation" -> Just <| Svg.Attributes.orientation value
    "origin" -> Just <| Svg.Attributes.origin value
    "overline-position" -> Just <| Svg.Attributes.overlinePosition value
    "overline-thickness" -> Just <| Svg.Attributes.overlineThickness value
    "panose-1" -> Just <| Svg.Attributes.panose1 value
    "path" -> Just <| Svg.Attributes.path value
    "pathLength" -> Just <| Svg.Attributes.pathLength value
    "patternContentUnits" -> Just <| Svg.Attributes.patternContentUnits value
    "patternTransform" -> Just <| Svg.Attributes.patternTransform value
    "patternUnits" -> Just <| Svg.Attributes.patternUnits value
    "point-order" -> Just <| Svg.Attributes.pointOrder value
    "points" -> Just <| Svg.Attributes.points value
    "pointsAtX" -> Just <| Svg.Attributes.pointsAtX value
    "pointsAtY" -> Just <| Svg.Attributes.pointsAtY value
    "pointsAtZ" -> Just <| Svg.Attributes.pointsAtZ value
    "preserveAlpha" -> Just <| Svg.Attributes.preserveAlpha value
    "preserveAspectRatio" -> Just <| Svg.Attributes.preserveAspectRatio value
    "primitiveUnits" -> Just <| Svg.Attributes.primitiveUnits value
    "r" -> Just <| Svg.Attributes.r value
    "radius" -> Just <| Svg.Attributes.radius value
    "refX" -> Just <| Svg.Attributes.refX value
    "refY" -> Just <| Svg.Attributes.refY value
    "rendering-intent" -> Just <| Svg.Attributes.renderingIntent value
    "repeatCount" -> Just <| Svg.Attributes.repeatCount value
    "repeatDur" -> Just <| Svg.Attributes.repeatDur value
    "requiredExtensions" -> Just <| Svg.Attributes.requiredExtensions value
    "requiredFeatures" -> Just <| Svg.Attributes.requiredFeatures value
    "restart" -> Just <| Svg.Attributes.restart value
    "result" -> Just <| Svg.Attributes.result value
    "rotate" -> Just <| Svg.Attributes.rotate value
    "rx" -> Just <| Svg.Attributes.rx value
    "ry" -> Just <| Svg.Attributes.ry value
    "scale" -> Just <| Svg.Attributes.scale value
    "seed" -> Just <| Svg.Attributes.seed value
    "slope" -> Just <| Svg.Attributes.slope value
    "spacing" -> Just <| Svg.Attributes.spacing value
    "specularConstant" -> Just <| Svg.Attributes.specularConstant value
    "specularExponent" -> Just <| Svg.Attributes.specularExponent value
    "speed" -> Just <| Svg.Attributes.speed value
    "spreadMethod" -> Just <| Svg.Attributes.spreadMethod value
    "startOffset" -> Just <| Svg.Attributes.startOffset value
    "stdDeviation" -> Just <| Svg.Attributes.stdDeviation value
    "stemh" -> Just <| Svg.Attributes.stemh value
    "stemv" -> Just <| Svg.Attributes.stemv value
    "stitchTiles" -> Just <| Svg.Attributes.stitchTiles value
    "strikethrough-position" -> Just <| Svg.Attributes.strikethroughPosition value
    "strikethrough-thickness" -> Just <| Svg.Attributes.strikethroughThickness value
    "string" -> Just <| Svg.Attributes.string value
    "style" -> Just <| Svg.Attributes.style value
    "surfaceScale" -> Just <| Svg.Attributes.surfaceScale value
    "systemLanguage" -> Just <| Svg.Attributes.systemLanguage value
    "tableValues" -> Just <| Svg.Attributes.tableValues value
    "target" -> Just <| Svg.Attributes.target value
    "targetX" -> Just <| Svg.Attributes.targetX value
    "targetY" -> Just <| Svg.Attributes.targetY value
    "textLength" -> Just <| Svg.Attributes.textLength value
    "title" -> Just <| Svg.Attributes.title value
    "to" -> Just <| Svg.Attributes.to value
    "transform" -> Just <| Svg.Attributes.transform value
    "type'" -> Just <| Svg.Attributes.type' value
    "u1" -> Just <| Svg.Attributes.u1 value
    "u2" -> Just <| Svg.Attributes.u2 value
    "underline-position" -> Just <| Svg.Attributes.underlinePosition value
    "underline-thickness" -> Just <| Svg.Attributes.underlineThickness value
    "unicode" -> Just <| Svg.Attributes.unicode value
    "unicode-range" -> Just <| Svg.Attributes.unicodeRange value
    "units-per-em" -> Just <| Svg.Attributes.unitsPerEm value
    "v-alphabetic" -> Just <| Svg.Attributes.vAlphabetic value
    "v-hanging" -> Just <| Svg.Attributes.vHanging value
    "v-ideographic" -> Just <| Svg.Attributes.vIdeographic value
    "v-mathematical" -> Just <| Svg.Attributes.vMathematical value
    "values" -> Just <| Svg.Attributes.values value
    "version" -> Just <| Svg.Attributes.version value
    "vert-adv-y" -> Just <| Svg.Attributes.vertAdvY value
    "vert-origin-x" -> Just <| Svg.Attributes.vertOriginX value
    "vert-origin-y" -> Just <| Svg.Attributes.vertOriginY value
    "viewBox" -> Just <| Svg.Attributes.viewBox value
    "viewTarget" -> Just <| Svg.Attributes.viewTarget value
    "width" -> Just <| Svg.Attributes.width value
    "widths" -> Just <| Svg.Attributes.widths value
    "x" -> Just <| Svg.Attributes.x value
    "x-height" -> Just <| Svg.Attributes.xHeight value
    "x1" -> Just <| Svg.Attributes.x1 value
    "x2" -> Just <| Svg.Attributes.x2 value
    "xChannelSelector" -> Just <| Svg.Attributes.xChannelSelector value
    "xlink:actuate" -> Just <| Svg.Attributes.xlinkActuate value
    "xlink:arcrole" -> Just <| Svg.Attributes.xlinkArcrole value
    "xlink:href" -> Just <| Svg.Attributes.xlinkHref value
    "xlink:role" -> Just <| Svg.Attributes.xlinkRole value
    "xlink:show" -> Just <| Svg.Attributes.xlinkShow value
    "xlink:title" -> Just <| Svg.Attributes.xlinkTitle value
    "xlink:type" -> Just <| Svg.Attributes.xlinkType value
    "xml:base" -> Just <| Svg.Attributes.xmlBase value
    "xml:lang" -> Just <| Svg.Attributes.xmlLang value
    "xml:space" -> Just <| Svg.Attributes.xmlSpace value
    "y" -> Just <| Svg.Attributes.y value
    "y1" -> Just <| Svg.Attributes.y1 value
    "y2" -> Just <| Svg.Attributes.y2 value
    "yChannelSelector" -> Just <| Svg.Attributes.yChannelSelector value
    "z" -> Just <| Svg.Attributes.z value
    "zoomAndPan" -> Just <| Svg.Attributes.zoomAndPan value
    "alignment-baseline" -> Just <| Svg.Attributes.alignmentBaseline value
    "baseline-shift" -> Just <| Svg.Attributes.baselineShift value
    "clip-path" -> Just <| Svg.Attributes.clipPath value
    "clip-rule" -> Just <| Svg.Attributes.clipRule value
    "clip" -> Just <| Svg.Attributes.clip value
    "color-interpolation-filters" -> Just <| Svg.Attributes.colorInterpolationFilters value
    "color-interpolation" -> Just <| Svg.Attributes.colorInterpolation value
    "color-profile" -> Just <| Svg.Attributes.colorProfile value
    "color-rendering" -> Just <| Svg.Attributes.colorRendering value
    "color" -> Just <| Svg.Attributes.color value
    "cursor" -> Just <| Svg.Attributes.cursor value
    "direction" -> Just <| Svg.Attributes.direction value
    "display" -> Just <| Svg.Attributes.display value
    "dominant-baseline" -> Just <| Svg.Attributes.dominantBaseline value
    "enable-background" -> Just <| Svg.Attributes.enableBackground value
    "fill-opacity" -> Just <| Svg.Attributes.fillOpacity value
    "fill-rule" -> Just <| Svg.Attributes.fillRule value
    "fill" -> Just <| Svg.Attributes.fill value
    "filter" -> Just <| Svg.Attributes.filter value
    "flood-color" -> Just <| Svg.Attributes.floodColor value
    "flood-opacity" -> Just <| Svg.Attributes.floodOpacity value
    "font-family" -> Just <| Svg.Attributes.fontFamily value
    "font-size-adjust" -> Just <| Svg.Attributes.fontSizeAdjust value
    "font-size" -> Just <| Svg.Attributes.fontSize value
    "font-stretch" -> Just <| Svg.Attributes.fontStretch value
    "font-style" -> Just <| Svg.Attributes.fontStyle value
    "font-variant" -> Just <| Svg.Attributes.fontVariant value
    "fontweight" -> Just <| Svg.Attributes.fontWeight value
    "glyph-orientation-horizontal" -> Just <| Svg.Attributes.glyphOrientationHorizontal value
    "glyph-orientation-vertical" -> Just <| Svg.Attributes.glyphOrientationVertical value
    "image-rendering" -> Just <| Svg.Attributes.imageRendering value
    "kerning" -> Just <| Svg.Attributes.kerning value
    "letter-spacing" -> Just <| Svg.Attributes.letterSpacing value
    "lighting-color" -> Just <| Svg.Attributes.lightingColor value
    "marker-end" -> Just <| Svg.Attributes.markerEnd value
    "marker-mid" -> Just <| Svg.Attributes.markerMid value
    "marker-start" -> Just <| Svg.Attributes.markerStart value
    "mask" -> Just <| Svg.Attributes.mask value
    "opacity" -> Just <| Svg.Attributes.opacity value
    "overflow" -> Just <| Svg.Attributes.overflow value
    "pointer-events" -> Just <| Svg.Attributes.pointerEvents value
    "shape-rendering" -> Just <| Svg.Attributes.shapeRendering value
    "stop-color" -> Just <| Svg.Attributes.stopColor value
    "stop-opacity" -> Just <| Svg.Attributes.stopOpacity value
    "stroke-dasharray" -> Just <| Svg.Attributes.strokeDasharray value
    "stroke-dashoffset" -> Just <| Svg.Attributes.strokeDashoffset value
    "stroke-linecap" -> Just <| Svg.Attributes.strokeLinecap value
    "stroke-linejoin" -> Just <| Svg.Attributes.strokeLinejoin value
    "stroke-miterlimit" -> Just <| Svg.Attributes.strokeMiterlimit value
    "stroke-opacity" -> Just <| Svg.Attributes.strokeOpacity value
    "stroke-width" -> Just <| Svg.Attributes.strokeWidth value
    "stroke" -> Just <| Svg.Attributes.stroke value
    "text-anchor" -> Just <| Svg.Attributes.textAnchor value
    "text-decoration" -> Just <| Svg.Attributes.textDecoration value
    "text-rendering" -> Just <| Svg.Attributes.textRendering value
    "unicode-bidi" -> Just <| Svg.Attributes.unicodeBidi value
    "visibility" -> Just <| Svg.Attributes.visibility value
    "word-spacing" -> Just <| Svg.Attributes.wordSpacing value
    "writing-mode" -> Just <| Svg.Attributes.writingMode value
    _ -> Nothing