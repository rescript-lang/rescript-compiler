open SharedTypes

let domLabels =
  let bool = "bool" in
  let float = "float" in
  let int = "int" in
  let string = "string" in
  [
    ("ariaDetails", string);
    ("ariaDisabled", bool);
    ("ariaHidden", bool);
    ("ariaKeyshortcuts", string);
    ("ariaLabel", string);
    ("ariaRoledescription", string);
    ("ariaExpanded", bool);
    ("ariaLevel", int);
    ("ariaModal", bool);
    ("ariaMultiline", bool);
    ("ariaMultiselectable", bool);
    ("ariaPlaceholder", string);
    ("ariaReadonly", bool);
    ("ariaRequired", bool);
    ("ariaSelected", bool);
    ("ariaSort", string);
    ("ariaValuemax", float);
    ("ariaValuemin", float);
    ("ariaValuenow", float);
    ("ariaValuetext", string);
    ("ariaAtomic", bool);
    ("ariaBusy", bool);
    ("ariaRelevant", string);
    ("ariaGrabbed", bool);
    ("ariaActivedescendant", string);
    ("ariaColcount", int);
    ("ariaColindex", int);
    ("ariaColspan", int);
    ("ariaControls", string);
    ("ariaDescribedby", string);
    ("ariaErrormessage", string);
    ("ariaFlowto", string);
    ("ariaLabelledby", string);
    ("ariaOwns", string);
    ("ariaPosinset", int);
    ("ariaRowcount", int);
    ("ariaRowindex", int);
    ("ariaRowspan", int);
    ("ariaSetsize", int);
    ("defaultChecked", bool);
    ("defaultValue", string);
    ("accessKey", string);
    ("className", string);
    ("contentEditable", bool);
    ("contextMenu", string);
    ("dir", string);
    ("draggable", bool);
    ("hidden", bool);
    ("id", string);
    ("lang", string);
    ("style", "style");
    ("spellCheck", bool);
    ("tabIndex", int);
    ("title", string);
    ("itemID", string);
    ("itemProp", string);
    ("itemRef", string);
    ("itemScope", bool);
    ("itemType", string);
    ("accept", string);
    ("acceptCharset", string);
    ("action", string);
    ("allowFullScreen", bool);
    ("alt", string);
    ("async", bool);
    ("autoComplete", string);
    ("autoCapitalize", string);
    ("autoFocus", bool);
    ("autoPlay", bool);
    ("challenge", string);
    ("charSet", string);
    ("checked", bool);
    ("cite", string);
    ("crossOrigin", string);
    ("cols", int);
    ("colSpan", int);
    ("content", string);
    ("controls", bool);
    ("coords", string);
    ("data", string);
    ("dateTime", string);
    ("default", bool);
    ("defer", bool);
    ("disabled", bool);
    ("download", string);
    ("encType", string);
    ("form", string);
    ("formAction", string);
    ("formTarget", string);
    ("formMethod", string);
    ("headers", string);
    ("height", string);
    ("high", int);
    ("href", string);
    ("hrefLang", string);
    ("htmlFor", string);
    ("httpEquiv", string);
    ("icon", string);
    ("inputMode", string);
    ("integrity", string);
    ("keyType", string);
    ("label", string);
    ("list", string);
    ("loop", bool);
    ("low", int);
    ("manifest", string);
    ("max", string);
    ("maxLength", int);
    ("media", string);
    ("mediaGroup", string);
    ("method", string);
    ("min", string);
    ("minLength", int);
    ("multiple", bool);
    ("muted", bool);
    ("name", string);
    ("nonce", string);
    ("noValidate", bool);
    ("open_", bool);
    ("optimum", int);
    ("pattern", string);
    ("placeholder", string);
    ("playsInline", bool);
    ("poster", string);
    ("preload", string);
    ("radioGroup", string);
    ("readOnly", bool);
    ("rel", string);
    ("required", bool);
    ("reversed", bool);
    ("rows", int);
    ("rowSpan", int);
    ("sandbox", string);
    ("scope", string);
    ("scoped", bool);
    ("scrolling", string);
    ("selected", bool);
    ("shape", string);
    ("size", int);
    ("sizes", string);
    ("span", int);
    ("src", string);
    ("srcDoc", string);
    ("srcLang", string);
    ("srcSet", string);
    ("start", int);
    ("step", float);
    ("summary", string);
    ("target", string);
    ("type_", string);
    ("useMap", string);
    ("value", string);
    ("width", string);
    ("wrap", string);
    ("onCopy", "ReactEvent.Clipboard.t => unit");
    ("onCut", "ReactEvent.Clipboard.t => unit");
    ("onPaste", "ReactEvent.Clipboard.t => unit");
    ("onCompositionEnd", "ReactEvent.Composition.t => unit");
    ("onCompositionStart", "ReactEvent.Composition.t => unit");
    ("onCompositionUpdate", "ReactEvent.Composition.t => unit");
    ("onKeyDown", "ReactEvent.Keyboard.t => unit");
    ("onKeyPress", "ReactEvent.Keyboard.t => unit");
    ("onKeyUp", "ReactEvent.Keyboard.t => unit");
    ("onFocus", "ReactEvent.Focus.t => unit");
    ("onBlur", "ReactEvent.Focus.t => unit");
    ("onChange", "ReactEvent.Form.t => unit");
    ("onInput", "ReactEvent.Form.t => unit");
    ("onSubmit", "ReactEvent.Form.t => unit");
    ("onInvalid", "ReactEvent.Form.t => unit");
    ("onClick", "ReactEvent.Mouse.t => unit");
    ("onContextMenu", "ReactEvent.Mouse.t => unit");
    ("onDoubleClick", "ReactEvent.Mouse.t => unit");
    ("onDrag", "ReactEvent.Mouse.t => unit");
    ("onDragEnd", "ReactEvent.Mouse.t => unit");
    ("onDragEnter", "ReactEvent.Mouse.t => unit");
    ("onDragExit", "ReactEvent.Mouse.t => unit");
    ("onDragLeave", "ReactEvent.Mouse.t => unit");
    ("onDragOver", "ReactEvent.Mouse.t => unit");
    ("onDragStart", "ReactEvent.Mouse.t => unit");
    ("onDrop", "ReactEvent.Mouse.t => unit");
    ("onMouseDown", "ReactEvent.Mouse.t => unit");
    ("onMouseEnter", "ReactEvent.Mouse.t => unit");
    ("onMouseLeave", "ReactEvent.Mouse.t => unit");
    ("onMouseMove", "ReactEvent.Mouse.t => unit");
    ("onMouseOut", "ReactEvent.Mouse.t => unit");
    ("onMouseOver", "ReactEvent.Mouse.t => unit");
    ("onMouseUp", "ReactEvent.Mouse.t => unit");
    ("onSelect", "ReactEvent.Selection.t => unit");
    ("onTouchCancel", "ReactEvent.Touch.t => unit");
    ("onTouchEnd", "ReactEvent.Touch.t => unit");
    ("onTouchMove", "ReactEvent.Touch.t => unit");
    ("onTouchStart", "ReactEvent.Touch.t => unit");
    ("onPointerOver", "ReactEvent.Pointer.t => unit");
    ("onPointerEnter", "ReactEvent.Pointer.t => unit");
    ("onPointerDown", "ReactEvent.Pointer.t => unit");
    ("onPointerMove", "ReactEvent.Pointer.t => unit");
    ("onPointerUp", "ReactEvent.Pointer.t => unit");
    ("onPointerCancel", "ReactEvent.Pointer.t => unit");
    ("onPointerOut", "ReactEvent.Pointer.t => unit");
    ("onPointerLeave", "ReactEvent.Pointer.t => unit");
    ("onGotPointerCapture", "ReactEvent.Pointer.t => unit");
    ("onLostPointerCapture", "ReactEvent.Pointer.t => unit");
    ("onScroll", "ReactEvent.UI.t => unit");
    ("onWheel", "ReactEvent.Wheel.t => unit");
    ("onAbort", "ReactEvent.Media.t => unit");
    ("onCanPlay", "ReactEvent.Media.t => unit");
    ("onCanPlayThrough", "ReactEvent.Media.t => unit");
    ("onDurationChange", "ReactEvent.Media.t => unit");
    ("onEmptied", "ReactEvent.Media.t => unit");
    ("onEncrypetd", "ReactEvent.Media.t => unit");
    ("onEnded", "ReactEvent.Media.t => unit");
    ("onError", "ReactEvent.Media.t => unit");
    ("onLoadedData", "ReactEvent.Media.t => unit");
    ("onLoadedMetadata", "ReactEvent.Media.t => unit");
    ("onLoadStart", "ReactEvent.Media.t => unit");
    ("onPause", "ReactEvent.Media.t => unit");
    ("onPlay", "ReactEvent.Media.t => unit");
    ("onPlaying", "ReactEvent.Media.t => unit");
    ("onProgress", "ReactEvent.Media.t => unit");
    ("onRateChange", "ReactEvent.Media.t => unit");
    ("onSeeked", "ReactEvent.Media.t => unit");
    ("onSeeking", "ReactEvent.Media.t => unit");
    ("onStalled", "ReactEvent.Media.t => unit");
    ("onSuspend", "ReactEvent.Media.t => unit");
    ("onTimeUpdate", "ReactEvent.Media.t => unit");
    ("onVolumeChange", "ReactEvent.Media.t => unit");
    ("onWaiting", "ReactEvent.Media.t => unit");
    ("onAnimationStart", "ReactEvent.Animation.t => unit");
    ("onAnimationEnd", "ReactEvent.Animation.t => unit");
    ("onAnimationIteration", "ReactEvent.Animation.t => unit");
    ("onTransitionEnd", "ReactEvent.Transition.t => unit");
    ("accentHeight", string);
    ("accumulate", string);
    ("additive", string);
    ("alignmentBaseline", string);
    ("allowReorder", string);
    ("alphabetic", string);
    ("amplitude", string);
    ("arabicForm", string);
    ("ascent", string);
    ("attributeName", string);
    ("attributeType", string);
    ("autoReverse", string);
    ("azimuth", string);
    ("baseFrequency", string);
    ("baseProfile", string);
    ("baselineShift", string);
    ("bbox", string);
    ("bias", string);
    ("by", string);
    ("calcMode", string);
    ("capHeight", string);
    ("clip", string);
    ("clipPath", string);
    ("clipPathUnits", string);
    ("clipRule", string);
    ("colorInterpolation", string);
    ("colorInterpolationFilters", string);
    ("colorProfile", string);
    ("colorRendering", string);
    ("contentScriptType", string);
    ("contentStyleType", string);
    ("cursor", string);
    ("cx", string);
    ("cy", string);
    ("d", string);
    ("decelerate", string);
    ("descent", string);
    ("diffuseConstant", string);
    ("direction", string);
    ("display", string);
    ("divisor", string);
    ("dominantBaseline", string);
    ("dur", string);
    ("dx", string);
    ("dy", string);
    ("edgeMode", string);
    ("elevation", string);
    ("enableBackground", string);
    ("exponent", string);
    ("externalResourcesRequired", string);
    ("fill", string);
    ("fillOpacity", string);
    ("fillRule", string);
    ("filter", string);
    ("filterRes", string);
    ("filterUnits", string);
    ("floodColor", string);
    ("floodOpacity", string);
    ("focusable", string);
    ("fontFamily", string);
    ("fontSize", string);
    ("fontSizeAdjust", string);
    ("fontStretch", string);
    ("fontStyle", string);
    ("fontVariant", string);
    ("fontWeight", string);
    ("fomat", string);
    ("from", string);
    ("fx", string);
    ("fy", string);
    ("g1", string);
    ("g2", string);
    ("glyphName", string);
    ("glyphOrientationHorizontal", string);
    ("glyphOrientationVertical", string);
    ("glyphRef", string);
    ("gradientTransform", string);
    ("gradientUnits", string);
    ("hanging", string);
    ("horizAdvX", string);
    ("horizOriginX", string);
    ("ideographic", string);
    ("imageRendering", string);
    ("in2", string);
    ("intercept", string);
    ("k", string);
    ("k1", string);
    ("k2", string);
    ("k3", string);
    ("k4", string);
    ("kernelMatrix", string);
    ("kernelUnitLength", string);
    ("kerning", string);
    ("keyPoints", string);
    ("keySplines", string);
    ("keyTimes", string);
    ("lengthAdjust", string);
    ("letterSpacing", string);
    ("lightingColor", string);
    ("limitingConeAngle", string);
    ("local", string);
    ("markerEnd", string);
    ("markerHeight", string);
    ("markerMid", string);
    ("markerStart", string);
    ("markerUnits", string);
    ("markerWidth", string);
    ("mask", string);
    ("maskContentUnits", string);
    ("maskUnits", string);
    ("mathematical", string);
    ("mode", string);
    ("numOctaves", string);
    ("offset", string);
    ("opacity", string);
    ("operator", string);
    ("order", string);
    ("orient", string);
    ("orientation", string);
    ("origin", string);
    ("overflow", string);
    ("overflowX", string);
    ("overflowY", string);
    ("overlinePosition", string);
    ("overlineThickness", string);
    ("paintOrder", string);
    ("panose1", string);
    ("pathLength", string);
    ("patternContentUnits", string);
    ("patternTransform", string);
    ("patternUnits", string);
    ("pointerEvents", string);
    ("points", string);
    ("pointsAtX", string);
    ("pointsAtY", string);
    ("pointsAtZ", string);
    ("preserveAlpha", string);
    ("preserveAspectRatio", string);
    ("primitiveUnits", string);
    ("r", string);
    ("radius", string);
    ("refX", string);
    ("refY", string);
    ("renderingIntent", string);
    ("repeatCount", string);
    ("repeatDur", string);
    ("requiredExtensions", string);
    ("requiredFeatures", string);
    ("restart", string);
    ("result", string);
    ("rotate", string);
    ("rx", string);
    ("ry", string);
    ("scale", string);
    ("seed", string);
    ("shapeRendering", string);
    ("slope", string);
    ("spacing", string);
    ("specularConstant", string);
    ("specularExponent", string);
    ("speed", string);
    ("spreadMethod", string);
    ("startOffset", string);
    ("stdDeviation", string);
    ("stemh", string);
    ("stemv", string);
    ("stitchTiles", string);
    ("stopColor", string);
    ("stopOpacity", string);
    ("strikethroughPosition", string);
    ("strikethroughThickness", string);
    (string, string);
    ("stroke", string);
    ("strokeDasharray", string);
    ("strokeDashoffset", string);
    ("strokeLinecap", string);
    ("strokeLinejoin", string);
    ("strokeMiterlimit", string);
    ("strokeOpacity", string);
    ("strokeWidth", string);
    ("surfaceScale", string);
    ("systemLanguage", string);
    ("tableValues", string);
    ("targetX", string);
    ("targetY", string);
    ("textAnchor", string);
    ("textDecoration", string);
    ("textLength", string);
    ("textRendering", string);
    ("transform", string);
    ("u1", string);
    ("u2", string);
    ("underlinePosition", string);
    ("underlineThickness", string);
    ("unicode", string);
    ("unicodeBidi", string);
    ("unicodeRange", string);
    ("unitsPerEm", string);
    ("vAlphabetic", string);
    ("vHanging", string);
    ("vIdeographic", string);
    ("vMathematical", string);
    ("values", string);
    ("vectorEffect", string);
    ("version", string);
    ("vertAdvX", string);
    ("vertAdvY", string);
    ("vertOriginX", string);
    ("vertOriginY", string);
    ("viewBox", string);
    ("viewTarget", string);
    ("visibility", string);
    ("widths", string);
    ("wordSpacing", string);
    ("writingMode", string);
    ("x", string);
    ("x1", string);
    ("x2", string);
    ("xChannelSelector", string);
    ("xHeight", string);
    ("xlinkActuate", string);
    ("xlinkArcrole", string);
    ("xlinkHref", string);
    ("xlinkRole", string);
    ("xlinkShow", string);
    ("xlinkTitle", string);
    ("xlinkType", string);
    ("xmlns", string);
    ("xmlnsXlink", string);
    ("xmlBase", string);
    ("xmlLang", string);
    ("xmlSpace", string);
    ("y", string);
    ("y1", string);
    ("y2", string);
    ("yChannelSelector", string);
    ("z", string);
    ("zoomAndPan", string);
    ("about", string);
    ("datatype", string);
    ("inlist", string);
    ("prefix", string);
    ("property", string);
    ("resource", string);
    ("typeof", string);
    ("vocab", string);
    ("dangerouslySetInnerHTML", "{\"__html\": string}");
    ("suppressContentEditableWarning", bool);
  ]

(* List and explanations taken from
   https://www.tutorialrepublic.com/html-reference/html5-tags.php. *)
let htmlElements =
  [
    ("a", "Defines a hyperlink.", false);
    ("abbr", "Defines an abbreviated form of a longer word or phrase.", false);
    ("acronym", "Defines an acronym. Use <abbr> instead.", true);
    ("address", "Specifies the author's contact information.", false);
    ( "applet",
      "Embeds a Java applet (mini Java applications) on the page. Use <object> \
       instead.",
      true );
    ("area", "Defines a specific area within an image map.", false);
    ("article", "Defines an article.", false);
    ("aside", "Defines some content loosely related to the page content.", false);
    ("audio", "Embeds a sound, or an audio stream in an HTML document.", false);
    ("b", "Displays text in a bold style.", false);
    ("base", "Defines the base URL for all relative URLs in a document.", false);
    ("basefont", "Specifies the base font for a page. Use CSS instead.", true);
    ( "bdi",
      "Represents text that is isolated from its surrounding for the purposes \
       of bidirectional text formatting.",
      false );
    ("bdo", "Overrides the current text direction.", false);
    ("big", "Displays text in a large size. Use CSS instead.", true);
    ( "blockquote",
      "Represents a section that is quoted from another source.",
      false );
    ("body", "Defines the document's body.", false);
    ("br", "Produces a single line break.", false);
    ("button", "Creates a clickable button.", false);
    ( "canvas",
      "Defines a region in the document, which can be used to draw graphics on \
       the fly via scripting (usually JavaScript).",
      false );
    ("caption", "Defines the caption or title of the table.", false);
    ("center", "Align contents in the center. Use CSS instead.", true);
    ("cite", "Indicates a citation or reference to another source.", false);
    ("code", "Specifies text as computer code.", false);
    ( "col",
      "Defines attribute values for one or more columns in a table.",
      false );
    ("colgroup", "Specifies attributes for multiple columns in a table.", false);
    ( "data",
      "Links a piece of content with a machine-readable translation.",
      false );
    ( "datalist",
      "Represents a set of pre-defined options for an <input> element.",
      false );
    ( "dd",
      "Specifies a description, or value for the term (<dt>) in a description \
       list (<dl>).",
      false );
    ("del", "Represents text that has been deleted from the document.", false);
    ( "details",
      "Represents a widget from which the user can obtain additional \
       information or controls on-demand.",
      false );
    ("dfn", "Specifies a definition.", false);
    ("dialog", "Defines a dialog box or subwindow.", false);
    ("dir", "Defines a directory list. Use <ul> instead.", true);
    ("div", "Specifies a division or a section in a document.", false);
    ("dl", "Defines a description list.", false);
    ("dt", "Defines a term (an item) in a description list.", false);
    ("em", "Defines emphasized text.", false);
    ( "embed",
      "Embeds external application, typically multimedia content like audio or \
       video into an HTML document.",
      false );
    ("fieldset", "Specifies a set of related form fields.", false);
    ("figcaption", "Defines a caption or legend for a figure.", false);
    ("figure", "Represents a figure illustrated as part of the document.", false);
    ("font", "Defines font, color, and size for text. Use CSS instead.", true);
    ("footer", "Represents the footer of a document or a section.", false);
    ("form", "Defines an HTML form for user input.", false);
    ("frame", "Defines a single frame within a frameset.", true);
    ("frameset", "Defines a collection of frames or other frameset.", true);
    ( "head",
      "Defines the head portion of the document that contains information \
       about the document such as title.",
      false );
    ("header", "Represents the header of a document or a section.", false);
    ("hgroup", "Defines a group of headings.", false);
    ("h1", "Defines HTML headings.", false);
    ("h2", "Defines HTML headings.", false);
    ("h3", "Defines HTML headings.", false);
    ("h4", "Defines HTML headings.", false);
    ("h5", "Defines HTML headings.", false);
    ("h6", "Defines HTML headings.", false);
    ("hr", "Produce a horizontal line.", false);
    ("html", "Defines the root of an HTML document.", false);
    ("i", "Displays text in an italic style.", false);
    ("iframe", "Displays a URL in an inline frame.", false);
    ("img", "Represents an image.", false);
    ("input", "Defines an input control.", false);
    ( "ins",
      "Defines a block of text that has been inserted into a document.",
      false );
    ("kbd", "Specifies text as keyboard input.", false);
    ( "keygen",
      "Represents a control for generating a public-private key pair.",
      false );
    ("label", "Defines a label for an <input> control.", false);
    ("legend", "Defines a caption for a <fieldset> element.", false);
    ("li", "Defines a list item.", false);
    ( "link",
      "Defines the relationship between the current document and an external \
       resource.",
      false );
    ("main", "Represents the main or dominant content of the document.", false);
    ("map", "Defines a client-side image-map.", false);
    ("mark", "Represents text highlighted for reference purposes.", false);
    ("menu", "Represents a list of commands.", false);
    ( "menuitem",
      "Defines a list (or menuitem) of commands that a user can perform.",
      false );
    ("meta", "Provides structured metadata about the document content.", false);
    ("meter", "Represents a scalar measurement within a known range.", false);
    ("nav", "Defines a section of navigation links.", false);
    ( "noframes",
      "Defines an alternate content that displays in browsers that do not \
       support frames.",
      true );
    ( "noscript",
      "Defines alternative content to display when the browser doesn't support \
       scripting.",
      false );
    ("object", "Defines an embedded object.", false);
    ("ol", "Defines an ordered list.", false);
    ( "optgroup",
      "Defines a group of related options in a selection list.",
      false );
    ("option", "Defines an option in a selection list.", false);
    ("output", "Represents the result of a calculation.", false);
    ("p", "Defines a paragraph.", false);
    ("param", "Defines a parameter for an object or applet element.", false);
    ("picture", "Defines a container for multiple image sources.", false);
    ("pre", "Defines a block of preformatted text.", false);
    ("progress", "Represents the completion progress of a task.", false);
    ("q", "Defines a short inline quotation.", false);
    ( "rp",
      "Provides fall-back parenthesis for browsers that that don't support \
       ruby annotations.",
      false );
    ( "rt",
      "Defines the pronunciation of character presented in a ruby annotations.",
      false );
    ("ruby", "Represents a ruby annotation.", false);
    ( "s",
      "Represents contents that are no longer accurate or no longer relevant.",
      false );
    ("samp", "Specifies text as sample output from a computer program.", false);
    ( "script",
      "Places script in the document for client-side processing.",
      false );
    ( "section",
      "Defines a section of a document, such as header, footer etc.",
      false );
    ("select", "Defines a selection list within a form.", false);
    ("small", "Displays text in a smaller size.", false);
    ( "source",
      "Defines alternative media resources for the media elements like <audio> \
       or <video>.",
      false );
    ("span", "Defines an inline styleless section in a document.", false);
    ("strike", "Displays text in strikethrough style.", true);
    ("strong", "Indicate strongly emphasized text.", false);
    ( "style",
      "Inserts style information (commonly CSS) into the head of a document.",
      false );
    ("sub", "Defines subscripted text.", false);
    ("summary", "Defines a summary for the <details> element.", false);
    ("sup", "Defines superscripted text.", false);
    ( "svg",
      "Embed SVG (Scalable Vector Graphics) content in an HTML document.",
      false );
    ("table", "Defines a data table.", false);
    ( "tbody",
      "Groups a set of rows defining the main body of the table data.",
      false );
    ("td", "Defines a cell in a table.", false);
    ( "template",
      "Defines the fragments of HTML that should be hidden when the page is \
       loaded, but can be cloned and inserted in the document by JavaScript.",
      false );
    ("textarea", "Defines a multi-line text input control (text area).", false);
    ( "tfoot",
      "Groups a set of rows summarizing the columns of the table.",
      false );
    ("th", "Defines a header cell in a table.", false);
    ( "thead",
      "Groups a set of rows that describes the column labels of a table.",
      false );
    ("time", "Represents a time and/or date.", false);
    ("title", "Defines a title for the document.", false);
    ("tr", "Defines a row of cells in a table.", false);
    ( "track",
      "Defines text tracks for the media elements like <audio> or <video>.",
      false );
    ("tt", "Displays text in a teletype style.", true);
    ("u", "Displays text with an underline.", false);
    ("ul", "Defines an unordered list.", false);
    ("var", "Defines a variable.", false);
    ("video", "Embeds video content in an HTML document.", false);
    ("wbr", "Represents a line break opportunity.", false);
  ]

let getJsxLabels ~componentPath ~findTypeOfValue ~package =
  match componentPath @ ["make"] |> findTypeOfValue with
  | Some (typ, make_env) ->
    let rec getFieldsV3 (texp : Types.type_expr) =
      match texp.desc with
      | Tfield (name, _, t1, t2) ->
        let fields = t2 |> getFieldsV3 in
        if name = "children" then fields else (name, t1, make_env) :: fields
      | Tlink te | Tsubst te | Tpoly (te, []) -> te |> getFieldsV3
      | Tvar None -> []
      | _ -> []
    in
    let getFieldsV4 ~path ~typeArgs =
      match References.digConstructor ~env:make_env ~package path with
      | Some
          ( env,
            {
              item =
                {
                  decl =
                    {
                      type_kind = Type_record (labelDecls, _repr);
                      type_params = typeParams;
                    };
                };
            } ) ->
        labelDecls
        |> List.map (fun (ld : Types.label_declaration) ->
               let name = Ident.name ld.ld_id in
               let t =
                 ld.ld_type |> TypeUtils.instantiateType ~typeParams ~typeArgs
               in
               (name, t, env))
      | _ -> []
    in
    let rec getLabels (t : Types.type_expr) =
      match t.desc with
      | Tlink t1
      | Tsubst t1
      | Tpoly (t1, [])
      | Tconstr (Pident {name = "function$"}, [t1; _], _) ->
        getLabels t1
      | Tarrow
          ( Nolabel,
            {
              desc =
                ( Tconstr (* Js.t *) (_, [{desc = Tobject (tObj, _)}], _)
                | Tobject (tObj, _) );
            },
            _,
            _ ) ->
        (* JSX V3 *)
        getFieldsV3 tObj
      | Tconstr (p, [propsType], _) when Path.name p = "React.component" -> (
        let rec getPropsType (t : Types.type_expr) =
          match t.desc with
          | Tlink t1 | Tsubst t1 | Tpoly (t1, []) -> getPropsType t1
          | Tconstr (path, typeArgs, _) when Path.last path = "props" ->
            Some (path, typeArgs)
          | _ -> None
        in
        match propsType |> getPropsType with
        | Some (path, typeArgs) -> getFieldsV4 ~path ~typeArgs
        | None -> [])
      | Tarrow (Nolabel, {desc = Tconstr (path, typeArgs, _)}, _, _)
        when Path.last path = "props" ->
        (* JSX V4 *)
        getFieldsV4 ~path ~typeArgs
      | Tconstr
          ( clPath,
            [
              {
                desc =
                  ( Tconstr (* Js.t *) (_, [{desc = Tobject (tObj, _)}], _)
                  | Tobject (tObj, _) );
              };
              _;
            ],
            _ )
        when Path.name clPath = "React.componentLike" ->
        (* JSX V3 external or interface *)
        getFieldsV3 tObj
      | Tconstr (clPath, [{desc = Tconstr (path, typeArgs, _)}; _], _)
        when Path.name clPath = "React.componentLike"
             && Path.last path = "props" ->
        (* JSX V4 external or interface *)
        getFieldsV4 ~path ~typeArgs
      | Tarrow (Nolabel, typ, _, _) -> (
        (* Component without the JSX PPX, like a make fn taking a hand-written
           type props. *)
        let rec digToConstr typ =
          match typ.Types.desc with
          | Tlink t1 | Tsubst t1 | Tpoly (t1, []) -> digToConstr t1
          | Tconstr (path, typeArgs, _) when Path.last path = "props" ->
            Some (path, typeArgs)
          | _ -> None
        in
        match digToConstr typ with
        | None -> []
        | Some (path, typeArgs) -> getFieldsV4 ~path ~typeArgs)
      | _ -> []
    in
    typ |> getLabels
  | None -> []

type prop = {
  name: string;
  posStart: int * int;
  posEnd: int * int;
  exp: Parsetree.expression;
}

type jsxProps = {
  compName: Longident.t Location.loc;
  props: prop list;
  childrenStart: (int * int) option;
}

let findJsxPropsCompletable ~jsxProps ~endPos ~posBeforeCursor
    ~firstCharBeforeCursorNoWhite ~charAtCursor ~posAfterCompName =
  let allLabels =
    List.fold_right
      (fun prop allLabels -> prop.name :: allLabels)
      jsxProps.props []
  in
  let beforeChildrenStart =
    match jsxProps.childrenStart with
    | Some childrenPos -> posBeforeCursor < childrenPos
    | None -> posBeforeCursor <= endPos
  in
  let rec loop props =
    match props with
    | prop :: rest ->
      if prop.posStart <= posBeforeCursor && posBeforeCursor < prop.posEnd then (
        if Debug.verbose () then
          print_endline "[jsx_props_completable]--> Cursor on the prop name";

        Some
          (Completable.Cjsx
             ( Utils.flattenLongIdent ~jsx:true jsxProps.compName.txt,
               prop.name,
               allLabels )))
      else if
        prop.posEnd <= posBeforeCursor
        && posBeforeCursor < Loc.start prop.exp.pexp_loc
      then (
        if Debug.verbose () then
          print_endline
            "[jsx_props_completable]--> Cursor between the prop name and expr \
             assigned";
        match (firstCharBeforeCursorNoWhite, prop.exp) with
        | Some '=', {pexp_desc = Pexp_ident {txt = Lident txt}} ->
          if Debug.verbose () then
            Printf.printf
              "[jsx_props_completable]--> Heuristic for empty JSX prop expr \
               completion.\n";
          Some
            (Cexpression
               {
                 contextPath =
                   CJsxPropValue
                     {
                       pathToComponent =
                         Utils.flattenLongIdent ~jsx:true jsxProps.compName.txt;
                       propName = prop.name;
                       emptyJsxPropNameHint = Some txt;
                     };
                 nested = [];
                 prefix = "";
               })
        | _ -> None)
      else if prop.exp.pexp_loc |> Loc.hasPos ~pos:posBeforeCursor then (
        if Debug.verbose () then
          print_endline "[jsx_props_completable]--> Cursor on expr assigned";
        match
          CompletionExpressions.traverseExpr prop.exp ~exprPath:[]
            ~pos:posBeforeCursor ~firstCharBeforeCursorNoWhite
        with
        | Some (prefix, nested) ->
          Some
            (Cexpression
               {
                 contextPath =
                   CJsxPropValue
                     {
                       pathToComponent =
                         Utils.flattenLongIdent ~jsx:true jsxProps.compName.txt;
                       propName = prop.name;
                       emptyJsxPropNameHint = None;
                     };
                 nested = List.rev nested;
                 prefix;
               })
        | _ -> None)
      else if prop.exp.pexp_loc |> Loc.end_ = (Location.none |> Loc.end_) then (
        if Debug.verbose () then
          print_endline "[jsx_props_completable]--> Loc is broken";
        if CompletionExpressions.isExprHole prop.exp then (
          if Debug.verbose () then
            print_endline "[jsx_props_completable]--> Expr was expr hole";
          Some
            (Cexpression
               {
                 contextPath =
                   CJsxPropValue
                     {
                       pathToComponent =
                         Utils.flattenLongIdent ~jsx:true jsxProps.compName.txt;
                       propName = prop.name;
                       emptyJsxPropNameHint = None;
                     };
                 prefix = "";
                 nested = [];
               }))
        else None)
      else if
        rest = [] && beforeChildrenStart && charAtCursor = '>'
        && firstCharBeforeCursorNoWhite = Some '='
      then (
        (* This is a special case for: <SomeComponent someProp=> (completing directly after the '=').
           The completion comes at the end of the component, after the equals sign, but before any
           children starts, and '>' marks that it's at the end of the component JSX.
           This little heuristic makes sure we pick up this special case. *)
        if Debug.verbose () then
          print_endline
            "[jsx_props_completable]--> Special case: last prop, '>' after \
             cursor";
        Some
          (Cexpression
             {
               contextPath =
                 CJsxPropValue
                   {
                     pathToComponent =
                       Utils.flattenLongIdent ~jsx:true jsxProps.compName.txt;
                     propName = prop.name;
                     emptyJsxPropNameHint = None;
                   };
               prefix = "";
               nested = [];
             }))
      else loop rest
    | [] ->
      let afterCompName = posBeforeCursor >= posAfterCompName in
      if afterCompName && beforeChildrenStart then (
        if Debug.verbose () then
          print_endline "[jsx_props_completable]--> Complete for JSX prop name";
        Some
          (Cjsx
             ( Utils.flattenLongIdent ~jsx:true jsxProps.compName.txt,
               "",
               allLabels )))
      else None
  in
  loop jsxProps.props

let extractJsxProps ~(compName : Longident.t Location.loc) ~args =
  let thisCaseShouldNotHappen =
    {
      compName = Location.mknoloc (Longident.Lident "");
      props = [];
      childrenStart = None;
    }
  in
  let rec processProps ~acc args =
    match args with
    | (Asttypes.Labelled "children", {Parsetree.pexp_loc}) :: _ ->
      {
        compName;
        props = List.rev acc;
        childrenStart =
          (if pexp_loc.loc_ghost then None else Some (Loc.start pexp_loc));
      }
    | ((Labelled s | Optional s), (eProp : Parsetree.expression)) :: rest -> (
      let namedArgLoc =
        eProp.pexp_attributes
        |> List.find_opt (fun ({Asttypes.txt}, _) -> txt = "res.namedArgLoc")
      in
      match namedArgLoc with
      | Some ({loc}, _) ->
        processProps
          ~acc:
            ({
               name = s;
               posStart = Loc.start loc;
               posEnd = Loc.end_ loc;
               exp = eProp;
             }
            :: acc)
          rest
      | None -> processProps ~acc rest)
    | _ -> thisCaseShouldNotHappen
  in
  args |> processProps ~acc:[]
