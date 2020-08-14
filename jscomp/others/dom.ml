type _baseClass

type animation (* Web Animations API *)

(* TODO: Should we bother with this indirection?
(* core *)
type domString = string
type domTimestamp = float
*)

(* css *)
type cssStyleDeclaration
type cssStyleSheet

(* events (early) *)
type 'a eventTarget_like
type eventTarget = _baseClass eventTarget_like

(* nodes *)
type 'a _node
type 'a node_like = 'a _node eventTarget_like
type node = _baseClass node_like
type _attr
type attr = _attr node_like
type 'a _characterData
type 'a characterData_like = 'a _characterData node_like
type characterData = _baseClass characterData_like
type _cdataSection
type cdataSection = _cdataSection characterData_like
type _comment
type comment = _comment characterData_like
type 'a _document
type 'a document_like = 'a _document node_like
type document = _baseClass document_like
type _documentFragment
type documentFragment = _documentFragment node_like
type _documentType
type documentType = _documentType node_like
type domImplementation
type 'a _element
type 'a element_like = 'a _element node_like
type element = _baseClass element_like
type htmlCollection
type htmlFormControlsCollection
type htmlOptionsCollection
type intersectionObserver
type intersectionObserverEntry
type mutationObserver
type mutationRecord
type performanceObserver
type performanceObserverEntryList
type reportingObserver
type reportingObserverOptions
type resizeObserver
type resizeObserverEntry
type namedNodeMap
type nodeList
type radioNodeList
type processingInstruction
type _shadowRoot
type shadowRoot = _shadowRoot node_like
type _text
type text = _text characterData_like

(* geometry *)
type domRect

(* html *)
type dataTransfer (* Drag and Drop API *)
type domStringMap
type history
type _htmlDocument
type htmlDocument = _htmlDocument document_like
type 'a _htmlElement
type 'a htmlElement_like = 'a _htmlElement element_like
type htmlElement = _baseClass htmlElement_like
type _htmlAnchorElement
type htmlAnchorElement = _htmlAnchorElement htmlElement_like
type _htmlAreaElement
type htmlAreaElement = _htmlAreaElement htmlElement_like
type _htmlAudioElement
type htmlAudioElement = _htmlAudioElement htmlElement_like
type _htmlBaseElement
type htmlBaseElement = _htmlBaseElement htmlElement_like
type _htmlBodyElement
type htmlBodyElement = _htmlBodyElement htmlElement_like
type _htmlBrElement
type htmlBrElement = _htmlBrElement htmlElement_like
type _htmlButtonElement
type htmlButtonElement = _htmlButtonElement htmlElement_like
type _htmlCanvasElement
type htmlCanvasElement = _htmlCanvasElement htmlElement_like
type _htmlDataElement
type htmlDataElement = _htmlDataElement htmlElement_like
type _htmlDataListElement
type htmlDataListElement = _htmlDataListElement htmlElement_like
type _htmlDialogElement
type htmlDialogElement = _htmlDialogElement htmlElement_like
type _htmlDivElement
type htmlDivElement = _htmlDivElement htmlElement_like
type _htmlDlistElement
type htmlDlistElement = _htmlDlistElement htmlElement_like
type _htmlEmbedElement
type htmlEmbedElement = _htmlEmbedElement htmlElement_like
type _htmlFieldSetElement
type htmlFieldSetElement = _htmlFieldSetElement htmlElement_like
type _htmlFormElement
type htmlFormElement = _htmlFormElement htmlElement_like
type _htmlHeadElement
type htmlHeadElement = _htmlHeadElement htmlElement_like
type _htmlHeadingElement
type htmlHeadingElement = _htmlHeadingElement htmlElement_like
type _htmlHrElement
type htmlHrElement = _htmlHrElement htmlElement_like
type _htmlHtmlElement
type htmlHtmlElement = _htmlHtmlElement htmlElement_like
type _htmlIframeElement
type htmlIframeElement = _htmlIframeElement htmlElement_like
type _htmlImageElement
type htmlImageElement = _htmlImageElement htmlElement_like
type _htmlInputElement
type htmlInputElement = _htmlInputElement htmlElement_like
type _htmlLabelElement
type htmlLabelElement = _htmlLabelElement htmlElement_like
type _htmlLegendElement
type htmlLegendElement = _htmlLegendElement htmlElement_like
type _htmlLiElement
type htmlLiElement = _htmlLiElement htmlElement_like
type _htmlLinkElement
type htmlLinkElement = _htmlLinkElement htmlElement_like
type _htmlMapElement
type htmlMapElement = _htmlMapElement htmlElement_like
type _htmlMediaElement
type htmlMediaElement = _htmlMediaElement htmlElement_like
type _htmlMenuElement
type htmlMenuElement = _htmlMenuElement htmlElement_like
type _htmlMetaElement
type htmlMetaElement = _htmlMetaElement htmlElement_like
type _htmlMeterElement
type htmlMeterElement = _htmlMeterElement htmlElement_like
type _htmlModElement
type htmlModElement = _htmlModElement htmlElement_like
type _htmlOListElement
type htmlOListElement = _htmlOListElement htmlElement_like
type _htmlObjectElement
type htmlObjectElement = _htmlObjectElement htmlElement_like
type _htmlOptGroupElement
type htmlOptGroupElement = _htmlOptGroupElement htmlElement_like
type _htmlOptionElement
type htmlOptionElement = _htmlOptionElement htmlElement_like
type _htmlOutputElement
type htmlOutputElement = _htmlOutputElement htmlElement_like
type _htmlParagraphElement
type htmlParagraphElement = _htmlParagraphElement htmlElement_like
type _htmlParamElement
type htmlParamElement = _htmlParamElement htmlElement_like
type _htmlPreElement
type htmlPreElement = _htmlPreElement htmlElement_like
type _htmlProgressElement
type htmlProgressElement = _htmlProgressElement htmlElement_like
type _htmlQuoteElement
type htmlQuoteElement = _htmlQuoteElement htmlElement_like
type _htmlScriptElement
type htmlScriptElement = _htmlScriptElement htmlElement_like
type _htmlSelectElement
type htmlSelectElement = _htmlSelectElement htmlElement_like
type _htmlSlotElement
type htmlSlotElement = _htmlSlotElement htmlElement_like
type _htmlSourceElement
type htmlSourceElement = _htmlSourceElement htmlElement_like
type _htmlSpanElement
type htmlSpanElement = _htmlSpanElement htmlElement_like
type _htmlStyleElement
type htmlStyleElement = _htmlStyleElement htmlElement_like
type _htmlTableCaptionElement
type htmlTableCaptionElement = _htmlTableCaptionElement htmlElement_like
type _htmlTableCellElement
type htmlTableCellElement = _htmlTableCellElement htmlElement_like
type _htmlTableColElement
type htmlTableColElement = _htmlTableColElement htmlElement_like
type _htmlTableDataCellElement
type htmlTableDataCellElement = _htmlTableDataCellElement htmlElement_like
type _htmlTableElement
type htmlTableElement = _htmlTableElement htmlElement_like
type _htmlTableHeaderCellElement
type htmlTableHeaderCellElement = _htmlTableHeaderCellElement htmlElement_like
type _htmlTableRowElement
type htmlTableRowElement = _htmlTableRowElement htmlElement_like
type _htmlTableSectionElement
type htmlTableSectionElement = _htmlTableSectionElement htmlElement_like
type _htmlTextAreaElement
type htmlTextAreaElement = _htmlTextAreaElement htmlElement_like
type _htmlTimeElement
type htmlTimeElement = _htmlTimeElement htmlElement_like
type _htmlTitleElement
type htmlTitleElement = _htmlTitleElement htmlElement_like
type _htmlTrackElement
type htmlTrackElement = _htmlTrackElement htmlElement_like
type _htmlUlistElement
type htmlUlistElement = _htmlUlistElement htmlElement_like
type _htmlUnknownElement
type htmlUnknownElement = _htmlUnknownElement htmlElement_like
type _htmlVideoElement
type htmlVideoElement = _htmlVideoElement htmlElement_like
type location
type window
type _xmlDocument
type xmlDocument = _xmlDocument document_like

(* events *)
type 'a event_like
type event = _baseClass event_like
type 'a _uiEvent
type 'a uiEvent_like = 'a _uiEvent event_like
type uiEvent = _baseClass uiEvent_like
type _animationEvent
type animationEvent = _animationEvent event_like
type _beforeUnloadEvent
type beforeUnloadEvent = _beforeUnloadEvent event_like
type _clipboardEvent
type clipboardEvent = _clipboardEvent event_like
type _closeEvent
type closeEvent = _closeEvent event_like
type _compositionEvent
type compositionEvent = _compositionEvent uiEvent_like
type _customEvent
type customEvent = _customEvent event_like
type _dragEvent
type dragEvent = _dragEvent event_like
type _errorEvent
type errorEvent = _errorEvent event_like
type _focusEvent
type focusEvent = _focusEvent uiEvent_like
type _idbVersionChangeEvent
type idbVersionChangeEvent = _idbVersionChangeEvent event_like
type _inputEvent
type inputEvent = _inputEvent uiEvent_like
type _keyboardEvent
type keyboardEvent = _keyboardEvent uiEvent_like
type 'a _mouseEvent
type 'a mouseEvent_like = 'a _mouseEvent uiEvent_like
type mouseEvent = _baseClass mouseEvent_like
type _pageTransitionEvent
type pageTransitionEvent = _pageTransitionEvent event_like
type _pointerEvent
type pointerEvent = _pointerEvent mouseEvent_like
type _popStateEvent
type popStateEvent = _popStateEvent event_like
type _progressEvent
type progressEvent = _progressEvent event_like
type _relatedEvent
type relatedEvent = _relatedEvent event_like
type _storageEvent
type storageEvent = _storageEvent event_like
type _svgZoomEvent
type svgZoomEvent = _svgZoomEvent event_like
type _timeEvent
type timeEvent = _timeEvent event_like
type _touchEvent
type touchEvent = _touchEvent uiEvent_like
type _trackEvent
type trackEvent = _trackEvent event_like
type _transitionEvent
type transitionEvent = _transitionEvent event_like
type _webGlContextEvent
type webGlContextEvent = _webGlContextEvent event_like
type _wheelEvent
type wheelEvent = _wheelEvent uiEvent_like

(* ranges *)
type range

(* selection (TODO: move out of dom?) *)
type selection

(* sets *)
type domTokenList
type domSettableTokenList

(* traversal *)
type nodeFilter = {
  acceptNode: element -> int (* return type should be NodeFilter.action, but that would create a cycle *)
}
type nodeIterator
type treeWalker

(* SVG *)
type svgRect
type svgPoint

(* special *)
type eventPointerId


module Storage = Dom_storage
module Storage2 = Dom_storage2
