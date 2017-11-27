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
type mutationObserver
type mutationRecord
type namedNodeMap
type nodeList
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
type _htmlSlotElement
type htmlSlotElement = _htmlSlotElement htmlElement_like
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