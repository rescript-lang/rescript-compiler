'use strict';

var Curry = require("../../lib/js/curry.js");
var React = require("react");
var ReasonReactOptimizedCreateClass = require("./reasonReactOptimizedCreateClass.js");

function createDomElement(s, props, children) {
  var vararg = [
      s,
      props
    ].concat(children);
  return React.createElement.apply(null, vararg);
}

function anyToUnit(param) {
  
}

function anyToTrue(param) {
  return true;
}

function willReceivePropsDefault(param) {
  return param.state;
}

function renderDefault(_self) {
  return "RenderNotImplemented";
}

function initialStateDefault(param) {
  
}

function reducerDefault(_action, _state) {
  return /* NoUpdate */0;
}

function convertPropsIfTheyreFromJs(props, jsPropsToReason, debugName) {
  var match = props.reasonProps;
  if (!(match == null)) {
    return match;
  }
  if (jsPropsToReason !== undefined) {
    return /* Element */{
            _0: jsPropsToReason(props)
          };
  }
  throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "A JS component called the Reason component " + (debugName + " which didn't implement the JS->Reason React props conversion."),
        Error: new Error()
      };
}

function createClass(debugName) {
  return ReasonReactOptimizedCreateClass.createClass({
              displayName: debugName,
              subscriptions: null,
              self: (function (state, retainedProps) {
                  var $$this = this ;
                  return {
                          handle: $$this.handleMethod,
                          state,
                          retainedProps,
                          send: $$this.sendMethod,
                          onUnmount: $$this.onUnmountMethod
                        };
                }),
              getInitialState: (function () {
                  var thisJs = this;
                  var convertedReasonProps = convertPropsIfTheyreFromJs(thisJs.props, thisJs.jsPropsToReason, debugName);
                  return {
                          reasonState: Curry._1(convertedReasonProps._0.initialState, undefined)
                        };
                }),
              componentDidMount: (function () {
                  var $$this = this ;
                  var thisJs = this;
                  var convertedReasonProps = convertPropsIfTheyreFromJs(thisJs.props, thisJs.jsPropsToReason, debugName);
                  var component = convertedReasonProps._0;
                  var curTotalState = thisJs.state;
                  var curReasonState = curTotalState.reasonState;
                  var self = $$this.self(curReasonState, component.retainedProps);
                  if (component.didMount !== anyToUnit) {
                    return Curry._1(component.didMount, self);
                  }
                  
                }),
              componentDidUpdate: (function (prevProps, prevState) {
                  var $$this = this ;
                  var thisJs = this;
                  var curState = thisJs.state;
                  var curReasonState = curState.reasonState;
                  var newJsProps = thisJs.props;
                  var newConvertedReasonProps = convertPropsIfTheyreFromJs(newJsProps, thisJs.jsPropsToReason, debugName);
                  var newComponent = newConvertedReasonProps._0;
                  if (newComponent.didUpdate === anyToUnit) {
                    return ;
                  }
                  var oldConvertedReasonProps = prevProps === newJsProps ? newConvertedReasonProps : convertPropsIfTheyreFromJs(prevProps, thisJs.jsPropsToReason, debugName);
                  var prevReasonState = prevState.reasonState;
                  var newSelf = $$this.self(curReasonState, newComponent.retainedProps);
                  var oldSelf_handle = newSelf.handle;
                  var oldSelf_retainedProps = oldConvertedReasonProps._0.retainedProps;
                  var oldSelf_send = newSelf.send;
                  var oldSelf_onUnmount = newSelf.onUnmount;
                  var oldSelf = {
                    handle: oldSelf_handle,
                    state: prevReasonState,
                    retainedProps: oldSelf_retainedProps,
                    send: oldSelf_send,
                    onUnmount: oldSelf_onUnmount
                  };
                  return Curry._1(newComponent.didUpdate, {
                              oldSelf,
                              newSelf
                            });
                }),
              componentWillUnmount: (function () {
                  var $$this = this ;
                  var thisJs = this;
                  var convertedReasonProps = convertPropsIfTheyreFromJs(thisJs.props, thisJs.jsPropsToReason, debugName);
                  var component = convertedReasonProps._0;
                  var curState = thisJs.state;
                  var curReasonState = curState.reasonState;
                  if (component.willUnmount !== anyToUnit) {
                    Curry._1(component.willUnmount, $$this.self(curReasonState, component.retainedProps));
                  }
                  var subs = $$this.subscriptions;
                  if (subs !== null) {
                    subs.forEach(function (unsubscribe) {
                          return Curry._1(unsubscribe, undefined);
                        });
                    return ;
                  }
                  
                }),
              componentWillUpdate: (function (nextProps, nextState) {
                  var $$this = this ;
                  var thisJs = this;
                  var newConvertedReasonProps = convertPropsIfTheyreFromJs(nextProps, thisJs.jsPropsToReason, debugName);
                  var newComponent = newConvertedReasonProps._0;
                  if (newComponent.willUpdate === anyToUnit) {
                    return ;
                  }
                  var oldJsProps = thisJs.props;
                  var oldConvertedReasonProps = nextProps === oldJsProps ? newConvertedReasonProps : convertPropsIfTheyreFromJs(oldJsProps, thisJs.jsPropsToReason, debugName);
                  var curState = thisJs.state;
                  var curReasonState = curState.reasonState;
                  var nextReasonState = nextState.reasonState;
                  var newSelf = $$this.self(nextReasonState, newComponent.retainedProps);
                  var oldSelf_handle = newSelf.handle;
                  var oldSelf_retainedProps = oldConvertedReasonProps._0.retainedProps;
                  var oldSelf_send = newSelf.send;
                  var oldSelf_onUnmount = newSelf.onUnmount;
                  var oldSelf = {
                    handle: oldSelf_handle,
                    state: curReasonState,
                    retainedProps: oldSelf_retainedProps,
                    send: oldSelf_send,
                    onUnmount: oldSelf_onUnmount
                  };
                  return Curry._1(newComponent.willUpdate, {
                              oldSelf,
                              newSelf
                            });
                }),
              componentWillReceiveProps: (function (nextProps) {
                  var $$this = this ;
                  var thisJs = this;
                  var newConvertedReasonProps = convertPropsIfTheyreFromJs(nextProps, thisJs.jsPropsToReason, debugName);
                  var newComponent = newConvertedReasonProps._0;
                  if (newComponent.willReceiveProps === willReceivePropsDefault) {
                    return ;
                  }
                  var oldJsProps = thisJs.props;
                  var oldConvertedReasonProps = nextProps === oldJsProps ? newConvertedReasonProps : convertPropsIfTheyreFromJs(oldJsProps, thisJs.jsPropsToReason, debugName);
                  var oldComponent = oldConvertedReasonProps._0;
                  return thisJs.setState((function (curTotalState, param) {
                                var curReasonState = curTotalState.reasonState;
                                var oldSelf = $$this.self(curReasonState, oldComponent.retainedProps);
                                var nextReasonState = Curry._1(newComponent.willReceiveProps, oldSelf);
                                if (nextReasonState !== curTotalState) {
                                  return {
                                          reasonState: nextReasonState
                                        };
                                } else {
                                  return curTotalState;
                                }
                              }), null);
                }),
              shouldComponentUpdate: (function (nextJsProps, nextState, param) {
                  var $$this = this ;
                  var thisJs = this;
                  var curJsProps = thisJs.props;
                  var oldConvertedReasonProps = convertPropsIfTheyreFromJs(thisJs.props, thisJs.jsPropsToReason, debugName);
                  var newConvertedReasonProps = nextJsProps === curJsProps ? oldConvertedReasonProps : convertPropsIfTheyreFromJs(nextJsProps, thisJs.jsPropsToReason, debugName);
                  var newComponent = newConvertedReasonProps._0;
                  var nextReasonState = nextState.reasonState;
                  var newSelf = $$this.self(nextReasonState, newComponent.retainedProps);
                  if (newComponent.shouldUpdate === anyToTrue) {
                    return true;
                  }
                  var curState = thisJs.state;
                  var curReasonState = curState.reasonState;
                  var oldSelf_handle = newSelf.handle;
                  var oldSelf_retainedProps = oldConvertedReasonProps._0.retainedProps;
                  var oldSelf_send = newSelf.send;
                  var oldSelf_onUnmount = newSelf.onUnmount;
                  var oldSelf = {
                    handle: oldSelf_handle,
                    state: curReasonState,
                    retainedProps: oldSelf_retainedProps,
                    send: oldSelf_send,
                    onUnmount: oldSelf_onUnmount
                  };
                  return Curry._1(newComponent.shouldUpdate, {
                              oldSelf,
                              newSelf
                            });
                }),
              onUnmountMethod: (function (subscription) {
                  var $$this = this ;
                  var subs = $$this.subscriptions;
                  if (subs !== null) {
                    subs.push(subscription);
                  } else {
                    $$this.subscriptions = [subscription];
                  }
                  
                }),
              handleMethod: (function (callback) {
                  var $$this = this ;
                  var thisJs = this;
                  return function (callbackPayload) {
                    var curState = thisJs.state;
                    var curReasonState = curState.reasonState;
                    var convertedReasonProps = convertPropsIfTheyreFromJs(thisJs.props, thisJs.jsPropsToReason, debugName);
                    return Curry._2(callback, callbackPayload, $$this.self(curReasonState, convertedReasonProps._0.retainedProps));
                  };
                }),
              sendMethod: (function (action) {
                  var $$this = this ;
                  var thisJs = this;
                  var convertedReasonProps = convertPropsIfTheyreFromJs(thisJs.props, thisJs.jsPropsToReason, debugName);
                  var component = convertedReasonProps._0;
                  if (component.reducer === reducerDefault) {
                    return ;
                  }
                  var sideEffects = {
                    contents: (function (prim) {
                        
                      })
                  };
                  var partialStateApplication = Curry._1(component.reducer, action);
                  return thisJs.setState((function (curTotalState, param) {
                                var curReasonState = curTotalState.reasonState;
                                var reasonStateUpdate = Curry._1(partialStateApplication, curReasonState);
                                if (reasonStateUpdate === /* NoUpdate */0) {
                                  return null;
                                }
                                var nextTotalState;
                                if (typeof reasonStateUpdate === "number") {
                                  nextTotalState = curTotalState;
                                } else {
                                  switch (reasonStateUpdate.TAG | 0) {
                                    case /* Update */0 :
                                        nextTotalState = {
                                          reasonState: reasonStateUpdate._0
                                        };
                                        break;
                                    case /* SideEffects */1 :
                                        sideEffects.contents = reasonStateUpdate._0;
                                        nextTotalState = curTotalState;
                                        break;
                                    case /* UpdateWithSideEffects */2 :
                                        sideEffects.contents = reasonStateUpdate._1;
                                        nextTotalState = {
                                          reasonState: reasonStateUpdate._0
                                        };
                                        break;
                                    
                                  }
                                }
                                if (nextTotalState !== curTotalState) {
                                  return nextTotalState;
                                } else {
                                  return null;
                                }
                              }), $$this.handleMethod(function (param, self) {
                                  return Curry._1(sideEffects.contents, self);
                                }));
                }),
              render: (function () {
                  var $$this = this ;
                  var thisJs = this;
                  var convertedReasonProps = convertPropsIfTheyreFromJs(thisJs.props, thisJs.jsPropsToReason, debugName);
                  var created = convertedReasonProps._0;
                  var curState = thisJs.state;
                  var curReasonState = curState.reasonState;
                  return Curry._1(created.render, $$this.self(curReasonState, created.retainedProps));
                })
            });
}

function basicComponent(debugName) {
  return {
          debugName,
          reactClassInternal: createClass(debugName),
          handedOffState: {
            contents: undefined
          },
          willReceiveProps: willReceivePropsDefault,
          didMount: anyToUnit,
          didUpdate: anyToUnit,
          willUnmount: anyToUnit,
          willUpdate: anyToUnit,
          shouldUpdate: anyToTrue,
          render: renderDefault,
          initialState: initialStateDefault,
          retainedProps: undefined,
          reducer: reducerDefault,
          jsElementWrapped: undefined
        };
}

var statelessComponent = basicComponent;

var statelessComponentWithRetainedProps = basicComponent;

var reducerComponent = basicComponent;

var reducerComponentWithRetainedProps = basicComponent;

function element(keyOpt, refOpt, component) {
  var key = keyOpt !== undefined ? keyOpt : undefined;
  var ref = refOpt !== undefined ? refOpt : undefined;
  var element$1 = /* Element */{
    _0: component
  };
  var jsElementWrapped = component.jsElementWrapped;
  if (jsElementWrapped !== undefined) {
    return Curry._2(jsElementWrapped, key, ref);
  } else {
    return React.createElement(component.reactClassInternal, {
                key,
                ref,
                reasonProps: element$1
              });
  }
}

function wrapReasonForJs(component, jsPropsToReason) {
  var uncurriedJsPropsToReason = Curry.__1(jsPropsToReason);
  component.reactClassInternal.prototype.jsPropsToReason = uncurriedJsPropsToReason;
  return component.reactClassInternal;
}

var dummyInteropComponent = basicComponent("interop");

function wrapJsForReason(reactClass, props, children) {
  var jsElementWrapped = (function (param, param$1) {
      var props$1 = Object.assign(Object.assign({}, props), {
            ref: param$1,
            key: param
          });
      var varargs = [
          reactClass,
          props$1
        ].concat(children);
      return React.createElement.apply(null, varargs);
    });
  return {
          debugName: dummyInteropComponent.debugName,
          reactClassInternal: dummyInteropComponent.reactClassInternal,
          handedOffState: dummyInteropComponent.handedOffState,
          willReceiveProps: dummyInteropComponent.willReceiveProps,
          didMount: dummyInteropComponent.didMount,
          didUpdate: dummyInteropComponent.didUpdate,
          willUnmount: dummyInteropComponent.willUnmount,
          willUpdate: dummyInteropComponent.willUpdate,
          shouldUpdate: dummyInteropComponent.shouldUpdate,
          render: dummyInteropComponent.render,
          initialState: dummyInteropComponent.initialState,
          retainedProps: dummyInteropComponent.retainedProps,
          reducer: dummyInteropComponent.reducer,
          jsElementWrapped
        };
}

var Router;

exports.statelessComponent = statelessComponent;
exports.statelessComponentWithRetainedProps = statelessComponentWithRetainedProps;
exports.reducerComponent = reducerComponent;
exports.reducerComponentWithRetainedProps = reducerComponentWithRetainedProps;
exports.element = element;
exports.wrapReasonForJs = wrapReasonForJs;
exports.createDomElement = createDomElement;
exports.wrapJsForReason = wrapJsForReason;
exports.Router = Router;
/* dummyInteropComponent Not a pure module */
