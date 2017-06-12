var ReactDOM = require('react-dom');
var React = require('react');

// Import a ReasonReact component! `comp` is the exposed, underlying ReactJS class
var PageReason = require('../../lib/js/src/interop/pageReason').comp;

var App = React.createClass({
  render: function() {
    return React.createElement('div', null,
      React.createElement(PageReason, {message: 'Hello!'})
    );
    // didn't feel like dragging in Babel. Here's the equivalent JSX:
    // <div><PageReason message="Hello!"></div>
  }
});

ReactDOM.render(React.createElement(App), document.getElementById('index'));
