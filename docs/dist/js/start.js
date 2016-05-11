hljs.initHighlightingOnLoad();

var toggle_menu_button = $("#menu-toggle")
var wrapper = $("#wrapper");
toggle_menu_button.click(function(e){
    e.preventDefault();
    wrapper.toggleClass("toggled")
})