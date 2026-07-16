// Scroll preservation for the scale map editor.
//
// Every button in the editor funnels through Shiny.setInputValue, and the
// server answers by removing and re-inserting the editor's whole contents
// (see R/scale-map-editor.R, render_editor()). While the container sits
// empty the browser clamps the scroll offset of whatever ancestor scrolls
// the editor, so adding a level or a variable throws the sidebar back to
// the top. Record the offset at click time, before the rebuild, and put it
// back once the replacement nodes are in.

(function () {
  "use strict";

  var MSG = "blockr.theme-scale-map-scroll";

  var pending = null;

  // The editor has no overflow of its own; an ancestor scrolls it (the board
  // options sidebar body, as things stand). Find it by walking up rather than
  // naming its class, so this stays independent of the surrounding UI package.
  function scrollParent(el) {
    for (var node = el.parentElement; node; node = node.parentElement) {
      var overflow = window.getComputedStyle(node).overflowY;
      if (
        (overflow === "auto" || overflow === "scroll") &&
        node.scrollHeight > node.clientHeight
      ) {
        return node;
      }
    }
    return null;
  }

  function saveScroll(editorId) {
    var editor = document.getElementById(editorId);
    var scroller = editor && scrollParent(editor);
    pending = scroller ? { scroller: scroller, top: scroller.scrollTop } : null;
  }

  // Shiny requires a handler of arity one, hence the unused argument.
  function restoreScroll(_msg) {
    var target = pending;
    pending = null;
    if (!target) {
      return;
    }
    // The insert is already applied by the time this message is handled, but
    // the colour inputs bind and take up their height afterwards. Wait a frame
    // so the scroll height is final, otherwise the offset gets clamped again.
    window.requestAnimationFrame(function () {
      target.scroller.scrollTop = target.top;
    });
  }

  window.blockrScaleMapEditor = { saveScroll: saveScroll };

  Shiny.addCustomMessageHandler(MSG, restoreScroll);
})();
