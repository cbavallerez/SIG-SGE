// Make sure the Shiny connection is established
$(document).on('shiny:connected', function(event) {
  /********** NON-REACTIVE DOM MANIPULATION **********/
  // Detect input change and change UI to reflect that
  var prevText = '';
  $(document).on('shiny:inputchanged', function(event) {
    if (event.name === 'mimapa_marker_click' && event.value !== prevText) {
      prevText = event.value;
      $('#establecimiento_seleccionado').value = "666"
    }
  });
});