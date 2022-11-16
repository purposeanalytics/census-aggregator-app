function polygonDrawControl(map) {

  var draw = new MapboxDraw({
    displayControlsDefault: false,
    controls: {
      polygon: true
    },
    defaultMode: 'draw_polygon'
  });

  // When the selection tool changes
  Shiny.addCustomMessageHandler('selection_tool', function (message) {

    if (message === 'polygon') {

      // If the map does not already have a drawing control
      if (!(map._draw)) {
        map.addControl(draw);
      }

      map._draw = true; // Flag drawing control for above check

      // Clear everything any time the aggregate area changes - by removing and adding the control back on
      // This ensures that the polygon is deleted and the features are cleared
      Shiny.addCustomMessageHandler('aggregate_area', function (message) {
        map.removeControl(draw);
        map.addControl(draw);
      })

      // Also just do this when 'reset' button is clicked - easiest way to reset everything
      Shiny.addCustomMessageHandler('reset', function (message) {
        map.removeControl(draw);
        map.addControl(draw);
      })

      map.on('draw.create', (e) => getFeaturesFromPolygon(e, map, 'csd'));
      map.on('draw.update', (e) => getFeaturesFromPolygon(e, map, 'csd'));
      map.on('draw.delete', (e) => clearFeatures(e, 'csd'));
      // Only allows drawing one polygon at a time - clears existing polygon and features if you go to draw a new one
      map.on('draw.modechange', (e) => clearPolygonAndFeatures(e, map, 'csd', draw));

      map.on('draw.create', (e) => getFeaturesFromPolygon(e, map, 'ct'));
      map.on('draw.update', (e) => getFeaturesFromPolygon(e, map, 'ct'));
      map.on('draw.delete', (e) => clearFeatures(e, 'ct'));
      map.on('draw.modechange', (e) => clearPolygonAndFeatures(e, map, 'ct', draw));
    }

    // Remove drawing control and set to false if the selection tool is click
    if (message === 'click' && map._draw) {
      map.removeControl(draw);
      map._draw = false;
    }

  })
}
