function showZoomPopup(map) {
  const constZoomPopup = new mapboxgl.Popup({ closeOnClick: false, closeButton: false, maxWidth: 'none' })
    .setLngLat([-96, 37.8]) // Doesn't matter where this is since with CSS it is in top left corner
    .setHTML('Please zoom in to see geographies.')
    .addTo(map)

  constZoomPopup.addClassName('zoom-popup')

  var zoomPopup = document.getElementsByClassName('zoom-popup')[0];

  // Initialize as not visible
  zoomPopup.style.display = 'none';

  // Listen to zoom end and check for rendered features

  map.on('zoomend', function () {
    var newZoom = map.getZoom();

    ctFeatures = map.queryRenderedFeatures({ layers: ['ct_fill_click'] });
    csdFeatures = map.queryRenderedFeatures({ layers: ['csd_fill_click'] });

    if (ctFeatures.length == 0 & csdFeatures.length == 0) {
      zoomPopup.style.display = '';
    } else {
      zoomPopup.style.display = 'none';
    }
  });

  // Listen to area changing, hide popup when it does and check based on zoom again
  Shiny.addCustomMessageHandler('aggregate_area', function (message) {
    zoomPopup.style.display = 'none';

    var curZoom = map.getZoom();

    if (curZoom < 6 & message == 'ct') {
      zoomPopup.style.display = '';
    } else if (curZoom < 5 & message == 'csd') {
      zoomPopup.style.display = '';
    } else {
      zoomPopup.style.display = 'none';
    }
  });
}
