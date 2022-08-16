function highlightGeographyOnHover(map) {
  let hoveredGeoUid = null;

  highlightSpecificGeographyOnHover(map, hoveredGeoUid, "csd")
  highlightSpecificGeographyOnHover(map, hoveredGeoUid, "ct")

}

function highlightSpecificGeographyOnHover(map, hoveredGeoUid, geography) {
  // When the user moves their mouse over the csd_line_hover or ct_line_hover layer, we'll update the
  // feature state for the feature under the mouse.
  // Note that the mouse is observed on csd_fill_click and ct_fill_click, which are fill layers, so it's observed when their mouse is inside the geography
  // But the layer that is actually updated is csd_line_hover or ct_line_hover, in order to just update the border of the geography
  map.on('mousemove', geography + '_fill_click', (e) => {
    map.getCanvas().style.cursor = 'pointer';
    if (e.features.length > 0) {
      if (hoveredGeoUid !== null) {
        map.setFeatureState(
          { source: '2016_' + geography, sourceLayer: '2016_census_' + geography, id: hoveredGeoUid },
          { hover: false }
        );
      }
      hoveredGeoUid = e.features[0].id;
      map.setFeatureState(
        { source: '2016_' + geography, sourceLayer: '2016_census_' + geography, id: hoveredGeoUid },
        { hover: true }
      );
    }
  });
  // When the mouse leaves the csd_fill_click layer, update the feature state of the
  // previously hovered feature.
  map.on('mouseleave', geography + '_fill_click', () => {
    if (hoveredGeoUid !== null) {
      map.setFeatureState(
        { source: '2016_' + geography, sourceLayer: '2016_census_' + geography, id: hoveredGeoUid },
        { hover: false }
      );
    }
    hoveredGeoUid = null;
    // Reset the cursor style
    map.getCanvas().style.cursor = '';
  })
}
