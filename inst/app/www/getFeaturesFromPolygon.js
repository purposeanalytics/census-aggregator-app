function getFeaturesFromPolygon(e, map, geography) {

  var userPolygon = e.features[0];
  var polygonBoundingBox = turf.bbox(userPolygon)
  var southWest = [polygonBoundingBox[0], polygonBoundingBox[1]];
  var northEast = [polygonBoundingBox[2], polygonBoundingBox[3]];

  var northEastPointPixel = map.project(northEast);
  var southWestPointPixel = map.project(southWest);

  var features = map.queryRenderedFeatures([southWestPointPixel, northEastPointPixel], { layers: [geography + '_fill_click'] });
  var filter = [];

  for (const feature of features) {
    if (!(null === turf.intersect(feature, userPolygon))) {
      filter.push(feature.id);
    }
  }

  Shiny.setInputValue(geography + '_polygon_filter', filter);

};
