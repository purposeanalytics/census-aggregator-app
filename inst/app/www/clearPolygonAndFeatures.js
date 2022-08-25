// Thank you kindly: https://stackoverflow.com/questions/51073328/allow-drawing-only-one-shape-at-a-time-with-mapbox-gl-draw
function clearPolygonAndFeatures(e, map, geography, draw) {
  // Clear polygon
  const data = draw.getAll();
  if (draw.getMode() == 'draw_polygon') {
    var pids = []

    // ID of the added template empty feature
    const lid = data.features[data.features.length - 1].id

    data.features.forEach((f) => {
      if (f.geometry.type === 'Polygon' && f.id !== lid) {
        pids.push(f.id)
      }
    })
    draw.delete(pids)

    // Set filter to empty, clear fill
    clearFeatures(e, geography);
  }
}
