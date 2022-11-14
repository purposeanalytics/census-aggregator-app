function toggleLegend() {
  csdLegend = document.getElementById('csd-legend');
  ctLegend = document.getElementById('ct-legend');

  Shiny.addCustomMessageHandler('aggregate_area', function (aggregate_area_for_legend) {
    if (aggregate_area_for_legend == 'csd') {
      csdLegend.style.display = 'block';
      ctLegend.style.display = 'none';
    } else if (aggregate_area_for_legend = 'ct') {
      ctLegend.style.display = 'block';
      csdLegend.style.display = 'none';
    }
  })
}
