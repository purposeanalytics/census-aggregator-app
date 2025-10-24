function toggleLegend() {
  csdLegend = document.getElementById('csd-legend');
  ctLegend = document.getElementById('ct-legend');
  ridingLegend = document.getElementByID('riding-legend');

  Shiny.addCustomMessageHandler('aggregate_area', function (aggregate_area_for_legend) {
    if (aggregate_area_for_legend == 'csd') {
      csdLegend.style.display = 'block';
      ctLegend.style.display = 'none';
      ridingLegend.display = 'none';
    } else if (aggregate_area_for_legend == 'ct') {
      ctLegend.style.display = 'block';
      csdLegend.style.display = 'none';
      ridingLegend.display = 'none';
    } else if(aggregate_area_for_legend == 'ridings') {
      ctLegend.style.display = 'none';
      csdLegend.style.display = 'none';
      ridingLegend.display = 'block';

    }
  })
}
