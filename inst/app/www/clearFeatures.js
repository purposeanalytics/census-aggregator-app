function clearFeatures(e, geography) {
    // Set filter to empty, clear fill
    Shiny.setInputValue(geography + '_polygon_filter', '');
}
