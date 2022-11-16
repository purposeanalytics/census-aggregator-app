# CensusAggregator

<a href = "https://censusaggregator.ca" target = "blank_">CensusAggregator</a> makes it easier to retrieve and aggregate common census variables for regions that cover multiple census geographic areas. Follow the steps below to create a custom area on the map and download a summary report, data file, and boundary file for that area. CensusAggregator uses data from the 2021 Canadian census.

To serve the app locally, clone this repository and restore the dependencies as prompted, using `renv::restore()`, then run the app:

```r
library(censusaggregatorapp)

run_app()
```

## About

CensusAggregator was developed by <a href = "https://purposeanalytics.ca" target = "_blank">Purpose Analytics</a> in collaboration with <a href = "https://sharla.online" target = "_blank">Sharla Gelfand</a> and Mountain Math, the creator of <a href = "https://censusmapper.ca" target = "_blank">CensusMapper</a>.

Data from CensusAggregator is adapted from Statistics Canada's 2021 Census of Population and may be freely used, reproduced, published, and distributed under the <a href = "https://www.statcan.gc.ca/eng/reference/licence" target = "_blank">Statistics Canada Open Data Licence</a>. Statistics Canada must be acknowledged as the source when using the data; acknowledgement of CensusAggregator is optional. The geographic boundaries have undergone some processing and may differ from the boundary files originally released by Statistics Canada. Statistics Canada randomly rounds to the nearest 0 or 5 - this can impact the precision of percentages shown in CensusAggregator.

The project team recognizes that the Canadian census presents complex challenges around the inclusion and representation of Indigenous communities. This includes technical issues related to incomplete enumeration of some First Nations reserves and settlements and high non-response rates in remote areas as well as fundamental issues related to Indigenous data sovereignty. We further acknowledge that the lands depicted in CensusAggregator are the traditional lands of First Nations, Métis, and Inuit peoples and that the boundaries that we display are colonial instruments that continue to dominate public narratives. More information about the portrayal of Indigenous peoples in the 2021 Canadian census can be found in Statistics Canada's <a href = "https://www12.statcan.gc.ca/census-recensement/2021/ref/98-500/009/98-500-x2021009-eng.cfm" target = "_blank">Indigenous Peoples Reference Guide</a>.

This initial release CensusAggregator is provided as a stable prototype which means that it serves core functionality but that it has not been optimized for speed or high-volume traffic. This release was built using the R programming language and the source code is freely available in this repository.

CensusAggregator is part of Purpose Analytics' commitment to develop products and services that return a public benefit. Purpose Analytics is a non-profit that supports other non-profits and charities to use data more effectively. CensusAggregator is funded entirely through surplus revenue from Purpose Analytics service fees.
