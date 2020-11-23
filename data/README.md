# Metadata

## Cliamte.csv

This dataset containes the E-OBS cliamte data used in the driver analysis. The raw data is available from https://www.ecad.eu//download/millennium/millennium.php

Columns:

- id: station id
- year: year of the observation
- annual: annual averages
- winter: winter averages
- summer: summer averaged
- country: country of station
- station: station name
- index: climate index

## fao.csv

This dataset containes estimated wood extraction volumes from the FAOSTAT database. The raw data is available from http://www.fao.org/faostat/en/#data/FO

Columns:

- country: country of estimate
- year: year of estimate
- volume: estimated timber extraction volume (m3)
- stock: estimated standing stock (m3)
- volume_rate: rate of wood extraction (volume/stock)

## grey_lit.csv

This dataset containes grey-literature estimates of bark beetle and wind salvage fellings. The data is from Schelhaas, M. J., et al. (2003). Natural disturbances in the European forests in the 19th and 20th centuries. Global Change Biology 9(11): 1620-1633.

Columns:

- country: country of estimate
- year: year of estimate
- volume: estimated salvage volume (m3)
- agent: disturbance agent
- stock: estimated standing stock (m3)
- volume_rate: rate of salvage (volume/stock)

## hansen_fc.RData

This dataset contains forest cover estimates for Central Europe from the year 2000 extracted from Hansen, M. C., et al. (2013). High-resolution global maps of 21st-century forest cover change. Science 342(6160): 850-853. The data is used for validating the stand-replacing/non-stand-replacing mortality categories.

## icp.csv

This dataset contains the aggregated ICP data. The data is from Neumann, M., et al. (2017). Climate variability drives recent tree mortality in Europe. Global Change Biology 23(11): 4788-4797.

Columns:

- plotid: id of plot
- treeid: id of tree
- year: year of measurement
- species: species of the tree
- dead: whether tree is dead (1) or alive (0)
- age: age of tree
- country: country of measurement

## structure.csv

This dataset contains structural information used in the driver analysis. The data is from Seidl, R., et al. (2014). Increasing forest disturbances in Europe and their impact on carbon storage. Nature Climate Change 4(9): 806-810; Seidl, R., et al. (2011). Unraveling the drivers of intensifying forest disturbance regimes in Europe. Global Change Biology 17(9): 2842-2852; and Vilén, T., et al. (2012). Reconstructed forest age structure in Europe 1950–2010. Forest Ecology and Management 286: 203-218.

Columns:

- country: country of estimate
- year: year of estimate
- growing.stock: growing stock in 1000 m3
- median.age: median age of growing stock in years
- older100: share of growing stock older 100 year
- conifer: share of growing stock consisting of conifer species

## timesync.csv

TimeSync data collected for 24,000 plots across six countries in Central Europe.

Columns:

- project_code: country of the plot
- plotid: plotid
- image_year: the year of the vertex
- image_julday: the julian date of the vertex
- dominant_landuse: The land use of each vertex (i.e., other or forest)
- dominant_landcover: The land cover of each vertex (i.e., treed, non-treed)
- change_process: Change process desribing the change segment (i.e., stable, harvest, wind, decline, growth/recovery) **NOTE:** The agent attribution is based on a best guess and is NOT reliable!
- change_process_notes: notes taken during interpretation
- interpreter: interpreter code
