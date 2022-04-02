---
output: 
  html_document: 
    toc: no
---

## Run Order
* You can run `main.R` to run all of the import and tidy scripts. 
* __OR__ you can run `main-import.R` and `main-tidy.R` separate (which is what `main.R` calls).
* Run order for files in `scripts`

### Mandatory

#### IMPORT
* `import/01_historical-ucr-data.R` -- downloads the historical UCR data (2009-May 2018) as well as the remaining 2018 data that uses the NIBRS method.
* `import/02_current-nibrs-data.R` -- downloads NIBRS data (2019-present); requires manually validation and updating
* `import/03_non-crime-data.R` -- downloads police beats and districts (map), major roads, and a few other items

#### TIDY
* `tidy/01_crime-data_combine.R` -- read all crime data files into single table
* `tidy/02_crime-data_cleanse.R` -- resolve anomalies within data (categories, etc)
* `tidy/03_grided-population-data_extract.R` -- use grided population data to estimate police beat population
  * :exclamation: :exclamation: __There is a missing step here -- the script requires a GPW_TX file (texas portion of the GPW grided data set)__
  * I don't know where that script is located
  * I'll do my best to find solution (dropboxing the file or wedging in a new file)
* `tidy/06_maps_simplify.R` -- simplify the police beats
* `tidy/07_leaflet-data_setup.R` -- produce datasets to feed directly into leaflet (in the app). You will need to mainly copy and paste the new datasets into the HoustonCrimeViewer `/data/` folder.

### Optional

* `import/0x_311-data.R` -- 311 call data (big)
* `import/0x_wapo-data.R` -- Washington Post homicide data

### Incomplete

* `import/db/*` -- load data into PostgreSQL

## Items of Interest
* [HPD Premise Codes](https://www.houstontx.gov/police/cs/beatpages/premise.htm)

## Similar Projects
* [Analyzing When and Where San Francisco Criminal Arrests Occur Using R and ggplot2](https://github.com/minimaxir/sf-arrests-when-where/blob/master/crime_data_sf.ipynb)

![](https://raw.githubusercontent.com/minimaxir/sf-arrests-when-where/fae38132108bd248f369682e361666f5750a307c/sf-arrest-when-2.png)

![](https://raw.githubusercontent.com/minimaxir/sf-arrests-when-where/fae38132108bd248f369682e361666f5750a307c/sf-arrest-when-4.png)

## Other Data Sources
These are potential data sources.

* [Washington Post Homicide Tracker](https://raw.githubusercontent.com/washingtonpost/data-homicides/master/homicide-data.csv)
* [AskTED](http://tea4avholly.tea.state.tx.us/tea.askted.web/Forms/Home.aspx)
    * __School districts can be downloaded [here](http://schoolsdata2-tea-texas.opendata.arcgis.com/datasets/e115fed14c0f4ca5b942dc3323626b1c_0)__
    * [Current schools](http://schoolsdata2-tea-texas.opendata.arcgis.com/datasets/059432fd0dcb4a208974c235e837c94f_0) offers point location of every regular, charter, and alternative school in Texas. EXCLUDES displinary, juvenile, and WIndham school districts and institutions
    * An interesting [idea from city-data.com](http://www.city-data.com/forum/houston/1927474-public-school-district-boundaries-houston-tx.html)
* [City-Data.com](http://www.city-data.com/city/Houston-Texas.html)
    * A lot of ideas here
* [Open Data Houston](http://data.houstontx.gov/dataset)
    * A variety of datasets
* [Houston Public Works](http://www.gims.houstontx.gov/PortalWS/MainPortal.aspx)
    * Floodplain data
* [Planet OSM](http://planet.openstreetmap.org/)
    * OpenStreetMap data
    * [Downloading OSM data wiki](https://wiki.openstreetmap.org/wiki/Downloading_data)
    * __WAY TOO LARGE TO DOWNLOAD__
    * __USE THIS INSTEAD: [OpenStreetMap Data Extracts](http://download.geofabrik.de/)__
        * Small, manageable extracts of OSM data
        * [Format description PDF](http://download.geofabrik.de/osm-data-in-gis-formats-free.pdf)
* [PUBLIC 311 Location](http://cohgis-mycity.opendata.arcgis.com/datasets/public-311-location)
    * A different location for the same 311 data (may be more up-to-date)
