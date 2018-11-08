library(tigris)
library(sp)
library(geojsonio)
library(cartogram)
library(leaflet)
library(scales)

# help from https://rstudio.github.io/leaflet/choropleths.html
# and https://rstudio.github.io/leaflet/json.html
# for the cartogram, check out the dev's github: https://github.com/sjewo/cartogram
# In future versions I may want to futz with the leaflet background
# that will be based on this post: https://cran.r-project.org/web/packages/parlitools/vignettes/using-cartograms.html

# load JSON data
# data was found here: https://github.com/deldersveld/topojson/blob/master/countries/us-states/OR-41-oregon-counties.json
OR_counties <- topojson_read("./OR-41-oregon-counties.json")

# load table for election results
# source was here: http://results.oregonvotes.gov/ResultsExport.aspx
# I selected "Excel" format and then clicked a county.This summarized governor results across all counties
# Note I had to do a lot of data cleanup here, which I did outside R
# Also note I got population data from 2017 estimates: https://www.pdx.edu/prc/population-reports-estimates
pops_votes <- read.table("./governoronly_voteData_20181107_11am_fixed.csv", sep = ",", header = TRUE)

# make color palette with divergence:
# dark red (<50% of vote) to white (50%) dark blue (>50% )
votesPal <- colorNumeric(c('dark red','white','dark blue'),
                         domain = pops_votes$BrownvBuehler)

# use geojsoinio to merge my dataframe with topojson object
counties_merged <- geo_join(OR_counties, pops_votes, "NAME" , "County")

# Cartogram to adjust for population size
counties_merged_tr <- cartogram_ncont(counties_merged, weight = "pop_2016")

# make pop-ups
hover_info <- paste0(counties_merged$NAME, " County", '\n',
                     as.character(round(counties_merged$BrownvBuehler, 3)*100), "%")
popup_info <- paste0("Brown: ", as.character(comma(counties_merged$Kate.Brown)), "\n",
                     "Buehler: ", as.character(comma(counties_merged$Knute.Buehler)), "\n",
                     "Other: ", as.character(comma(counties_merged$Other)), "\n",
                     "Population: ", as.character(comma(counties_merged$pop_2016))
                     )

# now the way supported for geojsonio work
leaflet() %>%
  addTiles(
    # focused on outdoors. has major raods. also has forests, shading for evelation and such
    urlTemplate = "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}",
    attribution = 'Maps by <a href="http://server.arcgisonline.com/ArcGIS/rest/services">esri</a>',
    group = "Simple"
  ) %>%
  setView(lng = -121.2, lat = 44, zoom = 6.5) %>%
  addPolygons(data = counties_merged,
              fillColor = ~votesPal(counties_merged$BrownvBuehler),
              fillOpacity = 0.7,
              weight = 0.5,
              color = "black",
              smoothFactor = 0.2,
              popup=~popup_info,
              label=~hover_info,
              group = "Area") %>%
  addPolygons(data = counties_merged_tr,
              fillColor = ~votesPal(counties_merged_tr$BrownvBuehler),
              fillOpacity = 0.7,
              weight = 0.5,
              color = "black",
              smoothFactor = 0.2,
              popup=~popup_info,
              label=~hover_info,
              group = "Population") %>%
  addLegend(pal = votesPal,
            values = counties_merged$BrownvBuehler,
            position = "bottomright",
            title = "Voted for Brown") %>%
  addLayersControl(baseGroups = c("Area", "Population"),
                   options = layersControlOptions(collapsed = TRUE, position = "bottomright"))