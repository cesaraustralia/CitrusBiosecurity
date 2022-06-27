library(terra)
library(sf)

sf::st_layers("C:/Users/61423/Desktop/Geopackage_2016_CLDA_for_AUST/census2016_clda_aus_short.gpkg")
cens <- sf::st_read("C:/Users/61423/Desktop/Geopackage_2016_CLDA_for_AUST/census2016_clda_aus_short.gpkg", layer = "census2016_clda_aus_poa_short")

r <- rast("data/bio01_aus.tif")
terra::setMinMax(r)
plot(r)


plot(cens$geom)


chpop <- terra::rasterize(x = terra::vect(cens),
                          y = r,
                          field = "Chinese_Tot_Resp",
                          fun = "mean",
                          filename = "data/chinese_total.tif",
                          overwrite = TRUE)

plot(chpop)
