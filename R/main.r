#############################################
# 3D relief map of Europe in R
# Milos Popovic 2022/06/14
#############################################

# libraries we need
libs <- c(
    "elevatr", "rayshader", "tidyverse", "sf", "giscoR", "jsonlite",
    "httr", "png"
)

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
    install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs, library, character.only = T))

# 1. GET COUNTRY MAP
#-------------------
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"

get_europe_sf <- function(europe_sf, europe_transformed) {
    europe_sf <- giscoR::gisco_get_countries(
        year = "2016", epsg = "4326",
        resolution = "10", region = c("Europe", "Asia")
    )

    europe_transformed <- sf::st_transform(europe_sf, crs = crsLONGLAT)

    return(europe_transformed)
}

europe_transformed <- get_europe_sf()

get_europe_cropped <- function(bbox, bb, eur) {

    # bounding box
    bbox <- st_sfc(
        st_polygon(list(cbind(
            c(-10.5, 48.5, 48.5, -10.5, -10.5),
            c(30.0, 30.0, 69.5, 69.5, 30.0)
        ))),
        crs = crsLONGLAT
    )

    bb <- sf::st_bbox(bbox)

    eur <- sf::st_crop(europe_transformed, bb)

    return(eur)
}

eur <- get_europe_cropped()

get_elevation_data <- function(europe_elevation, elevation_mat) {
    europe_elevation <- elevatr::get_elev_raster(
        locations = eur,
        z = 5, clip = "locations"
    )

    elevation_mat <- rayshader::raster_to_matrix(europe_elevation)
    return(elevation_mat)
}

elevation_mat <- get_elevation_data()

# define query parameters
h <- 2472
w <- 3536

bb <- sf::st_bbox(eur)
type <- "World_Imagery"
file <- NULL
height <- h * 2
width <- w * 2
crs_bb <- 4326

get_satellite_img <- function(url, params, res) {
    url <- parse_url("https://utility.arcgisonline.com/arcgis/rest/services/Utilities/PrintingTools/GPServer/Export%20Web%20Map%20Task/execute")

    # define query
    params <- list(
        baseMap = list(
            baseMapLayers =
                list(
                    list(
                        url = unbox("https://services.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer")
                    )
                )
        ),
        exportOptions = list(outputSize = c(width, height)),
        mapOptions = list(extent = list(
            spatialReference = list(wkid = unbox(crs_bb)),
            xmin = unbox(bb["xmin"]),
            ymin = unbox(bb["ymin"]),
            xmax = unbox(bb["xmax"]),
            ymax = unbox(bb["ymax"])
        ))
    )

    res <- GET(url, query = list(
        f = "json", Format = "PNG32", Layout_Template = "MAP_ONLY",
        Web_Map_as_JSON = toJSON(params)
    ))

    return(res)
}

res <- get_satellite_img()

write_map_png <- function(res_body, img_res, img_bin, file) {
    res_body <- httr::content(res, type = "application/json")
    img_res <- httr::GET(res_body$results[[1]]$value$url)
    img_bin <- httr::content(img_res, "raw")
    file <- paste0(getwd(), "/europe_image.png")
    writeBin(img_bin, file)
}

write_map_png()

get_map_png <- function(img_file, eur_img) {
    img_file <- "europe_image.png"
    eur_img <- png::readPNG(img_file)

    return(eur_img)
}

eur_img <- get_map_png()

# 4. 3D MAP
#---------

elevation_mat %>%
    sphere_shade(texture = "desert") %>%
    add_overlay(eur_img, alphalayer = 0.99) %>%
    plot_3d(elevation_mat,
        zscale = 15, fov = 0, theta = 0, zoom = 0.55, solid = F,
        solidcolor = "white", solidlinecolor = "white", phi = 85,
        shadow_darkness = 0, shadowdepth = 0, shadowwidth = 0,
        windowsize = c(w, h), background = "black"
    )

render_highquality(
    filename = "europe_dem.png", lightintensity = 2500,
    lightaltitude = 90, title_text = "",
    width = w * 2, height = h * 2
)
