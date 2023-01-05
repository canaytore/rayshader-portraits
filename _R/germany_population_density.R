# Germany Population Density Map - by @canaytore - data from Kontur

library(sf)
library(tidyverse)
library(tigris)
library(ggplot2)
library(stars)
library(MetBrewer)
library(colorspace)
library(rayshader)
library(scales)

# source -> https://data.humdata.org/dataset/kontur-population-germany
germany <- st_read("kontur_population_DE_20220630.gpkg")

bb <- st_bbox(germany)

bottom_left <- st_point(c(bb[["xmin"]], bb[["ymin"]])) %>%
  st_sfc(crs = st_crs(germany))

bottom_right <- st_point(c(bb[["xmax"]], bb[["ymin"]])) %>%
  st_sfc(crs = st_crs(germany))

width <- st_distance(bottom_left, bottom_right)

top_left <- st_point(c(bb[["xmin"]], bb[["ymax"]])) %>%
  st_sfc(crs = st_crs(germany))

height <- st_distance(bottom_left, top_left)

# handle conditions of width or height being the longer side
if(width > height) {
  w_ratio <- 1
  h_ratio <- as.numeric(height / width)
} else {
  h_ratio <- 1
  w_ratio <- as.numeric(width / height)
} 

# convert to raster so we can then convert to matrix
size <- 5000

germany_rast <- st_rasterize(germany, nx = floor(size * w_ratio), ny = floor(size * h_ratio))

mat <- matrix(germany_rast$population, nrow = floor(size * w_ratio), ncol = floor(size * h_ratio))

# create color pallette
colors <- MetBrewer::met.brewer(name="Demuth")
tx <- grDevices::colorRampPalette(colors, bias = 1.5)(256)
swatchplot(colors)
swatchplot(tx)

# initial graphic
mat %>%
  height_shade(texture = tx) %>%
  add_overlay(sphere_shade(mat, texture = "desert", 
                           zscale=0.5, colorintensity=4), alphalayer=0.5) %>%
  plot_3d(heightmap = mat,
          windowsize = c(1920, 1080),
          background = "#d0c4ae",
          zscale = 30,
          solid = FALSE,
          fov = 0,
          theta = -16, zoom = 0.5, phi = 30,
          shadowdepth = -30,
          shadowcolor = colors[7],
          shadow_darkness = 2)


outfile <- "germany_population_density.png"

{
  start_time <- Sys.time()
  cat(crayon::cyan(start_time), "\n")
  if(!file.exists(outfile)) {
    png::writePNG(matrix(1), target = outfile)
  }
  
  render_highquality (
    filename = outfile,
    interactive = FALSE,
    parallel = TRUE,
    lightdirection = 350,
    lightaltitude = c(20,80),
    lightcolor = c(colors[5], "white"),
    lightintensity = c(800,200),
    samples = 300,
    width = 1920, 
    height = 1080
  )
  
  end_time <- Sys.time()
  diff <- end_time - start_time
  cat(crayon::cyan(diff), "\n")
}


# generate 720 frame AV (60 fps - 12 sec video)
angles = seq(0, 360, length.out = 721)[-1]
for(i in 1:720) {
  render_camera(theta=-45+angles[i])
  render_snapshot(filename = sprintf("germany%i.png", i), 
                  title_text = "Germany Population Density Map 360°",
                  title_bar_color = "#d0c4ae", title_color = colors[1], title_position = "north", title_bar_alpha = 1)
  Sys.sleep(0.1)
}

rgl::rgl.close()

av::av_encode_video(sprintf("germany%d.png",seq(1,720,by=1)), framerate = 60, output = "germany_population_density_360.mp4")

system("ffmpeg -framerate 60 -i germany%d.png -pix_fmt yuv420p germany_population_density_360.mp4")


# adjust annotation
library(magick)
library(glue)

img <- image_read("germany_population_density.png")
text_color <- colors[1]
text_color2 <- colors[2]
text_color3 <- colors[3]

# compute area / population / density
area = round(as.numeric(sum(st_area(germany)) / 10**6), 0)
pop = sum(germany$population)
density = round(pop / area, 0)

# title
img_1 <- image_annotate(img, "Germany Population Density Map", font = "Lobster",
                        color = text_color, size = 80, weight = 600, gravity = "north",
                        location = "+0+75")

# caption
img_2 <- image_annotate(img_1, "Graphic by @canaytore | Data from Kontur 2022",
                        font = "Lobster", location = "+440+40",
                        color = alpha("dodgerblue4", .8), size = 30, gravity = "south")

# Quick info about TR
img_3 <- image_annotate(img_2, glue("Population: {label_comma()(round(pop, 1))}\n",
                                    "Area: {label_comma()(round(area))} km²\n",
                                    "Density: {label_comma()(round(density))} /km²"), weight = 570,
                        font = "Lobster", location = "-600+830",
                        color = text_color2, size = 36, gravity = "north")

# Berlin
img_4 <- image_annotate(img_3, "Berlin",
                        font = "Lobster", location = "+410+365", style = "italic",
                        color = text_color3, size = 28, gravity = "north")

# Hamburg
img_5 <- image_annotate(img_4, "Hamburg",
                        font = "Lobster", location = "-160+243", style = "italic",
                        color = text_color3, size = 28, gravity = "north")

# Munich
img_6 <- image_annotate(img_5, "Munich",
                        font = "Lobster", location = "+540+845", style = "italic",
                        color = text_color3, size = 28, gravity = "north")

image_write(img_6, glue("germany_population_density_annotated.png"))
