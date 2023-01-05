# Turkey Population Density Map - by @canaytore - data from Kontur

library(sf)
library(tigris)
library(tidyverse)
library(ggplot2)
library(stars)
library(MetBrewer)
library(colorspace)
library(rayshader)
library(scales)

windowsFonts("Lobster" = windowsFont("Lobster"),
             "Montserrat" = windowsFont("Montserrat"))

# source -> https://data.humdata.org/dataset/kontur-population-turkiye
turkey <- st_read("kontur_population_TR_20220630.gpkg")

bb <- st_bbox(turkey)

bottom_left <- st_point(c(bb[["xmin"]], bb[["ymin"]])) %>%
  st_sfc(crs = st_crs(turkey))

bottom_right <- st_point(c(bb[["xmax"]], bb[["ymin"]])) %>%
  st_sfc(crs = st_crs(turkey))

width <- st_distance(bottom_left, bottom_right)

top_left <- st_point(c(bb[["xmin"]], bb[["ymax"]])) %>%
  st_sfc(crs = st_crs(turkey))

height <- st_distance(bottom_left, top_left)

# handle conditions of width or height being the longer side
if(width > height) {
  w_ratio <- 1
  h_ratio <- as.numeric(height / width)
} else {
  h_ratio <- 1
  w_ratio <- as.numeric(width / height)
} 

# convert to raster so we could then convert to matrix
size <- 5000

turkey_rast <- st_rasterize(turkey, nx = floor(size * w_ratio), ny = floor(size * h_ratio))

mat <- matrix(turkey_rast$population, nrow = floor(size * w_ratio), ncol = floor(size * h_ratio))

# create color pallette
colors <- c("#284177", "#006BBD", "#83CEEC", "#EDE8E4", "#C2AFA8")
tx <- grDevices::colorRampPalette(colors, bias = 2)(256)
swatchplot(colors)
swatchplot(tx)

# initial graphic
mat %>%
  height_shade(texture = tx) %>%
  plot_3d(heightmap = mat,
          windowsize = c(1920, 1080),
          background = "#b8c9cf",
          zscale = 30,
          solid = FALSE,
          shadowdepth = -30,
          fov = 50,
          theta = -16, zoom = 0.5, phi = 30)


outfile <- "turkey_population_density.png"

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
    lightdirection = 280,
    lightaltitude = c(20,80),
    lightcolor = c(colors[3], "white"),
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
angles = seq(0, 360, length.out=721)[-1]
for(i in 1:720) {
  render_camera(theta=-45+angles[i])
  render_snapshot(filename = sprintf("turkey%i.png", i), 
                  title_text = "Turkey Population Density Map 360°",
                  title_bar_color = "#b8c9cf", title_color = colors[1], title_position = "north", title_bar_alpha = 1)
  Sys.sleep(0.1)
}

rgl::rgl.close()

av::av_encode_video(sprintf("turkey%d.png", seq(1,720,by=1)), framerate=60, output="turkey_population_density_360.mp4")

system("ffmpeg -framerate 60 -i turkey%d.png -pix_fmt yuv420p turkey_population_density_360.mp4")


# adjust annotation
library(magick)
library(glue)

img <- image_read("turkey_population_density.png")
text_color <- colors[1]
text_color2 <- colors[2]

# compute area / population / density
area = round(as.numeric(sum(st_area(turkey)) / 10**6), 0)
pop = sum(turkey$population)
density = round(pop / area, 0)

# title
img_1 <- image_annotate(img, "Turkey Population Density Map", font = "Lobster",
                      color = text_color, size = 80, weight = 600, gravity = "north",
                      location = "+0+100")

# caption
img_2 <- image_annotate(img_1, "Graphic by @canaytore | Data from Kontur 2022",
                       font = "Lobster", location = "+420+60",
                       color = alpha("dodgerblue4", .8), size = 32, gravity = "south")

# Quick info about TR
img_3 <- image_annotate(img_2, glue("Population: {label_comma()(round(pop, 1))}\n",
                                    "Area: {label_comma()(round(area))} km²\n",
                                    "Density: {label_comma()(round(density))} /km²"), weight = 570,
                       font = "Lobster", location = "+260+290",
                       color = text_color2, size = 36, gravity = "north")

# Istanbul
img_4 <- image_annotate(img_3, "Istanbul",
                       font = "Lobster", location = "-442+575", style = "italic",
                       color = "dodgerblue4", size = 28, gravity = "north")

# Ankara
img_5 <- image_annotate(img_4, "Ankara",
                       font = "Lobster", location = "-180+465", style = "italic",
                       color = "dodgerblue4", size = 28, gravity = "north")

# Izmir
img_6 <- image_annotate(img_5, "Izmir",
                       font = "Lobster", location = "-775+818", style = "italic",
                       color = "dodgerblue4", size = 28, gravity = "north")

image_write(img_6, glue("turkey_population_density_annotated.png"))
