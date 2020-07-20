# load packages ----

library(here)
library(rgdal)
library(raster)
library(tidyverse)
library(broom)
library(mapproj)
library(readxl)
library(ggsn)
library(sf)
library(ggrepel)

# read in table 2.1 ----

table_21 <- read_excel(here("data", "2016-isle-of-man-census-report-data-tables.xlsx"),
                       "Table 2.1", "A2:E30") %>%
  rename(name = "Area of Residence", pop_16 = "2016", per_16 = "% of Total", 
         pop_11 = "2011", per_change = "% Change")

# read in shapefiles ----

outline <- shapefile(here("shapefiles", "outline.shp"))

areas <- shapefile(here("shapefiles", "areas.shp"))
areas$area = area(areas)/1000000

# convert shapefiles to tibbles ----

outline_tidy <- tidy(outline)

areas_tidy <- tidy(areas)
areas$id <- row.names(areas)
areas_tidy <- left_join(areas_tidy, areas@data)
areas_tidy[areas_tidy == "Port Saint Mary"] <- "Port St Mary"

# input town coordinates ----

towns <- tibble(place = c("Ramsey", "Peel", "Laxey", "Onchan", "Douglas",
                          "Port Erin", "Port St Mary", "Castletown"),
                lat = c(54.32273, 54.22209, 54.23017, 54.17329, 54.15,
                        54.08487, 54.07405, 54.07445),
                long = c(-4.38526, -4.69099, -4.39985, -4.45324, -4.48333, 
                         -4.75099, -4.73858, -4.65365))

#calculate population densities ----

pops <- inner_join(areas_tidy, table_21, by = "name") %>%
  mutate(pop_density_16 = pop_16 / area)

# plots areas ----

pop_plot <- ggplot(data = pops, aes(x = long, y = lat, group = group, fill = pop_density_16)) +
  geom_polygon(color = "white", size = 0.1) +
  coord_map() +
  theme_void() +
  labs(fill = expression("People per km"^2)) +
  scale_fill_gradient(trans = "log10", low = "white", high = "darkred")

# plots outline and scale bar ----

pop_plot <- pop_plot +
  geom_path(data = outline_tidy, aes(fill = NA), colour = "black", size = 0.1) + 
  scalebar(data = pops, dist = 5, dist_unit = "km", st.size= 3.5, border.size = 0.5,
           transform = TRUE, model = "WGS84")

#plots town points and labels ----

pop_plot <- pop_plot +
  geom_point(data = towns, group = NA, fill = "white", colour = "black", size = 2, shape = 21) +
  geom_text_repel(data = towns, aes(label = place, fill = NA),
                  nudge_x = c(0.06,-0.02,0.05,0.08,0.06,-0.06,0.04,0.11), size = 3.5,
                  nudge_y = c(0.01,0.02,-0.02,-0.01,-0.02,0.04,-0.02,-0.01), group = NA)

# generates plot and pdf ----

pop_plot
ggsave(here("outputs", "table21_fig1.pdf"))
