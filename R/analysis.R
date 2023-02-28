library(dplyr)
library(stringr)
library(ggplot2)


data_dictionary <- read.csv("data/data_dictionary.csv")
resorts <- read.csv("data/resorts.csv", fileEncoding = "iso-8859-1")
snow <- read.csv("data/snow.csv")

############################## Cleaning the data ###############################
# Fixing some accents
resorts <- resorts %>% 
  mutate(Resort = 
           case_when(
             grepl("(Les 3 Valle?es)", Resort, fixed = TRUE) ~ 
               str_replace(Resort, fixed("(Les 3 Valle?es)"), "(Les 3 Vallées)"),
             grepl("(4 Valle?es)", Resort, fixed = TRUE) ~ 
               str_replace(Resort, fixed("(4 Valle?es)"), "(4 Vallées)"),
             grepl("Sauze d\u0092Oulx", Resort, fixed = TRUE) ~ 
               str_replace(Resort, fixed("Sauze d\u0092Oulx"), "Sauze d'Oulx"),
             grepl("La Tania-Val Thorens/?Les Menuires/?Me?ribel", Resort, fixed = TRUE) ~ 
               str_replace(Resort, fixed("La Tania-Val Thorens/?Les Menuires/?Me?ribel"), "La Tania-Val Thorens/Les Menuires/Méribel"),
             grepl("Me?ribel", Resort, fixed = TRUE) ~ 
               str_replace(Resort, fixed("Me?ribel"), "Méribel"),
             grepl("?Les Menuires", Resort, fixed = TRUE) ~ 
               str_replace(Resort, fixed("?Les Menuires"), "Les Menuires"),
             grepl("Montgene?vre", Resort, fixed = TRUE) ~ 
               str_replace(Resort, fixed("Montgene?vre"), "Montgenèvre"),
             .default = Resort
           ) 
         )  

# Checking for NA values
df_na <- tibble::rownames_to_column(
  as.data.frame(colSums(is.na(resorts)))
)
names(df_na) <- c("column", "quantity")

df_na %>% 
  ggplot(aes(y = column,  x = quantity)) +
  geom_col()

# Checking for zero values
df_zeroes <- tibble::rownames_to_column(
  as.data.frame(colSums(resorts == 0))
)
names(df_zeroes) <- c("column", "quantity")

df_zeroes %>% 
  ggplot(aes(y = column,  x = quantity)) +
  geom_col()

############################## Finished cleaning ###############################

resorts %>% 
  count(Country) %>% 
  arrange(desc(n))

resorts %>% 
  group_by(Country) %>% 
  summarise(avg_price = mean(Price)) %>% 
  arrange(desc(avg_price))


resorts %>% 
  select(Country, Resort, Highest.point) %>% 
  arrange(desc(Highest.point)) %>% 
  head(17)


resorts %>% 
  select(Country, Resort, Longest.run) %>% 
  arrange(desc(Longest.run)) %>% 
  head(17)


resorts %>% 
  # select(Country, Resort, Total.slopes, Beginner.slopes, Intermediate.slopes, Difficult.slopes) %>% 
  # select(Country, Resort, Total.slopes, Price) %>% 
  arrange(desc(Total.slopes)) %>% 
  head(17)


resorts %>% 
  arrange(Price) %>% 
  head(17)


resorts %>% 
  count(Season) %>% 
  arrange(desc(n))

resorts %>% 
  filter(Season == "Year-round")

spatial_limits <- tibble(
  min_latitude = min(resorts$Latitude),
  max_latitude = max(resorts$Latitude),
  min_longitude = min(resorts$Longitude),
  max_longitude = max(resorts$Longitude)
)

snow %>% 
  filter(
    Latitude >= spatial_limits$min_latitude &
      Latitude <= spatial_limits$max_latitude &
      Longitude >= spatial_limits$min_longitude &
      Longitude <= spatial_limits$max_longitude
  ) %>% 
  mutate(
    abs_latitude = abs(Latitude),
    abs_longitude = abs(Longitude)
  ) %>% 
  filter(Snow >= 90) %>% 
  group_by(Month) %>% 
  summarise(lat = min(abs_latitude), long = min(abs_longitude), lat_t = min(Latitude)) %>% 
  ungroup()


library(sf)
library(tmap)
library(cowplot)
data(World, land)

resorts_points <- st_as_sf(resorts, coords = c("Longitude", "Latitude"), crs = 4326)


mapa <- tm_shape(World) +
  tm_borders()+
  tm_shape(land) +
    tm_raster("elevation", palette = terrain.colors(10)) +
  tm_shape(resorts_points) + 
    # tm_dots()
    tm_bubbles(size = .2, col = "red")

map_grob <- tmap_grob(mapa)

ggplot_test <- resorts %>% 
  ggplot(aes(x = 1, y = 2)) +
  geom_point() +
  scale_x_continuous(limits = c(-180, 180), breaks = c(-180, 0, 180)) +
  scale_y_continuous(limits = c(-90, 90), breaks = c(-90, 0, 90)) +
  theme_classic() +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA)
  )

ggdraw() +
  draw_plot(map_grob) +
  draw_plot(ggplot_test)
