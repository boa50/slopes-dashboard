library(dplyr)
library(stringr)
library(ggplot2)

### The task is to build a one-page dashboard to help skiers find their ideal destination.

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

########################### Getting interest points ############################

### Getting Highest point
resorts %>% 
  select(Country, Resort, Latitude, Longitude, Highest.point) %>% 
  arrange(desc(Highest.point)) %>% 
  head(5)

### Getting the Longest run
resorts %>% 
  select(Country, Resort, Latitude, Longitude, Longest.run) %>% 
  arrange(desc(Longest.run)) %>% 
  head(5)

### Getting the most total slopes
resorts %>% 
  select(Country, Resort, Latitude, Longitude, Total.slopes) %>% 
  arrange(desc(Total.slopes)) %>% 
  head(5)

### Getting resorts that offer skiing on summer
resorts %>% 
  filter(Summer.skiing == "Yes") 

### Getting the most total slopes
resorts %>% 
  select(Country, Resort, Latitude, Longitude, Lowest.point) %>% 
  arrange(Lowest.point) %>% 
  head(5)

### Getting the lowest point in Asia
resorts %>% 
  filter(Continent == "Asia") %>% 
  arrange(Lowest.point) %>% 
  head(5)

### Getting places where we can go skiing at night
resorts %>% 
  filter(Continent == "Oceania") %>% 
  filter(Nightskiing == "Yes")

### Getting the most expensive resort
resorts %>% 
  arrange(desc(Price)) %>% 
  head(1) %>% 
  select(Country, Resort, Latitude, Longitude, Price)

### Getting the lowest longitude
resorts %>% 
  filter(Longitude <= -149)

resorts %>% 
  count(Season) %>% 
  arrange(desc(n))

resorts %>% 
  filter(Season == "Year-round")

### Points gotten
# Lowest longitude
# Most expensive
# Summer skiing on South America
# Total slopes
# Highest point in Europe
# Longest run (with others)
# Lowest point in Asia
# Nightskiing in Oceania (with others)
interest_points <- tibble(
  Latitude = c(61.10327, 
               39.60488, 
               -33.35296, 
               45.39139, 
               45.96301, 
               41.83441, 
               43.34197, 
               -44.91589),
  Longitude = c(-149.7407, 
                -106.51500, 
                -70.248678, 
                6.574283, 
                7.715412, 
                23.484170,
                142.38319, 
                168.7396)
)

######################## Playing with the snow dataset #########################

# spatial_limits <- tibble(
#   min_latitude = min(resorts$Latitude),
#   max_latitude = max(resorts$Latitude),
#   min_longitude = min(resorts$Longitude),
#   max_longitude = max(resorts$Longitude)
# )
# 
# snow %>% 
#   filter(
#     Latitude >= spatial_limits$min_latitude &
#       Latitude <= spatial_limits$max_latitude &
#       Longitude >= spatial_limits$min_longitude &
#       Longitude <= spatial_limits$max_longitude
#   ) %>% 
#   mutate(
#     abs_latitude = abs(Latitude),
#     abs_longitude = abs(Longitude)
#   ) %>% 
#   filter(Snow >= 90) %>% 
#   group_by(Month) %>% 
#   summarise(lat = min(abs_latitude), long = min(abs_longitude), lat_t = min(Latitude)) %>% 
#   ungroup()

############################### Plotting the map ###############################

library(sf)
library(tmap)
library(cowplot)
data(World)

map_grob <- (tm_shape(World) +
  tm_borders(col = "#c9c9c8") +
  tm_layout(frame = FALSE)) %>% 
  tmap_grob()



### Added fake points to make the curve smoother
slope_line_points <- interest_points %>% 
  add_row(Latitude = 33.34197, Longitude = 75) %>% 
  add_row(Latitude = 55.10327, Longitude = -155.7407) %>% 
  add_row(Latitude = -52.91589, Longitude = 175.7396) %>% 
  do(as.data.frame(spline(x = .[["Longitude"]], 
                          y = .[["Latitude"]], 
                          n = 100)))

slope <- interest_points %>% 
  ggplot(aes(x = Longitude, y = Latitude)) +
  # Got the snow unicode representation from here: https://www.compart.com/en/unicode/search?q=snow#characters
  geom_point(data = resorts, 
             aes(x = Longitude, y = Latitude), 
             shape = "\u2744", 
             size = 5, 
             colour = "#82a4b3", 
             alpha = 0.5) +
  geom_line(data = slope_line_points, 
            aes(x = x, y = y), 
            linewidth = 6, 
            colour = "#c9cecb") +
  geom_line(data = slope_line_points, 
            aes(x = x, y = y), 
            linewidth = 5, 
            colour = "#eff1f4") +
  geom_point(size = 1, colour = "#c9cecb") +
  scale_x_continuous(limits = c(-180, 180), 
                     breaks = c(-180, 0, 180),
                     expand = expansion(mult = 0)) +
  scale_y_continuous(limits = c(-90, 90), 
                     breaks = c(-90, 0, 90),
                     expand = expansion(mult = 0)) +
  theme_classic() +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        plot.margin = margin(l = 19, t = 25, r = 20, b = 25),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(colour = "transparent")
  )

ggdraw() +
  draw_plot(map_grob) +
  draw_plot(slope)
