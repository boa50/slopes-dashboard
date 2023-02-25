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

nrow(resorts)

  
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
