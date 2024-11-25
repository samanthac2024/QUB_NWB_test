#' _Github Demo_ 
#' Read in the data provided 
#' Plot a barchart to show the most popular lunch destination
#' Clean the data to make the plot better
#' Create your own version in a new repo for your own university/ site
#' 
#' Load libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, # data manipulation
               viridis,
               leaflet,
               svglite)

#' Import data
df <- read_csv("~/Downloads/Data_test.csv") # proportion patients returning by dose

# Group and count venue choices 
ug <- df %>%
  mutate_at(vars(Country), factor) %>%
  group_by(Country) %>%
  dplyr::summarise(n = length(Country))

# make bar plot to show popular choices
p <- ggplot(data = ug, aes(x = Country, y = n)) +
  geom_bar(stat="identity")
p

ggsave(filename = "nwb_test.svg", plot = p, device = "svg", path = "~/Documents/GitHub/QUB_NWB_test/")

# Bit of a mess - lets clean up the data
table(df$name) # get rid of non-unique names
table(df$university) # clean up misspellings

# examples of cleaning up messy data
df$venue <- gsub("DM", "Dumpling monkey", df$venue)
# or
df <- df %>%
  dplyr::mutate(venue = recode(venue,
                'Sound Bitea' = 'Sound Bites',
                'Sound Bite' = 'Sound Bites',
                'IHI cafeteria' = 'IHI cafe'))

df <- df %>%
  dplyr::mutate(university = recode(university,
                                    'Glasgw' = 'Glasgow'))

# save clean data (without overwriting the raw data!)
if(!dir.exists("outputs")){dir.create("outputs")}
write.csv(df, file = "outputs/venues_clean.csv", row.names = FALSE)
  
# replot
# make bar plot to show popular choices
ug <- df %>% 
  filter(university == "Glasgow")  %>%
  mutate_at(vars(venue), factor) %>%
  group_by(venue) %>%
  summarise(n = length(venue)) %>%
  ggplot(aes(x = venue, y = n)) +
  geom_bar(stat="identity")
ug

# stacked barplot
p <- df %>% 
  group_by(venue, university) %>%
  summarise(n = length(venue)) %>%
  ggplot(aes(x = venue, y = n, fill = university)) +
  geom_bar(stat="identity") + 
  theme_minimal()
p

# save an svg file
if(!dir.exists("figs")){dir.create("figs")}
ggsave("figs/demo_stacked_chart.svg", p, device = "svg")

# See if the file can be edited in ppt! Follow instructions here:
# https://nalinan.medium.com/how-to-vectorize-plots-from-r-in-powerpoint-bad7c238e86a



