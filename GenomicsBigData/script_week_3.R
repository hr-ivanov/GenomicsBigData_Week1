.libPaths("T:/GBD-R-packages")
library("tidyverse")

surveys <- read_csv("data/portal_data_joined.csv")
str(surveys)
view(surveys)
class(surveys)

select(surveys, plot_id, species_id, weight)
select(surveys, -record_id, -species_id)
filter(surveys, year<=1995)

surveys2 <- filter(surveys, weight<5)
surveys_sml <- select(surveys2, species_id, sex, weight)
surveys_sml

surveys_sml <- select(filter(surveys, weight<5), species_id, sex, weight)
surveys_sml

surveys %>% 
  filter(weight<5) %>% select(species_id, weight)


surveys %>% 
  filter(year>1990) %>% select(year, genus, species)

surveys %>% 
  mutate(weight_kg = weight/1000) %>% head()

surveys %>% 
  filter(!is.na(weight)) %>% 
  mutate(weight_kg=weight/1000) %>% 
  head()

surveys %>% 
  mutate(years_since_start=year-1977, weight_kg = weight/1000) %>% 
  filter(!is.na(weight_kg), years_since_start>=10) %>% 
  select(species_id, years_since_start, weight_kg)

surveys_tibble <- surveys %>% 
  mutate(years_since_start=year-1977, weight_kg = weight/1000) %>% 
  filter(!is.na(weight_kg), years_since_start>=10) %>% 
  select(species_id, years_since_start, weight_kg)

surveys %>% 
  mutate(years_since_start=year-1977) %>% 
  mutate(weight_kg = weight/1000) %>% 
  filter(!is.na(weight_kg)) %>% 
  filter(years_since_start>=10) %>% 
  select(species_id, years_since_start, weight_kg)