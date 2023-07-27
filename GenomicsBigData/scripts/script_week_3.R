install.packages("tidyverse", dependencies = TRUE)
install.packages("hexbin")
install.packages("gapminder")
install.packages("devtools")
#.libPaths("T:/GBD-R-packages")
library("tidyverse")

surveys <- read_csv("data/portal_data_joined.csv")
str(surveys)
view(surveys)
class(surveys)
surveys

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

args(na.rm)

surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(sex, species_id) %>% 
  summarise(mean_weight = mean(weight))

surveys %>% 
  filter(!is.na(sex)) %>% 
  group_by(sex, species_id) %>% 
  summarise(mean_weight = mean(weight, na.rm=TRUE),
            min_weight = min(weight)) %>% 
  arrange(desc(mean_weight)) %>% 
  print(n=30)

surveys %>% 
  count(sex, sort=TRUE)

surveys %>% 
  count(sex, species)

#Question_3
surveys %>% 
  count(plot_type)

surveys %>% 
  count(year, sort = TRUE) %>% 
  first()

surveys %>% 
  filter(!is.na(hindfoot_length)) %>% 
  group_by(species_id) %>% 
  summarise(meanH=mean(hindfoot_length),
            minH=min(hindfoot_length),
            maxH=max(hindfoot_length),
            numb=n()) %>% 
  arrange(desc(numb)) %>% 
  print(n=length(c(unique(surveys$species_id))))

surveys %>% 
  group_by(year) %>% 
  summarise(nSp=n_distinct(species_id)) %>% 
  arrange(desc(nSp)) %>% 
  first()

surveys %>% #???????????????????????
  filter(!is.na(weight)) %>% 
  group_by(year) %>% 
  summarise(maxHeav=max(weight), first(genus), first(species_id)) %>% 
  arrange(desc(maxHeav))



surveys_complete
dim(surveys)
dim(surveys_complete)

write_csv(surveys_complete, path="data_output/surveys_complete.csv")

#Question4
transcripts <- read_csv("data/20190110.ensembl_genes.csv.gz")

dim(transcripts)
str(transcripts)
view(transcripts)

transcripts %>% 
  summarise(nT=n_distinct(transcript_id))

transcripts %>% 
  summarise(nG=n_distinct(gene_stable_id))

n_distinct(transcripts$gene_stable_id) #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

transcripts %>% 
  group_by(gene_stable_id) %>% 
  summarise(nT=n_distinct(transcript_id)) %>% 
  arrange(desc(nT)) %>% 
  first()

transcripts %>% 
  summarise(meanGC=mean(gc_content))

transcripts %>% 
  group_by(transcript_type) %>% 
  summarise(nTtype=n()) %>% 
  arrange(desc(nTtype)) %>% 
  print(n=5)

transcripts %>% 
  filter(transcript_type == "protein_coding") %>% 
  mutate(gene_length=gene_end-gene_start) %>% 
  arrange(desc(gene_length)) %>% 
  summarise(maxL=max(gene_length), first(gene_name), first(gene_description))

surveys_complete <- surveys %>%
  filter(!is.na(weight),           # remove missing weight
         !is.na(hindfoot_length),  # remove missing hindfoot_length
         !is.na(sex))

## Extract the most common species_id
species_counts <- surveys_complete %>%
  count(species_id) %>% 
  filter(n >= 50)

## Only keep the most common species
surveys_complete <- surveys_complete %>%
  filter(species_id %in% species_counts$species_id)


surveys_complete <- surveys %>% 
  filter(!is.na(weight),
         !is.na(hindfoot_length),
         !is.na(sex))

species_counts <- surveys_complete %>% 
  count(species_id) %>% 
  filter(n>=50)

surveys_complete <- surveys_complete %>% 
  filter(species_id %in% species_counts$species_id)

dim(surveys_complete)
