#.libPaths("T:/GBD-R-packages")
library("tidyverse")

#RStudio
iris_1 <- readRDS(file="iris_2.rds")
iris_2 <- sessionInfo()
iris_2
class(iris_1)
typeof(iris_1)
typeof(append)


#Introduction to R
weight_g <- c(50, 60, 65, 82)
class(weight_g)
typeof(weight_g)
str(weight_g)

weight_g>50
"a"<"b"
"10"<"2"

heights <- c(160, 175, 152, 165, NA, 172, 154, 178, 155, 150, 163, 175, 160, 161, NA, 182, 165, 162, 178, 192, 167)
heights <- heights[!is.na(heights)]
median(heights, na.rm = TRUE)
tall_people <- heights[heights>=175 & !is.na(heights)] #!!!!!!!!
tall_people


#Working with data
download.file(url = "https://ndownloader.figshare.com/files/2292169",
              destfile = "data/portal_data_joined.csv")

surveys <- read.csv("data/portal_data_joined.csv")
surveys <- read.table(file="data/portal_data_joined.csv", sep=",", header=TRUE)
view(surveys)
class(surveys)
typeof(surveys)
str(surveys)
dim(surveys)
names(surveys)
rownames(surveys)
summary(surveys)
length(c(unique(surveys$species)))
surveys_400 <- surveys[400, ]
surveys_400
surveys[nrow(surveys)/2, ]
surveys[-c(9:nrow(surveys)), ]


#Data manipulation with dplyr
surveys <- read_csv("data/portal_data_joined.csv")
class(surveys)
typeof(surveys)
str(surveys)
view(surveys)
surveys %>% 
  filter(year>1990) %>% 
  select(year, genus, species)
surveys_kg <- surveys %>% 
              mutate(years_since_start = year-min(surveys$year, na.rm = TRUE)) %>% 
              mutate(weight_kg = !is.na(weight/1000)) %>% 
              filter(years_since_start>=10)
surveys_kg

surveys %>% 
  count(plot_type)

surveys %>% #!!!!!!!!!!!!!!!!!!
  count(year, sort = TRUE) %>% 
  first()

surveys %>% 
  filter(!is.na(hindfoot_length)) %>% 
  group_by(species_id) %>% 
  summarise(meanL = mean(hindfoot_length))

surveys %>% 
  group_by(year) %>%
  summarise(numb=n_distinct(species_id)) %>% 
  arrange(desc(numb)) %>% 
  first()

surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(year) %>% 
  summarise(maxWeight = max(weight), first(species_id), first(year), first(genus))

download.file(url = "https://zenodo.org/record/2540547/files/20190110.ensembl_genes.csv.gz", destfile="data/transcripts2")
transcripts <- read_csv("data/transcripts2")
ncol(transcripts)

str(transcripts)
n_distinct(transcripts$transcript_id)
n_distinct(transcripts$gene_stable_id)

transcripts %>% 
  count(gene_stable_id, sort=TRUE) %>% 
  first()

transcripts %>% 
  summarise(meanGC = mean(gc_content, na.rm=TRUE))

transcripts %>% 
  count(transcript_type, sort=TRUE) %>% 
  select(transcript_type, n) %>% 
  print(n=5)

transcripts %>% 
  filter(!is.na(transcript_length) & transcript_type=="protein_coding") %>% 
  mutate(gene_length = gene_end-gene_start) %>% 
  select(gene_name, gene_length, transcript_type) %>% 
  arrange(desc(gene_length)) %>% 
  first()
