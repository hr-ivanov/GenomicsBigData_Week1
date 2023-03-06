surveys <- read.csv("data/portal_data_joined.csv")
surveys
head(surveys)
View(surveys)
surveys <- read.table("data/portal_data_joined.csv", sep = ",", header = TRUE)
str(surveys)
class(surveys)
nrow(surveys)
ncol(surveys)
names(surveys)
rownames(surveys)
summary(surveys)
dim(surveys)

uq <- c(unique(surveys$species_id))
uq
length(uq)

length(c(unique(surveys$species_id)))

# first element in the first column of the data frame (as a vector)
surveys[1, 1]  
# first element in the 6th column (as a vector)
surveys[1, 6]   
# first column of the data frame (as a vector)
surveys[, 1]    
# first column of the data frame (as a data.frame)
surveys[1]      
# first three elements in the 7th column (as a vector)
surveys[1:3, 7] 
# the 3rd row of the data frame (as a data.frame)
surveys[3, ]    
# equivalent to head_surveys <- head(surveys)
head_surveys <- surveys[1:6, ] 
head_surveys

surveys[, -1]          # The whole data frame, except the first column
surveys[-c(7:34786), ] # Equivalent to head(surveys)

surveys["species_id"]       # Result is a data.frame
surveys[, "species_id"]     # Result is a vector
surveys[["species_id"]]     # Result is a vector
surveys$species_id          # Result is a vector

#Quetion2
surveys_400 <- surveys[400, ]
surveys_400

surveys[nrow(surveys), ]
tail(surveys)

surveys_last <- surveys[nrow(surveys), ]
surveys_last

surveys_middle <- surveys[nrow(surveys)/2, ]
surveys_middle

surveys[-c(9:nrow(surveys)), ]

surveys %>% 
  mutate(year_since_start=year-1977, weight_kg=weight/1000) %>% 
  filter(!is.na(weight_kg)) %>% 
  select(species_id, year_since_start, weight_kg)
