library(tidyverse)
library(gapminder)
library(ggplot2)
library(hexbin)

#DATA ANALYSIS

lifeExpEurope <- gapminder %>% 
  filter(continent=="Europe") %>% 
  select(country, year, lifeExp) %>% 
  spread(key=year, value = lifeExp)

lifeExpEurope

#Question 1
#a
str(PlantGrowth)
ggplot(PlantGrowth, mapping = aes(x=group, y=weight)) +
  geom_boxplot()

#b
treat1 <- PlantGrowth %>% 
  filter(group=="trt1") %>% 
  pull(weight)

treat2 <- PlantGrowth %>% 
  filter(group=="trt2") %>% 
  pull(weight)

t.test(treat2, treat1)
#Plants treated with treatment 2
#are significantly heavier than plants treated with treatment 1
#t = 3.0101, p-value = 0.009298

#Question 2
#a
str(msleep)
ggplot(msleep, mapping = aes(brainwt)) +
  geom_histogram()

ggplot(msleep, mapping = aes(sample=brainwt)) +
  geom_qq_line() +
  geom_qq()

#b
herbiBRain <- msleep %>% 
  filter(vore=="herbi") %>% 
  pull(brainwt)

carnibrain <- msleep %>% 
  filter(vore=="carni") %>% 
  pull(brainwt)

wilcox.test(herbiBRain, carnibrain, paired = FALSE)
#carnivores don't have siginificantly heavier brains, p-valu = 0.5943

#c
str(msleep)
ggplot(msleep, mapping = aes(sleep_total)) +
  geom_histogram()

ggplot(msleep, mapping = aes(sample=sleep_total)) +
  geom_qq_line() +
  geom_qq()

#d
herbiSleep <- msleep %>% 
  filter(vore=="herbi") %>% 
  pull(sleep_total)

carniSleep <- msleep %>% 
  filter(vore=="carni") %>% 
  pull(sleep_total)

wilcox.test(herbiSleep, carniSleep, paired = FALSE)
t.test(herbiSleep, carniSleep)
shapiro.test(msleep$sleep_total)

#Question 3
#a
brainWeightAll <- msleep %>% 
  pull(brainwt)

bodyWeightAll <- msleep %>% 
  pull(bodywt)

summary(lm(brainWeightAll ~ bodyWeightAll))
#(Intercept)  bodyWeightAll (slope) 
#0.0859173    0.0009639

cor.test(brainWeightAll, bodyWeightAll)

ggplot(msleep, mapping = aes(x=bodywt, y=brainwt)) +
  geom_point() +
  geom_smooth(method = 'lm')

msleepNoHeavy <- msleep %>% 
  filter(bodywt<2000)

brainWeightNoHeavy <- msleepNoHeavy %>% 
  filter(bodywt<2000) %>% 
  pull(brainwt)

bodyWeightNoHeavy <- msleepNoHeavy %>% 
  filter(bodywt<2000) %>% 
  pull(bodywt)

summary(lm(brainWeightNoHeavy~bodyWeightNoHeavy))
#(Intercept)  bodyWeightNoHeavy  
#0.059725     0.001029

ggplot(msleepNoHeavy, mapping = aes(x=bodywt, y=brainwt)) +
  geom_point()

#b
ggplot(msleepNoHeavy, mapping = aes(x=bodywt, y=brainwt)) +
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  geom_smooth(method = "lm")

cor.test(brainWeightNoHeavy, bodyWeightNoHeavy, method = "spearman")
cor.test(brainWeightNoHeavy, bodyWeightNoHeavy, method = "pearson")
cor(brainWeightNoHeavy, bodyWeightNoHeavy, use = "complete.obs", method = "spearman")


result <- lm(brainwt~bodywt, data = msleep)
summary(result)

#WORKING WITH DATA FRAMES
#Question 2
surveys %>% 
  mutate(years_since_start = year-1977, weight_kg = weight/1000) %>%
  filter(!is.na(weight_kg), years_since_start>=10) %>% 
  select(species_id, years_since_start, weight_kg)

#Question 3
#a
surveys %>% 
  group_by(plot_type) %>% 
  count()

#b
surveys %>% 
  group_by(year) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  first()

#c
surveys %>% 
  group_by(species_id) %>% 
  filter(!is.na(hindfoot_length)) %>% 
  summarise(meanHind = min(hindfoot_length), nanimals = n())

#d
surveys %>% 
  group_by(year) %>% 
  summarise(nspecies = n_distinct(species_id)) %>% 
  arrange(desc(nspecies)) %>% 
  first()

#e
surveys %>% 
  group_by(year) %>% 
  filter(!is.na(weight)) %>% 
  summarize(maxWeight = max(weight), first(genus), first(species_id)) %>% 
  arrange(desc(maxWeight))


surveys %>%
  count(sex, species, sort = TRUE) 





#Exam2021
download.file(url = "https://mbdata.science.ru.nl/share/heeringen/gbd_exam/genome_size.1fx0.csv", destfile="data/transcripts2.csv")
genes <- read_csv("data/transcripts2.csv")
str(genes)

#1c
genes %>% 
  filter(Organism_Group=="Eukaryota", Chromosomes>0) %>% 
  count()

#2b
sc <- read_tsv('http://www.nxn.se/single-cell-studies/data.tsv')
sc <- sc %>% rename_all(~str_replace_all(., '\s+', '_'))

str(sc)
sc

sc %>%
  filter(Tissue=="Culture", !is.na(`Cell source`)) %>% 
  group_by(`Cell source`) %>% 
  summarize(nCellSourse = n()) %>% 
  arrange(desc(nCellSourse)) %>% 
  first()

sc %>% 
  filter(Tissue=="Culture", !is.na(`Cell source`)) %>% 
  group_by(`Cell source`) %>% 
  count(sort = TRUE) %>% 
  first()

#2c
sc2c <- sc %>% 
  filter(Organism %in% c("Human", "Mouse", "Drosophila","Zebrafish")) %>% 
  mutate(year = substr(Date, 0, 4))

ggplot(sc2c, mapping = aes(x=year, y=`Reported cells total`)) +
  geom_point() +
  facet_wrap(~Organism) +
  scale_y_log10()

str(sc)
