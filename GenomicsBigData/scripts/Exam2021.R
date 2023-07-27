library(tidyverse)
library(ggplot2)

#Question 1
genes <- read_csv("https://mbdata.science.ru.nl/share/heeringen/gbd_exam/genome_size.1fx0.csv")

#a
dim(genes)
n_distinct(genes)
#40000

#b
genes_new <- genes %>% 
  rename(
    "Name" = Organism_Name,
    "Group" = Organism_Group
  )

str(genes_new)
view(genes_new)

#c
genes %>% 
  filter(Organism_Group=="Eukaryota", Chromosomes>0) %>% 
  count()
#619

#d
genes %>% 
  arrange(desc(Size_Mb)) %>% 
  first()
#Ambystoma mexicanum

#e
genesWithChr <- genes %>% 
  filter(Chromosomes>0, Size_Mb>0, !is.na(Chromosomes), !is.na(Size_Mb))

summary(lm(Size_Mb~Chromosomes, data=genesWithChr))
#There is a slight positive linear correlation (Multiple R-squared:  0.2236), p-value < 2.2e-16
#a significant result - p-value<2.2e-16

ggplot(genesWithChr, mapping = aes(x=Chromosomes, y=Size_Mb))+
  geom_point()

#f
ggplot(genes, mapping = aes(x= Organism_Group, y=Size_Mb))+
  geom_boxplot()+
  scale_y_log10()

#Question 2
sc <- read_tsv('http://www.nxn.se/single-cell-studies/data.tsv')
sc <- sc %>% rename_all(~str_replace_all(., '\s+', '_'))
view(sc)

#a
sc %>% 
  filter(`Number of reported cell types or clusters`>=3, !is.na(`Number of reported cell types or clusters`)) %>% 
  count()
#558

#b
sc %>% 
  filter(Tissue=="Culture", !is.na(`Cell source`)) %>% 
  group_by(`Cell source`) %>% 
  summarize(nCellSource = n()) %>% 
  arrange(desc(nCellSource)) %>% 
  first()
#mESCs - 9 times

#c
scForPlot <- sc %>% 
  filter(Organism %in% c("Human", "Mouse", "Drosophila", "Zebrafish"))

ggplot(scForPlot, mapping = aes(x=Date, y=`Reported cells total`))+
  geom_point()+
  scale_y_log10()+
  facet_wrap(~Organism)+
  theme(axis.text.x = element_text(angle=45))

str(scForPlot)

#d
InDrops <- sc %>% 
  filter(Technique=="InDrops", !is.na(`Reported cells total`)) %>% 
  pull(`Reported cells total`)

SMARTer <- sc %>% 
  filter(Technique=="SMARTer", !is.na(`Reported cells total`)) %>% 
  pull(`Reported cells total`)

InDrops
SMARTer

shapiro.test(InDrops)
shapiro.test(SMARTer)
#Both tests produced significant results, thus neither dataset is normally distributed
#A non-parametric test is needed

wilcox.test(InDrops, SMARTer, paired = FALSE, alternative = "greater")
#Conclusion: Experiments using the InDrops technique have a larger number of cells,
#W = 440, p-value = 8.568e-09