library(tidyverse)
library(ggplot2)

#Question 1
transcriptomics_data <- read_csv(file = "data/transcriptomics_data.csv")
transcriptomics_sample <- read_csv(file = "data/transcriptomics_sample.csv")
transcriptomics_gene <- read_csv(file = "data/transcriptomics_gene.csv")

#1
n_distinct(transcriptomics_sample)
#The researchers used 8 samples for their experiment:
#4 of the WT genotype and 4 of the KO genotype

#2
n_distinct(transcriptomics_sample$genotype)
#The researchers used 2 genotypes - WT and KO

#3
transcriptomics_data %>% 
  filter(WT_1==0.0 & WT_2==0.0 & WT_3==0.0 & WT_4==0.0 & KO_1==0.0 & KO_2==0.0 & KO_3==0.0 & KO_4==0.0) %>% 
  count()
#For 6571 genes no transcripts are detected across all samples

#4
transcriptomics_data_symbols = merge(x=transcriptomics_data, y=transcriptomics_gene, by = "geneID", all.x = TRUE)
dim(transcriptomics_data_symbols)
transcriptomics_data <- transcriptomics_data_symbols[, c(10,1,2,3,4,5,6,7,8,9)]
#https://www.geeksforgeeks.org/joining-of-dataframes-in-r-programming/
#http://www.sthda.com/english/wiki/reordering-data-frame-columns-in-r

#5
abundantGenesWT <- transcriptomics_data %>% 
  mutate(WT = WT_1+WT_2+WT_3+WT_4) %>% 
  select(symbol, WT) %>% 
  arrange(desc(WT))

abundantGenesWT[1:10, 1:2]
#Most abundant genes across the wild type:
#1    Eef1a1 252614408326
#2  Hsp90ab1 220554849028
#3      Actb 194482177533
#4       Ncl 186927820146
#5      Eef2 186095869509
#6     Actg1 170079674059
#7     Hspa8 160017734088
#8      Npm1 158405209249
#9      Rps2 148417546764
#10    Gapdh 140619830828

abundantGenesKO<- transcriptomics_data %>% 
  mutate(KO = KO_1+KO_2+KO_3+KO_4) %>% 
  select(symbol, KO) %>% 
  arrange(desc(KO))

abundantGenesKO[1:10, 1:2]
#Most abundant genes across the knockout type:
#1    Eef1a1 262589846917
#2  Hsp90ab1 229267920751
#3      Actb 207506488972
#4     Hspa8 185601469793
#5       Ncl 181767010859
#6      Eef2 178574119102
#7     Actg1 162214955262
#8      Npm1 159307592778
#9      Rps2 144849459145
#10    Gapdh 142095677390

#6
transcriptomics_data_new <- transcriptomics_data %>% 
  mutate(TPM_WT1 = WT_1/1000000,
         TPM_WT2 = WT_2/1000000,
         TPM_WT3 = WT_3/1000000,
         TPM_WT4 = WT_4/1000000,
         TPM_KO1 = KO_1/1000000,
         TPM_KO2 = KO_2/1000000,
         TPM_KO3 = KO_3/1000000,
         TPM_KO4 = KO_4/1000000
         ) %>% 
  select(geneID, symbol, TPM_WT1, TPM_WT2,
         TPM_WT3, TPM_WT4, TPM_KO1, TPM_KO2,
         TPM_KO3, TPM_KO4)

view(transcriptomics_data_new)

#Question 2
proteomics_data <- read_csv(file = "data/proteomics_data.csv")
proteomics_sample <- read_csv(file = "data/proteomics_sample.csv")
proteomics_gene <- read_csv(file = "data/proteomics_gene.csv")

#1
proteomics_data_symbols = merge(x=proteomics_data, y=proteomics_gene, by="GENEID", all.x = TRUE)
dim(proteomics_data_symbols)
proteomics_data <- proteomics_data_symbols[, c(10,1,2,3,4,5,6,7,8,9)]

#2
proteomics_data %>% 
  filter(p_value<0.05) %>% 
  filter(log2FC>1|log2FC<(-1)) %>% 
  count()
#1602 proteins are with significant changes in their abundances

#3
proteomics_data %>% 
  filter(p_value<0.05) %>% 
  filter(log2FC>2|log2FC<(-2)) %>% 
  count()
#755 proteins are with significant changes by 4 times in their abundances

#4
proteomics_data <- proteomics_data %>% 
  mutate(p_value_log = -log10(p_value))

#5
volcanoPlot <- ggplot(proteomics_data, mapping = aes(x=log2FC, y=p_value_log))+
  geom_point()
volcanoPlot

#6
volcanoPlot+
  geom_text(aes(label = ifelse(p_value<0.05&(log2FC==min(log2FC)|log2FC==max(log2FC)), as.character(SYMBOL),'')), hjust=0.6, vjust=-0.5)

#https://stackoverflow.com/questions/55739339/automatic-outlier-labeling-in-ggplot

#7
proteomics_data_bonus_question <- proteomics_data %>% 
  mutate(Coloured = ifelse(p_value<0.05, ifelse(log2FC<(-2), "blue", ifelse(log2FC>2, "red", "black")), "black"))

bonusPlot <- ggplot(proteomics_data_bonus_question, mapping = aes(x=log2FC, y=p_value_log, color = Coloured))+
  geom_point()+
  scale_color_identity() +
  geom_text(aes(label = ifelse(p_value<0.05&(log2FC==min(log2FC)|log2FC==max(log2FC)), as.character(SYMBOL),'')), hjust=0.6, vjust=-0.5)

bonusPlot
#https://stackoverflow.com/questions/11838278/plot-with-conditional-colors-based-on-values-in-r
