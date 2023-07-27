library(tidyverse)
library(ggplot2)

#Q1
#1
transcriptomics_data <- read_csv(file = "data/practice_transcriptomics_data.csv")
transcriptomics_sample <- read_csv(file = "data/practice_transcriptomics_sample.csv")
transcriptomics_gene <- read_csv(file = "data/practice_transcriptomics_gene.csv")

view(transcriptomics_gene)
view(transcriptomics_data)
view(transcriptomics_sample)
n_distinct(transcriptomics_sample)
#The researchers used 7 samples: 3 with the WT genotype, and 4 with the KO genotype
n_distinct(transcriptomics_data)

#2
n_distinct(transcriptomics_sample$genotype)
#The researchers used 2 genotypes for this experiment - WT and KO

#3
transcriptomics_data %>% 
  filter(WT_1==0|WT_2==0|WT_3==0|KO_1==0|KO_2==0|KO_3==0|KO_4==0) %>% 
  count()

#4
transcriptomics_data_symbols = merge(x=transcriptomics_data, y=transcriptomics_gene, by = "geneID", all.x = TRUE)
transcriptomics_data_symbols <- transcriptomics_data_symbols %>% 
  select(-...1.x, -...1.y)

str(transcriptomics_data_symbols)
view(transcriptomics_data_symbols)

#Q2
#1
proteomics_data <- read_csv(file = "data/practice_proteomics_data.csv")
proteomics_gene <- read_csv(file = "data/practice_proteomics_gene.csv")
proteomics_sample <- read_csv(file = "data/practice_proteomics_sample.csv")

n_distinct(proteomics_data)
proteomics_sample
proteomics_gene

proteomics_data_symbols = merge(x=proteomics_data, y=proteomics_gene, by="GENEID", all.x = TRUE)
proteomics_data_symbols <- proteomics_data_symbols %>% 
  select(Naive_1, Naive_2, Naive_3, Primed_1, Primed_2, Primed_3, p_value, log2FC, GENEID, SYMBOL)
view(proteomics_data_symbols)

#https://www.geeksforgeeks.org/joining-of-dataframes-in-r-programming/

#2
proteomics_data_symbols %>% 
  filter(p_value<0.05) %>% 
  count()

#3
proteomics_data_symbols2 <- proteomics_data_symbols %>% 
  mutate(p_value_log = -log10(p_value))

#4
ggplot(proteomics_data_symbols2, mapping = aes(x=log2FC, y=p_value_log)) +
  geom_point()

str(proteomics_data_symbols2)
