library("gapminder")
library(msleep)
install.packages("ggrepel")
library(ggrepel)
gapminder

#HISTOGRAMS

ggplot(gapminder, mapping=aes(lifeExp)) +
  geom_histogram(binwidth = 1)
#You can either select the number of bins with nbins
#or the bin width with binwidth

ggplot(gapminder, mapping=aes(lifeExp)) +
  geom_histogram() +
  facet_wrap(~continent)

#Q-Q PLOTS
ggplot(gapminder, mapping=aes(sample=lifeExp)) +
  stat_qq() +
  stat_qq_line()

surveys <- read_csv("data/surv.csv")

surveys_complete <- surveys %>%
  filter(!is.na(weight),           # remove missing weight
         !is.na(hindfoot_length),  # remove missing hindfoot_length
         !is.na(sex))                # remove missing sex

surveys_complete %>% 
  filter(species_id=="PB") %>% 
  ggplot(mapping=aes(sample=weight)) +
  stat_qq() +
  stat_qq_line()


#STUDENT'S T-TEST
#In the following sample we test the null hypothesis
#that the weights of male and female mice are different.

pb_male <- surveys_complete %>% 
  filter(species_id=="PB", sex=="M") %>% 
  pull(weight)
#pull(.data, var=..., name=NULL, ...)
#var can be:
  #a literal variable name
  #a positive integer, giving the position counting from the left
  #a negative integer, giving the position counting from the right.

pb_female <- surveys_complete %>% 
  filter(species_id=="PB", sex=="F") %>% 
  pull(weight)

t.test(pb_female, pb_male)
t.test(pb_male, pb_female)
t.test(pb_female, pb_male, paired = TRUE) #!!!!!!!!!!!!!!

#2 version
pb <- surveys_complete %>% 
  filter(species_id=="PB")

t.test(pb$weight ~ pb$sex)

#X2-TEST
#If you run the chisq.test() function with a vector
#(or a table with one row or column) then a goodness-of-fit X2-test is performed.

b <- tibble(
  season=c("Winter", "Spring", "Summer", "Fall"),
  births=c(258, 278, 211, 253)
)

chisq.test(b$births)

#When the first argument, called x, to chisq.test() is a table (matrix),
#then the X2-test of independence is performed.

#WILCOXON TEST
exp_europe <- gapminder %>% 
  filter(year==2007, continent=="Europe") %>% 
  pull(lifeExp)

exp_americas <- gapminder %>% 
  filter(year==2007, continent=="Americas") %>% 
  pull(lifeExp)

wilcox.test(exp_americas, exp_europe)

#Paired
life_exp_europe <- gapminder %>% 
  filter(continent=="Europe") %>%
  select(country, year, lifeExp) %>% 
  spread(key=year, value=lifeExp) #???????

wilcox.test(life_exp_europe$'2002', life_exp_europe$'2007', paired=TRUE)

#MULTIPLE TESTS
#The FDR is the expected number of false positives divided by the number of reported significant 

#????

#LINEAR REGRESSION
result <- lm(brainwt~bodywt, data=msleep)
summary(result)
cor.test(msleep$brainwt, msleep$bodywt)
ggplot(msleep, aes(x=bodywt, y=brainwt)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm")


#Question 1
is_outlier <- function(x)
  {
    return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
  }

plantOutliers <- PlantGrowth %>% 
  # pivot_longer(names_to = continent, values_to = total_litres_of_pure_alcohol, -type) %>% 
  group_by(group) %>%
  mutate(outlier_check = is_outlier(weight)) %>% 
  mutate(weight_m = ifelse(outlier_check==FALSE, weight, ""))

#a
str(PlantGrowth)
ggplot(data=plantOutliers, mapping = aes(x=group, y=weight)) +
  geom_boxplot() +
  geom_text_repel(label=ifelse(plantOutliers$outlier_check == TRUE, plantOutliers$weight,""))

#b
trt1 <- PlantGrowth %>% 
  filter(group=="trt1") %>% 
  pull(weight)

trt2 <- PlantGrowth %>% 
  filter(group=="trt2") %>% 
  pull(weight)

t.test(trt1, trt2)

#ANOVA
plantTreatments <- aov(weight~group, data = PlantGrowth)
summary(plantTreatments)

TukeyHSD(plantTreatments)


#Question 2
#a

shapiro.test(msleep$brainwt)

ggplot(msleep, mapping = aes(brainwt)) +
  geom_histogram()

ggplot(msleep, mapping = aes(sample=brainwt)) +
  geom_qq() +
  geom_qq_line()

carniv <- msleep %>% 
  filter(vore=="carni") %>% 
  pull(brainwt)

herbiv <- msleep %>% 
  filter(vore=="herbi") %>% 
  pull(brainwt)

t.test(carniv, herbiv)

ggplot(msleep, mapping = aes(x=vore, y=brainwt)) +
  geom_boxplot()

#c, d
shapiro.test(msleep$sleep_total)

ggplot(msleep, mapping = aes(sleep_total)) +
  geom_histogram(binwidth = 1)

carniv <- msleep %>% 
  filter(vore=="carni") %>% 
  pull(sleep_total)

herbiv <- msleep %>% 
  filter(vore=="herbi") %>% 
  pull(sleep_total)

t.test(carniv, herbiv)

#Question 3
lm(brainwt~bodywt, msleep)
cor.test(msleep$brainwt, msleep$bodywt, method = "spearman")

msleepNoHeavy <- msleep %>% 
  filter(bodywt<2000)

msleepLog <- msleep %>% 
mutate(logbrain = log10(brainwt))

cor.test(msleepNoHeavy$brainwt, msleepNoHeavy$bodywt)
