alcohol <- read_csv("data/alcohol.csv")
alcohol
library(tidyverse)

#Most beer
alcohol %>% 
  arrange(desc(beer_servings)) %>% 
  select(country, beer_servings) %>% 
  print(n=3)

#Most hard liquor
alcohol %>% 
  arrange(desc(spirit_servings)) %>% 
  select(country, spirit_servings) %>% 
  print(n=3)

#Most wine
alcohol %>% 
  arrange(desc(wine_servings)) %>% 
  select(country, wine_servings) %>% 
  print(n=3)

#Most alcohol
alcohol %>% 
  arrange(desc(total_litres_of_pure_alcohol)) %>% 
  select(country, total_litres_of_pure_alcohol) %>% 
  print(n=3)

#beer vs spirits
beer <- alcohol %>% 
  pull(beer_servings)

liquor <- alcohol %>% 
  pull(spirit_servings)

wilcox.test(beer, liquor, paired = TRUE)

ggplot(alcohol, mapping = aes(x=beer_servings, y=spirit_servings, color = continent)) +
  geom_point()

cor.test(beer, liquor)

#Total alcohol per continent

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

alcOutliers <- alcohol %>% 
 # pivot_longer(names_to = continent, values_to = total_litres_of_pure_alcohol, -type) %>% 
  group_by(continent) %>% 
  mutate(outlier = if_else(is_outlier(total_litres_of_pure_alcohol), country, as.character(NA)))
  
ggplot(alcOutliers, mapping = aes(x=continent, y=total_litres_of_pure_alcohol)) +
  geom_boxplot() +
  #geom_text(aes(label=outlier), position = position_nudge(x=0, y=0), hjust=0.3) +
  labs(x="Continent", y="Total alcohol consumption (litres)") +
  theme_bw() +
  theme(axis.text = element_text(colour="grey20", size=12, angle=90, hjust = 0.5, vjust=0.5),
        axis.text.y = element_text(colour = "grey20", size=12, margin = margin(t=0), angle = 0),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 10, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        text=element_text(size = 16),
        plot.margin = margin(t = 20, r = 30, b = 10, l = 30),
        axis.text.x = element_text(angle=45, vjust = 0.5))

continentAlcohol <- aov(total_litres_of_pure_alcohol~continent, data = alcohol)
summary(continentAlcohol)

TukeyHSD(continentAlcohol)
