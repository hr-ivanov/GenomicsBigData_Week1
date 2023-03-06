whyVisualize <- read.csv("data/week_6_why_visualize.csv")
whyVisualize
mean(whyVisualize$x)
mean(whyVisualize$y)
cor(whyVisualize$x, whyVisualize$y)
library(ggplot2)
ggplot(data=whyVisualize, mapping = aes(x=whyVisualize$x, y=whyVisualize$y)) +
  geom_point()
#A Pearson correlation is a measure of a linear association between 2 normally distributed random variables.
#https://journals.lww.com/anesthesia-analgesia/fulltext/2018/05000/correlation_coefficients__appropriate_use_and.50.aspx#:~:text=Correlation%20coefficients%20describe%20the%20strength,monotonic%20relationship%20between%202%20variables.

datasaurus <- read.csv("data/week_6_DatasaurusDozen.csv")
view(datasaurus)

datasaurus_grouped <- datasaurus %>% 
  group_by(object_name) %>% 
  summarise(meanX = mean(x), meanY = mean(y), pearsonCorr = cor(x,y))
datasaurus_grouped

ggplot(data = datasaurus, mapping = aes(x=datasaurus$x, y=datasaurus$y)) +
  geom_point() +
  facet_wrap(~object_name)
