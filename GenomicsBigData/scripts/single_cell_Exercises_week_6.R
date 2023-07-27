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

#Assignment
myPlot <- read_tsv("data/Single cell studies database - Data.tsv")

myPlotGrouped <- myPlot %>% 
  filter(!is.na(Date), !is.na(`Number of reported cell types or clusters`)) %>% 
  mutate(year=substr(Date, 0, 4)) %>% 
  group_by(year) %>% 
  summarise(nCells = sum(`Number of reported cell types or clusters`))

myPlotGrouped
ggplot(data=myPlotGrouped, mapping = aes(x=year, y=nCells)) +
  geom_point(show.legend = FALSE) +
  labs(title = "Total number of cell types or clusters for the last 20 years",
                     x = "Year",
                     y = "Number of cell types or clusters") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        text=element_text(family = "Calibri"),
        axis.title.y = element_text(size = 15,
                                  margin = margin(t = 0, r = 20, b = 40, l = 20)),
        axis.title.x = element_text(size = 15,
                                    margin = margin(t = 20, r = 0, b = 10, l = 0)),
        plot.margin = margin(t = 20, r = 30, b = 10, l = 0),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle=45, vjust = 0.5),
        )

ggplot(data=myPlotGrouped, mapping = aes(x=year, y=nCells)) +
  geom_bar(stat = 'identity')

barplotGrouped <- myPlot %>% 
  filter(!is.na(Organism), !is.na(`Reported cells total`)) %>% 
  group_by(Organism) %>% 
  summarise(totalCells = sum(`Reported cells total`))
barplotGrouped

ggplot(data=barplotGrouped, mapping = aes(x=Organism, y=totalCells)) +
  geom_bar(stat = 'identity')
