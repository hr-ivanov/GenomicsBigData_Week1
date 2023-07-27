download.file(url="https://ndownloader.figshare.com/files/2292169",
              destfile = "data/surv.csv")
surveys <- read_csv("data/surv.csv")

surveys_complete <- surveys %>%
  filter(!is.na(weight),           # remove missing weight
         !is.na(hindfoot_length),  # remove missing hindfoot_length
         !is.na(sex))                # remove missing sex

## Extract the most common species_id
species_counts <- surveys_complete %>%
  count(species_id) %>% 
  filter(n >= 50)

## Only keep the most common species
surveys_complete <- surveys_complete %>%
  filter(species_id %in% species_counts$species_id)

write_csv(surveys_complete, file = "data_output/surveys_complete.csv")
surveys_complete <- read_csv("data_output/surveys_complete.csv")


#ggplot(data = <DATA>, mapping = aes(<MAPPINGS>)) +  <GEOM_FUNCTION>()
surveys_plot <- ggplot(data = surveys_complete, mapping = aes(x=weight, y=hindfoot_length))
#geom_point() for scatter plots, dot plots, etc.
#geom_boxplot() for, well, boxplots!
#geom_line() for trend lines, time series, etc.
surveys_plot +
  geom_point(alpha=0.1, color="gold")
#alpha = trancparency
# This is the correct syntax for adding layers
#surveys_plot +
 # geom_point()

# This will not add the new layer and will return an error message
#surveys_plot
#+ geom_point()

install.packages("hexbin")
library(hexbin)
library(ggplot2)

surveys_plot + 
  geom_hex()

surveys_plot +
  geom_point(alpha=0.1, aes(color = species_id))

surveys_plot + 
  geom_jitter(alpha = 0.1, aes(color = species_id))

surveys_plot_weights <- ggplot(data = surveys_complete, mapping = aes(x=species_id, y=weight))
surveys_plot_weights + 
  geom_jitter(alpha = 0.3) +
  geom_boxplot(alpha = 0, aes(color = plot_type))

#Question 3
surveys_plot_weights2 <- ggplot(data = surveys_complete, mapping = aes(x=species_id, y=(weight))) #!!!
surveys_plot_weights2 +
  scale_y_log10() + 
  geom_jitter(alpha = 0.3) +
  geom_violin()

surveys_boxplot_legs <- ggplot(data = surveys_complete, mapping = aes(x=species_id, y=hindfoot_length, color=species_id))
surveys_boxplot_legs +
  geom_jitter(alpha=0.3) +
  geom_boxplot()

yearly_counts <- surveys_complete %>% 
  count(year, species_id, sex)

ggplot(data = yearly_counts, mapping = aes(x=year, y=n, group = species_id, color=species_id)) +
  geom_line()

ggplot(data = yearly_counts, mapping = aes(x=year, y=n, color=sex)) +
  geom_line() +
  facet_wrap(~ species_id) +
  theme_bw() + #white background
  theme(panel.grid=element_blank())

ggplot(data = yearly_counts, mapping = aes(x=year, y=n, color=sex)) +
  geom_line() +
  facet_wrap(~ species_id) +
  theme_minimal() + #no borderline
  theme(panel.grid=element_blank())

#Question 4
yearly_av_weight <- surveys_complete %>% 
  group_by(year, species_id) %>% 
  summarise(avg_w = mean(weight))
yearly_av_weight

ggplot(data = yearly_av_weight, mapping = aes(x=year, y=avg_w, color = species_id)) +
  geom_line() +
  facet_wrap(~species_id)

# One column, facet by rows
yearly_sex_weight <- surveys_complete %>%
  group_by(year, sex, species_id) %>%
  summarize(avg_weight = mean(weight))

ggplot(data = yearly_sex_weight, 
       mapping = aes(x = year, y = avg_weight, color = species_id)) +
  geom_line() +
  facet_grid(sex ~ .)

#The facet_wrap geometry extracts plots into
#an arbitrary number of dimensions to allow them
#to cleanly fit on one page. On the other hand,
#the facet_grid geometry allows you to explicitly
#specify how you want your plots to be arranged via formula notation
#(rows ~ columns; a . can be used as a placeholder that indicates only one row or column).

# One row, facet by column
ggplot(data=yearly_sex_weight,
       mapping = aes(x = year, y=avg_weight, color=species_id)) +
  geom_line() +
  facet_grid(.~ sex)

yearly_sex_counts <- surveys_complete %>%
  count(year, species_id, sex)

ggplot(data = yearly_sex_counts, mapping = aes(x = year, y=n, color = sex)) +
  geom_line() +
  facet_wrap(~ species_id) +
  labs(title = "Observed species in time",
       x = "Year of observation",
       y = "Number of species") +
  theme_bw() +
  theme(axis.text = element_text(colour="grey20", size=12, angle=90, hjust = 0.5, vjust=0.5),
        axis.text.y = element_text(colour = "grey20", size=12),
        text=element_text(size = 16))
#https://stackoverflow.com/questions/7263849/what-do-hjust-and-vjust-do-when-making-a-plot-using-ggplot

install.packages("extrafont")
library(extrafont)
font_import()
y

grey_theme <- theme(axis.text.x = element_text(colour="grey20", size=12, angle=90, hjust=0.5, vjust=0.5),
                    axis.text.y = element_text(colour = "grey20", size = 12),
                    text=element_text(size=16))

ggplot(data=surveys_complete, aes(x=species_id, y=hindfoot_length)) +
  geom_boxplot() +
  grey_theme

#Question 5
#a.
#http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/
#https://stackoverflow.com/questions/74287605/how-do-i-change-the-legend-title-for-this-ggplot-in-r

sleep_plot <- ggplot(data=msleep, mapping = aes(x=sleep_cycle, y=sleep_total, color=vore)) +
  geom_point()

sleep_plot + 
  scale_color_hue(labels=c("Carnivore", "Herbivore", "Insectivore", "Omnivore", "No data")) +
  labs(fill ="Diet", color="Diet",
    x="Sleep cycle", y="Total amount of sleep (hours)") +
  scale_fill_discrete(labels=c("Carnivore", "Herbivore", "Insectivore", "Omnivore", "No data"))

#b
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/

n_animals_plot_type <- surveys_complete %>% 
  count(plot_type, year, sex)

animals_plot_type_plot <- ggplot(data=n_animals_plot_type, mapping=aes(x=year, y=n, color=plot_type)) +
  geom_line(size=1) +
  scale_colour_manual(values = cbbPalette) +
  facet_wrap(~sex)

#c - you can ignore it; was just playing with different plot types
#http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html

ggplot(data=head(surveys), mapping = aes(x=record_id, y=year)) +
  geom_count()

ggsave(filename = 'data_output/animals_plot_type_plot.png', animals_plot_type_plot, width=15, height = 10)
