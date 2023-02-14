#Loan in necessary functions for analysis
library(readr)
library(tidyverse)

#Read in data for the analysis
gapminder_1997 <- read_csv("gapminder_1997.csv")
View(gapminder_1997)

#Explore different R commands
?read_csv
sum(5,6)
Sys.Date()


#Creating a plot
gapminder_1997
ggplot(data = gapminder_1997) +
  aes(x = gdpPercap, y = lifeExp, color = continent, size = pop/1000000) +
  labs(x = "GDP Per Capita", y = "Life Expectancy (yrs)", 
       title = "Do people in wealthy countries live longer?", 
       size = "Population (in millions)") +
  geom_point() +
  theme_bw() +
  scale_color_brewer(palette = "Set1") #didn't know this!!


#Read in full gapminder dataset
gapminder_data <- read_csv(file = "gapminder_data.csv")


#Plot time (x-axis) and life expectancy (y-axis) points
gapminder_data
ggplot(data = gapminder_data) +
  #add the aes objects time & lifeExp
  aes(x = year, y = lifeExp, color = continent, group = country) +
  #add the human readable labels
  labs(x = "Year", y = "Life Expectancy (yrs)") +
  #points!
  geom_line() +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")


#Make a boxplot with continent on the x and lifeExp on the y
ggplot(data = gapminder_1997) +
  aes(x = continent, y = lifeExp) +
  labs(x = "Continent", y = "Life Expectancy (yrs)", 
       title = "What is the life expectancy for each continent?") +
  geom_boxplot(outlier.color = "cornflowerblue") +
  theme_minimal()


#geom_violin
ggplot(data = gapminder_1997) +
  aes(x = continent, y = lifeExp, color = continent) +
  labs(x = "Continent", y = "Life Expectancy (yrs)", 
       title = "What is the life expectancy for each continent?") +
  geom_violin(fill = "black") +
  geom_point()


#boxplot
ggplot(data = gapminder_1997) +
  aes(x = continent, y = lifeExp, color = continent) +
  labs(x = "Continent", y = "Life Expectancy (yrs)", 
       title = "What is the life expectancy for each continent?") +
  geom_boxplot(outlier.shape = NA)+
  geom_point(alpha = 0.5, size = 5) #creating a density affect
  #geom_jitter() will scatter the points so they don't overlap




##Finish the rest of the tutorial independently

#putting jitter first puts the dots behind
ggplot(data = gapminder_1997) +
  aes(x = continent, y = lifeExp) +
  geom_jitter(aes(size = pop)) + 
  geom_violin()

#coloring the outline
ggplot(data = gapminder_1997) +
  aes(x = continent, y = lifeExp) +
  geom_violin(color="pink") 

#coloring the fill
ggplot(data = gapminder_1997) +
  aes(x = continent, y = lifeExp) +
  geom_violin(fill="pink") 

ggplot(data = gapminder_1997) +
  aes(x = continent, y = lifeExp) +
  geom_violin(aes(fill=continent))

#Univariate plots
ggplot(gapminder_1997) +
  aes(x = lifeExp) +
  geom_histogram(bins = 20)

#Plot themes
ggplot(gapminder_1997) +
  aes(x = lifeExp) +
  geom_histogram(bins = 20) +
  theme_classic()

#Facets
ggplot(gapminder_1997) +
  aes(x = gdpPercap, y = lifeExp) +
  geom_point() +
  facet_wrap(vars(continent))

ggplot(gapminder_1997) +
  aes(x = gdpPercap, y = lifeExp) +
  geom_point() +
  facet_grid(rows = vars(continent))

#Saving plots
ggsave("awesome_plot.jpg", width=6, height=4)

violin_plot <- ggplot(data = gapminder_1997) +
  aes(x = continent, y = lifeExp) +
  geom_violin(aes(fill=continent))
violin_plot

violin_plot + theme_bw()

violin_plot <- violin_plot + theme_bw()
ggsave("awesome_violin_plot.jpg", plot = violin_plot, width=6, height=4)

#Practice:
my_plot <- ggplot(gapminder_data) +
  aes(x = continent, y = gdpPercap, fill = continent) +
  geom_violin()
my_plot
ggsave("my_plot.jpg", plot = my_plot, width=6, height=4)


#Creating complext plots
install.packages(c("gganimate", "gifski"))
library(gganimate)
library(gifski)

staticHansPlot <- ggplot(data = gapminder_data)+
  aes(x = log(gdpPercap), y = lifeExp, size = pop/1000000, color = continent)+
  geom_point(alpha = 0.5) + # we made our points slightly transparent, 
  #because it makes it easier to see overlapping points
  scale_color_brewer(palette = "Set1") +
  labs(x = "GDP Per Capita", y = "Life Expectancy", color= "Continent", 
       size="Population (in millions)")+
  theme_classic()

staticHansPlot

animatedHansPlot <- staticHansPlot +
  transition_states(year,  transition_length = 1, state_length = 1)+
  ggtitle("{closest_state}")

animatedHansPlot

anim_save("hansAnimatedPlot.gif", 
          plot = animatedHansPlot,
          renderer = gifski_renderer())

#Map plots
# make sure names of countries match between the map info and the data
# NOTE: we haven't learned how to modify the data in this way yet, but we'll learn about that in the next lesson. Just take for granted that it works for now :)
#install.packages("maps")
#install.packages("ggthemes")
library(maps)
library(ggthemes)
mapdata <- map_data("world") %>%
  mutate(region = recode(region,
                         USA="United States",
                         UK="United Kingdom"))

#install.packages("mapproj")

