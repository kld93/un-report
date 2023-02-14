#Carpentry tutuorial
#R for data analysis

#Loading in the data
library(tidyverse)
gapminder_data <- read_csv("gapminder_data.csv")

#Getting stats with summarize()
summarize(gapminder_data, averageLifeExp=mean(lifeExp))

gapminder_data %>%
  summarize(averageLifeExp=mean(lifeExp))

gapminder_data_summarized <- gapminder_data %>%
  summarize(averageLifeExp=mean(lifeExp))

#Narrow down rows with filter()
gapminder_data %>%
  filter(year == 2007) %>%
  summarize(average=mean(lifeExp))

gapminder_data %>%
  summarize(first_year=min(year)) #1952 is the first year
gapminder_data %>%
  filter(year == 1952) %>%
  summarize(average_gdp=mean(gdpPercap))

#Grouping rows with group_by()
gapminder_data %>%
  group_by(year) %>%
  summarize(average=mean(lifeExp))
gapminder_data %>%
  group_by(continent) %>%
  summarize(average=mean(lifeExp))

gapminder_data %>%
  group_by(continent) %>%
  summarize(average=mean(lifeExp), min=min(lifeExp)) #adding multiple new columns

#Making new variables with mutate()
gapminder_data %>%
  mutate(gdp = pop * gdpPercap)
gapminder_data %>%  
  mutate(gdp = pop * gdpPercap, popInMillions = pop / 1000000)  

#Subset columns with select()
gapminder_data %>%
  select(pop, year) #only want pop and year
gapminder_data %>%
  select(-continent) #want everything except continent
gapminder_data %>%
  select(country, continent, year, lifeExp)

#Changing the shape of the data
gapminder_data %>%
  select(country, continent, year, lifeExp) %>%
  pivot_wider(names_from = year, values_from = lifeExp)

#Data we'll be using for the rest:
gapminder_data_2007 <- read_csv("gapminder_data.csv") %>% 
  filter(year == 2007 & continent == "Americas") %>% 
  select(-year, -continent)


#Cleaning up data
read_csv("co2-un-data.csv")
read_csv("co2-un-data.csv", skip=1)
co2_emissions_dirty <- read_csv("co2-un-data.csv", skip=2,
                                col_names=c("region", "country", "year", "series", "value", "footnotes", "source"))
co2_emissions_dirty

co2_emissions <- co2_emissions_dirty %>% 
  select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from=series, values_from=value) %>% 
  filter(year == 2005) %>% 
  select(-year)


#Joining dataframes
inner_join(gapminder_data_2007, co2_emissions, by="country")
anti_join(gapminder_data_2007, co2_emissions, by="country")

co2_emissions <- read_csv("co2-un-data.csv", skip=2,
                          col_names=c("region", "country", "year",
                                      "series", "value", "footnotes", "source")) %>%
  select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita")) %>%
  pivot_wider(names_from=series, values_from=value) %>%
  filter(year==2005) %>%
  select(-year) %>%
  mutate(country=recode(country,
                        "Bolivia (Plurin. State of)" = "Bolivia",
                        "United States of America" = "United States",
                        "Venezuela (Boliv. Rep. of)" = "Venezuela") #recoding the names to match gapminder
  )

anti_join(gapminder_data_2007, co2_emissions, by="country") #left with Puerto Rico

gapminder_data_2007 <- read_csv("gapminder_data.csv") %>%
  filter(year == 2007 & continent == "Americas") %>%
  select(-year, -continent) %>%
  mutate(country = recode(country, "Puerto Rico" = "United States")) 

gapminder_data_2007 <- read_csv("gapminder_data.csv") %>%
  filter(year == 2007 & continent == "Americas") %>%
  select(-year, -continent) %>%
  mutate(country = recode(country, "Puerto Rico" = "United States")) %>%
  group_by(country) %>%
  summarize(lifeExp = sum(lifeExp * pop)/sum(pop),
            gdpPercap = sum(gdpPercap * pop)/sum(pop),
            pop = sum(pop) #weighted average
  )

anti_join(gapminder_data_2007, co2_emissions, by="country")

gapminder_co2 <- inner_join(gapminder_data_2007, co2_emissions, by="country")

gapminder_co2 %>%  #create groups North America and South America
  mutate(region = if_else(country == "Canada" | country == "United States" | country == "Mexico", "north", "south"))  

#Save the new csv
write_csv(gapminder_co2, "gapminder_co2.csv")


#Analyzing combined data
ggplot(gapminder_co2, aes(x=gdpPercap, y=per_capita)) +
  geom_point() +
  labs(x="GDP (per capita)",
       y="CO2 emitted (per capita)",
       title="There is a strong association between a nation's GDP \nand the amount of CO2 it produces"
  ) + #using \n creates a new line for the rest of the title to be on
  geom_smooth(method="lm")

gapminder_co2 %>%
  mutate(region = if_else(country == "Canada" |
                            country == "United States" |
                            country == "Mexico", "north", "south")) %>%
  group_by(region) %>%
  summarize(sumtotal = sum(total),
            sumpop = sum(pop))
# “or”= |
#“and”= &&
#“not”= !


#Sort data with arrange()
gapminder_data %>%
  filter(year==2007) %>%
  group_by(continent) %>%
  summarise(average= mean(lifeExp)) %>%
  arrange(desc(average))


#Bonus exercises
gapminder_co2 %>%
  mutate(region = if_else(country == "Canada" | country == "United States" | country == "Mexico", "north", "south")) %>%
  mutate(totalPercent = total/sum(total)*100,
         popPercent = pop/sum(pop)*100)

gapminder_co2 %>%
  mutate(region = if_else(country == "Canada" | country == "United States" | country == "Mexico", "north", "south")) %>%
  mutate(totalPercent = total/sum(total)*100,
         popPercent = pop/sum(pop)*100) %>%
  group_by(region) %>%
  summarize(sumTotalPercent = sum(totalPercent),
            sumPopPercent = sum(popPercent))

#Bar plot
gapminder_co2 %>%
  mutate(region = if_else(country == "Canada" | country == "United States" | country == "Mexico", "north", "south")) %>%
  mutate(totalPercent = total/sum(total)*100,
         popPercent = pop/sum(pop)*100) %>% 
  ggplot(aes(x = reorder(country, - totalPercent), y = totalPercent, fill = region)) +
           geom_col() +  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 

#Lowest per capita emissions
gapminder_co2 %>%
  mutate(region = if_else(country == "Canada" | country == "United States" | country == "Mexico", "north", "south")) %>%
  mutate(totalPercent = total/sum(total)*100,
         popPercent = pop/sum(pop)*100) %>% 
  arrange(per_capita)

gapminder_co2 %>% filter(country == "Haiti" | country == "Paraguay" | country == "Nicaragua") %>%  
  ggplot(aes(x = reorder(country, -per_capita), y = per_capita)) +  
  geom_col() 
