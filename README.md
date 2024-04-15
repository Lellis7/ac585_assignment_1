# ac585_assignment_1
GitHub username is Liam_Ellis
Repository name is Order Beyond Borders
Project folder name is project
html file name is report.html
---
title: <span style="color:blue;">Orders</span> <span style="color:white;">Beyond</span> <span style="color:red;">Borders</span>
subtitle: "The **Influence** of **American Foreign Policy** and **Climate Change** on 
**Internal Displacement**"
author: "Liam Ellis"
date: "2024-04-14"
format:
  html:
    theme: moon
    toc: true
backgroundcolor: lightgreen
execute: 
  echo: false
  warning: false
  message: false
---
This year is an important year for the future of world **Economic**, *Foreign*, and **Climate policy** as over <span style="color:blue; font-family:Arial; font-size:18px;">2 billion people</span> head to the ballot boxes to cast their votes in their respective general elections. Perhaps most importantly is the US General election which is due to take place in November. The US economy's **gross domestic product** is <span style="color:green; font-family:Georgia; font-size:16px;">$25.4397 trillion</span> or **25%** of the world's GDP, therefore its leadership has a significant impact on world affairs. At the present the two most prominent candidates are ***Joe Biden*** (*Democrat*) and ***Donald Trump*** (*Republican*). Both of these candidates have very opposing views in relation to foreign and environmental policies which both significantly affect the displacement of people in the poorer nations either directly through military intervention or lack thereof and indirectly through the negative consequences of CO2 emissions where America is the second largest emitte
```{r}
#|  label: setup
#|  include: false

# libraries
library(tidyverse)
library(maps)
library(plotly)
# data
american_presidents_2_American_Presidents_1_ <- read_csv("american_presidents_2 - American_Presidents (1).csv")
data_right_3_1_data_right_3_1_ <- read_csv("data_right_3 (1) - data_right_3 (1).csv")
unicef_child_mortality_Unicef_Child_Mortality_1_ <- read_csv("unicef_child_mortality - Unicef_Child_Mortality (1).csv")
unicef_percent_pop_unimpr_water_2_Unicef_Percent_Pop_Unimpr_Water_1_ <- read_csv("unicef_percent_pop_unimpr_water_2 - Unicef_Percent_Pop_Unimpr_Water (1).csv")
unicef_internally_displaced_people_Unicef_Internally_Displaced_people <- read_csv("unicef_internally_displaced_people - Unicef_Internally_Displaced_people.csv")
#transformations
unimproved_water <- full_join(american_presidents_2_American_Presidents_1_,unicef_percent_pop_unimpr_water_2_Unicef_Percent_Pop_Unimpr_Water_1_,by = c("year"))
view(unimproved_water)
unicef_data <- unicef_child_mortality_Unicef_Child_Mortality_1_ %>%
  full_join(unicef_internally_displaced_people_Unicef_Internally_Displaced_people, by = c("country", "year")) %>%
  full_join(unicef_percent_pop_unimpr_water_2_Unicef_Percent_Pop_Unimpr_Water_1_, by = c("country", "year")) %>%
  full_join(american_presidents_2_American_Presidents_1_, by = c("year"))%>%
  full_join(data_right_3_1_data_right_3_1_,by = c("country"))
excluded_year <- c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008)
filtered_data <- unicef_data[!unicef_data$year %in% excluded_year, ]
excluded_variables <- c(NA)
filtered_data_2 <- filtered_data[!filtered_data$dis %in% excluded_variables, ]
excluded_variables_2 <- c(NA)
filtered_data_3 <- filtered_data_2[!filtered_data_2$wat %in% excluded_variables_2, ]
excluded_variables_3 <- c(NA)
filtered_data_4 <- filtered_data_3[!filtered_data_3$mor %in% excluded_variables_3, ]
excluded_variables_4 <- c(NA)
filtered_data_5 <- filtered_data_4[!filtered_data_4$pre %in% excluded_variables_4, ]
```
```{r}
map_world <- map_data("world")
pop_2022_water <- unimproved_water %>% 
  filter(year == 2022,wat > 2)
map_pop_2022_water <- full_join(map_world,pop_2022_water, by = c("region" = "country"))
#map_output
map_plot <- unimproved_water %>% 
  filter(year == 2022, wat > 2) %>%
  full_join(map_world, by = c("country" = "region")) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = wat))+
  geom_polygon(na.rm = TRUE)+
  labs(
    title = "Population who don't have access to Improved Drinking Water in 2022",
    subtitle = "Countries in grey have no available data for the period",
    caption = ("Source: UNICEF Data Warehouse"),
    x = "Longitude",
    y = "Latitude",
    fill = " ") +
  theme_classic() +
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(size = 12))
map_plot + guides(fill = "none")      
```
**Figure 1:** *Map*  
This outlines population of internally displaced people throughout the world. Although many of these countries are in <span style="color:blue; font-style:italic;">Africa</span> their numbers are relatively small in relation to the countries that have been plagued by war e.g. <span style="color:green; font-size:18px; font-family:cursive;">Sudan</span>. This data represents the **population of internally displaced people** in *2022*. ***Grey area has no data available***.
```{r}
library(dplyr)
library(ggplot2)
library(scales)
filtered_data_2021 <- filtered_data_5 %>%
  filter(year == 2021)
ggplot(filtered_data_2021) +
  aes(wat, dis, color = continent, size = mor) +
  geom_point(alpha = 1) +
  labs(
    title = "Correlation between Internal Displacement and Unimproved Drinking Water in 2021",
    caption = "Source: UNICEF Data Warehouse",
    y = "Internally Displaced People",
    color = "Continent" 
  ) +
  scale_size_continuous(name = "Child Mortality Rate per 1000 Children") + 
  scale_y_continuous(labels = comma_format()) +  
  theme_classic() +
  theme(
    text = element_text(family = "calibri"),
    plot.title = element_text(size = 12, color = "blue", face = "bold.italic"),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    panel.background = element_rect(fill = "#E0F0FF") 
  )
```
**Figure 2:** *Scatterplot*  
Outlines the correlation between **Internally Displaced** people and those using <span style="color:blue;">*Unimproved Drinking water*</span>. Although the data points are skewed to the right in the first number of years, it can be observed that as time progresses more countries present themselves in the left pane of the scatterplot, outlining that people are becoming displaced in countries where people have better access to cleaner drinking water. The scatterplot indicates that there is a **positive correlation** between using *unimproved drinking water* and the number of people becoming affected by internal displacement.
```{r}
library(scales)
filtered_data_5 %>%
  group_by(continent, year) %>%
  summarise(mean_dis = mean(dis, na.rm = TRUE)) %>%
  ggplot() +
  aes(reorder(continent, mean_dis), mean_dis, fill = continent) +
  geom_col() +
  facet_wrap(~year, nrow = 1) +
  labs(
    title = "Internal Displacement by Continent",
    y = "Internally Displaced People"
  ) +
  theme_classic() + 
  theme(
    text = element_text(family = "arial"),
    plot.title = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.title.x = element_blank()
  ) +
  scale_fill_manual(
    name = "Continent",
    labels = c("Asia", "Europe", "Africa", "Americas", "Oceania"),
    values = c("purple", "green", "red", "orange", "brown")
  ) +
  scale_y_continuous(labels = comma_format())
```
```{r}
ggplot(filtered_data_5, aes(pre, dis, fill = continent)) +
  geom_col(position = "dodge") +
  labs(
    title = "Average no. Displaced People for each Presidental Period by Continent",
    y = "Average no. Internally Displaced People"
  ) +
  theme_classic() + 
  theme(
    panel.background = element_rect(fill = "lightblue"),
    text = element_text(family = "gothic"),
    plot.title = element_text(size = 12),
    axis.title.x = element_blank()
  )  +
  scale_fill_manual(
    name = "Continent",
    labels = c("Asia", "Europe", "Africa", "Americas", "Oceania"),
    values = c("purple", "green", "red", "orange", "brown")
  ) +
  scale_y_continuous(labels = comma_format())
```
**Figure 3:** *Barchart*   
Outlines the population of the world that were *Internally Displaced* during each **President's tenure** in office. During the period of ***2009-2022***, this was <span style="color:blue;font-family:Arial;">242,196,069</span> people. In ***2009***, when ***<span style="color:green;font-weight:bold;">Barack Obama</span>*** was in office, there were **<span style="color:#FF5733;">10,716,690</span>** displaced people. This rose to **<span style="color:#8A2BE2;">29,699,350</span>** when ***<span style="font-style:italic;">Joe Biden</span>*** held the role in ***2022***. This is a ***<span style="color:red;font-weight:bold;font-size:16pt;">177%</span>*** increase over the **12 years**. Use the filter button below to select one or more of presidents. *Figure 3a* outlines the ***Internal Displacement in each Continent*** between the period **2009 to 2021**. *Figure 3b* outlines the ***Average Number of People Displaced by each President*** under their presidency. *Note that Barrack Obama was president for 8 years while Donald Trump and Joe Biden have only been president for 4 years each respectively.*
```{r}
timeseries_plot_1 <- filtered_data_5 %>%
  ggplot() +
  aes(year, mor, group = country, color = continent) +
  geom_line() +
  theme_bw() + 
  labs(
    title= "Child Mortality by Country",
    y = "Mortality rate per 1,000 children aged 1-4 years"
  ) +
  theme(
    text = element_text(family = "calibri"),
    plot.title = element_text(size = 12),
    axis.title.x = element_blank()
  ) +
  scale_color_manual(
    name = "Continent",
    labels = c("Asia", "Europe", "Africa", "Americas", "Oceania"),
    values = c("purple", "green", "red", "orange", "brown")
  )
ggplotly(timeseries_plot_1)
```
```{r}
library(dplyr)
library(plotly)
average_mor_by_continent_year <- filtered_data_5 %>%
  group_by(continent, year) %>%
  summarise(average_mor = mean(mor, na.rm = TRUE))
plot_ly(average_mor_by_continent_year, x = ~year, y = ~average_mor, color = ~continent, type = "scatter", mode = "markers+lines") %>%
  layout(
    title = "Average Mortality Rate by Continent",
    xaxis = list(title = "Year"),
    yaxis = list(title = "Mortality rate per 1,000 children aged 1-4 years"),
    legend = list(title = "Continent"),
    font = list(family = "calibri", size = 12)
  )
```
**Figure 4:** *Timeseries*   
Outlines the Child Mortality per 1,000 children aged 1-48 months. The data indicates that Africa has a significantly higher Child Mortality
rate than other parts of the world. Some of the factors that may affect this include lack of nutrition as a result of starvation from adverse weather conditions, violence due to ongoing wars or civil unrest and poor water quality resulting from lack of wells and drought. ***<span style="color:purple;font-weight:bold;">This shows the affect American Foreign and Climate policy can have on people outside the borders in which these decisions are made in.</span>***. **Figure 4a:** outlines ***<span style="color:yellow;font-weight:bold;"> Child Mortality by Country while</span>*** while **Figure 4b:**outlines ***<span style="color:yellow;font-weight:bold;"> Child Mortality by Continent</span>***.
The *Economist* commissioned a **US General Election Survey** of 1,670 registered voters between the **11th and 13th of February 2024**. The findings indicate that **34%** of respondents stated they would vote for *Joe Biden*, **45%** of respondents stated they would vote for *Donald Trump*, and **21%** stated they were unsure which to vote for. Both of these candidates have ***<span style="color:red;font-weight:bold;font-size:16pt;">extremely opposing views</span>*** in relation to what the best course of action for America going forward. Given Trump's recent comments in relation to ***<span style="color:green;font-weight:bold;">NATO</span>*** and his views in relation to inciting ***<span style="color:green;font-weight:bold;">Russia</span>*** and any other hostile countries/groups to attack countries "not paying their bills" is worrying. If America fails to support its NATO allies under article 5, this will lead to **large increases in internally displaced people and an increase in child mortality** due to the violence from attacking parties.
*Joe Biden* is voting on the promise of a more integrated, stronger western world with America being the centerpoint of any such axis. In contrast, *Trump* is voting on the promise of *isolationism* with views like ***<span style="color:blue;font-weight:bold;font-size:16pt;">"America First"</span>*** and  ***<span style="color:white;font-weight:bold;font-size:16pt;">"Make America Great Again"</span>***. **51%** of Republican party members believe that funding for *Israel* should be maintained or even increased. At present, there are ***<span style="color:orange;font-weight:bold;font-size:16pt;">1.7 million</span>*** internally displaced people in *Gaza*, ***<span style="color:black;font-weight:bold;font-size:20pt;">half of these are children.</span>*** ***<span style="color:orange;font-weight:bold;font-size:16pt;">Maintaining or even increasing military spending in this way would only result in more child casualties and a deeper humanitarian crisis.</span>***
*Joe Biden* is voting on the promise of a more integrated, stronger western world with America being the centerpoint of any such axis. In contrast, *Trump* is voting on the promise of isolationism with views like ***<span style="color:blue;font-weight:bold;font-size:16pt;">"America First"</span>*** and ***<span style="color:white;font-weight:bold;font-size:16pt;">"Make America Great Again"</span>***. **51%** of Republican party members believe that funding for *Israel* should be maintained or even increased. At present, there are **1.7 million** internally displaced people in *Gaza*, half of these are children. Maintaining or even increasing military spending in this way would only result in more child casualties and a deeper humanitarian crisis. 
*Biden's* policies in relation to environment and climate change couldn't be further from that of *Donald Trump*, who removed America from the *Paris Agreement* effective from the ***<span style="color:white;font-weight:bold;font-size:14pt;">4th of November 2020</span>***. He has often referred to **climate change** as a ***<span style="color:purple;font-weight:bold;font-size:16pt;">"hoax"</span>***. In contrast to this, *Biden* has implemented the ***<span style="color:darkpink;font-weight:bold;font-size:20pt;">"Inflation Reduction Act"</span>*** in order to ensure an efficient green energy transition in the US. As aforementioned, one of the reasons for people becoming internally displaced is climate change, and poor involvement from the world's second-largest CO2 emitter would not be enough to counter any adverse weather effects in these *poorer countries.* 
We here at ***<span style="color:blue;font-weight:bold;font-size:18pt;">UNICEF</span>*** have seen large increases among the numbers of internally displaced people and must significantly among children. The data indicates the strong relationships between the percentage of the population using unimproved drinking water, child mortality, and the population of internally displaced people.<span style="text-decoration: underline;">***<span style="color:red;font-weight:bold;font-size:20pt;">"Action is needed from policymakers and politicians across the political spectrum and the world in order to prevent this becoming a bigger issue."</span>***
