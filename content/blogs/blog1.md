---
title: "Tour de France Analysis"
author: "Joseph"
date: "9/16/2020"
output: html_document
---

![](https://todaycycling.com/wp-content/uploads/2020/09/tadej-pogacar-%C3%A9tape-20-tour-de-france.jpg)


*Tadej Pogacar, on his way to winning the 2020 Tour de France*


```{r, setup, echo=FALSE}
knitr::opts_chunk$set(
  message = FALSE, 
  warning = FALSE, 
  tidy=FALSE,     # display code as typed
  size="small")   # slightly smaller font for code
options(digits = 3)

# default figure size
knitr::opts_chunk$set(
                      fig.width=6.75, 
                      fig.height=6.75,
                      fig.align = "center",
                      error = TRUE, 
                      echo = TRUE
                      )
```


```{r load-libraries,echo=FALSE}
library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(janitor)
library(png)
library(skimr)
library(vroom)
library(mosaic)
library(here)
library(rmarkdown)
library(ggExtra)
library(stringr)
library(dplyr)
library(reshape2)
library(tidyr)
library(forcats)

#remotes::install_github("kjhealy/socviz")
library(socviz)
```

## Background Information

As the Tour de France 2020 is currently underway, the peloton will have to ride over 3500km in a bit less than 3 weeks. While the outcome of this year's Tour, the first one to happen in September, is quite certain, this race is usually the theatre of much suspense and drama. Every year since 1903, the Tour de France is a cycling race taking place in the summer which slowly became inherent to the french culture. 

Today, any citizen will closely follow the route of the Tour and hope for it to pass by its village and get the chance to see the 'real' Maillot Jaune (leader of general classification). 
But while this race is globally known for its difficulty, let us get a closer look to its history. 

```{r ataglace dataset, echo=FALSE, warning=FALSE}
# Let's start by downloading the historical records of the Tour de France from 2018 in csv.

pretourdefrance <- read_csv("/Users/josephperrin/Desktop/London Business School/jperrinwebsite/data/2018_historical_guide.csv")

#Let's get a glimpse of the dataset
glimpse(pretourdefrance)

#Change variable type to numeric
pretourdefrance$Pace <- as.numeric(as.character(pretourdefrance$Pace))

# add in NA for years when there were no Tour (WWI and WWII)
names(pretourdefrance)[1] <- 'Year'
all_years <- data.frame(Year=seq(min(pretourdefrance$Year), max(pretourdefrance$Year), by=1))
tourdefrance <- merge(all_years, pretourdefrance, all=TRUE)


```

## Tour de France Overview

#### The Tour de France travels less distance, but the pace is dramatically increasing

```{r ataglance pace and distance, echo=FALSE, warning=FALSE}
# Make a plot with two different Y-axis on the same plot
ggplot(data = tourdefrance) + 
#build left axis
  geom_point(aes(x = Year, y = Distance), color = 'navy') +
#build right axis
  geom_point(data = tourdefrance, aes(x = Year, y = Pace*100), color = '#ffd942') +
#arrange the scale
  scale_y_continuous(sec.axis = sec_axis(~./100, name = 'Pace (in km/h)'))+ 
#let's arrange the titles and elements of the graph
  ggtitle('Stage distances are getting shorter, while pace is increasing', subtitle = 'Historical Distance and Pace of the Tour de France') +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.y.right = element_text(color = '#ffd942'),
        axis.title.y.right = element_text(color = '#ffd942'), 
        axis.text.y=element_text(color = 'navy'),
        axis.title.y=element_text(color = 'navy'), 
        axis.title.x = element_blank()) 
```



```{r number of stages, echo=FALSE, warning=FALSE, fig.width =5, fig.height=3}
# Make a plot with displaying the evolution of the number of stages along the years
ggplot(data = tourdefrance) + 
  geom_smooth(aes(x = Year, y = Stages), color = 'navy') +
  ggtitle('Number of stages increased since 1903', subtitle = 'Number of stages per Tour de France')
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 4), 
        axis.title.x = element_blank()) 
```

The first Tour de France was very short and consisted of only 8 stages. Rapidly, the number of stages increased and so did the distance of each Tour. Indeed the first Tours were almost 5000 km long on average, with the longest ever Tour taking place in 1926 (5745km!). In recent years the Tour de France was shorter (~3500km), with the shortest ever distance in the modern era appearing in 2002 with a total distance of 3278km (see table below).
Concerning the speed of the riders, the professionalization of the sport and the technological advances had the effect of dramatically increasing the speed of the peloton. The slowest Tour de France was run at an average speed of 24km/h while Lance Armstrong won one of his Tours at an average speed of over 41km/h. More recently, the Tour de France average pace stabilised, for evident physical reason (no man could cycle 3 weeks at average 80 km/h !), but also because the UCI (Union Cycliste Internationale) has implemented a growing number of standards and regulation to control the performance of riders. 

```{r tourdefrancerecords, echo=FALSE, warning=FALSE}
#Let us calculate the top 3 fastest riders of the Tour
fastestpace<- tourdefrance %>%
  select(Year,Pace, Winner) %>% 
  arrange(desc(Pace)) %>% 
  head(3)
fastestpace

#Let us calculate the top 3 slowest riders of the Tour
slowestpace<- tourdefrance %>%
  select(Year,Pace, Winner) %>% 
  arrange(desc(-Pace)) %>% 
  head(3)
slowestpace

#Let us calculate the top 3 longest Tour
maxdistance <- tourdefrance %>%
  select(Year,Distance) %>% 
  arrange(desc(Distance)) %>% 
  head(3)
maxdistance

#Let us calculate the top 3 shortest Tour
shortestdistance <- tourdefrance %>%
  select(Year,Distance) %>% 
  arrange(desc(-Distance)) %>% 
  head(3)
shortestdistance

#Let us calculate the top 3 shortest Tour in the modern Era
shortestdistancemodern <- tourdefrance %>%
  select(Year,Distance) %>%
  filter(Year > 1980) %>% 
  arrange(desc(-Distance)) %>% 
  head(3)
shortestdistancemodern

```

**Records **  
Fastest Pace: *Lance Armstron (41.7 km/h)*  
Slowest Pace: *Firmin Lambot (24.7 km/h)*  
Shortest Tour: *1903, 1904 (2428 km)*  
Shortest Modern Tour: *2002 (3278 km)*  
Longest Tour: *1926 (5745 km)*


#### The number of riders and finishers of the Tour de France increased over time


```{r tour de france completion, echo=FALSE, warning=FALSE, fig.width =7, fig.height=5}
#Let us plot the completion rate of riders of the Tour de France

tourdefrance %>% 
  ggplot +
#Let's define our graph
  geom_point(aes(x = Year, y = Ended/Started*100), color="navy")+
  geom_point(data = subset(tourdefrance, Year %in% 1998), aes(x = Year, y = Ended/Started*100), color="tomato") +
  geom_text(data =  subset(tourdefrance, Year %in% 1998), aes(x = Year * 1.003, y = Ended/Started*100*1.02, label = 1998)) +
#Let's add some titles
  ggtitle('More riders are finishing the tour over time', subtitle = "Historical completion rate of the Tour") +
  ylab('% of Cyclists Completing the Tour') +
  xlab('Year')
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.y=element_text(color = 'navy'),
        axis.title.y=element_text(color = 'navy'), 
        axis.title.x = element_blank()) 
```

The first years of the Tour de France were the theatre of an incredible spectacle between riders who would cycle night and day to join two cities. The lack of preparation and equipment led to many retirements and abandons. At the time, there were quite many disqualification as many occurrences of cheating, such as taking the train or a car to shorten the distance, were reported. However, drinking wine to boost your performance was not illegal!

In the more recent era of cycling, the falls and crashes are the most common reason for abandons. Another reason explaining disqualifications are the maximum delay rules, which entails that participants must finish the stage under a certain time, a rule which can be difficult for some riders in the mountain stages. 

Note that the 1998 Tour de France saw the abandon of many cycling teams participating to the Tour due to the "Festina Scandal", one of the first doping scandals of the sport. 

#### France counts the most wins

```{r tour de france winner country, echo=FALSE, warning=FALSE}
# Identify all the champions who have won the Tour de France more than once
mWinner <- unique(tourdefrance$Winner[duplicated(tourdefrance$Winner)])
mWinner <- mWinner[!is.na(mWinner)]
tourdefrance$multiWinner[which(tourdefrance$Winner %in% mWinner)] <- as.character(tourdefrance$Winner[tourdefrance$Winner %in% mWinner])

# Reorder the Country to go from most to least wins
tourdefrance$Country <- fct_infreq(tourdefrance$Country)

ggplot(data = tourdefrance) +
  geom_bar(aes(x = Country, fill = multiWinner)) +
  ggtitle('France has the largest number of Winners', subtitle = 'Distribution of Tour de France wins by country') +
  ylab('# of Champions') +  
  xlab('Country')+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.y=element_text(color = 'navy'),
        axis.title.y=element_text(color = 'navy'), 
        axis.title.x = element_blank(),
        legend.position="none") 

```
Let us now analyse the victories of the Tour de France, the most prestigious cycling race in the world. 
With regards to nationality, France counts the most champions with 36 victories. Note however that the last French winner of the tour de France Bernard Hinault was in 1985... Belgium comes second with 18 victories while the Spain is third (12 victories). 

**Records**  
7 Wins: ~~*Lance Armstrong (USA)*~~  
5 Wins: *Jacques Anquetil (FRA), Eddy Merckx (BEL), Bernard Hinault (GBR), Miguel Indurain (ESP)*  
4 Wins: *Christopher Froome (GBR)*  
3 Wins: *Philippe Thys (BEL), Louison Bobet (FRA), Greg LeMond (USA)*  
2 Wins: *Lucien Petit-Breton (FRA), Firmin Lambot (BEL), Ottavio Bottecchia (ITA), Nicolas Frantz (LUX), André Leducq (FRA), Antonin Magne (FRA), Sylvère Maes (BEL), Gino Bartali (ITA), Fausto Coppi (ITA), Bernard Thévenet (FRA), Laurent Fignon (FRA), Alberto Contador (ESP)*

```{r tour de france winners record, echo=FALSE, warning=FALSE}

winningrecords <- tourdefrance %>%
  select(Winner) %>%
  na.omit() %>% 
  group_by(Winner) %>%
  summarise(Count = count(Winner)) %>%
  filter(Count>1) %>% 
  arrange(desc(Count))

winningrecords

```

With regards to individual winners of the Tour de France, the rider with the most victories is Lance Armstrong, which was however convicted of doping a few years and saw all his wins cancelled. 4 riders have won the Tour 5 times, and Christopher Froome who won it 4 times already will try to join them in the classification in the Tour 2021. In total, 21 riders won the Tour at least 2 times. 



**Sources:**  
[Wikipedia](https://en.wikipedia.org/wiki/Tour_de_France)  
[Le Tour de France Official Website](https://www.letour.fr/en/history)  
