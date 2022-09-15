---
title: "Task 6 Karis Saplys"
author: "Karis Saplys"
date: "2022-09-13"
output: 
  html_document:
    theme: cosmo
    keep_md: TRUE
editor_options: 
  chunk_output_type: console
---




```r
#install.packages('lubridate')
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
library(readr)
library(tidyverse)
```

```
## ── Attaching packages
## ───────────────────────────────────────
## tidyverse 1.3.2 ──
```

```
## ✔ ggplot2 3.3.6     ✔ dplyr   1.0.9
## ✔ tibble  3.1.8     ✔ stringr 1.4.1
## ✔ tidyr   1.2.0     ✔ forcats 0.5.2
## ✔ purrr   0.3.4     
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ lubridate::as.difftime() masks base::as.difftime()
## ✖ lubridate::date()        masks base::date()
## ✖ dplyr::filter()          masks stats::filter()
## ✖ lubridate::intersect()   masks base::intersect()
## ✖ dplyr::lag()             masks stats::lag()
## ✖ lubridate::setdiff()     masks base::setdiff()
## ✖ lubridate::union()       masks base::union()
```

```r
library(knitr)
library(downloader)
library(dplyr)
```


```r
irisdat <- iris
head(irisdat)
```

```
##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
## 1          5.1         3.5          1.4         0.2  setosa
## 2          4.9         3.0          1.4         0.2  setosa
## 3          4.7         3.2          1.3         0.2  setosa
## 4          4.6         3.1          1.5         0.2  setosa
## 5          5.0         3.6          1.4         0.2  setosa
## 6          5.4         3.9          1.7         0.4  setosa
```


```r
best_pwidth <- iris %>%
  group_by(Species) %>%
  filter(row_number(desc(Petal.Width)) == 1)

best_plength <- iris %>%
  group_by(Species) %>%
  filter(row_number(desc(Petal.Length)) == 1)

ggplot(data = iris, mapping = aes(y = Sepal.Width, 
                                  x = Sepal.Length, 
                                  color = Species,
                                  shape = Species)) +
  geom_point() +
  geom_point(size = 3, shape = 1, color = "black", data = best_pwidth) +
  geom_point(size = 3, shape = 1, color = "black", data = best_plength) +
  geom_text(aes(color = Species, label = "Largest Petal Width"), data = best_pwidth, nudge_y = -0.15) +
  geom_text(aes(color = Species, label = "Largest Petal Length"), data = best_plength,nudge_y = -0.15) +
  labs(y = "Sepal Width (cm)",
       x = "Sepal Length (cm)",
       title = "Different Iris Species Have Different Sepal Sizes",
       subtitle = "The Largest Petal Sizes For Each Species Do Not Correspond To The Largest Sepal Sizes") +
  theme(legend.position = "bottom")
```

![](Task-6-Karis-Saplys_files/figure-html/iris plot-1.png)<!-- -->


```r
ScrabbleData <- tempfile()
download.file("https://media.githubusercontent.com/media/fivethirtyeight/data/master/scrabble-games/scrabble_games.csv",
ScrabbleData, mode = "wb")
ScrabbleDat <- read_csv(ScrabbleData)
```

```
## Rows: 1542642 Columns: 19
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr   (2): winnername, losername
## dbl  (14): gameid, tourneyid, winnerid, winnerscore, winneroldrating, winner...
## lgl   (2): tie, lexicon
## date  (1): date
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
ScrabbleAll <- ScrabbleDat %>%
  select(date, winnerscore, loserscore) %>%
  pivot_longer(c("winnerscore","loserscore"), names_to = "win-loss", values_to = "score") %>%
  filter(score > 0) %>%
  mutate(year = year(date),
         week = week(date)
  )
```


```r
avg_scrabble <- ScrabbleAll %>%
group_by(year, week) %>%
summarise(avglength = mean(score), Date = min(date))
```

```
## `summarise()` has grouped output by 'year'. You can override using the
## `.groups` argument.
```

```r
head(avg_scrabble)
```

```
## # A tibble: 6 × 4
## # Groups:   year [2]
##    year  week avglength Date      
##   <dbl> <dbl>     <dbl> <date>    
## 1  1976    49      367. 1976-12-05
## 2  1977     5      324  1977-02-01
## 3  1977     9      317. 1977-02-27
## 4  1977    10      414. 1977-03-05
## 5  1977    13      371. 1977-03-26
## 6  1977    19      354. 1977-05-07
```

```r
after_avg <- ScrabbleAll %>% 
group_by(year, week) %>%
filter(date > as.Date("2006-03-01")) %>%
summarise(avglength = mean (score), Date = min(date))
```

```
## `summarise()` has grouped output by 'year'. You can override using the
## `.groups` argument.
```

```r
before_avg <- ScrabbleAll %>% 
group_by(year, week) %>%
filter(date <= as.Date("2006-03-01")) %>%
summarise(avglength = mean (score), Date = min(date))
```

```
## `summarise()` has grouped output by 'year'. You can override using the
## `.groups` argument.
```

```r
ggplot(data = avg_scrabble, mapping = aes(x = Date, y = avglength)) +
  geom_point() +
  geom_smooth(data = before_avg, method = 'lm', color="orange") +
  annotate("text", x=as.Date("2005-12-01"), y = 380, label = "Trend Before", color = "orange", size = 3) + geom_smooth(data = after_avg, method = 'lm', color = "blue") +
  annotate("text", x=as.Date("2006-07-15"), y = 380, label = "Trend After", color = "blue", size = 3) + 
  geom_vline(xintercept=as.Date("2006-03-01"), linetype = "dotted") +
  annotate ("text", x = as.Date("2006-03-01"), y = 390, label = "Dictionary\nUpdated", size = 3) +
  labs(y = "Average Score", x = "", title = "Scrabble scores in the age of 'QI' and 'ZA'", subtitle = 'Weekly average scores before and after the addition of around 11,000 words to the Scrabble dictionary', caption = 'Source: FiveThirtyEight') +
  coord_cartesian(ylim = c(355,405), expand = FALSE) +
  scale_x_date(date_breaks = "3 month", 
  limits = as.Date(c('1/6/2005', '1/9/2006'), format = "%d/%m/%Y"), 
  date_labels = "%b-%y" ) + 
  theme_bw()
```

```
## `geom_smooth()` using formula 'y ~ x'
```

```
## Warning: Removed 542 rows containing non-finite values (stat_smooth).
```

```
## `geom_smooth()` using formula 'y ~ x'
```

```
## Warning: Removed 550 rows containing non-finite values (stat_smooth).
```

```
## Warning: Removed 1092 rows containing missing values (geom_point).
```

![](Task-6-Karis-Saplys_files/figure-html/scrabble plot-1.png)<!-- -->





