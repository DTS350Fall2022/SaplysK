---
title: "Case Study 3 Karis Saplys DTS350"
author: "Karis Saplys"
date: "2022-09-05"
output: 
  html_document:
    keep_md: TRUE
---




```r
#install.packages("gapminder")
library(gapminder)
head(gapminder)
```

```
## # A tibble: 6 × 6
##   country     continent  year lifeExp      pop gdpPercap
##   <fct>       <fct>     <int>   <dbl>    <int>     <dbl>
## 1 Afghanistan Asia       1952    28.8  8425333      779.
## 2 Afghanistan Asia       1957    30.3  9240934      821.
## 3 Afghanistan Asia       1962    32.0 10267083      853.
## 4 Afghanistan Asia       1967    34.0 11537966      836.
## 5 Afghanistan Asia       1972    36.1 13079460      740.
## 6 Afghanistan Asia       1977    38.4 14880372      786.
```

```r
library(readr)
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
## ✔ ggplot2 3.3.6     ✔ dplyr   1.0.9
## ✔ tibble  3.1.8     ✔ stringr 1.4.1
## ✔ tidyr   1.2.0     ✔ forcats 0.5.2
## ✔ purrr   0.3.4     
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```

```r
library(knitr)
library(downloader)
library(dplyr)
```


```r
dtf <- filter(gapminder, country != "Kuwait")
dtf
```

```
## # A tibble: 1,692 × 6
##    country     continent  year lifeExp      pop gdpPercap
##    <fct>       <fct>     <int>   <dbl>    <int>     <dbl>
##  1 Afghanistan Asia       1952    28.8  8425333      779.
##  2 Afghanistan Asia       1957    30.3  9240934      821.
##  3 Afghanistan Asia       1962    32.0 10267083      853.
##  4 Afghanistan Asia       1967    34.0 11537966      836.
##  5 Afghanistan Asia       1972    36.1 13079460      740.
##  6 Afghanistan Asia       1977    38.4 14880372      786.
##  7 Afghanistan Asia       1982    39.9 12881816      978.
##  8 Afghanistan Asia       1987    40.8 13867957      852.
##  9 Afghanistan Asia       1992    41.7 16317921      649.
## 10 Afghanistan Asia       1997    41.8 22227415      635.
## # … with 1,682 more rows
```


```r
ggplot(data = dtf) +
  geom_point(mapping = aes(size = pop/100000, x = lifeExp, y = gdpPercap, color = continent, group = year)) +
  facet_wrap(~ year, nrow = 1) +
  scale_y_continuous(trans = "sqrt") +
  scale_size_continuous(name = "Population (100k)") +
  xlab("Life Expectancy") +
  ylab("GDP per capita") +
  theme_bw()
```

![](Case-Study-3-Karis-Saplys-DTS350_files/figure-html/plot 1-1.png)<!-- -->


```r
weighted_avg <- dtf %>%
  group_by(year, continent) %>%
  summarise(average = weighted.mean(gdpPercap), population = pop/10000)
```

```
## `summarise()` has grouped output by 'year', 'continent'. You can override using
## the `.groups` argument.
```

```r
weighted_avg
```

```
## # A tibble: 1,692 × 4
## # Groups:   year, continent [60]
##     year continent average population
##    <int> <fct>       <dbl>      <dbl>
##  1  1952 Africa      1253.      928. 
##  2  1952 Africa      1253.      423. 
##  3  1952 Africa      1253.      174. 
##  4  1952 Africa      1253.       44.2
##  5  1952 Africa      1253.      447. 
##  6  1952 Africa      1253.      245. 
##  7  1952 Africa      1253.      501. 
##  8  1952 Africa      1253.      129. 
##  9  1952 Africa      1253.      268. 
## 10  1952 Africa      1253.       15.4
## # … with 1,682 more rows
```

```r
ggplot(data = dtf, mapping = aes(x = year, y = gdpPercap)) +
  geom_point(data = dtf, mapping = aes(color = continent)) +
  geom_line(data = dtf, mapping = aes(color = continent, group = country)) +
  geom_point(data = weighted_avg, aes(x = year, y = average, size = population)) +
  geom_line(data = weighted_avg, aes(x = year, y = average)) +
  facet_wrap(~ continent, nrow = 1) +
  xlab("Year") + ylab("GDP Per Capita") +
  scale_size_continuous(name = "Population (100K)", breaks = c(10000, 20000, 30000)) +
  theme_bw()
```

![](Case-Study-3-Karis-Saplys-DTS350_files/figure-html/plot 2-1.png)<!-- -->
