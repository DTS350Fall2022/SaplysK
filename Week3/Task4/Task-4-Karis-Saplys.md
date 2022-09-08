---
title: "Task 4 Karis Saplys"
author: "Karis Saplys"
date: "2022-09-08"
output: 
  html_document:
    theme: cosmo
    keep_md: true
editor_options: 
  chunk_output_type: console
---




```r
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
## ✔ tibble  3.1.8     ✔ dplyr   1.0.9
## ✔ tidyr   1.2.0     ✔ stringr 1.4.1
## ✔ readr   2.1.2     ✔ forcats 0.5.2
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```

```r
iris_data <- as_tibble(iris)
iris_data
```

```
## # A tibble: 150 × 5
##    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
##           <dbl>       <dbl>        <dbl>       <dbl> <fct>  
##  1          5.1         3.5          1.4         0.2 setosa 
##  2          4.9         3            1.4         0.2 setosa 
##  3          4.7         3.2          1.3         0.2 setosa 
##  4          4.6         3.1          1.5         0.2 setosa 
##  5          5           3.6          1.4         0.2 setosa 
##  6          5.4         3.9          1.7         0.4 setosa 
##  7          4.6         3.4          1.4         0.3 setosa 
##  8          5           3.4          1.5         0.2 setosa 
##  9          4.4         2.9          1.4         0.2 setosa 
## 10          4.9         3.1          1.5         0.1 setosa 
## # … with 140 more rows
```


```r
sepal_table <- arrange(iris, Sepal.Length)
head(sepal_table,10)
```

```
##    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
## 1           4.3         3.0          1.1         0.1  setosa
## 2           4.4         2.9          1.4         0.2  setosa
## 3           4.4         3.0          1.3         0.2  setosa
## 4           4.4         3.2          1.3         0.2  setosa
## 5           4.5         2.3          1.3         0.3  setosa
## 6           4.6         3.1          1.5         0.2  setosa
## 7           4.6         3.4          1.4         0.3  setosa
## 8           4.6         3.6          1.0         0.2  setosa
## 9           4.6         3.2          1.4         0.2  setosa
## 10          4.7         3.2          1.3         0.2  setosa
```


```r
testdat <- select(iris_data,Species,Petal.Width)
testdat
```

```
## # A tibble: 150 × 2
##    Species Petal.Width
##    <fct>         <dbl>
##  1 setosa          0.2
##  2 setosa          0.2
##  3 setosa          0.2
##  4 setosa          0.2
##  5 setosa          0.2
##  6 setosa          0.4
##  7 setosa          0.3
##  8 setosa          0.2
##  9 setosa          0.2
## 10 setosa          0.1
## # … with 140 more rows
```


```r
species_mean <-iris_data %>%
  group_by(Species) %>%
  summarise(mean(Petal.Width), mean(Petal.Length), mean(Sepal.Length), mean(Sepal.Width))
species_mean
```

```
## # A tibble: 3 × 5
##   Species    `mean(Petal.Width)` `mean(Petal.Length)` mean(Sepal.Lengt…¹ mean(…²
##   <fct>                    <dbl>                <dbl>              <dbl>   <dbl>
## 1 setosa                   0.246                 1.46               5.01    3.43
## 2 versicolor               1.33                  4.26               5.94    2.77
## 3 virginica                2.03                  5.55               6.59    2.97
## # … with abbreviated variable names ¹​`mean(Sepal.Length)`, ²​`mean(Sepal.Width)`
```


```r
iris_min1 <- select(iris_data, Sepal.Width, Petal.Width)
iris_min <- filter(iris_min1, Sepal.Width >= 3, Petal.Width != 2.5)
iris_min
```

```
## # A tibble: 90 × 2
##    Sepal.Width Petal.Width
##          <dbl>       <dbl>
##  1         3.5         0.2
##  2         3           0.2
##  3         3.2         0.2
##  4         3.1         0.2
##  5         3.6         0.2
##  6         3.9         0.4
##  7         3.4         0.3
##  8         3.4         0.2
##  9         3.1         0.1
## 10         3.7         0.2
## # … with 80 more rows
```


```r
iris_size <- iris_data %>%
  mutate(Sepal.Size =
           case_when(
             Sepal.Length < 5 ~ "small",
             Sepal.Length >= 5 & Sepal.Length < 6.5 ~ "medium",
             Sepal.Length >= 6.5 ~ "large"))
head(iris_size)
```

```
## # A tibble: 6 × 6
##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species Sepal.Size
##          <dbl>       <dbl>        <dbl>       <dbl> <fct>   <chr>     
## 1          5.1         3.5          1.4         0.2 setosa  medium    
## 2          4.9         3            1.4         0.2 setosa  small     
## 3          4.7         3.2          1.3         0.2 setosa  small     
## 4          4.6         3.1          1.5         0.2 setosa  small     
## 5          5           3.6          1.4         0.2 setosa  medium    
## 6          5.4         3.9          1.7         0.4 setosa  medium
```


```r
iris_rank <- arrange(iris_data, desc(Petal.Length))
mutate(iris_rank, rank=min_rank(Petal.Length))
```

```
## # A tibble: 150 × 6
##    Sepal.Length Sepal.Width Petal.Length Petal.Width Species    rank
##           <dbl>       <dbl>        <dbl>       <dbl> <fct>     <int>
##  1          7.7         2.6          6.9         2.3 virginica   150
##  2          7.7         3.8          6.7         2.2 virginica   148
##  3          7.7         2.8          6.7         2   virginica   148
##  4          7.6         3            6.6         2.1 virginica   147
##  5          7.9         3.8          6.4         2   virginica   146
##  6          7.3         2.9          6.3         1.8 virginica   145
##  7          7.2         3.6          6.1         2.5 virginica   142
##  8          7.4         2.8          6.1         1.9 virginica   142
##  9          7.7         3            6.1         2.3 virginica   142
## 10          6.3         3.3          6           2.5 virginica   140
## # … with 140 more rows
```


```r
?summarise_all
```

```
## starting httpd help server ... done
```

```r
meansd_iris <- iris_data %>%
  group_by(Species) %>%
  summarize_all(list(Mean=mean, Std_dev = sd))
meansd_iris
```

```
## # A tibble: 3 × 9
##   Species    Sepal.Len…¹ Sepal…² Petal…³ Petal…⁴ Sepal…⁵ Sepal…⁶ Petal…⁷ Petal…⁸
##   <fct>            <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
## 1 setosa            5.01    3.43    1.46   0.246   0.352   0.379   0.174   0.105
## 2 versicolor        5.94    2.77    4.26   1.33    0.516   0.314   0.470   0.198
## 3 virginica         6.59    2.97    5.55   2.03    0.636   0.322   0.552   0.275
## # … with abbreviated variable names ¹​Sepal.Length_Mean, ²​Sepal.Width_Mean,
## #   ³​Petal.Length_Mean, ⁴​Petal.Width_Mean, ⁵​Sepal.Length_Std_dev,
## #   ⁶​Sepal.Width_Std_dev, ⁷​Petal.Length_Std_dev, ⁸​Petal.Width_Std_dev
```

##Report Questions

#Question 1: Which electoral dstricts are poised to be the most competitive in the upcoming midterm election?

#Question 2: Which areas of the United States are seeing the highst rates of purchase of electric vehicles? Are EV purchases correlated exclusively with wealthy Zip Codes?

#Question 3: How have smoking rates changed since 1970 if adjusted to include users of electronic cigarettes / vapes?

#Feedback: All in all, feedback for my questions was generally very positive. Individuals seemed not only intrigued, but curious as to what the answers behind my questions were. I believe that the amalgomation of data can answer every one of these questions in a meaningful way, and shed light on some of the most pressing topics in the United States today. FInally, what made my questions interesting was the specific relevence of the subject matter to people from my demographic (young, college-age students).

#Professional Feedback: I presented my third question to my uncle, who is a pediatric surgeon, and therefore highly concerned with the health effects of smoking/vaping by children. He concluded that th question was well thought out and ought to be adressed, but that given only new and emerging data on rates of vaping in aprticular, answering it thoroughly in 2 months would be a challenge.
