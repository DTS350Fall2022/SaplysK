---
title: "Task 5 Karis Saplys"
author: "Karis Saplys"
date: "2022-09-05"
output: 
  html_document:
    theme: cosmo
    keep_md: true
editor_options: 
  chunk_output_type: console
code_folding: 'hide'
---




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
SoloData <- read_csv("solo-artist-followers.csv")
```

```
## Rows: 139 Columns: 5
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (5): name, band, followers, band_followers, follower_difference
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
SoloData
```

```
## # A tibble: 139 × 5
##    name              band                followers band_followers follower_dif…¹
##    <chr>             <chr>               <chr>     <chr>          <chr>         
##  1 Daron Jones       112                 1.28k     783k           −782k         
##  2 Slim              112                 2.14k     783k           −781k         
##  3 Q Parker          112                 3.51k     783k           −780k         
##  4 JC Chasez         *NSYNC              30.8k     1.44M          −1.41M        
##  5 Joey Fatone       *NSYNC              1.13k     1.44M          −1.44M        
##  6 Justin Timberlake *NSYNC              10.3M     1.44M          8.90M         
##  7 Ashton Irwin      5 Seconds of Summer 130k      7.14M          −7.01M        
##  8 Abz Love          5ive                223       19.0k          −18.7k        
##  9 Jeff Timmons      98º                 111       302k           −302k         
## 10 Nick Lachey       98º                 142k      302k           −160k         
## # … with 129 more rows, and abbreviated variable name ¹​follower_difference
```

```r
str(SoloData)
```

```
## spec_tbl_df [139 × 5] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
##  $ name               : chr [1:139] "Daron Jones" "Slim" "Q Parker" "JC Chasez" ...
##  $ band               : chr [1:139] "112" "112" "112" "*NSYNC" ...
##  $ followers          : chr [1:139] "1.28k" "2.14k" "3.51k" "30.8k" ...
##  $ band_followers     : chr [1:139] "783k" "783k" "783k" "1.44M" ...
##  $ follower_difference: chr [1:139] "−782k" "−781k" "−780k" "−1.41M" ...
##  - attr(*, "spec")=
##   .. cols(
##   ..   name = col_character(),
##   ..   band = col_character(),
##   ..   followers = col_character(),
##   ..   band_followers = col_character(),
##   ..   follower_difference = col_character()
##   .. )
##  - attr(*, "problems")=<externalptr>
```

Data imported correctly as characters.


```r
BillboardData <- read_csv("billboard-hits.csv")
```

```
## Rows: 456 Columns: 5
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (3): name, band, title
## dbl  (1): peak_rank
## date (1): peak_date
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
BillboardData
```

```
## # A tibble: 456 × 5
##    name   band  title                     peak_date  peak_rank
##    <chr>  <chr> <chr>                     <date>         <dbl>
##  1 *NSYNC <NA>  It's Gonna Be Me          2000-07-28         1
##  2 *NSYNC <NA>  Music Of My Heart         1999-10-15         2
##  3 *NSYNC <NA>  Bye Bye Bye               2000-04-14         4
##  4 *NSYNC <NA>  This I Promise You        2000-12-01         5
##  5 *NSYNC <NA>  Girlfriend                2002-04-05         5
##  6 *NSYNC <NA>  A Little More Time On You 1999-02-26         8
##  7 *NSYNC <NA>  Gone                      2001-11-23        11
##  8 *NSYNC <NA>  I Want You Back           1998-05-01        13
##  9 *NSYNC <NA>  Pop                       2001-06-15        19
## 10 *NSYNC <NA>  Tearin' Up My Heart       1998-12-04        59
## # … with 446 more rows
```

```r
str("BillboardData")
```

```
##  chr "BillboardData"
```

Data imported correctly as characters.


```r
sixtop100hits <- BillboardData %>%
  group_by(name) %>%
  filter(n() > 6, band != "")
sixtop100hits
```

```
## # A tibble: 144 × 5
## # Groups:   name [10]
##    name              band   title                           peak_date  peak_rank
##    <chr>             <chr>  <chr>                           <date>         <dbl>
##  1 Justin Timberlake *NSYNC SexyBack                        2006-09-08         1
##  2 Justin Timberlake *NSYNC My Love                         2006-11-10         1
##  3 Justin Timberlake *NSYNC What Goes Around...Comes Around 2007-03-02         1
##  4 Justin Timberlake *NSYNC Can't Stop The Feeling!         2016-05-27         1
##  5 Justin Timberlake *NSYNC Mirrors                         2013-06-14         2
##  6 Justin Timberlake *NSYNC Cry Me A River                  2003-01-31         3
##  7 Justin Timberlake *NSYNC Suit & Tie                      2013-04-05         3
##  8 Justin Timberlake *NSYNC Rock Your Body                  2003-05-09         5
##  9 Justin Timberlake *NSYNC Summer Love                     2007-06-08         6
## 10 Justin Timberlake *NSYNC Not A Bad Thing                 2014-05-02         8
## # … with 134 more rows
```

Singers without 6 top 100 hits removed.


```r
sixtop100_bands <- BillboardData %>%
  group_by(name) %>%
  filter(name %in% sixtop100hits$band)
sixtop100_bands
```

```
## # A tibble: 101 × 5
## # Groups:   name [8]
##    name   band  title                     peak_date  peak_rank
##    <chr>  <chr> <chr>                     <date>         <dbl>
##  1 *NSYNC <NA>  It's Gonna Be Me          2000-07-28         1
##  2 *NSYNC <NA>  Music Of My Heart         1999-10-15         2
##  3 *NSYNC <NA>  Bye Bye Bye               2000-04-14         4
##  4 *NSYNC <NA>  This I Promise You        2000-12-01         5
##  5 *NSYNC <NA>  Girlfriend                2002-04-05         5
##  6 *NSYNC <NA>  A Little More Time On You 1999-02-26         8
##  7 *NSYNC <NA>  Gone                      2001-11-23        11
##  8 *NSYNC <NA>  I Want You Back           1998-05-01        13
##  9 *NSYNC <NA>  Pop                       2001-06-15        19
## 10 *NSYNC <NA>  Tearin' Up My Heart       1998-12-04        59
## # … with 91 more rows
```

Corresponding data in the data set for the top singers’ bands.


```r
ggplot(data = sixtop100hits, mapping=aes(x = peak_date, y = peak_rank, color = name, group = name)) +
  geom_point() +
  geom_line() +
  geom_point(data = sixtop100_bands, color = "black") +
  geom_line(data = sixtop100_bands, color = "black", linetype = "dotted") +
  facet_wrap(~ band, scales = "free") +
  xlab("peak_date") + ylab("peak_rank") +
  theme_bw()
```

![](Task-5-Karis-Saplys_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

From the above plots, I can observe that in general, most bands experienced peak popularity between 2014 and 2019, with a few exceptions. Destiny's CHild seems to be among the most consistent, with periodic spikes likely corresponding to new releases of music. 


```r
climate_data <- read_csv("Climate Data.csv")
```

```
## New names:
## Rows: 2105 Columns: 30
## ── Column specification
## ──────────────────────────────────────────────────────── Delimiter: "," chr
## (2): Year, Troposphere dbl (28): Mo, Globe, Land...4, Ocean...5, NH, Land...7,
## Ocean...8, SH, Land....
## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
## Specify the column types or set `show_col_types = FALSE` to quiet this message.
## • `Land` -> `Land...4`
## • `Ocean` -> `Ocean...5`
## • `Land` -> `Land...7`
## • `Ocean` -> `Ocean...8`
## • `Land` -> `Land...10`
## • `Ocean` -> `Ocean...11`
## • `Land` -> `Land...13`
## • `Ocean` -> `Ocean...14`
## • `Land` -> `Land...16`
## • `Ocean` -> `Ocean...17`
## • `Land` -> `Land...19`
## • `Ocean` -> `Ocean...20`
## • `Land` -> `Land...22`
## • `Ocean` -> `Ocean...23`
## • `Land` -> `Land...25`
## • `Ocean` -> `Ocean...26`
```

```r
head(climate_data)
```

```
## # A tibble: 6 × 30
##   Year     Mo Globe Land...4 Ocean…¹    NH Land.…² Ocean…³    SH Land.…⁴ Ocean…⁵
##   <chr> <dbl> <dbl>    <dbl>   <dbl> <dbl>   <dbl>   <dbl> <dbl>   <dbl>   <dbl>
## 1 1978     12 -0.48    -0.51   -0.47 -0.44   -0.46   -0.42 -0.52   -0.62   -0.5 
## 2 1979      1 -0.47    -0.64   -0.41 -0.64   -0.86   -0.5  -0.31   -0.13   -0.34
## 3 1979      2 -0.43    -0.56   -0.39 -0.47   -0.57   -0.41 -0.39   -0.53   -0.37
## 4 1979      3 -0.38    -0.51   -0.33 -0.46   -0.51   -0.44 -0.3    -0.53   -0.26
## 5 1979      4 -0.4     -0.57   -0.34 -0.47   -0.62   -0.37 -0.34   -0.46   -0.31
## 6 1979      5 -0.4     -0.56   -0.33 -0.52   -0.54   -0.52 -0.27   -0.62   -0.19
## # … with 19 more variables: Trpcs <dbl>, Land...13 <dbl>, Ocean...14 <dbl>,
## #   NoExt <dbl>, Land...16 <dbl>, Ocean...17 <dbl>, SoExt <dbl>,
## #   Land...19 <dbl>, Ocean...20 <dbl>, NoPol <dbl>, Land...22 <dbl>,
## #   Ocean...23 <dbl>, SoPol <dbl>, Land...25 <dbl>, Ocean...26 <dbl>,
## #   USA48 <dbl>, USA49 <dbl>, AUST <dbl>, Troposphere <chr>, and abbreviated
## #   variable names ¹​Ocean...5, ²​Land...7, ³​Ocean...8, ⁴​Land...10, ⁵​Ocean...11
```

```r
str(climate_data)
```

```
## spec_tbl_df [2,105 × 30] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
##  $ Year       : chr [1:2105] "1978" "1979" "1979" "1979" ...
##  $ Mo         : num [1:2105] 12 1 2 3 4 5 6 7 8 9 ...
##  $ Globe      : num [1:2105] -0.48 -0.47 -0.43 -0.38 -0.4 -0.4 -0.39 -0.31 -0.4 -0.32 ...
##  $ Land...4   : num [1:2105] -0.51 -0.64 -0.56 -0.51 -0.57 -0.56 -0.61 -0.57 -0.54 -0.44 ...
##  $ Ocean...5  : num [1:2105] -0.47 -0.41 -0.39 -0.33 -0.34 -0.33 -0.31 -0.21 -0.35 -0.28 ...
##  $ NH         : num [1:2105] -0.44 -0.64 -0.47 -0.46 -0.47 -0.52 -0.5 -0.21 -0.34 -0.33 ...
##  $ Land...7   : num [1:2105] -0.46 -0.86 -0.57 -0.51 -0.62 -0.54 -0.61 -0.33 -0.37 -0.31 ...
##  $ Ocean...8  : num [1:2105] -0.42 -0.5 -0.41 -0.44 -0.37 -0.52 -0.44 -0.14 -0.32 -0.34 ...
##  $ SH         : num [1:2105] -0.52 -0.31 -0.39 -0.3 -0.34 -0.27 -0.29 -0.41 -0.46 -0.32 ...
##  $ Land...10  : num [1:2105] -0.62 -0.13 -0.53 -0.53 -0.46 -0.62 -0.62 -1.11 -0.91 -0.73 ...
##  $ Ocean...11 : num [1:2105] -0.5 -0.34 -0.37 -0.26 -0.31 -0.19 -0.22 -0.26 -0.36 -0.23 ...
##  $ Trpcs      : num [1:2105] -0.6 -0.47 -0.36 -0.36 -0.35 -0.46 -0.37 -0.41 -0.37 -0.35 ...
##  $ Land...13  : num [1:2105] -0.62 -0.54 -0.25 -0.43 -0.37 -0.55 -0.49 -0.55 -0.35 -0.42 ...
##  $ Ocean...14 : num [1:2105] -0.59 -0.45 -0.39 -0.34 -0.34 -0.43 -0.33 -0.37 -0.38 -0.33 ...
##  $ NoExt      : num [1:2105] -0.37 -0.73 -0.54 -0.53 -0.54 -0.54 -0.55 -0.15 -0.36 -0.33 ...
##  $ Land...16  : num [1:2105] -0.44 -0.93 -0.67 -0.53 -0.72 -0.52 -0.63 -0.33 -0.43 -0.29 ...
##  $ Ocean...17 : num [1:2105] -0.3 -0.55 -0.42 -0.52 -0.38 -0.56 -0.48 0.01 -0.3 -0.36 ...
##  $ SoExt      : num [1:2105] -0.46 -0.23 -0.41 -0.26 -0.33 -0.18 -0.27 -0.37 -0.47 -0.29 ...
##  $ Land...19  : num [1:2105] -0.55 0.12 -0.69 -0.59 -0.43 -0.7 -0.74 -1.34 -1.17 -0.95 ...
##  $ Ocean...20 : num [1:2105] -0.45 -0.29 -0.36 -0.21 -0.31 -0.09 -0.19 -0.2 -0.34 -0.17 ...
##  $ NoPol      : num [1:2105] -0.39 -0.46 -2.01 -0.56 -0.84 -0.77 -0.76 -0.2 -0.26 -0.18 ...
##  $ Land...22  : num [1:2105] -0.68 -0.95 -2.3 -0.47 -0.81 -0.56 -1.14 -0.38 -0.35 -0.02 ...
##  $ Ocean...23 : num [1:2105] -0.06 0.1 -1.66 -0.65 -0.88 -1 -0.33 0.01 -0.15 -0.35 ...
##  $ SoPol      : num [1:2105] -0.45 -0.16 -0.8 -0.52 -0.26 0.05 -0.98 -0.95 -1 -0.11 ...
##  $ Land...25  : num [1:2105] -0.38 -0.15 -1.25 -1.25 0.26 -0.42 -1.62 -2.18 -1.67 -0.65 ...
##  $ Ocean...26 : num [1:2105] -0.49 -0.16 -0.58 -0.18 -0.51 0.27 -0.67 -0.37 -0.69 0.15 ...
##  $ USA48      : num [1:2105] -1.29 -3.22 -1.76 -0.7 -0.72 -0.82 -0.62 -0.16 -0.74 0.61 ...
##  $ USA49      : num [1:2105] -1.15 -2.42 -1.84 -0.39 -0.46 -0.75 -0.67 -0.09 -0.36 0.68 ...
##  $ AUST       : num [1:2105] -1.29 0.92 -0.3 0.23 -1.12 -1.1 -0.56 -1.16 -1.05 -0.76 ...
##  $ Troposphere: chr [1:2105] "Lower" "Lower" "Lower" "Lower" ...
##  - attr(*, "spec")=
##   .. cols(
##   ..   Year = col_character(),
##   ..   Mo = col_double(),
##   ..   Globe = col_double(),
##   ..   Land...4 = col_double(),
##   ..   Ocean...5 = col_double(),
##   ..   NH = col_double(),
##   ..   Land...7 = col_double(),
##   ..   Ocean...8 = col_double(),
##   ..   SH = col_double(),
##   ..   Land...10 = col_double(),
##   ..   Ocean...11 = col_double(),
##   ..   Trpcs = col_double(),
##   ..   Land...13 = col_double(),
##   ..   Ocean...14 = col_double(),
##   ..   NoExt = col_double(),
##   ..   Land...16 = col_double(),
##   ..   Ocean...17 = col_double(),
##   ..   SoExt = col_double(),
##   ..   Land...19 = col_double(),
##   ..   Ocean...20 = col_double(),
##   ..   NoPol = col_double(),
##   ..   Land...22 = col_double(),
##   ..   Ocean...23 = col_double(),
##   ..   SoPol = col_double(),
##   ..   Land...25 = col_double(),
##   ..   Ocean...26 = col_double(),
##   ..   USA48 = col_double(),
##   ..   USA49 = col_double(),
##   ..   AUST = col_double(),
##   ..   Troposphere = col_character()
##   .. )
##  - attr(*, "problems")=<externalptr>
```
https://www.kaggle.com/datasets/csafrit2/latest-global-temperatures 

The above dataset contains NOAA satelite data measuring natural microwave thermal emissions from various layers of the atmosphere. The data comes from 15 different instruments, and is updated every year to show changes in atmospheric temperatures. 


```r
rent_index1 <- read_csv("price.csv")
```

```
## Rows: 13131 Columns: 81
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (4): City, Metro, County, State
## dbl (77): City Code, Population Rank, November 2010, December 2010, January ...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
rent_index2 <- read_csv("pricepersqft.csv")
```

```
## Rows: 11919 Columns: 81
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (4): City, Metro, County, State
## dbl (77): City Code, Population Rank, November 2010, December 2010, January ...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
head(rent_index1)
```

```
## # A tibble: 6 × 81
##   `City Code` City    Metro County State Popul…¹ Novem…² Decem…³ Janua…⁴ Febru…⁵
##         <dbl> <chr>   <chr> <chr>  <chr>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
## 1        6181 New Yo… New … Queens NY          1      NA      NA      NA      NA
## 2       12447 Los An… Los … Los A… CA          2    2184    2184    2183    2188
## 3       17426 Chicago Chic… Cook   IL          3    1563    1555    1547    1537
## 4       39051 Houston Hous… Harris TX          4    1198    1199    1199    1200
## 5       13271 Philad… Phil… Phila… PA          5    1092    1099    1094    1087
## 6       40326 Phoenix Phoe… Maric… AZ          6    1087    1080    1071    1067
## # … with 71 more variables: `March 2011` <dbl>, `April 2011` <dbl>,
## #   `May 2011` <dbl>, `June 2011` <dbl>, `July 2011` <dbl>,
## #   `August 2011` <dbl>, `September 2011` <dbl>, `October 2011` <dbl>,
## #   `November 2011` <dbl>, `December 2011` <dbl>, `January 2012` <dbl>,
## #   `February 2012` <dbl>, `March 2012` <dbl>, `April 2012` <dbl>,
## #   `May 2012` <dbl>, `June 2012` <dbl>, `July 2012` <dbl>,
## #   `August 2012` <dbl>, `September 2012` <dbl>, `October 2012` <dbl>, …
```

```r
head(rent_index2)
```

```
## # A tibble: 6 × 81
##   `City Code` City    Metro County State Popul…¹ Novem…² Decem…³ Janua…⁴ Febru…⁵
##         <dbl> <chr>   <chr> <chr>  <chr>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
## 1        6181 New Yo… New … Queens NY          1  NA      NA      NA      NA    
## 2       12447 Los An… Los … Los A… CA          2   1.58    1.58    1.58    1.58 
## 3       17426 Chicago Chic… Cook   IL          3   1.24    1.25    1.25    1.25 
## 4       39051 Houston Hous… Harris TX          4   0.788   0.784   0.784   0.786
## 5       13271 Philad… Phil… Phila… PA          5   0.854   0.858   0.858   0.858
## 6       40326 Phoenix Phoe… Maric… AZ          6   0.764   0.766   0.766   0.766
## # … with 71 more variables: `March 2011` <dbl>, `April 2011` <dbl>,
## #   `May 2011` <dbl>, `June 2011` <dbl>, `July 2011` <dbl>,
## #   `August 2011` <dbl>, `September 2011` <dbl>, `October 2011` <dbl>,
## #   `November 2011` <dbl>, `December 2011` <dbl>, `January 2012` <dbl>,
## #   `February 2012` <dbl>, `March 2012` <dbl>, `April 2012` <dbl>,
## #   `May 2012` <dbl>, `June 2012` <dbl>, `July 2012` <dbl>,
## #   `August 2012` <dbl>, `September 2012` <dbl>, `October 2012` <dbl>, …
```

```r
str(rent_index1)
```

```
## spec_tbl_df [13,131 × 81] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
##  $ City Code      : num [1:13131] 6181 12447 17426 39051 13271 ...
##  $ City           : chr [1:13131] "New York" "Los Angeles" "Chicago" "Houston" ...
##  $ Metro          : chr [1:13131] "New York" "Los Angeles" "Chicago" "Houston" ...
##  $ County         : chr [1:13131] "Queens" "Los Angeles" "Cook" "Harris" ...
##  $ State          : chr [1:13131] "NY" "CA" "IL" "TX" ...
##  $ Population Rank: num [1:13131] 1 2 3 4 5 6 7 8 9 10 ...
##  $ November 2010  : num [1:13131] NA 2184 1563 1198 1092 ...
##  $ December 2010  : num [1:13131] NA 2184 1555 1199 1099 ...
##  $ January 2011   : num [1:13131] NA 2183 1547 1199 1094 ...
##  $ February 2011  : num [1:13131] NA 2188 1537 1200 1087 ...
##  $ March 2011     : num [1:13131] NA 2189 1526 1203 1080 ...
##  $ April 2011     : num [1:13131] NA 2189 1517 1205 1080 ...
##  $ May 2011       : num [1:13131] NA 2188 1507 1204 1083 ...
##  $ June 2011      : num [1:13131] NA 2191 1497 1199 1082 ...
##  $ July 2011      : num [1:13131] NA 2189 1493 1194 1082 ...
##  $ August 2011    : num [1:13131] NA 2186 1491 1190 1085 ...
##  $ September 2011 : num [1:13131] NA 2183 1489 1188 1095 ...
##  $ October 2011   : num [1:13131] NA 2183 1485 1186 1098 ...
##  $ November 2011  : num [1:13131] NA 2182 1480 1188 1094 ...
##  $ December 2011  : num [1:13131] 1746 2178 1483 1190 1085 ...
##  $ January 2012   : num [1:13131] 1752 2172 1484 1194 1080 ...
##  $ February 2012  : num [1:13131] 1764 2175 1485 1196 1083 ...
##  $ March 2012     : num [1:13131] 1778 2177 1489 1199 1087 ...
##  $ April 2012     : num [1:13131] 1792 2183 1494 1200 1091 ...
##  $ May 2012       : num [1:13131] 1804 2186 1496 1199 1092 ...
##  $ June 2012      : num [1:13131] 1813 2190 1493 1197 1094 ...
##  $ July 2012      : num [1:13131] 1814 2192 1491 1193 1096 ...
##  $ August 2012    : num [1:13131] 1810 2189 1491 1193 1095 ...
##  $ September 2012 : num [1:13131] 1805 2189 1498 1197 1092 ...
##  $ October 2012   : num [1:13131] 1806 2185 1509 1201 1090 ...
##  $ November 2012  : num [1:13131] 1817 2183 1513 1203 1089 ...
##  $ December 2012  : num [1:13131] 1831 2186 1517 1199 1087 ...
##  $ January 2013   : num [1:13131] 1851 2194 1514 1201 1083 ...
##  $ February 2013  : num [1:13131] 1870 2203 1511 1208 1081 ...
##  $ March 2013     : num [1:13131] 1888 2212 1512 1218 1083 ...
##  $ April 2013     : num [1:13131] 1901 2222 1527 1227 1089 ...
##  $ May 2013       : num [1:13131] 1918 2229 1544 1236 1093 ...
##  $ June 2013      : num [1:13131] 1941 2236 1560 1248 1095 ...
##  $ July 2013      : num [1:13131] 1968 2239 1562 1258 1094 ...
##  $ August 2013    : num [1:13131] 1987 2246 1568 1265 1093 ...
##  $ September 2013 : num [1:13131] 1999 2255 1574 1269 1087 ...
##  $ October 2013   : num [1:13131] 2004 2267 1584 1277 1083 ...
##  $ November 2013  : num [1:13131] 2014 2278 1585 1287 1082 ...
##  $ December 2013  : num [1:13131] 2026 2283 1593 1295 1085 ...
##  $ January 2014   : num [1:13131] 2040 2285 1606 1297 1092 ...
##  $ February 2014  : num [1:13131] 2052 2283 1616 1296 1098 ...
##  $ March 2014     : num [1:13131] 2058 2285 1619 1293 1105 ...
##  $ April 2014     : num [1:13131] 2064 2283 1614 1294 1108 ...
##  $ May 2014       : num [1:13131] 2071 2285 1612 1296 1108 ...
##  $ June 2014      : num [1:13131] 2080 2288 1615 1301 1106 ...
##  $ July 2014      : num [1:13131] 2104 2303 1628 1310 1111 ...
##  $ August 2014    : num [1:13131] 2132 2320 1636 1322 1121 ...
##  $ September 2014 : num [1:13131] 2169 2343 1649 1334 1136 ...
##  $ October 2014   : num [1:13131] 2191 2367 1658 1344 1150 ...
##  $ November 2014  : num [1:13131] 2206 2395 1672 1355 1164 ...
##  $ December 2014  : num [1:13131] 2214 2423 1677 1367 1175 ...
##  $ January 2015   : num [1:13131] 2216 2445 1677 1377 1182 ...
##  $ February 2015  : num [1:13131] 2229 2464 1668 1384 1185 ...
##  $ March 2015     : num [1:13131] 2241 2479 1668 1389 1183 ...
##  $ April 2015     : num [1:13131] 2248 2493 1669 1394 1178 ...
##  $ May 2015       : num [1:13131] 2253 2502 1667 1402 1176 ...
##  $ June 2015      : num [1:13131] 2251 2511 1668 1410 1179 ...
##  $ July 2015      : num [1:13131] 2246 2521 1670 1419 1179 ...
##  $ August 2015    : num [1:13131] 2259 2536 1668 1425 1177 ...
##  $ September 2015 : num [1:13131] 2276 2546 1660 1428 1175 ...
##  $ October 2015   : num [1:13131] 2304 2555 1652 1428 1179 ...
##  $ November 2015  : num [1:13131] 2322 2564 1649 1429 1184 ...
##  $ December 2015  : num [1:13131] 2334 2577 1653 1431 1189 ...
##  $ January 2016   : num [1:13131] 2335 2596 1668 1436 1196 ...
##  $ February 2016  : num [1:13131] 2331 2607 1671 1439 1200 ...
##  $ March 2016     : num [1:13131] 2329 2622 1682 1442 1205 ...
##  $ April 2016     : num [1:13131] 2334 2637 1684 1444 1206 ...
##  $ May 2016       : num [1:13131] 2339 2662 1686 1446 1211 ...
##  $ June 2016      : num [1:13131] 2345 2687 1687 1446 1218 ...
##  $ July 2016      : num [1:13131] 2344 2704 1685 1443 1222 ...
##  $ August 2016    : num [1:13131] 2336 2716 1681 1440 1223 ...
##  $ September 2016 : num [1:13131] 2324 2723 1675 1438 1220 ...
##  $ October 2016   : num [1:13131] 2318 2731 1668 1437 1216 ...
##  $ November 2016  : num [1:13131] 2321 2740 1656 1437 1211 ...
##  $ December 2016  : num [1:13131] 2321 2748 1644 1435 1209 ...
##  $ January 2017   : num [1:13131] 2322 2753 1632 1430 1212 ...
##  - attr(*, "spec")=
##   .. cols(
##   ..   `City Code` = col_double(),
##   ..   City = col_character(),
##   ..   Metro = col_character(),
##   ..   County = col_character(),
##   ..   State = col_character(),
##   ..   `Population Rank` = col_double(),
##   ..   `November 2010` = col_double(),
##   ..   `December 2010` = col_double(),
##   ..   `January 2011` = col_double(),
##   ..   `February 2011` = col_double(),
##   ..   `March 2011` = col_double(),
##   ..   `April 2011` = col_double(),
##   ..   `May 2011` = col_double(),
##   ..   `June 2011` = col_double(),
##   ..   `July 2011` = col_double(),
##   ..   `August 2011` = col_double(),
##   ..   `September 2011` = col_double(),
##   ..   `October 2011` = col_double(),
##   ..   `November 2011` = col_double(),
##   ..   `December 2011` = col_double(),
##   ..   `January 2012` = col_double(),
##   ..   `February 2012` = col_double(),
##   ..   `March 2012` = col_double(),
##   ..   `April 2012` = col_double(),
##   ..   `May 2012` = col_double(),
##   ..   `June 2012` = col_double(),
##   ..   `July 2012` = col_double(),
##   ..   `August 2012` = col_double(),
##   ..   `September 2012` = col_double(),
##   ..   `October 2012` = col_double(),
##   ..   `November 2012` = col_double(),
##   ..   `December 2012` = col_double(),
##   ..   `January 2013` = col_double(),
##   ..   `February 2013` = col_double(),
##   ..   `March 2013` = col_double(),
##   ..   `April 2013` = col_double(),
##   ..   `May 2013` = col_double(),
##   ..   `June 2013` = col_double(),
##   ..   `July 2013` = col_double(),
##   ..   `August 2013` = col_double(),
##   ..   `September 2013` = col_double(),
##   ..   `October 2013` = col_double(),
##   ..   `November 2013` = col_double(),
##   ..   `December 2013` = col_double(),
##   ..   `January 2014` = col_double(),
##   ..   `February 2014` = col_double(),
##   ..   `March 2014` = col_double(),
##   ..   `April 2014` = col_double(),
##   ..   `May 2014` = col_double(),
##   ..   `June 2014` = col_double(),
##   ..   `July 2014` = col_double(),
##   ..   `August 2014` = col_double(),
##   ..   `September 2014` = col_double(),
##   ..   `October 2014` = col_double(),
##   ..   `November 2014` = col_double(),
##   ..   `December 2014` = col_double(),
##   ..   `January 2015` = col_double(),
##   ..   `February 2015` = col_double(),
##   ..   `March 2015` = col_double(),
##   ..   `April 2015` = col_double(),
##   ..   `May 2015` = col_double(),
##   ..   `June 2015` = col_double(),
##   ..   `July 2015` = col_double(),
##   ..   `August 2015` = col_double(),
##   ..   `September 2015` = col_double(),
##   ..   `October 2015` = col_double(),
##   ..   `November 2015` = col_double(),
##   ..   `December 2015` = col_double(),
##   ..   `January 2016` = col_double(),
##   ..   `February 2016` = col_double(),
##   ..   `March 2016` = col_double(),
##   ..   `April 2016` = col_double(),
##   ..   `May 2016` = col_double(),
##   ..   `June 2016` = col_double(),
##   ..   `July 2016` = col_double(),
##   ..   `August 2016` = col_double(),
##   ..   `September 2016` = col_double(),
##   ..   `October 2016` = col_double(),
##   ..   `November 2016` = col_double(),
##   ..   `December 2016` = col_double(),
##   ..   `January 2017` = col_double()
##   .. )
##  - attr(*, "problems")=<externalptr>
```

```r
str(rent_index2)
```

```
## spec_tbl_df [11,919 × 81] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
##  $ City Code      : num [1:11919] 6181 12447 17426 39051 13271 ...
##  $ City           : chr [1:11919] "New York" "Los Angeles" "Chicago" "Houston" ...
##  $ Metro          : chr [1:11919] "New York" "Los Angeles" "Chicago" "Houston" ...
##  $ County         : chr [1:11919] "Queens" "Los Angeles" "Cook" "Harris" ...
##  $ State          : chr [1:11919] "NY" "CA" "IL" "TX" ...
##  $ Population Rank: num [1:11919] 1 2 3 4 5 6 7 8 9 10 ...
##  $ November 2010  : num [1:11919] NA 1.578 1.244 0.788 0.854 ...
##  $ December 2010  : num [1:11919] NA 1.578 1.248 0.784 0.858 ...
##  $ January 2011   : num [1:11919] NA 1.58 1.254 0.784 0.858 ...
##  $ February 2011  : num [1:11919] NA 1.582 1.254 0.786 0.858 ...
##  $ March 2011     : num [1:11919] NA 1.586 1.248 0.792 0.856 ...
##  $ April 2011     : num [1:11919] NA 1.588 1.24 0.796 0.854 ...
##  $ May 2011       : num [1:11919] NA 1.59 1.232 0.796 0.854 ...
##  $ June 2011      : num [1:11919] NA 1.594 1.224 0.794 0.852 ...
##  $ July 2011      : num [1:11919] NA 1.594 1.22 0.788 0.85 ...
##  $ August 2011    : num [1:11919] NA 1.594 1.212 0.784 0.848 ...
##  $ September 2011 : num [1:11919] NA 1.592 1.206 0.778 0.85 ...
##  $ October 2011   : num [1:11919] NA 1.59 1.194 0.772 0.85 ...
##  $ November 2011  : num [1:11919] NA 1.592 1.188 0.772 0.852 ...
##  $ December 2011  : num [1:11919] 1.39 1.592 1.18 0.776 0.852 ...
##  $ January 2012   : num [1:11919] 1.388 1.59 1.176 0.776 0.854 ...
##  $ February 2012  : num [1:11919] 1.392 1.588 1.174 0.78 0.858 ...
##  $ March 2012     : num [1:11919] 1.396 1.588 1.174 0.782 0.86 ...
##  $ April 2012     : num [1:11919] 1.402 1.588 1.176 0.786 0.862 ...
##  $ May 2012       : num [1:11919] 1.412 1.59 1.178 0.786 0.86 ...
##  $ June 2012      : num [1:11919] 1.42 1.588 1.182 0.788 0.86 ...
##  $ July 2012      : num [1:11919] 1.424 1.59 1.188 0.788 0.86 ...
##  $ August 2012    : num [1:11919] 1.422 1.588 1.184 0.794 0.858 ...
##  $ September 2012 : num [1:11919] 1.42 1.592 1.182 0.798 0.858 ...
##  $ October 2012   : num [1:11919] 1.426 1.594 1.186 0.798 0.858 ...
##  $ November 2012  : num [1:11919] 1.442 1.598 1.198 0.798 0.856 ...
##  $ December 2012  : num [1:11919] 1.458 1.602 1.202 0.796 0.854 ...
##  $ January 2013   : num [1:11919] 1.468 1.608 1.198 0.8 0.854 ...
##  $ February 2013  : num [1:11919] 1.478 1.616 1.2 0.806 0.852 ...
##  $ March 2013     : num [1:11919] 1.486 1.622 1.204 0.812 0.852 ...
##  $ April 2013     : num [1:11919] 1.496 1.628 1.214 0.818 0.852 ...
##  $ May 2013       : num [1:11919] 1.502 1.634 1.222 0.824 0.852 ...
##  $ June 2013      : num [1:11919] 1.51 1.64 1.232 0.83 0.852 ...
##  $ July 2013      : num [1:11919] 1.518 1.648 1.232 0.836 0.852 ...
##  $ August 2013    : num [1:11919] 1.526 1.656 1.236 0.84 0.852 ...
##  $ September 2013 : num [1:11919] 1.538 1.662 1.238 0.848 0.852 ...
##  $ October 2013   : num [1:11919] 1.544 1.672 1.252 0.856 0.854 ...
##  $ November 2013  : num [1:11919] 1.552 1.676 1.264 0.866 0.856 ...
##  $ December 2013  : num [1:11919] 1.558 1.682 1.272 0.872 0.86 ...
##  $ January 2014   : num [1:11919] 1.568 1.68 1.28 0.874 0.864 ...
##  $ February 2014  : num [1:11919] 1.582 1.68 1.286 0.872 0.866 ...
##  $ March 2014     : num [1:11919] 1.592 1.678 1.286 0.868 0.87 ...
##  $ April 2014     : num [1:11919] 1.6 1.682 1.28 0.868 0.872 ...
##  $ May 2014       : num [1:11919] 1.604 1.688 1.276 0.874 0.872 ...
##  $ June 2014      : num [1:11919] 1.612 1.696 1.272 0.882 0.872 ...
##  $ July 2014      : num [1:11919] 1.63 1.708 1.274 0.892 0.876 ...
##  $ August 2014    : num [1:11919] 1.65 1.72 1.282 0.902 0.882 ...
##  $ September 2014 : num [1:11919] 1.672 1.738 1.292 0.914 0.892 ...
##  $ October 2014   : num [1:11919] 1.682 1.76 1.296 0.924 0.898 ...
##  $ November 2014  : num [1:11919] 1.69 1.78 1.304 0.934 0.904 ...
##  $ December 2014  : num [1:11919] 1.69 1.8 1.302 0.944 0.908 ...
##  $ January 2015   : num [1:11919] 1.69 1.82 1.3 0.95 0.91 ...
##  $ February 2015  : num [1:11919] 1.7 1.834 1.298 0.956 0.91 ...
##  $ March 2015     : num [1:11919] 1.708 1.85 1.304 0.958 0.91 ...
##  $ April 2015     : num [1:11919] 1.718 1.862 1.306 0.962 0.91 ...
##  $ May 2015       : num [1:11919] 1.726 1.87 1.308 0.964 0.916 ...
##  $ June 2015      : num [1:11919] 1.736 1.878 1.31 0.968 0.924 ...
##  $ July 2015      : num [1:11919] 1.74 1.888 1.316 0.972 0.93 ...
##  $ August 2015    : num [1:11919] 1.752 1.9 1.322 0.974 0.932 ...
##  $ September 2015 : num [1:11919] 1.764 1.908 1.326 0.974 0.932 ...
##  $ October 2015   : num [1:11919] 1.78 1.914 1.328 0.974 0.93 ...
##  $ November 2015  : num [1:11919] 1.788 1.92 1.33 0.972 0.93 ...
##  $ December 2015  : num [1:11919] 1.792 1.93 1.332 0.974 0.93 ...
##  $ January 2016   : num [1:11919] 1.794 1.948 1.338 0.976 0.934 ...
##  $ February 2016  : num [1:11919] 1.8 1.962 1.34 0.98 0.938 ...
##  $ March 2016     : num [1:11919] 1.804 1.978 1.346 0.982 0.944 ...
##  $ April 2016     : num [1:11919] 1.806 1.99 1.354 0.984 0.948 ...
##  $ May 2016       : num [1:11919] 1.81 2.004 1.362 0.984 0.956 ...
##  $ June 2016      : num [1:11919] 1.816 2.018 1.37 0.982 0.962 ...
##  $ July 2016      : num [1:11919] 1.824 2.026 1.374 0.98 0.964 ...
##  $ August 2016    : num [1:11919] 1.828 2.032 1.378 0.976 0.964 ...
##  $ September 2016 : num [1:11919] 1.836 2.038 1.38 0.974 0.966 ...
##  $ October 2016   : num [1:11919] 1.844 2.042 1.38 0.974 0.968 ...
##  $ November 2016  : num [1:11919] 1.858 2.048 1.38 0.976 0.972 ...
##  $ December 2016  : num [1:11919] 1.866 2.056 1.376 0.976 0.974 ...
##  $ January 2017   : num [1:11919] 1.872 2.064 1.374 0.974 0.974 ...
##  - attr(*, "spec")=
##   .. cols(
##   ..   `City Code` = col_double(),
##   ..   City = col_character(),
##   ..   Metro = col_character(),
##   ..   County = col_character(),
##   ..   State = col_character(),
##   ..   `Population Rank` = col_double(),
##   ..   `November 2010` = col_double(),
##   ..   `December 2010` = col_double(),
##   ..   `January 2011` = col_double(),
##   ..   `February 2011` = col_double(),
##   ..   `March 2011` = col_double(),
##   ..   `April 2011` = col_double(),
##   ..   `May 2011` = col_double(),
##   ..   `June 2011` = col_double(),
##   ..   `July 2011` = col_double(),
##   ..   `August 2011` = col_double(),
##   ..   `September 2011` = col_double(),
##   ..   `October 2011` = col_double(),
##   ..   `November 2011` = col_double(),
##   ..   `December 2011` = col_double(),
##   ..   `January 2012` = col_double(),
##   ..   `February 2012` = col_double(),
##   ..   `March 2012` = col_double(),
##   ..   `April 2012` = col_double(),
##   ..   `May 2012` = col_double(),
##   ..   `June 2012` = col_double(),
##   ..   `July 2012` = col_double(),
##   ..   `August 2012` = col_double(),
##   ..   `September 2012` = col_double(),
##   ..   `October 2012` = col_double(),
##   ..   `November 2012` = col_double(),
##   ..   `December 2012` = col_double(),
##   ..   `January 2013` = col_double(),
##   ..   `February 2013` = col_double(),
##   ..   `March 2013` = col_double(),
##   ..   `April 2013` = col_double(),
##   ..   `May 2013` = col_double(),
##   ..   `June 2013` = col_double(),
##   ..   `July 2013` = col_double(),
##   ..   `August 2013` = col_double(),
##   ..   `September 2013` = col_double(),
##   ..   `October 2013` = col_double(),
##   ..   `November 2013` = col_double(),
##   ..   `December 2013` = col_double(),
##   ..   `January 2014` = col_double(),
##   ..   `February 2014` = col_double(),
##   ..   `March 2014` = col_double(),
##   ..   `April 2014` = col_double(),
##   ..   `May 2014` = col_double(),
##   ..   `June 2014` = col_double(),
##   ..   `July 2014` = col_double(),
##   ..   `August 2014` = col_double(),
##   ..   `September 2014` = col_double(),
##   ..   `October 2014` = col_double(),
##   ..   `November 2014` = col_double(),
##   ..   `December 2014` = col_double(),
##   ..   `January 2015` = col_double(),
##   ..   `February 2015` = col_double(),
##   ..   `March 2015` = col_double(),
##   ..   `April 2015` = col_double(),
##   ..   `May 2015` = col_double(),
##   ..   `June 2015` = col_double(),
##   ..   `July 2015` = col_double(),
##   ..   `August 2015` = col_double(),
##   ..   `September 2015` = col_double(),
##   ..   `October 2015` = col_double(),
##   ..   `November 2015` = col_double(),
##   ..   `December 2015` = col_double(),
##   ..   `January 2016` = col_double(),
##   ..   `February 2016` = col_double(),
##   ..   `March 2016` = col_double(),
##   ..   `April 2016` = col_double(),
##   ..   `May 2016` = col_double(),
##   ..   `June 2016` = col_double(),
##   ..   `July 2016` = col_double(),
##   ..   `August 2016` = col_double(),
##   ..   `September 2016` = col_double(),
##   ..   `October 2016` = col_double(),
##   ..   `November 2016` = col_double(),
##   ..   `December 2016` = col_double(),
##   ..   `January 2017` = col_double()
##   .. )
##  - attr(*, "problems")=<externalptr>
```
https://www.kaggle.com/datasets/zillow/rent-index

Zillow's sector-leading economics team compiles various real estate, rental and mortgage-related metrics data to answer important questions about the state of the US housing market. Data are compiled from 500 different markets nationwide. It is important to note that the data are broken up into 2 different sets (price and pricepersqft).


```r
election_2020 <- read_csv("president_county.csv")
```

```
## Rows: 4633 Columns: 5
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (2): state, county
## dbl (3): current_votes, total_votes, percent
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
head(election_2020)
```

```
## # A tibble: 6 × 5
##   state                county               current_votes total_votes percent
##   <chr>                <chr>                        <dbl>       <dbl>   <dbl>
## 1 Delaware             Kent County                  87025       87025     100
## 2 Delaware             New Castle County           287633      287633     100
## 3 Delaware             Sussex County               129352      129352     100
## 4 District of Columbia District of Columbia         41681       41681     100
## 5 District of Columbia Ward 2                       32881       32881     100
## 6 District of Columbia Ward 3                       44231       44231     100
```

```r
str(election_2020)
```

```
## spec_tbl_df [4,633 × 5] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
##  $ state        : chr [1:4633] "Delaware" "Delaware" "Delaware" "District of Columbia" ...
##  $ county       : chr [1:4633] "Kent County" "New Castle County" "Sussex County" "District of Columbia" ...
##  $ current_votes: num [1:4633] 87025 287633 129352 41681 32881 ...
##  $ total_votes  : num [1:4633] 87025 287633 129352 41681 32881 ...
##  $ percent      : num [1:4633] 100 100 100 100 100 100 100 100 100 100 ...
##  - attr(*, "spec")=
##   .. cols(
##   ..   state = col_character(),
##   ..   county = col_character(),
##   ..   current_votes = col_double(),
##   ..   total_votes = col_double(),
##   ..   percent = col_double()
##   .. )
##  - attr(*, "problems")=<externalptr>
```
https://www.kaggle.com/datasets/unanimad/us-election-2020

The above dataset conatins county-level data from the 2020 US presidential election. 
