Statistical assignment 3
================
Alicia Rey-Herme (014989)
21 February 2019

In this assignment you will need to reproduce 5 ggplot graphs. I supply graphs as images; you need to write the ggplot2 code to reproduce them and knit and submit a Markdown document with the reproduced graphs (as well as your .Rmd file).

First we will need to open and recode the data. I supply the code for this; you only need to change the file paths.

    ```r
    # Install tidyverse and data.table packages
    library(tidyverse)
    library(data.table)

    # Load in data and select variables
    Data8 <- fread("C:/Users/alici/Documents/DA3/data/SN6614/tab/ukhls_w8/h_indresp.tab")
    Data8 <- Data8 %>%
        select(pidp, h_age_dv, h_payn_dv, h_gor_dv)
    Stable <- fread("C:/Users/alici/Documents/DA3/data/SN6614/tab/ukhls_wx/xwavedat.tab")
    Stable <- Stable %>%
        select(pidp, sex_dv, ukborn, plbornc)

    # Join data
    Data <- Data8 %>% left_join(Stable, "pidp")
    rm(Data8, Stable)

    # Recode data
    Data <- Data %>%
        mutate(sex_dv = ifelse(sex_dv == 1, "male",
                           ifelse(sex_dv == 2, "female", NA))) %>%
        mutate(h_payn_dv = ifelse(h_payn_dv < 0, NA, h_payn_dv)) %>%
        mutate(h_gor_dv = recode(h_gor_dv,
                         `-9` = NA_character_,
                         `1` = "North East",
                         `2` = "North West",
                         `3` = "Yorkshire",
                         `4` = "East Midlands",
                         `5` = "West Midlands",
                         `6` = "East of England",
                         `7` = "London",
                         `8` = "South East",
                         `9` = "South West",
                         `10` = "Wales",
                         `11` = "Scotland",
                         `12` = "Northern Ireland")) %>%
        mutate(placeBorn = case_when(
                ukborn  == -9 ~ NA_character_,
                ukborn < 5 ~ "UK",
                plbornc == 5 ~ "Ireland",
                plbornc == 18 ~ "India",
                plbornc == 19 ~ "Pakistan",
                plbornc == 20 ~ "Bangladesh",
                plbornc == 10 ~ "Poland",
                plbornc == 27 ~ "Jamaica",
                plbornc == 24 ~ "Nigeria",
                TRUE ~ "other")
        )
    ```

Reproduce the following graphs as close as you can. For each graph, write two sentences (not more!) describing its main message.

(Note the position of the code chunks; each is preceded by four spaces. This helps display numbered lists correctly in the Markdown file with the output.)

1.  Histogram (20 points)

    ``` r
    Data %>%
      ggplot(aes(x = h_age_dv)) +
      geom_histogram(bins = 87) +
      xlab("Age") +
      ylab("Number of respondents")
    ```

    ![](Assignment4_knit_files/figure-markdown_github/unnamed-chunk-2-1.png)

2.  Scatter plot (20 points). The red line shows a linear fit; the blue line shows a quadratic fit. Note the size and position of points.

    ``` r
    Data %>%
      ggplot(aes(x = h_age_dv, y = h_payn_dv)) +
      geom_point() +
      xlim(NA, 65) +
      geom_smooth() +
      geom_smooth(method='lm', formula=y~x, col = 'red') +
      xlab("Age") +
      ylab("Monthly earnings")
    ```

    ![](Assignment4_knit_files/figure-markdown_github/unnamed-chunk-3-1.png)

3.  Faceted density chart (20 points).

4.  Ordered bar chart of summary statistics (20 points).

5.  Map (20 points). This is the most difficult problem in this set. You will need to use the NUTS Level 1 shape file (available here -- <https://data.gov.uk/dataset/2aa6727d-c5f0-462a-a367-904c750bbb34/nuts-level-1-january-2018-full-clipped-boundaries-in-the-united-kingdom>) and a number of packages for producing maps from shape files. You will need to google additional information; there are multiple webpages with the code that produces similar maps.
