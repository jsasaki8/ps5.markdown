ps5.markdown
================
Jake Sasaki
2023-02-17

``` r
library(tidyverse)
```

(1-2) Load data. How many rows/columns do we have?

``` r
gap <- read_delim("gapminder.csv")
```

    ## Rows: 13055 Columns: 25
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: "\t"
    ## chr  (6): iso3, name, iso2, region, sub-region, intermediate-region
    ## dbl (19): time, totalPopulation, fertilityRate, lifeExpectancy, childMortali...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
nrow(gap)
```

    ## [1] 13055

``` r
ncol(gap)
```

    ## [1] 25

(1-3) Print a small sample of data. Does it look OK?

``` r
gap %>% 
  sample_n(10)
```

    ## # A tibble: 10 × 25
    ##    iso3  name         iso2  region sub-r…¹ inter…²  time total…³ ferti…⁴ lifeE…⁵
    ##    <chr> <chr>        <chr> <chr>  <chr>   <chr>   <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 SGP   Singapore    SG    Asia   South-… <NA>     1963  1.80e6    5.16    66.7
    ##  2 RUS   Russian Fed… RU    Europe Easter… <NA>     2004  1.44e8    1.34    65.5
    ##  3 GTM   Guatemala    GT    Ameri… Latin … Centra…  1984  7.67e6    5.98    59.2
    ##  4 BMU   Bermuda      BM    Ameri… Northe… <NA>     2017  6.39e4    1.6     81.4
    ##  5 MHL   Marshall Is… MH    Ocean… Micron… <NA>     1976  2.66e4   NA       NA  
    ##  6 ARG   Argentina    AR    Ameri… Latin … South …  1967  2.28e7    3.05    65.8
    ##  7 CAF   Central Afr… CF    Africa Sub-Sa… Middle…  1976  2.00e6    5.92    47.0
    ##  8 TTO   Trinidad an… TT    Ameri… Latin … Caribb…  2005  1.30e6    1.77    70.7
    ##  9 GIN   Guinea       GN    Africa Sub-Sa… Wester…  1984  5.33e6    6.62    44.6
    ## 10 JPN   Japan        JP    Asia   Easter… <NA>     2002  1.27e8    1.32    81.6
    ## # … with 15 more variables: childMortality <dbl>, youthFemaleLiteracy <dbl>,
    ## #   youthMaleLiteracy <dbl>, adultLiteracy <dbl>, GDP_PC <dbl>,
    ## #   accessElectricity <dbl>, agriculturalLand <dbl>, agricultureTractors <dbl>,
    ## #   cerealProduction <dbl>, fertilizerHa <dbl>, co2 <dbl>,
    ## #   greenhouseGases <dbl>, co2_PC <dbl>, pm2.5_35 <dbl>, battleDeaths <dbl>,
    ## #   and abbreviated variable names ¹​`sub-region`, ²​`intermediate-region`,
    ## #   ³​totalPopulation, ⁴​fertilityRate, ⁵​lifeExpectancy

(2-1) How many countries are there in the dataset? Analyze all three:
iso3, iso2 and name.

``` r
gap %>% 
  select(iso3, iso2, name) %>% 
  summarise(name = n_distinct(name), iso2 = n_distinct(iso2), iso3 = n_distinct(iso3))
```

    ## # A tibble: 1 × 3
    ##    name  iso2  iso3
    ##   <int> <int> <int>
    ## 1   250   249   253

(2-2a) Find how many names are there for each iso-2 code. Are there any
iso-2 codes that correspond to more than one name? What are these
countries?

``` r
gap %>% 
  group_by(iso2) %>% 
  summarise(num = n_distinct(name)) %>% 
  arrange(desc(num))
```

    ## # A tibble: 249 × 2
    ##    iso2    num
    ##    <chr> <int>
    ##  1 <NA>      2
    ##  2 AD        1
    ##  3 AE        1
    ##  4 AF        1
    ##  5 AG        1
    ##  6 AI        1
    ##  7 AL        1
    ##  8 AM        1
    ##  9 AO        1
    ## 10 AQ        1
    ## # … with 239 more rows

``` r
gap %>% 
  filter(is.na(iso2)) %>% 
  group_by(name) %>% 
  distinct(name)
```

    ## # A tibble: 2 × 1
    ## # Groups:   name [2]
    ##   name   
    ##   <chr>  
    ## 1 <NA>   
    ## 2 Namibia

One of these countries is Namibia and the other is a country that
doesn’t have an iso2 code so it’s listed as “NA.”

(2-2b) Now repeat the same for name and iso3-code. Are there country
names that have more than one iso3-code? What are these countries? Hint:
two of these entitites are CHANISL and NLD CURACAO.

``` r
gap %>% 
  group_by(name) %>% 
  summarise(num = n_distinct(iso3)) %>% 
  arrange(desc(num))
```

    ## # A tibble: 250 × 2
    ##    name                  num
    ##    <chr>               <int>
    ##  1 <NA>                    4
    ##  2 Afghanistan             1
    ##  3 Albania                 1
    ##  4 Algeria                 1
    ##  5 American Samoa          1
    ##  6 Andorra                 1
    ##  7 Angola                  1
    ##  8 Anguilla                1
    ##  9 Antarctica              1
    ## 10 Antigua and Barbuda     1
    ## # … with 240 more rows

``` r
gap %>% 
  filter(is.na(name)) %>% 
  group_by(name) %>% 
  distinct(iso3)
```

    ## # A tibble: 4 × 2
    ## # Groups:   name [1]
    ##   name  iso3       
    ##   <chr> <chr>      
    ## 1 <NA>  CHANISL    
    ## 2 <NA>  GBM        
    ## 3 <NA>  KOS        
    ## 4 <NA>  NLD_CURACAO

There are four countries that do not have an iso3 code, which makes it
seems as if there are countries that have more than one iso3 code.

(2-3) What is the minimum and maximum year in these data?

``` r
gap %>% 
  filter(!is.na(time)) %>% 
  summarise(min=min(time), max=max(time))
```

    ## # A tibble: 1 × 2
    ##     min   max
    ##   <dbl> <dbl>
    ## 1  1960  2019

(3-1) How many missing co2 emissions are there for each year? Analyze
both missing CO2 and co2_PC. Which years have most missing data?

``` r
total_missing_co2 <- gap %>%
  group_by(time) %>% 
  summarise(missing_co2 = sum(is.na(co2)),
            missing_co2_pc = sum(is.na(co2_PC)))
total_missing_co2
```

    ## # A tibble: 61 × 3
    ##     time missing_co2 missing_co2_pc
    ##    <dbl>       <int>          <int>
    ##  1  1960          60             60
    ##  2  1961          60             60
    ##  3  1962          58             58
    ##  4  1963          57             57
    ##  5  1964          51             51
    ##  6  1965          51             51
    ##  7  1966          51             51
    ##  8  1967          51             51
    ##  9  1968          51             51
    ## 10  1969          51             51
    ## # … with 51 more rows

``` r
total_missing_co2 %>% 
  summarise(max_missing_co2 = time[which.max(missing_co2)],
            max_missing_co2_pc = time[which.max(missing_co2_pc)])
```

    ## # A tibble: 1 × 2
    ##   max_missing_co2 max_missing_co2_pc
    ##             <dbl>              <dbl>
    ## 1            2017               2017

2017 is the year missing the most data

(3-2) Make a plot of total CO2 emissions over time for the U.S, China,
and India. Add a few more countries of your choice. Explain what do you
see.

``` r
total_co2 <- gap %>% 
  group_by(iso3, time) %>% 
  filter(!is.na(co2), !is.na(name)) %>% 
  summarise(co2mean = mean(co2)) %>% 
  filter(iso3 == "USA" | iso3 == "CHN" | iso3 == "IND" | iso3 == "AUS" | iso3 == "RUS")
```

    ## `summarise()` has grouped output by 'iso3'. You can override using the
    ## `.groups` argument.

``` r
total_co2 %>% 
  ggplot(aes(time, co2mean, col=factor(iso3))) +
  geom_line() +
  labs(x = "Time", y = "CO2 Emissions", 
       col = "Country") +
  ggtitle("CO2 Emissions by Country")
```

![](ps5Rmarkdown_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

China sharply increased its emissions after the 2000s while India and
the US are steadily increasing. Australia is very slowly increasing its
emissions while remaining relatively low and Russia has actually
decreased its emissions since the 1990s.

(3-3) Now let’s analyze the CO2 emissions per capita (co2_PC ). Make a
similar plot of the same countries. What does this figure suggest?

``` r
total_co2_pc <- gap %>% 
  group_by(iso3, time) %>% 
  filter(!is.na(co2), !is.na(name)) %>% 
  summarise(co2meanpc = mean(co2_PC)) %>% 
  filter(iso3 == "USA" | iso3 == "CHN" | iso3 == "IND" | iso3 == "AUS" | iso3 == "RUS")
```

    ## `summarise()` has grouped output by 'iso3'. You can override using the
    ## `.groups` argument.

``` r
total_co2_pc %>% 
  ggplot(aes(time, co2meanpc, col=factor(iso3))) +
  geom_line() +
  labs(x = "Time", y = "CO2 Emissions per Capita", 
       col = "Country") +
  ggtitle("CO2 Emissions Per Capita by Country")
```

![](ps5Rmarkdown_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

In this figure the US and Australia have the highest CO2 emissions per
capita, meaning that per person they emit the most CO2. India and China
are the two lowest because of how large their populations are and and
Russia is between all of the other countries in terms of co2 emissions
per capita.

(3-4) Compute average CO2 emissions per capita across the continents
(assume region is the same as continent). Comment what do you see. Note:
just compute averages over countries and ignore the fact that countries
are of different size. Hint: Americas 2016 should be 4.80.

``` r
gap %>% 
  group_by(region) %>% 
  filter(time == 2016,
         region == "Americas") %>% 
  summarise(americas_co2pc = mean(co2_PC, na.rm=TRUE))
```

    ## # A tibble: 1 × 2
    ##   region   americas_co2pc
    ##   <chr>             <dbl>
    ## 1 Americas           4.80

``` r
avg_co2pc_cont <- gap %>% 
  group_by(region) %>% 
  filter(!is.na(region)) %>% 
  summarise(co2pc_region = mean(co2_PC, na.rm=TRUE))
avg_co2pc_cont
```

    ## # A tibble: 5 × 2
    ##   region   co2pc_region
    ##   <chr>           <dbl>
    ## 1 Africa          0.930
    ## 2 Americas        6.46 
    ## 3 Asia            6.21 
    ## 4 Europe          7.95 
    ## 5 Oceania         4.39

Europe has the highest CO2 emissions per capita in comparison to the
other continents at 7.95. America and Asia are next with 6.46 and 6.21.
And Oceania and Africa have the smallest with 4.39 and 0.93.

(3-5) Make a barplot where you show the previous results–average CO2
emissions per capita across continents in 1960 and 2016.

``` r
years_1960_2016 <- c("1960", "2016")
aco2ac <- gap %>% 
  filter(!is.na(co2_PC), !is.na(region)) %>% 
  filter(name !="") %>% 
  filter(time %in% years_1960_2016) %>% 
  group_by(time, region) %>% 
  summarise(avg_co2 = mean(co2_PC), .groups = "keep")

ggplot(aco2ac, aes(x = region, y = avg_co2, fill = time)) +
  geom_bar(stat = "identity", position = position_dodge2(0)) +
  labs(x = "Continent", y = "Average CO2 Emissions Per Capita")
```

![](ps5Rmarkdown_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

(3-6) Which countries are the three largest, and three smallest CO2
emitters (in terms of CO2 per capita) in 2019 for each continent?

``` r
gap %>%
  filter(time == 2019) %>% 
  group_by(region) %>% 
  arrange(co2_PC) %>% 
  slice_head(n = 3) %>% 
  select(region, name)
```

    ## # A tibble: 18 × 2
    ## # Groups:   region [6]
    ##    region   name                
    ##    <chr>    <chr>               
    ##  1 Africa   Angola              
    ##  2 Africa   Burundi             
    ##  3 Africa   Benin               
    ##  4 Americas Aruba               
    ##  5 Americas Argentina           
    ##  6 Americas Antigua and Barbuda 
    ##  7 Asia     Afghanistan         
    ##  8 Asia     United Arab Emirates
    ##  9 Asia     Armenia             
    ## 10 Europe   Albania             
    ## 11 Europe   Andorra             
    ## 12 Europe   Austria             
    ## 13 Oceania  American Samoa      
    ## 14 Oceania  Australia           
    ## 15 Oceania  Fiji                
    ## 16 <NA>     <NA>                
    ## 17 <NA>     <NA>                
    ## 18 <NA>     <NA>

``` r
gap %>%
  filter(time == 2019) %>% 
  group_by(region) %>% 
  arrange(co2_PC) %>% 
  slice_tail(n = 3) %>% 
  select(region, name)
```

    ## # A tibble: 18 × 2
    ## # Groups:   region [6]
    ##    region   name                              
    ##    <chr>    <chr>                             
    ##  1 Africa   South Africa                      
    ##  2 Africa   Zambia                            
    ##  3 Africa   Zimbabwe                          
    ##  4 Americas Venezuela (Bolivarian Republic of)
    ##  5 Americas Virgin Islands (British)          
    ##  6 Americas Virgin Islands (U.S.)             
    ##  7 Asia     Uzbekistan                        
    ##  8 Asia     Viet Nam                          
    ##  9 Asia     Yemen                             
    ## 10 Europe   Slovenia                          
    ## 11 Europe   Sweden                            
    ## 12 Europe   Ukraine                           
    ## 13 Oceania  Tuvalu                            
    ## 14 Oceania  Vanuatu                           
    ## 15 Oceania  Samoa                             
    ## 16 <NA>     <NA>                              
    ## 17 <NA>     <NA>                              
    ## 18 <NA>     <NA>

(4-1) Make a scatterplot of GDP per capita versus life expectancy by
country, using data for 1960. Make the point size dependent on the
country size, and color those according to the continent.

``` r
library(ggplot2)
gap_1960 <- gap[gap$time == 1960,]
  ggplot(gap_1960, aes(x = GDP_PC, y = lifeExpectancy, 
                     size = totalPopulation, color = region)) +
  geom_point(alpha = 0.6) +
  scale_size_continuous(range = c(1,10)) +
  xlab("GDP Per Capita") +
  ylab("Life Expectancy") +
  ggtitle("Life Expectancy vs. GDP Per Capita in 1960 by Country")
```

    ## Warning: Removed 164 rows containing missing values (`geom_point()`).

![](ps5Rmarkdown_files/figure-gfm/unnamed-chunk-15-1.png)<!-- --> The
countries with a lower GDP per capita appear to have a lower life
expectancy on average. There is also a large cluster of countries on the
left side of the plot, this is likely due to high poverty levels during
the time.

(4-2) Make a similar plot, but this time use 2019 data only.

``` r
library(ggplot2)
gap_2019 <- gap[gap$time == 2019,]
  ggplot(gap_2019, aes(x = GDP_PC, y = lifeExpectancy, 
                     size = totalPopulation, color = region)) +
  geom_point(alpha = 0.6) +
  scale_size_continuous(range = c(1,10)) +
  xlab("GDP Per Capita") +
  ylab("Life Expectancy") +
  ggtitle("Life Expectancy vs. GDP Per Capita in 2019 by Country")
```

    ## Warning: Removed 74 rows containing missing values (`geom_point()`).

![](ps5Rmarkdown_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

(4-3) Compare these two plots and comment what do you see. How has world
developed through the last 60 years?

Over the last 60 years, life expectancy and GDP per capita have
increased greatly. In 1960, the oldest life expectancy was about 74, but
it has increased to 85. And the largest GDP per capita has increased
from about 27000 to about 115000.

(4-4) Compute the average life expectancy for each continent in 1960 and
2019. Do the results fit with what do you see on the figures? Note: here
as average I mean just average over countries, ignore the fact that
countries are of different size.

``` r
gap %>% 
  group_by(region, time) %>% 
  filter(time == 1960 | time == 2019, !is.na(lifeExpectancy)) %>% 
  summarise(avg_LE = mean(lifeExpectancy))
```

    ## `summarise()` has grouped output by 'region'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 12 × 3
    ## # Groups:   region [6]
    ##    region    time avg_LE
    ##    <chr>    <dbl>  <dbl>
    ##  1 Africa    1960   41.5
    ##  2 Africa    2019   64.1
    ##  3 Americas  1960   58.6
    ##  4 Americas  2019   75.8
    ##  5 Asia      1960   51.6
    ##  6 Asia      2019   74.6
    ##  7 Europe    1960   68.3
    ##  8 Europe    2019   79.4
    ##  9 Oceania   1960   56.4
    ## 10 Oceania   2019   73.5
    ## 11 <NA>      1960   70.7
    ## 12 <NA>      2019   77.8

(4-5) Compute the average LE growth from 1960-2019 across the
continents. Show the results in the order of growth. Explain what do you
see.

``` r
gap %>% 
  filter(!is.na(region), !is.na(lifeExpectancy), !is.na(time)) %>% 
  filter(time %in% c("1960", "2019")) %>% 
  group_by(region, time) %>% 
  summarise(avg = mean(lifeExpectancy)) %>% 
  mutate(prev = lag(avg, default = 0), growth = (avg - prev)) %>% 
  arrange(rank(desc(growth)))
```

    ## `summarise()` has grouped output by 'region'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 10 × 5
    ## # Groups:   region [5]
    ##    region    time   avg  prev growth
    ##    <chr>    <dbl> <dbl> <dbl>  <dbl>
    ##  1 Europe    1960  68.3   0     68.3
    ##  2 Americas  1960  58.6   0     58.6
    ##  3 Oceania   1960  56.4   0     56.4
    ##  4 Asia      1960  51.6   0     51.6
    ##  5 Africa    1960  41.5   0     41.5
    ##  6 Asia      2019  74.6  51.6   23.0
    ##  7 Africa    2019  64.1  41.5   22.6
    ##  8 Americas  2019  75.8  58.6   17.2
    ##  9 Oceania   2019  73.5  56.4   17.1
    ## 10 Europe    2019  79.4  68.3   11.1

In this dataframe I noticed that the previous two countries with the
lowest life expectancy grew the most. Also, Europe, which had the
highest life expectancy in 1960 grew the least. I assume this is because
medication and treatments that were already available in Europe during
the 60s have become more available worldwide, causing the continents
like Asia and Africa to greatly increase their life expectancy.

(4-6) Show the histogram of GDP per capita for years of 1960 and 2019.
Try to put both histograms on the same graph, see how well you can do
it!

``` r
gap %>% 
  group_by(time) %>% 
  filter(time == 1960 | time == 2019) %>% 
  filter(!is.na(GDP_PC)) %>% 
  ggplot(aes(GDP_PC, fill=factor(time))) +
  geom_histogram(position = "dodge")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](ps5Rmarkdown_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

(4-7) What was the ranking of US in terms of life expectancy in 1960 and
in 2019?

``` r
usrank_1960 <- gap %>% 
  filter(time == 1960, !is.na(region)) %>% 
  mutate(ranking = rank(desc(lifeExpectancy))) %>% 
  filter(name == "United States of America") %>% 
  select(ranking) %>% 
  pull()
usrank_1960
```

    ## [1] 17

``` r
usrank <- gap %>% 
  filter(time %in% c(1960, 2019), !is.na(region)) %>% 
  group_by(time) %>% 
  mutate(ranking = rank(desc(lifeExpectancy))) %>% 
  filter(name == "United States of America") %>% 
  select(ranking) %>% 
  pull()
```

    ## Adding missing grouping variables: `time`

``` r
usrank
```

    ## [1] 17 46

The US was ranked 17th in 1960 and 46th in 2019.

(4-8) If you did this correctly, then you noticed that US ranking has
been falling quite a bit. But we also have more countries in 2019–what
about the relative rank divided by the corresponding number of countries
that have LE data in the corresponding year?

``` r
gap %>% 
  filter(time == 2019, !is.na(lifeExpectancy), name !="") %>% 
  summarise(num_countries_19=n())
```

    ## # A tibble: 1 × 1
    ##   num_countries_19
    ##              <int>
    ## 1              196

``` r
gap %>% 
  filter(time == 1960, !is.na(lifeExpectancy), name !="") %>% 
  summarise(num_countries_19=n())
```

    ## # A tibble: 1 × 1
    ##   num_countries_19
    ##              <int>
    ## 1              188

The relative rank divided by the corresponding number of countries in
2019 is 0.2347 (46 / 196 = 0.2347)

Finally tell us how many hours did you spend on this PS. This PS was
difficult for me. I kind of lost count but I spent at least 10 hours.
