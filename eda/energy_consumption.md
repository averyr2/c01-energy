Energy Consumption Sources by State
================
Avery Rogers
2020-02-05

  - [Part I: Energy Consumption for Electricity by State and
    Year](#part-i-energy-consumption-for-electricity-by-state-and-year)
      - [Cleaning Data](#cleaning-data)
      - [Getting percentages by source for each year and
        state](#getting-percentages-by-source-for-each-year-and-state)
      - [Creating necessary plotting
        functions](#creating-necessary-plotting-functions)
      - [Example plot: Alaska](#example-plot-alaska)
      - [Greatest Consumers By
        Fraction](#greatest-consumers-by-fraction)
      - [Example: Wood and Waste 2017](#example-wood-and-waste-2017)
  - [Part II: Energy Usage by
    Population](#part-ii-energy-usage-by-population)
      - [Cleaning Population Data](#cleaning-population-data)
      - [Combining The Tibbles](#combining-the-tibbles)
      - [Total Energy Consumption Per Capita by State over
        Time](#total-energy-consumption-per-capita-by-state-over-time)
      - [Example States](#example-states)
      - [State energy consumption per capita by
        source](#state-energy-consumption-per-capita-by-source)
      - [Example: Maine](#example-maine)

``` r
# Libraries
library(tidyverse)
library(ggalluvial)
library(lubridate)

# Parameters

energy_data <- 
  read_rds("/Users/averyrogers/GitHub/c01-energy/data/energy_consumption.rds")

population_data <- 
  read_rds("/Users/averyrogers/GitHub/c01-energy/data/state_population.rds")

#===============================================================================
```

# Part I: Energy Consumption for Electricity by State and Year

In this section, I am only looking at the sources of energy that
primarily contribute to electricity generation across states: coal,
geothermal power, hydropower, natural gas, nuclear power, solar power,
wood and waste power, and wind power. I remove all the other energy
usage statistics from the dataset to hone in on these. Most notably, I
remove petroleum, which is the largest source of energy in many states
but primarily for transportation rather than electriticy in the grid.
Petroleum usage will be covered in a later section.

An important note about this data: all energy usage is measured in BTU,
or British Thermal Units. One BTU is the amount of energy needed to
raise one pound of water by one degree Fahrenheit. (I’m not sure why
it’s called *British* thermal units if the measurements it uses are
imperial\!). One BTU is approximately 1,055 joules. BTU is the unit used
to sell natural gas in the US (dollars per million BTUs), and is the
best unit of measurement for comparing different energy sources.

## Cleaning Data

``` r
energy_data_clean <- 
  energy_data %>% 
  select(-data_status) %>% 
  pivot_longer(c("1960":"2017"), names_to = "year", values_to = "amount") %>% 
  mutate(year = as.integer(year)) %>% 
  select(state, year, msn, amount) %>% 
  filter(
    msn %in% c(
      "CLTCB", 
      "GETCB", 
      "HYTCB", 
      "NGTCB", 
      "NUETB", 
      "SOTCB", 
      "WWTCB",
      "WYTCB"
    )
  ) %>% 
  mutate(
    msn = 
      case_when(
        msn == "CLTCB" ~ "Coal",
        msn == "GETCB" ~ "Geothermal",
        msn == "HYTCB"~ "Hydropower",
        msn == "NGTCB" ~ "Natural Gas", 
        msn == "NUETB" ~ "Nuclear", 
        msn == "SOTCB" ~ "Solar",
        msn == "WWTCB" ~ "Wood and Waste", 
        msn == "WYTCB" ~ "Wind"
      ) 
  ) %>% 
  mutate(amount = replace_na(amount, 0))

energy_data_clean
```

    ## # A tibble: 24,128 x 4
    ##    state  year msn   amount
    ##    <chr> <int> <chr>  <dbl>
    ##  1 AK     1960 Coal    7189
    ##  2 AK     1961 Coal   11552
    ##  3 AK     1962 Coal   13559
    ##  4 AK     1963 Coal   11999
    ##  5 AK     1964 Coal   12029
    ##  6 AK     1965 Coal    9888
    ##  7 AK     1966 Coal   16431
    ##  8 AK     1967 Coal   18462
    ##  9 AK     1968 Coal   15998
    ## 10 AK     1969 Coal   13191
    ## # … with 24,118 more rows

## Getting percentages by source for each year and state

``` r
consumption_fractions <- 
  energy_data_clean %>% 
  group_by(state, year) %>% 
  mutate(total_cons = sum(amount)) %>% 
  ungroup() %>% 
  mutate(frac_amount = amount / total_cons) %>% 
  select(state, year, msn, frac_amount)

consumption_fractions
```

    ## # A tibble: 24,128 x 4
    ##    state  year msn   frac_amount
    ##    <chr> <int> <chr>       <dbl>
    ##  1 AK     1960 Coal        0.449
    ##  2 AK     1961 Coal        0.546
    ##  3 AK     1962 Coal        0.541
    ##  4 AK     1963 Coal        0.473
    ##  5 AK     1964 Coal        0.447
    ##  6 AK     1965 Coal        0.379
    ##  7 AK     1966 Coal        0.444
    ##  8 AK     1967 Coal        0.476
    ##  9 AK     1968 Coal        0.380
    ## 10 AK     1969 Coal        0.204
    ## # … with 24,118 more rows

## Creating necessary plotting functions

``` r
plot_fractions <- function(state_abbv, state) {
  consumption_fractions %>% 
    filter(state == state_abbv) %>% 
    ggplot(aes(x = year, y = frac_amount, alluvium = msn)) + 
    geom_alluvium(
      aes(fill = msn, color = msn), 
      width = 1/16, 
      alpha = 1/2, 
      decreasing = NA
    ) + 
    scale_x_continuous(
      breaks = seq(1960, 2015, 5),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      expand = c(0, 0),
      breaks = seq(0, 1, 0.1),
      minor_breaks = NULL,
      labels = scales::percent_format(accuracy = 1)
    ) +
    labs(
      x = "Year",
      y = "Percentage of Energy Consumption",
      title = str_glue("Energy Consumption by Source in {state}, Percentage"),
      caption = "Source: US Energy Information Administration"
    ) +
    theme(
      panel.background = element_rect(fill = "grey95"),
      panel.grid.major = element_line(color = "white"),
      panel.grid.minor = element_line(color = "white")
    )
} 

plot_amount <- function(state_abbv, state) {
  energy_data_clean %>% 
    filter(state == state_abbv) %>% 
    ggplot(aes(x = year, y = amount, alluvium = msn)) + 
    geom_alluvium(
      aes(fill = msn, color = msn), 
      width = 1/16, 
      alpha = 1/2, 
      decreasing = NA
    ) + 
    scale_x_continuous(
      breaks = seq(1960, 2015, 5),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      expand = c(0, 0),
      labels = scales::number_format(accuracy = 1)
    ) +
    labs(
      x = "Year",
      y = "Amount (billion BTU)" ,
      title = str_glue("Energy Consumption by Source in {state}, Total Amount"),
      caption = "Source: US Energy Information Administration",
      color = "Energy\nSource",
      fill = "Energy\nSource"
    ) +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(color = "gray92"),
      panel.grid.minor = element_line(color = "gray92")
    )
}
```

## Example plot: Alaska

The Alaska plots demonstrate why it is interesting and important to plot
not only the percentages of electricity-generating energy sources by
year, but also the total amount of electricity-generating enegy sources
by year. As we can see in the first plot, Alaska repidly transitions
from minimal to very high usage of natural gas in the 1960s and 70s as a
percentage of its energy sources. Looking at the second plot, we see a
more nuanced story: it was not that Alaska used large amount of coal in
the 1960s, but rather that their energy usage overall more than doubled
from 1960 to 1970, and continued to grow rapidly in the decades
afterwards.

``` r
plot_fractions("AK", "Alaska")
```

![](energy_consumption_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
plot_amount("AK", "Alaska")
```

![](energy_consumption_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

## Greatest Consumers By Fraction

``` r
greatest_consumer <- function(target_msn, target_year) {
  consumption_fractions %>% 
    filter(msn == target_msn, year == target_year) %>% 
    mutate(state = fct_reorder(state, frac_amount)) %>%
    ggplot(aes(state, frac_amount)) + 
    geom_point() +
    coord_flip() +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1)
    ) +
    labs(
      x = "State",
      y = str_glue("Percentage of Energy Consumption from {target_msn}"), 
      title = str_glue(
        "Comparative Energy Consumption from {target_msn} in {target_year}",
      caption = "Source: US Energy Information Administration"
      )
    ) +
    theme(
      panel.background = element_rect(fill = "grey98"),
      panel.grid.major = element_line(color = "grey92"),
      panel.grid.minor = element_line(color = "grey92")
    )
} 
```

## Example: Wood and Waste 2017

The northeastern states of Maine, New Hampshire, and Vermont consume a
large fraction of energy sourced from wood and waste as of 2017. Who
knew\!

``` r
greatest_consumer("Wood and Waste", 2017)
```

![](energy_consumption_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

# Part II: Energy Usage by Population

In this section, I wanted to understand the relationship between energy
usage and state population and see whether increases in energy usage
were accelerating faster or slower than overall population growth by
state. This will help shed more light on how energy consumption is
changing per capita and whether or not we are headed in the right
direction in terms of sustainability.

## Cleaning Population Data

``` r
population_data_clean <- 
  population_data %>% 
  pivot_longer(
    cols = c(AKPOP:WYPOP), 
    names_to = "state", 
    values_to = "population"
  ) %>% 
  mutate_at(vars(state), ~ str_sub(., 1, 2)) %>% 
  mutate(
    year = as.integer(year(DATE)),
    population_thousands = as.integer(population)
  ) %>% 
  select(year, state, population_thousands) %>% 
  filter(year > 1959, year < 2018)
```

## Combining The Tibbles

Here, I add a variable `amount_per_capita` to see how much energy is
being used per each 1,000 people in the state.

``` r
total_energy_population <- 
  energy_data_clean %>% 
  left_join(population_data_clean, by = c("year", "state")) %>% 
  mutate(amount_per_capita = amount / population_thousands)

fraction_energy_population <- 
  consumption_fractions %>% 
  left_join(population_data_clean, by = c("year", "state"))
```

## Total Energy Consumption Per Capita by State over Time

When comparatively plotting the total energy consumption per 1,000
people by state over time, we see very surprising results: some states
are using as much as 3x the amount of energy per capita as others.

``` r
per_capita_energy <- function(state_abbv) { 
  total_energy_population %>% 
    filter(state %in% state_abbv) %>% 
    count(state, year, wt = amount_per_capita) %>% 
    ggplot(aes(year, n, color = state)) +
    geom_line() +
    scale_x_continuous(
      breaks = seq(1960, 2020, 5), 
      expand = c(0, 0)
    ) +
    labs(
      x = "Year", 
      y = "Total Energy Consumption in BTUs per 1,000 People",
      title = "Comparing total energy use per capita in US states",
      color = "State",
      caption = "Sources: St. Louis Fred, US Energy Information Administration"
    ) +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(color = "grey92"),
      panel.grid.minor = element_line(color = "grey92")
    )
}
```

## Example States

As we can see, variation among states’ per-capita energy consumption is
high, with a state like Kentucky consuming nearly three times as many
BTUs of energy per 1,000 people as California.

``` r
per_capita_energy(c("FL", "CA", "CT", "CO", "PA", "KY"))
```

![](energy_consumption_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

## State energy consumption per capita by source

Here, I look at at comparing the use of different energy sources per
1,000 people within a state. It’s particularly interesting to see how
sources have substituted over time, and whether or not the overall trend
is increasing or decreasing. This supplements the total usage by source
graphs above by paying closer attention to the usage standardized for
population changes.

``` r
per_capita_compare <- function(state_abbv, state, msn_vars) {
  total_energy_population %>% 
  filter(state == state_abbv, msn %in% msn_vars) %>% 
  ggplot(aes(year, amount_per_capita, color = msn)) +
  geom_line() +
  scale_x_continuous(
    breaks = seq(1960, 2020, 5), 
    expand = c(0, 0)
  ) +
  scale_y_continuous(expand = c(0, 0)) + 
  labs(
    x = "Year", 
    y = "Total Energy Consumption per 1,000 People (BTUs)",
    title = str_glue("{state} energy use per capita by energy source"),
    color = "Energy\nSource",
    caption = "Sources: St. Louis FRED, US Energy Information Administration"
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey92"),
    panel.grid.minor = element_line(color = "grey92")
  )
}
```

## Example: Maine

We see a sudden spike in natural gas consumption per capita in Maine
that trails off after its peak. Hydropower and wood and waste power stay
relatively constant after 1980.

``` r
per_capita_compare(
  "ME", 
  "Maine", 
  c("Coal", "Natural Gas", "Wood and Waste", "Hydropower")
)
```

![](energy_consumption_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->
