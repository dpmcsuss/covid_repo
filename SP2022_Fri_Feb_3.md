MA \[46\]15 - Data Science in R
================
Daniel Sussman
2022-02-04

``` r
knitr::opts_chunk$set(tidy = FALSE, cache = FALSE) # turn off formatR's tidy
knitr::opts_chunk$set(comment = "") # remove comment tags
knitr::opts_chunk$set(cache.path = "cache/", fig.path='figures/', fig.asp=.66, fig.width = 7, fig.align='center', dev="svg")
```

``` r
library(tidyverse)
```

    ── Attaching packages ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.1 ──

    ✔ ggplot2 3.3.5     ✔ purrr   0.3.4
    ✔ tibble  3.1.6     ✔ dplyr   1.0.7
    ✔ tidyr   1.1.4     ✔ stringr 1.4.0
    ✔ readr   2.1.1     ✔ forcats 0.5.1

    ── Conflicts ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()

``` r
load("../lectures/data/covid19datahub/covid_updated_all_states.RData") 
load("../lectures/data/covid19datahub/covid_updated_big6.RData") 
```

------------------------------------------------------------------------

# Coding in general

Also, I try to make the code be organized by separating lines, but error
appears when running the code. `Error: Cannot use`+.gg()\`

------------------------------------------------------------------------

# Plots and ggplot questions

## Plots by date

``` r
ggplot(covid) +
  geom_line(aes(x = date, y = tests, color = state))
```

    Warning: Removed 305 row(s) containing missing values (geom_path).

<img src="figures/unnamed-chunk-3-1.svg" style="display: block; margin: auto;" />

I would like to make the data look less crowded and spread out the line
that show on the plot.

------------------------------------------------------------------------

``` r
ggplot(covid_6, aes(x=date,y=tests/population, color = state)) + geom_point()+geom_col()
```

    Warning: Removed 87 rows containing missing values (position_stack).

    Warning: Removed 87 rows containing missing values (geom_point).

<img src="figures/unnamed-chunk-4-1.svg" style="display: block; margin: auto;" />

------------------------------------------------------------------------

``` r
ggplot(covid_6,aes(x=date,y=tests,color=state))+geom_area()
```

    Warning: Removed 87 rows containing missing values (position_stack).

<img src="figures/unnamed-chunk-5-1.svg" style="display: block; margin: auto;" />

------------------------------------------------------------------------

``` r
ggplot(data=covid_6) + geom_line(aes(date, people_vaccinated, color = state))
```

    Warning: Removed 1804 row(s) containing missing values (geom_path).

<img src="figures/unnamed-chunk-6-1.svg" style="display: block; margin: auto;" />

------------------------------------------------------------------------

``` r
ggplot(weekly, aes(x = date, y = tests)) +
  geom_point(aes(color = as.factor(c2_workplace_closing)), size = .2) +
  geom_smooth(aes(linetype = state), size = 0.1)
```

    `geom_smooth()` using method = 'loess' and formula 'y ~ x'

    Warning: Removed 309 rows containing non-finite values (stat_smooth).

    Warning: Removed 309 rows containing missing values (geom_point).

<img src="figures/unnamed-chunk-7-1.svg" style="display: block; margin: auto;" />

------------------------------------------------------------------------

``` r
ggplot(data = covid_6) +
  geom_smooth(aes(x = date, y = deaths/confirmed, color = state)) +
  xlab("Date") +
  ylab("Death Rate (per case)") +
  labs(color = "State") +
  ggtitle("Covid Death Rate Over Time") +
  scale_y_continuous(labels=scales::percent)
```

    `geom_smooth()` using method = 'loess' and formula 'y ~ x'

    Warning: Removed 4 rows containing non-finite values (stat_smooth).

<img src="figures/unnamed-chunk-8-1.svg" style="display: block; margin: auto;" />

------------------------------------------------------------------------

``` r
ggplot(weekly_6,aes(x=date,y=(confirmed/population)))+
  geom_line(aes(color=state),size=1.1)+
  labs(
    title = "Confirmed cases per capita in 6 states",
    subtitle = "Rolling weekly average from the start of 2020 to present",
    caption = "Graph created by Jacob Stenstrom for MA415"
  ) +
  ylab("Confirmed cases per capita") +
  xlab("Date")
```

    Warning: Removed 4 row(s) containing missing values (geom_path).

<img src="figures/unnamed-chunk-9-1.svg" style="display: block; margin: auto;" />

Create a heat map with the darkest colored states being the one with the
highest case count; reorder a column/bar graph to be in descending
order.

------------------------------------------------------------------------

## Adding facets

``` r
ggplot(data=covid)+
  geom_point(mapping=aes(x=date,y=confirmed/population,color=state))+
  facet_wrap(~state,nrow=8)
```

    Warning: Removed 182 rows containing missing values (geom_point).

<img src="figures/unnamed-chunk-10-1.svg" style="display: block; margin: auto;" />

I wonder if there’s a way to make the x-axis(dates) look cleaner so that
it’s not overlapping on each other and more readable.

------------------------------------------------------------------------

``` r
ggplot(covid, aes(x = date, y = deaths / confirmed)) +
  geom_point(aes(color = state), size = 0.1) +
  facet_wrap(~ as.factor(population))
```

    Warning: Removed 182 rows containing missing values (geom_point).

<img src="figures/unnamed-chunk-11-1.svg" style="display: block; margin: auto;" />

------------------------------------------------------------------------

``` r
library(RColorBrewer)
number_ticks <- function(n) {function(limits) pretty(limits, n)}

ggplot(data = covid_6)+
  geom_line( aes(x = date, y = tests/10000, color = state)) +
  scale_color_brewer(palette="Dark2")+
  ylab("every 10,000 tests")+
  facet_grid(~state)
```

    Warning: Removed 87 row(s) containing missing values (geom_path).

<img src="figures/unnamed-chunk-12-1.svg" style="display: block; margin: auto;" />

I was unable to change the x scale to customize the date ticks.

------------------------------------------------------------------------

``` r
ggplot(covid_6) +
  geom_smooth(mapping = aes(x = date, y = deaths/population*100)) +
  labs(y = "Percentage Death per Population") +
  labs(title = "Deaths per Population Over Time") +
  facet_wrap(~state)
```

    `geom_smooth()` using method = 'loess' and formula 'y ~ x'

    Warning: Removed 4 rows containing non-finite values (stat_smooth).

<img src="figures/unnamed-chunk-13-1.svg" style="display: block; margin: auto;" />

Would like to format the dates to look nicer.

------------------------------------------------------------------------

## Coloring by a continuous variable

``` r
ggplot(covid_6)+geom_point(aes(x=date, y=log(deaths), color = stringency_index), size=1, position = 'jitter')+
       geom_line(aes(x=date,y=log(people_fully_vaccinated), color=stringency_index), size=1.5)+
       facet_wrap(~state)+theme(axis.text.x=element_text(angle=30))+
       labs(title='(Log) Cumulative Deaths and Vaccinations Over Time at Varying Stringency Levels', x='Date Since Tracking Began', y='Cumulative Deaths (lower) and Vaccinations(upper) in log scale', color='Level of Stringency')
```

    Warning: Removed 154 rows containing missing values (geom_point).

    Warning: Removed 323 row(s) containing missing values (geom_path).

<img src="figures/unnamed-chunk-14-1.svg" style="display: block; margin: auto;" />

I still don’t know how to label the lines without another package, but I
may be missing something.

------------------------------------------------------------------------

``` r
ggplot(covid_6, aes(x = date, y = deaths, color = vaccines)) + geom_point() + facet_wrap(~ state) + ylab("deaths due to covid") + ggtitle("Covid Deaths")
```

    Warning: Removed 4 rows containing missing values (geom_point).

<img src="figures/unnamed-chunk-15-1.svg" style="display: block; margin: auto;" />

------------------------------------------------------------------------

``` r
ggplot(covid_6)+
  geom_point(aes(x=date, y=deaths, color = stringency_index), position = 'jitter')+
  facet_wrap(~state)+
  theme(axis.text.x=element_text(angle=30))+
  labs(title='Cumulative Deaths Over Time at Varying Stringency Levels', x='Date Since Tracking Began', y='Cumulative Deaths')
```

    Warning: Removed 4 rows containing missing values (geom_point).

<img src="figures/unnamed-chunk-16-1.svg" style="display: block; margin: auto;" />

------------------------------------------------------------------------

## Comparing two numbers

``` r
ggplot(data = covid) + geom_point(mapping = aes(x = tests, y = confirmed, color = state))
```

    Warning: Removed 489 rows containing missing values (geom_point).

<img src="figures/unnamed-chunk-17-1.svg" style="display: block; margin: auto;" />

------------------------------------------------------------------------

``` r
ggplot(covid_6, aes(x=people_fully_vaccinated/population, y=deaths/confirmed, color=state))+geom_smooth()
```

    `geom_smooth()` using method = 'loess' and formula 'y ~ x'

    Warning: Removed 1804 rows containing non-finite values (stat_smooth).

<img src="figures/unnamed-chunk-18-1.svg" style="display: block; margin: auto;" />

------------------------------------------------------------------------

``` r
ggplot(covid_6) + geom_bar(aes(x=people_fully_vaccinated, y=deaths, color=state), stat='identity') + facet_wrap(~state)
```

    Warning: Removed 1804 rows containing missing values (position_stack).

<img src="figures/unnamed-chunk-19-1.svg" style="display: block; margin: auto;" />

------------------------------------------------------------------------

``` r
covid %>%
  mutate(avg_index = (stringency_index + government_response_index + containment_health_index + economic_support_index)/4) %>%
  ggplot() +
  geom_point(mapping = aes(x = avg_index, y = deaths))
```

    Warning: Removed 405 rows containing missing values (geom_point).

<img src="figures/unnamed-chunk-20-1.svg" style="display: block; margin: auto;" />

I wanted to tidy up the data, I’m not sure how much significance I can
derive from this plot considering that it looks a little confusing to
me. There are too many states, I hope I can show those states clearly

------------------------------------------------------------------------

## Discrete x- or y-axis

``` r
ggplot(covid)+geom_point(aes(y=state,x=tests)) +
  labs(title="US states v/s Number of tests")
```

    Warning: Removed 307 rows containing missing values (geom_point).

<img src="figures/unnamed-chunk-21-1.svg" style="display: block; margin: auto;" />

I did not know how to change the scale of the x axis

------------------------------------------------------------------------

``` r
covid %>%
  filter(date == '2022-01-25') %>%
  ggplot(aes(x = reorder(state, -stringency_index), y = stringency_index)) + 
  geom_col() + 
  theme(axis.text.x = element_text(angle = 90)) +
  xlab('State') + ylab('Stringency Index') +
  ggtitle('State Stringency Indexes as of Jan 25th 2022') +
  geom_hline(yintercept = 30.6, color = 'red')
```

<img src="figures/unnamed-chunk-22-1.svg" style="display: block; margin: auto;" />

Couldn’t add the horizontal line with the mean automatically (I had to
separately calculate the mean and then manually input it into
geom\_hline). I wanted to plot a second layer of geom\_point with the
number of confirmed cases (to see how stringency index and covid cases
were related), but it would just show the second layer (geom\_point)
without the original bar chart.

------------------------------------------------------------------------

``` r
ggplot(covid) + geom_bar(aes(x = state, fill = deaths))
```

<img src="figures/unnamed-chunk-23-1.svg" style="display: block; margin: auto;" />

------------------------------------------------------------------------

``` r
ggplot(covid) + geom_bar(aes(x = state, fill = deaths)) + coord_polar(theta = "x")
```

<img src="figures/unnamed-chunk-24-1.svg" style="display: block; margin: auto;" />

------------------------------------------------------------------------

``` r
ggplot(covid, mapping = aes(x = state, y = tests, color = state)) + 
  geom_point() +
  geom_boxplot() +
  labs(x = "Rate of Tests", y = "States", color = "States") +
  coord_flip()
```

    Warning: Removed 307 rows containing non-finite values (stat_boxplot).

    Warning: Removed 307 rows containing missing values (geom_point).

<img src="figures/unnamed-chunk-25-1.svg" style="display: block; margin: auto;" />

I wanted to be able to use coord\_polar() to see what this data would
look like in that sort of graph, but the labels of each state made the
visual of the graph itself less helpful.

------------------------------------------------------------------------

``` r
ggplot(covid)+geom_jitter(aes(y=state,x=tests, col=state)) +
  labs(title="US states v/s Number of tests")
```

    Warning: Removed 307 rows containing missing values (geom_point).

<img src="figures/unnamed-chunk-26-1.svg" style="display: block; margin: auto;" />

------------------------------------------------------------------------

``` r
cov_data <- data.frame(x = covid_6$h6_facial__coverings, y = covid_6$confirmed)
plot_data <- data.frame(x = cov_data$x, y = log(cov_data$y))
X <- cov_data$x
Y <- log(cov_data$y)
indicator <- X
plot <- ggplot(plot_data) + geom_boxplot(varwidth = TRUE, aes(x = X, y = Y, colour = indicator)) 
plot  + ggtitle("Evaluating number of confirmed cases in relation to mask mandates") + xlab("Mask mandate (ordinal scale)") + ylab("Log of number of confirmed cases")
```

    Warning: Removed 4 rows containing non-finite values (stat_boxplot).

<img src="figures/unnamed-chunk-27-1.svg" style="display: block; margin: auto;" />

I wanted to do a scatterplot but I quickly noticed that with the way the
data was (for facial covering) it would not work. I guess I just feel
like I did not know what else to do in terms of plots and just do not
know many of the plot functions available.

## Additional questions

Finally, I want to change the font and color of the title but keep it
default, since I cannot find the code for font and the specific color.

I wanted to try making a pie chart, but I thought maybe the type of data
we have wouldn’t fit well as a pie chart (and every time I tried I got
back very confusing plots)

I want to emphasize specific points and see what difference two states
have.

------------------------------------------------------------------------

# Coming Soon

``` r
cleaned <- separate(covid_6, "date", c("Year", "Month", "Day"), sep = "-")
x <- cleaned %>% filter(Year == "2020" & Month <= "06")
ggplot(data = x) + 
    geom_point(mapping = aes(x = Month, y = confirmed, color = Month)) + 
    facet_grid(.~state) + 
    labs(title = "Confirmed cases in 6 major states from Jan 2020 to Jun 2020")
```

    Warning: Removed 4 rows containing missing values (geom_point).

<img src="figures/unnamed-chunk-28-1.svg" style="display: block; margin: auto;" />

1.  I wanted to add a best fit line for each state to show the tendency
    of the growth of confirmed cases in each state, but I don’t know
    how.
2.  I also wanted to change the scale of y-axis, so that it would be
    easier for visualization, but I weren’t able to change.

------------------------------------------------------------------------

``` r
covid %>%
  filter(date >= "2021-01-01", date <= "2022-01-01") %>%
  #filter(confirmed >= 1000, confirmed <= 50000) %>%
  filter(tests > mean(tests, na.rm = TRUE))%>%
  filter(vaccines > mean(vaccines, na.rm = TRUE))%>%
  ggplot(aes(x = date, y = tests, colour = state)) +
  geom_point(size = 4, alpha = 0.5, position = "jitter", shape = 19) +
  geom_point(aes(x = date , y = vaccines),size = 2, position = "jitter", shape = 17) +
  #geom_smooth()+
  scale_y_continuous(
    breaks = seq(from = 0, to = 1500000000, by = 5000000)
  , labels = seq(from = 0, , to = 1500000000, by = 5000000)
  )+
  scale_x_date(breaks = scales::breaks_width("1 month"), labels = scales::label_date_short())+
  ggtitle("Number of Tests vs Vaccines in 2021") + 
  theme(plot.title = element_text(color = "black", size = 15, face = "bold",hjust = 0.5, vjust = -5))+
  labs(x="", y="Number of Tests")
```

<img src="figures/unnamed-chunk-29-1.svg" style="display: block; margin: auto;" />

------------------------------------------------------------------------

``` r
ggplot(filter(weekly_6, !is.na(confirmed), !is.na(c6_stay_at_home_requirements))) +
    geom_line(aes(x=date, y = confirmed, color=state)) +
    facet_wrap(~ as.factor(c6_stay_at_home_requirements)) +
    scale_x_date(date_labels="%m/%y")
```

<img src="figures/unnamed-chunk-30-1.svg" style="display: block; margin: auto;" />

I was unable to label the facet\_wraps as stay at home requirement
levels.

------------------------------------------------------------------------

I wanted to be able to start the graph closer to December 2020 (when
people started getting vaccinated) as this would reduce unnecessary
white space in the plot

------------------------------------------------------------------------

``` r
ggplot(daily_6) +
  geom_col(aes(x = state,y = sum(tests,na.rm=T)), fill="steelblue") +
  ggtitle("Number of Tests Performed from 2020-01-26 to 2022-01-25 per State") +
  labs(x="State", y = "Total Number of Tests Performed") +
  theme_minimal()
```

<img src="figures/unnamed-chunk-31-1.svg" style="display: block; margin: auto;" />

I wanted to be able to filter for a specific date, so for example I
could create a plot that showed the total amount of daily tests per
state (for a specific date).

------------------------------------------------------------------------

``` r
South_Northeast = (filter(covid, state == "Florida" | state == "Georgia" | state == "Alabama" | state == "New York" | state == "Connecticut" | state == "Massachusetts"))
ggplot(South_Northeast) +
  geom_col(aes(x = state, y = deaths/population, color = state)) + 
  labs(x = "State", y = "Deaths/Population", title = "State Death Rates Based on State Population")
```

    Warning: Removed 20 rows containing missing values (position_stack).

<img src="figures/unnamed-chunk-32-1.svg" style="display: block; margin: auto;" />

Order the states so it showed the South States together and the
Northeast states together

------------------------------------------------------------------------

``` r
ggplot(covid_6, aes(x = format(date, "%m"), y = government_response_index)) +
  geom_point(aes(color = state), size = 0.5, position = "jitter") +
  labs(x = "month")
```

    Warning: Removed 39 rows containing missing values (geom_point).

<img src="figures/unnamed-chunk-33-1.svg" style="display: block; margin: auto;" />

I tried to represent month with abbreviated month such as Jan, but the x
axis turns to be abbreviated month in alphabetical order but not the
original order.

------------------------------------------------------------------------

``` r
covid_plot <- covid %>% filter(population < 1e6 |population > 1e7) %>% filter(!is.na(confirmed))

ggplot(covid_plot) + geom_point(aes(y = confirmed/population * 100, x = date, color = population), size = 0.75) + labs(title = "Cumulative % Population With Confirmed COVID-19", subtitle = "In US States With Population Under 1 Million or Over 10 Million", x = "Date", y = "% Population", color = "Population") + scale_color_distiller(palette = "Spectral") + theme_bw()
```

<img src="figures/unnamed-chunk-34-1.svg" style="display: block; margin: auto;" />

I wanted to compare Washington, Oregon, California, New York,
Massachusetts, and Vermont and see how the trends in different regions
compared, but I don’t know how to add a column to the filtered dataset
that could add either “NW” or “NE” based on the state in that row so
that I could color the lines by region.

------------------------------------------------------------------------

``` r
CA1 <- filter(covid_6, state=="California", tests>=1000000)
ggplot(CA1, aes(x = date, y = deaths)) + geom_point(aes(color = state), size = 2) + stat_smooth(aes(group = state), color='green', geom = "line")
```

    `geom_smooth()` using method = 'loess' and formula 'y ~ x'

<img src="figures/unnamed-chunk-35-1.svg" style="display: block; margin: auto;" />

I want to choose more than one state.

------------------------------------------------------------------------

``` r
need1 <- filter(daily, vaccines > 0, state =='California')

p1 <- ggplot(need1, aes(x = date)) + geom_line(aes(y = ((cumsum(people_vaccinated)/population)*1000), color = 'red')) + geom_line(aes(y = ((deaths)), color = 'blue'), position = "jitter")+ labs(color = NULL) + scale_y_continuous(name = "Newly Deaths", limits = c(NA, NA), sec.axis = sec_axis(trans = ~. *.1, name = "vaccination rate ( %)")) 
p1 + scale_color_discrete( breaks = c("blue", "red"), label=c("newly death", "vaccine")) + theme_bw() + labs (title = "Vaccination VS. Newly Deaths") + theme(plot.title = element_text(size =15, face = "bold" ))
```

<img src="figures/unnamed-chunk-36-1.svg" style="display: block; margin: auto;" />

I want to calculate the rate of change of death initially, which (death
of date2- death of date1)/death of date 1, but I don’t know how to
calculate it in ggplot. Therefore, I choose just put newly death.

------------------------------------------------------------------------

``` r
new_data <- filter(weekly, date>='2020-12-31', state %in% c("Massachusetts","New York","Connecticut","Pennsylvania")
                   ,c4_restrictions_on_gatherings!='NA')
ggplot(new_data,aes(x=date, y=confirmed)) + 
  geom_line(aes(color=state)) + 
  facet_grid(~as.factor(c4_restrictions_on_gatherings))
```

<img src="figures/unnamed-chunk-37-1.svg" style="display: block; margin: auto;" />

I want to split the date in year, month and date to simplify the x-axis
of the graph.

I would like to be able to single out a single value by color. For
example, in one of the NYT visualizations, there were multiple lines for
each state, but the line for Massachusetts was green while all the
others were grey.

# other

I wanted to create a facet grid following the instruction on Week 2
slides, but maybe because of the size of the data, they would run a very
long time without actually providing me a plot.

I wish I knew how to adjust the labels to make the dates clearer, as
well as the number of cases. It would have also been great if I was able
to color coordinate the states on the plot and possibly add lines that
follow the number of cases.

``` r
y <- weekly_6$confirmed
x <- weekly_6$date
plot(x, y, main = "Confirmed Cases by Date",
  xlab = "Date",
  ylab = "Confirmed Cases")
```

<img src="figures/unnamed-chunk-38-1.svg" style="display: block; margin: auto;" />

# Data

I want to know how effective the vaccines are, but it seems like we do
not have enough informations.

I want to include the confirmed cases, but it is difficult to scale
confirmed cases, which it can fit into the data range of y axis.

------------------------------------------------------------------------

``` r
ggplot(covid, aes(x = date, y = tests)) +
  geom_point(aes(color = state), size = 1) +
  stat_smooth(aes(group = state), color='black', alpha = 0.5, geom = "line")
```

    `geom_smooth()` using method = 'loess' and formula 'y ~ x'

    Warning: Removed 307 rows containing non-finite values (stat_smooth).

    Warning: Removed 307 rows containing missing values (geom_point).

<img src="figures/unnamed-chunk-39-1.svg" style="display: block; margin: auto;" />

smooth-en out the data points to fit the best-fit curves more
