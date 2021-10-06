HW Week4
================
Isabella Xue

## Github link

`https://github.com/isabellaxue/STAT433.git`

## Introduction

I focus on **airport**, **wind speed**, **airline** to find the best
ways to avoid delays. Here are the findings:

1.  Average delay time by hour vary slightly in different airport:

<!-- end list -->

1)  at **5, 7, 6** o’clock, flights are least likely to delay at EWR
    airport.
2)  at **7, 6, 9** o’clock, flights are least likely to delay at LGA
    airport.
3)  at **7, 5, 8** o’clock, flights are least likely to delay at JFK
    airport.

<!-- end list -->

2.  The combination of strong wind (high wind speed) + late hours is
    related to great increase in delay. It is highly recommended to fly
    early if you know it is going to be windy.

3.  The four airlines *SkyWest Airlines, AirTran Airways, Mesa Airlines,
    Frontier Airlines* that have greater delay overall (by month) have
    some pattern when looking specifically by hour:

<!-- end list -->

1)  with AirTran Airways, passenger should avoid flying around **17,
    18** o’clock
2)  with Frontier Airline, passenger should avoid flying around **17**
    o’clock
3)  with Mesa Airlines, passenger should avoid flying around **10**
    o’clock
4)  with SkyWest Airlines, passenger should avoid flying around **16,
    11** o’clock

## Airport

For visualization purpose, I first made 3 separate table containing the
average delay from 3 airports by hour.

``` r
ewr_hour <- flights %>% 
  filter(origin == "EWR") %>%
  group_by(hour) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  mutate(hour = as.character(hour)) %>%
  arrange(arr_delay)

lga_hour <- flights %>% 
  filter(origin == "LGA") %>%
  group_by(hour) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(arr_delay)

jfk_hour <- flights %>% 
  filter(origin == "JFK") %>%
  group_by(hour) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(arr_delay) 

ewr_hour
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["hour"],"name":[1],"type":["chr"],"align":["left"]},{"label":["arr_delay"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"5","2":"-5.7525309"},{"1":"7","2":"-3.9632672"},{"1":"6","2":"-3.2705893"},{"1":"9","2":"-0.3731793"},{"1":"10","2":"0.8048855"},{"1":"11","2":"1.0216858"},{"1":"8","2":"1.1956187"},{"1":"12","2":"4.6963102"},{"1":"13","2":"8.1406621"},{"1":"14","2":"11.9566116"},{"1":"23","2":"13.5652174"},{"1":"15","2":"16.9063276"},{"1":"16","2":"18.1962963"},{"1":"21","2":"19.4390417"},{"1":"20","2":"19.6105030"},{"1":"18","2":"19.6590212"},{"1":"17","2":"20.1071002"},{"1":"22","2":"20.2459016"},{"1":"19","2":"24.9100298"},{"1":"1","2":"NaN"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
lga_hour
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["hour"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["arr_delay"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"7","2":"-5.666810"},{"1":"6","2":"-4.444511"},{"1":"9","2":"-3.386977"},{"1":"5","2":"-2.607843"},{"1":"8","2":"-1.928958"},{"1":"10","2":"1.442692"},{"1":"11","2":"1.678240"},{"1":"12","2":"3.672618"},{"1":"13","2":"6.568413"},{"1":"14","2":"8.825467"},{"1":"15","2":"9.753033"},{"1":"16","2":"13.869483"},{"1":"19","2":"15.135564"},{"1":"20","2":"15.166307"},{"1":"18","2":"15.182697"},{"1":"21","2":"15.271781"},{"1":"22","2":"15.910995"},{"1":"17","2":"16.802303"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
jfk_hour
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["hour"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["arr_delay"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"7","2":"-6.5826387"},{"1":"5","2":"-4.5557047"},{"1":"8","2":"-2.5478073"},{"1":"6","2":"-2.1570181"},{"1":"9","2":"-0.1729147"},{"1":"10","2":"0.6151515"},{"1":"12","2":"1.5926521"},{"1":"11","2":"1.7222600"},{"1":"13","2":"3.1543576"},{"1":"16","2":"6.7858756"},{"1":"14","2":"7.0764007"},{"1":"15","2":"9.6104403"},{"1":"18","2":"10.1305138"},{"1":"17","2":"11.4454737"},{"1":"23","2":"11.7144259"},{"1":"19","2":"12.1884335"},{"1":"20","2":"14.2102310"},{"1":"22","2":"15.8586297"},{"1":"21","2":"19.7196124"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

Then, I created bar graph showing average delay at each airport.

``` r
ewr_plot <- ggplot(ewr_hour, aes(x=reorder(hour, arr_delay), y=arr_delay)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = "Hour in a day", y = "Average delay in min",
       title = "The average departure delay at EWR airport by hour") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()

lga_plot <- ggplot(lga_hour, aes(x=reorder(hour, arr_delay), y=arr_delay)) +
  geom_bar(stat="identity", fill="seagreen4") +
  labs(x = "Hour in a day", y = "Average delay in min",
       title = "The average departure delay at LGA airport by hour") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()

jfk_plot <- ggplot(jfk_hour, aes(x=reorder(hour, arr_delay), y=arr_delay)) +
  geom_bar(stat="identity", fill="sienna4") +
  labs(x = "Hour in a day", y = "Average delay in min",
       title = "The average departure delay at JFK airport by hour") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()

plot_grid(ewr_plot, lga_plot, jfk_plot, nrow = 3, ncol =1)
```

    ## Warning: Removed 1 rows containing missing values (position_stack).

![](HW_Week4_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

It’s very useful to look at each airport separately to avoid flight
delay. After grouping by airport, I found the average departure delay by
hour and sort them in ascending order to show in plots. It is shown in
the plots that

1)  at **5, 7, 6** o’clock, flights are least likely to delay at EWR
    airport.
2)  at **7, 6, 9** o’clock, flights are least likely to delay at LGA
    airport.
3)  at **7, 5, 8** o’clock, flights are least likely to delay at JFK
    airport.

## Weather

I chose to show whether wind speed has influence on the average delay by
hour. I used left\_join to combine flights and weather dataframe. I
defined “strong wind” as the variable wind\_speed bigger than 30 mph
(based on information I got from internet). Then I created bar graph by
studying average delay separately when strong wind is present or not.

``` r
flight_weather <- flights %>% 
  group_by(year, month, day, hour) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  left_join(weather, by = c("year", "month", "day", "hour")) %>%
  mutate(strong_wind = ifelse((wind_speed > 30), 1, 0)) %>%
  group_by(hour, strong_wind) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  filter(!is.na(strong_wind), !is.na(arr_delay)) %>%
  mutate(strong_wind = as.factor(strong_wind)) 
```

    ## `summarise()` has grouped output by 'year', 'month', 'day'. You can override using the `.groups` argument.

    ## `summarise()` has grouped output by 'hour'. You can override using the `.groups` argument.

``` r
flight_weather
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["hour"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["strong_wind"],"name":[2],"type":["fct"],"align":["left"]},{"label":["arr_delay"],"name":[3],"type":["dbl"],"align":["right"]}],"data":[{"1":"5","2":"0","3":"-5.015300"},{"1":"5","2":"1","3":"2.000000"},{"1":"6","2":"0","3":"-3.510089"},{"1":"6","2":"1","3":"13.901408"},{"1":"7","2":"0","3":"-5.571235"},{"1":"7","2":"1","3":"12.343750"},{"1":"8","2":"0","3":"-1.171915"},{"1":"8","2":"1","3":"27.131088"},{"1":"9","2":"0","3":"-1.147194"},{"1":"9","2":"1","3":"4.383598"},{"1":"10","2":"0","3":"1.680770"},{"1":"10","2":"1","3":"6.592329"},{"1":"11","2":"0","3":"2.151090"},{"1":"11","2":"1","3":"5.096057"},{"1":"12","2":"0","3":"3.699909"},{"1":"12","2":"1","3":"11.351966"},{"1":"13","2":"0","3":"6.911555"},{"1":"13","2":"1","3":"10.661376"},{"1":"14","2":"0","3":"9.775246"},{"1":"14","2":"1","3":"13.872247"},{"1":"15","2":"0","3":"12.807506"},{"1":"15","2":"1","3":"15.473504"},{"1":"16","2":"0","3":"13.986326"},{"1":"16","2":"1","3":"55.910943"},{"1":"17","2":"0","3":"16.392073"},{"1":"17","2":"1","3":"26.615836"},{"1":"18","2":"0","3":"15.247420"},{"1":"18","2":"1","3":"13.247222"},{"1":"19","2":"0","3":"17.409719"},{"1":"19","2":"1","3":"119.283525"},{"1":"20","2":"0","3":"16.973560"},{"1":"20","2":"1","3":"50.727096"},{"1":"21","2":"0","3":"17.665481"},{"1":"21","2":"1","3":"62.043501"},{"1":"22","2":"0","3":"15.746145"},{"1":"22","2":"1","3":"34.185873"},{"1":"23","2":"0","3":"9.211189"},{"1":"23","2":"1","3":"8.583333"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
ggplot(flight_weather, aes(x=hour, y=arr_delay, fill = strong_wind)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_discrete(name = "Strong wind?", labels = c("No", "Yes")) +
  labs(x = "Hour in a day", y = "Average delay in min") +
  ggtitle(label = "The average departure delay by hour",
              subtitle = "Strong wind is defined as bigger than 30 mph") +
  theme(plot.title = element_text(hjust = 0.5, size = 13, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5)) +
  theme_minimal()
```

![](HW_Week4_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

From the graph, we can see that strong wind does have a effect on the
average delay time: the average delay is so much higher in the later
hours when strong wind is present. There is one takeaway from this:
strong wind + late hours are terrible\! Fly early if you know it is
going to be windy.

## Airline

First, I want to know what airlines have the longest average delay
monthly to get an big picture of each airline.

``` r
flights %>% 
  left_join(airlines, by = "carrier") %>%
  group_by(year, month, name) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>% 
  mutate(month = as.factor(month)) %>%
  arrange(desc(arr_delay)) %>%
  ggplot(aes(x=month, y=arr_delay, color = name)) +
  geom_point() +
  labs(y= "average arrival delay", title = "Average delay every month by airline")
```

    ## `summarise()` has grouped output by 'year', 'month'. You can override using the `.groups` argument.

![](HW_Week4_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

From the graph, we can see that **SkyWest Airlines, AirTran Airways,
Mesa Airlines, Frontier Airlines** stand out as having greater average
delay every month. Therefore, I want to see at what hours passenger
should avoid to fly with these airlines.

``` r
airline_delay <- flights %>% 
  left_join(airlines, by = "carrier") %>%
  filter(name == c("Mesa Airlines Inc.", "SkyWest Airlines Inc.", "AirTran Airways Corporation", "Frontier Airlines Inc.")) %>%
  group_by(hour, name) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  mutate(hour = as.factor(hour))
```

    ## `summarise()` has grouped output by 'hour'. You can override using the `.groups` argument.

``` r
airline_delay
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["hour"],"name":[1],"type":["fct"],"align":["left"]},{"label":["name"],"name":[2],"type":["chr"],"align":["left"]},{"label":["arr_delay"],"name":[3],"type":["dbl"],"align":["right"]}],"data":[{"1":"6","2":"AirTran Airways Corporation","3":"13.6164384"},{"1":"7","2":"AirTran Airways Corporation","3":"-0.8888889"},{"1":"7","2":"Frontier Airlines Inc.","3":"-22.0000000"},{"1":"8","2":"AirTran Airways Corporation","3":"5.7352941"},{"1":"8","2":"Frontier Airlines Inc.","3":"6.8378378"},{"1":"9","2":"AirTran Airways Corporation","3":"10.1250000"},{"1":"10","2":"AirTran Airways Corporation","3":"9.1960784"},{"1":"10","2":"Mesa Airlines Inc.","3":"166.0000000"},{"1":"11","2":"AirTran Airways Corporation","3":"9.1967213"},{"1":"11","2":"Mesa Airlines Inc.","3":"-10.1666667"},{"1":"11","2":"SkyWest Airlines Inc.","3":"107.0000000"},{"1":"13","2":"AirTran Airways Corporation","3":"21.9718310"},{"1":"13","2":"Frontier Airlines Inc.","3":"6.6428571"},{"1":"14","2":"AirTran Airways Corporation","3":"18.9848485"},{"1":"14","2":"Mesa Airlines Inc.","3":"-9.6000000"},{"1":"14","2":"SkyWest Airlines Inc.","3":"1.5000000"},{"1":"15","2":"Mesa Airlines Inc.","3":"22.0000000"},{"1":"16","2":"AirTran Airways Corporation","3":"19.0425532"},{"1":"16","2":"Mesa Airlines Inc.","3":"15.4782609"},{"1":"16","2":"SkyWest Airlines Inc.","3":"157.0000000"},{"1":"17","2":"AirTran Airways Corporation","3":"41.0576923"},{"1":"17","2":"Frontier Airlines Inc.","3":"19.4819277"},{"1":"17","2":"Mesa Airlines Inc.","3":"22.8529412"},{"1":"18","2":"AirTran Airways Corporation","3":"53.5116279"},{"1":"18","2":"SkyWest Airlines Inc.","3":"-2.2857143"},{"1":"19","2":"AirTran Airways Corporation","3":"14.5000000"},{"1":"19","2":"Mesa Airlines Inc.","3":"-20.0000000"},{"1":"20","2":"AirTran Airways Corporation","3":"24.8409091"},{"1":"20","2":"Mesa Airlines Inc.","3":"-36.0000000"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
ggplot(airline_delay, aes(x=hour, y=arr_delay)) +
  geom_point() +
  labs(x = "Hour in a day", y = "Average delay in min",
       title = "Average delay by hour by the top 4 'slowest' airlines") +
  theme(plot.title = element_text(hjust = 0.5, size = 13, face = "bold"))+
  theme_minimal() +
  facet_wrap(~name)
```

![](HW_Week4_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

The graphs above give an abstract idea that

1)  with AirTran Airways, passenger should avoid flying around **17,
    18** o’clock
2)  with Frontier Airline, passenger should avoid flying around **17**
    o’clock
3)  with Mesa Airlines, passenger should avoid flying around **10**
    o’clock
4)  with SkyWest Airlines, passenger should avoid flying around **16,
    11** o’clock
