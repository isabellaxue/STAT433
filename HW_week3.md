HW Week3
================
Isabella Xue

### Github link

`https://github.com/isabellaxue/STAT433.git`

### How many flights have a missing dep\_time? What other variables are missing? What might these rows represent?

``` r
missing_dep_time <- flights %>%
  filter(is.na(dep_time))
missing_dep_time
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
  </script>

</div>

*8255* flights have a missing dep\_time. For these flights, `dep_delay`,
`arr_time`, `arr_delay`, and `air_time` are also missing. Some are also
missing `tailnum`. This might mean that these flight never departed.

### Currently dep\_time and sched\_dep\_time are convenient to look at, but hard to compute with because they’re not really continuous numbers. Convert them to a more convenient representation of number of minutes since midnight.

``` r
new_flights <- flights %>% 
  mutate(dep_time_min = dep_time%%100+(dep_time-dep_time%%100)/100*60,
         sched_dep_time_min = sched_dep_time%%100+(sched_dep_time-sched_dep_time %%100)/100*60) 

new_flights %>% select(dep_time, dep_time_min, sched_dep_time, sched_dep_time_min)
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
  </script>

</div>

The two new variables are `dep_time_min` and `sched_dep_time_min`.

### Look at the number of canceled flights per day. Is there a pattern? Is the proportion of canceled flights related to the average delay? Use multiple dyplr operations, all on one line, concluding with ggplot(aes(x= ,y=)) + geom\_point()

``` r
missing_num <- 
  flights %>%
  mutate(canceled = ifelse((is.na(arr_delay)|is.na(dep_delay)), 1, 0)) %>%
  group_by(year, month, day) %>%
  summarise(missing_num = sum(canceled)) %>%
  pull(missing_num)
overall_num <- flights %>%
  group_by(year, month, day) %>%
  summarise(overall_flights = n()) %>%
  pull(overall_flights)
prop <- missing_num/overall_num

average_delay <- flights %>%
  group_by(year, month, day) %>%
  summarise(aver_delay = mean(dep_delay, na.rm = TRUE)) %>%
  pull(aver_delay)
q3 <- cbind(data.frame(prop), data.frame(average_delay))

ggplot(q3, aes(y=prop ,x=average_delay)) + 
  geom_point() +
  labs(y = "proportion of canceled flight", x="average delay")
```

![](HW_week3_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

From the plot, we can see that there is a small pattern and they are
somehow related. As the average delay increases, proportion of canceled
flight also increases. This indicates that the whole airport is
affected.