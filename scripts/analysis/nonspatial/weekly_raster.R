hou_hourly <- hou %>%
 group_by(Date, hour = hour(Date), `Offense Type`) %>%
 summarize(offense_count = sum(offense_count, na.rm = TRUE)) %>%
 ungroup()

x_labs <- c(paste0(c(12, 1:11), "am"),
            paste0(c(12, 1:11), "pm"))

# Average of all crimes per day per hour ----------------------------------

hou_hourly %>%
 mutate(week_day = wday(Date, label = TRUE)) %>%
 group_by(hour, week_day) %>%
 summarize(offense_count = mean(offense_count, na.rm = TRUE)) %>%
 ungroup() %>%
 ggplot(aes(hour, week_day, fill = offense_count)) +
 geom_tile() +
 scale_fill_viridis_c() +
 scale_x_continuous(expand = expand_scale()) +
 scale_y_discrete(expand = expand_scale())



# A specific crime --------------------------------------------------------

type <- "Aggravated Assaults"

hou_hourly %>%
 filter(`Offense Type` == type) %>%
 mutate(week_day = wday(Date, label = TRUE, week_start = 1)) %>%
 group_by(hour, week_day) %>%
 summarize(offense_count = mean(offense_count, na.rm = TRUE)) %>%
 ungroup() %>%
 ggplot(aes(hour, fct_rev(week_day), fill = offense_count)) +
 geom_tile() +
 scale_fill_viridis_c() +
 scale_x_continuous(expand = expand_scale()) +
 scale_y_discrete(expand = expand_scale())


# FACETTED ----------------------------------------------------------------

## BY `Offense Type`

hou_hourly %>%
 # filter(`Offense Type` == type) %>%
 mutate(week_day = wday(Date, label = TRUE, week_start = 1)) %>%
 group_by(`Offense Type`, hour, week_day) %>%
 summarize(offense_count = mean(offense_count, na.rm = TRUE)) %>%
 mutate(offense_count = index(offense_count)) %>%
 ungroup() %>%
 ggplot(aes(hour, fct_rev(week_day), fill = offense_count)) +
 geom_tile() +
 scale_fill_viridis_c() +
 scale_x_continuous(expand = expand_scale()) +
 scale_y_discrete(expand = expand_scale()) +
 facet_wrap(~`Offense Type`)

## BY YEAR PER `Offense Type`

type <- "Other Thefts"

hou_hourly %>%
 filter(`Offense Type` == type) %>%
 mutate(week_day = wday(Date, label = TRUE, week_start = 1)) %>%
 group_by(hour, week_day, year = year(Date), `Offense Type`) %>%
 summarize(offense_count = mean(offense_count, na.rm = TRUE)) %>%
 # mutate(off_index = index(offense_count)) %>%
 ungroup() %>%
 ggplot(aes(hour, fct_rev(week_day), fill = offense_count)) +
 geom_tile() +
 scale_fill_viridis_c() +
 scale_x_continuous(expand = expand_scale(), labels = x_labs[seq(1, 24, 5)],
                    breaks = seq(0, 23, 5)) +
 scale_y_discrete(expand = expand_scale()) +
 facet_grid(. ~ year)
