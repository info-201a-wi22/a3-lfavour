# Assignment 3: Incarceration (INFO 201: Winter 2022)
library("dplyr")
library("ggplot2")

data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# 1. This section calculates values for the summary table
# 1a. pop_year is a function that returns a data frame with the total prison
# population across all counties in a given year for each race. I chose to
# look at 2000 and 2015.

pop_year <- function(input_year) {
  return(data %>%
    filter(year == input_year) %>%
    select(year, aapi_prison_pop, black_prison_pop, latinx_prison_pop, native_prison_pop, other_race_prison_pop, white_prison_pop) %>%
    summarize(year, sum_aapi = sum(aapi_prison_pop, na.rm = TRUE), sum_black = sum(black_prison_pop, na.rm = TRUE),
              sum_latinx = sum(latinx_prison_pop, na.rm = TRUE), sum_native = sum(native_prison_pop, na.rm = TRUE),
              sum_other = sum(other_race_prison_pop, na.rm = TRUE), sum_white = sum(white_prison_pop, na.rm = TRUE)) %>%
    filter(row_number() == 1))
}

pop_2015 <- pop_year("2015")
pop_2000 <- pop_year("2000")

# 1b. This section calculates the percent increase in prison populations for
# each race from 2000 to 2015.
change_aapi <- ((pop_2015$sum_aapi - pop_2000$sum_aapi) / pop_2000$sum_aapi) * 100

change_black <- ((pop_2015$sum_black - pop_2000$sum_black) / pop_2000$sum_black) * 100

change_latinx <- ((pop_2015$sum_latinx - pop_2000$sum_latinx) / pop_2000$sum_latinx) * 100

change_native <- ((pop_2015$sum_native - pop_2000$sum_native) / pop_2000$sum_native) * 100

change_other <- ((pop_2015$sum_other - pop_2000$sum_other) / pop_2000$sum_other) * 100

change_white <- ((pop_2015$sum_white - pop_2000$sum_white) / pop_2000$sum_white) * 100

# 2. This section creates a scatter plot that graphs the total prison population
# from 2000 to 2015 and is colored by race. Creates a data frame for each race
# and joins them in order to graph them.
pop_2000_2015_aapi <- data %>%
  filter(year >= "2000" & year <= "2015") %>%
  select(year, aapi_prison_pop) %>%
  group_by(year) %>%
  summarize(sum = sum(aapi_prison_pop, na.rm = TRUE)) %>%
  mutate(race = "aapi")

pop_2000_2015_black <- data %>%
  filter(year >= "2000" & year <= "2015") %>%
  select(year, black_prison_pop) %>%
  group_by(year) %>%
  summarize(sum = sum(black_prison_pop, na.rm = TRUE)) %>%
  mutate(race = "black")

pop_2000_2015_latinx <- data %>%
  filter(year >= "2000" & year <= "2015") %>%
  select(year, latinx_prison_pop) %>%
  group_by(year) %>%
  summarize(sum = sum(latinx_prison_pop, na.rm = TRUE)) %>%
  mutate(race = "latinx")

pop_2000_2015_native <- data %>%
  filter(year >= "2000" & year <= "2015") %>%
  select(year, native_prison_pop) %>%
  group_by(year) %>%
  summarize(sum = sum(native_prison_pop, na.rm = TRUE)) %>%
  mutate(race = "native")

pop_2000_2015_other <- data %>%
  filter(year >= "2000" & year <= "2015") %>%
  select(year, other_race_prison_pop) %>%
  group_by(year) %>%
  summarize(sum = sum(other_race_prison_pop, na.rm = TRUE)) %>%
  mutate(race = "other")

pop_2000_2015_white <- data %>%
  filter(year >= "2000" & year <= "2015") %>%
  select(year, white_prison_pop) %>%
  group_by(year) %>%
  summarize(sum = sum(white_prison_pop, na.rm = TRUE)) %>%
  mutate(race = "white")

pop_2000_2015 <- pop_2000_2015_aapi %>%
  full_join(pop_2000_2015_black) %>%
  full_join(pop_2000_2015_latinx) %>%
  full_join(pop_2000_2015_native) %>%
  full_join(pop_2000_2015_other) %>%
  full_join(pop_2000_2015_white)

time_plot <- ggplot(pop_2000_2015, aes(x = year, y = sum, color = race)) +
  geom_point(na.rm = TRUE) +
  stat_smooth(method = "lm", se = FALSE) + 
  labs(title = "Changes in Prison Population From 2000 to 2015 by Race",
       x = "Year", y = "Total Prison Population")

# 3. Creates a scatter plot comparing the percentage of prisoners who are black
# versus the percentage of the total population that is black. Uses total
# population from 15 to 64 because that was the only non-prison category that
# also had columns for each race category.
black_pop_stats <- data %>%
  filter(year >= "2000" & year <= "2015") %>%
  select(year, total_pop_15to64, black_pop_15to64, total_prison_pop,
         black_prison_pop) %>%
  group_by(year) %>%
  summarize(total_pop_sum = sum(total_pop_15to64, na.rm = TRUE),
            total_black_sum = sum(black_pop_15to64, na.rm = TRUE),
            total_prison_sum = sum(total_prison_pop, na.rm = TRUE),
            black_prison_sum = sum(black_prison_pop, na.rm = TRUE)) %>%
  mutate(total_pop_percent = total_black_sum / total_pop_sum * 100) %>%
  mutate(total_prison_percent = black_prison_sum / total_prison_sum * 100)

black_pop_plot <- ggplot(black_pop_stats, aes(x = total_pop_percent, y = total_prison_percent, color = year)) +
  geom_point(na.rm = TRUE) +
  stat_smooth(method = "lm", se = FALSE) +
  labs(title = "The Percentage of Black People in the Total Population Versus
       the Prison Population", x = "Percent of Total Population", y = "Percent of Prison Population")

# 4. Creates a map of Washington that is colored by the ratio of black people
# in prison to black people in the total population.
county_fips <- readRDS("county_map_fips.rds")

county_diff <- county_fips %>%
  left_join(data, by = "fips") %>%
  filter(year == "2015") %>%
  filter(state == "WA") %>%
  filter(!is.na(black_prison_pop)) %>%
  filter(!is.na(total_prison_pop)) %>%
  filter(!is.na(black_pop_15to64)) %>%
  filter(!is.na(total_pop_15to64)) %>%
  mutate(black_ratio = (black_prison_pop / total_prison_pop) / (black_pop_15to64 / total_pop_15to64)) %>%
  select(long, lat, group, black_ratio)

wa_map <- county_fips %>%
  left_join(data, by = "fips") %>%
  filter(state == "WA") %>%
  select(long, lat, group)

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        
    axis.text = element_blank(),        
    axis.ticks = element_blank(),       
    axis.title = element_blank(),       
    plot.background = element_blank(),  
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_blank()      
  )

wa_ratio_map <- ggplot(data = county_diff) +
  geom_polygon(
    data = wa_map, 
    mapping = aes(x = long, y = lat, group = group),
    color = "black",
    size = 0.1
  ) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_ratio),
    color = "black",
    size = 0.1
  ) + 
  coord_map() +
  scale_fill_continuous(low = "#0000FF", high = "#A3A3FF") +
  labs(title = "Inflated Rate of Imprisonment of Black People In Washington", fill = "Ratio of Prison to General Population") +
  blank_theme
