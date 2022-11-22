library(tidyverse)
library(dplyr)
library(ggplot2)
# The functions might be useful for A4
source("C:/Users/steph/Documents/info201/assignments/a4-stephliu39/source/a4-helpers.R")
incarceration_data <- read.csv("C:/Users/steph/Documents/info201/data/incarceration_trends.csv")
View(incarceration_data)
## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#
female_population <- function() {
  count <- incarceration_data %>%
    filter(female_jail_pop == female_jail_pop) %>%
    summarise(count = n()) %>%
    pull(count)
  return(count)
}
female_population()

male_population <- function() {
  count <- incarceration_data %>%
    filter(male_jail_pop == male_jail_pop) %>%
    summarise(count = n()) %>%
    pull(count)
  return(count)
}
male_population()

difference_population <- function() {
  difference <- abs(male_population() - female_population())
  return(difference)
}
difference_population()

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
get_year_jail_pop <- function() {
  # TODO: Implement this function 
  df <- data.frame(Year = incarceration_data$year, Total_Jail_Population = 
                     incarceration_data$total_jail_pop)

return(df)   
}
View(get_year_jail_pop())
# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  # TODO: Implement this function 
  chart <- ggplot(get_year_jail_pop()) + 
    geom_col(mapping = aes(x = Year, y = Total_Jail_Population)) + 
    ggtitle("Increase of Jail Population in U.S. (1970-2018)") +
    labs(caption = "Fig 3.1: This chart shows that incarceration rates have generally increased over time.")
  
  return(chart)   
} 
plot(plot_jail_pop_for_us())

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
get_jail_pop_by_states <- function(states) {
  df <- incarceration_data %>%
    filter(state %in% states) %>%
    group_by(state, year) %>%
    summarise(Total_Jail_Population = sum(total_jail_pop, na.rm = TRUE))
  return(df)
}
View(get_jail_pop_by_states(c("WA", "OR", "CA")))

plot_jail_pop_by_states <- function(states) {
  chart2 <- ggplot(data = get_jail_pop_by_states(states), 
                   aes(x = year, 
                       y = Total_Jail_Population,
                       group = state)) + 
    geom_line(aes(color = state)) +
    ggtitle("Growth of Prison Population by State") + 
    labs(caption = "Fig 4.1: This chart shows the growth in prison population in various states.")
  return(chart2)
}
plot_jail_pop_by_states(c("WA", "OR", "CA"))

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
gender <- function() {
  df <- data.frame(Female_Jail_Population = incarceration_data$female_jail_pop, 
                   Male_Jail_Population = incarceration_data$male_jail_pop)
  return(df)
}
View(gender())

gender_plot <- function() {
  scatterplot <- ggplot(gender(), aes(x = Female_Jail_Population, y = Male_Jail_Population)) +
    geom_point() + 
    ggtitle("Male vs. Female Jail Population") + 
    labs(caption = "Fig 5.1: This chart shows the similarities and differences in male and female jail populations.")
    
  return(scatterplot)
}
gender_plot()

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
dff <- data.frame("Year" = incarceration_data$year,
                  "State" = incarceration_data$state,
                  "Female_pop" = incarceration_data$female_pop_15to64,
                  "Male_pop" = incarceration_data$male_pop_15to64,
                  "Female_prison" = incarceration_data$female_jail_pop,
                  "Male_prison" = incarceration_data$male_jail_pop)
#View(dff)

ratio_dff <- dff %>%
  summarize(state = State, f_ratio = Female_prison/Female_pop, 
            m_ratio = Male_prison/Male_pop)
#View(ratio_dff)


# Map for female prison population
female_pop_state <- ratio_dff %>%
  group_by(state) %>%
  summarise(female_pop_per_state = mean(f_ratio, na.rm = TRUE))
female_pop_state[is.na(female_pop_state)] = 0
female_pop_state[sapply(female_pop_state, is.infinite)] <- 0


female_pop_state_new <- female_pop_state %>%
  mutate(full_name = tolower(state.name[match(female_pop_state$state, state.abb)])) %>%
  rename(abbr = state) %>%
  rename(state = full_name)


female_state_shape <- map_data("state") %>%
  rename(state = region) %>%
  left_join(female_pop_state_new, by = "state")


plot_female <- function() {
  ggplot(female_state_shape) +
    geom_polygon(
      mapping = aes(x = long, y = lat, group = group, fill = female_pop_per_state, 
                    color = "white") 
    ) + 
    coord_map() +
    scale_fill_continuous(low = "#D8D7D7", high = "#F0A8A8", limits = c(0, 1)) +
    labs(fill = "Ratio",
         caption = "Fig 6.1 ratio of female prison population by state (1970-2018)",
         x = "",
         y = "") +
    theme(plot.caption = element_text(hjust = 0.5)) + 
    ggtitle("Distribution of Ratio of Female Prison Population by State")
}
plot_female()



# Map for male prison population
male_pop_state <- ratio_dff %>%
  group_by(state) %>%
  summarise(male_pop_per_state = mean(m_ratio, na.rm = TRUE))
male_pop_state[is.na(male_pop_state)] = 0
male_pop_state[sapply(male_pop_state, is.infinite)] <- 0


male_pop_state_new <- male_pop_state %>%
  mutate(full_name = tolower(state.name[match(male_pop_state$state, state.abb)])) %>%
  rename(abbr = state) %>%
  rename(state = full_name)


male_state_shape <- map_data("state") %>%
  rename(state = region) %>%
  left_join(male_pop_state_new, by = "state")


plot_male <- function() {
  ggplot(male_state_shape) +
    geom_polygon(
      mapping = aes(x = long, y = lat, group = group, fill = male_pop_per_state, 
                    color = "white") 
    ) + 
    coord_map() +
    scale_fill_continuous(low = "#D8D7D7", high = "#A9D5F9", limits = c(0, 1)) +
    labs(fill = "Ratio",
         caption = "Fig 6.2 ratio of male prison population by state (1970-2018)",
         x = "",
         y = "") +
    theme(plot.caption = element_text(hjust = 0.5)) + 
    ggtitle("Distribution of Ratio of Male Prison Population by State")
}
plot_male()

