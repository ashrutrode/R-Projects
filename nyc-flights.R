# Load the required libraries
library(dplyr)
library(nycflights13)

# Load the flights dataset
data(flights)

# View the flights dataset
View(flights)

# Get more information about the flights dataset
?flights

# View the first few rows of the flights dataset
head(flights)

# Summary statistics of the flights dataset
summary(flights)

# Example analysis using dplyr functions

# Selecting specific columns (variables)
selected_flights <- flights %>%
  select(origin, dest, carrier, dep_delay, arr_delay)
print(selected_flights, n=20)

# Adding a new column (variable) calculating total delay
mutated_flights <- selected_flights %>%
  mutate(total_delay = dep_delay + arr_delay)
print(mutated_flights, n=20)

# Filtering mutated_flights departing from JFK airport
jfk_flights <- mutated_flights %>%
  filter(origin == "JFK")
print(jfk_flights, n=20)

# Selecting distinct destinations
distinct_destinations <- jfk_flights %>%
  distinct(dest)
print(distinct_destinations, n=20)

# Grouping jfk_flights by destination airport and calculating average departure delay
avg_delay_by_dest <- jfk_flights %>%
  group_by(dest) %>%
  summarise(avg_dep_delay = mean(dep_delay, na.rm = TRUE))
print(avg_delay_by_dest, n=20)

# Arranging destinations by average departure delay in descending order
top_destinations <- avg_delay_by_dest %>%
  arrange(desc(avg_dep_delay))
print(top_destinations, n=20)

# Viewing the top 10 destinations with the highest average departure delay
head(top_destinations, 10)

# Counting and then viewing the number of flights for each destination airport
flights_count <- flights %>%
  count(dest)
flights_count

# Arrange flights_count by count of flights (n) in descending order
flights_count_arranged <- flights_count %>%
  arrange(desc(n))
flights_count_arranged

