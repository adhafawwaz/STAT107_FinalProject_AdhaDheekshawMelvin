metro <- read.csv("metro_ridership.csv")
head(metro)

# 1. Load the required package
library(ggplot2)

# 2. Read the data (if not already loaded)
metro <- read.csv("metro_ridership.csv")

# 3. Create the date column so line plots work properly
metro$date <- as.Date(paste(metro$year, metro$month, "01", sep = "-"))

# 4. Create the graph ~ looking at ridership over time for each line
ggplot(metro, aes(x = date, y = est_wkday_ridership, color = line_name)) +
  geom_line(linewidth = 0.8) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Estimated Weekday Ridership Over Time",
    x = "Date",
    y = "Estimated Weekday Ridership",
    color = "Metro Line"
  ) +
  theme_minimal()

library(ggplot2)

# Adding dates when the new Metro projects were opened
# October 2022: The K line is opened
# June 2023: Regional Connecter opening (Re-links of A, E, and L lines)
events <- data.frame(
  date = as.Date(c("2022-10-01", "2023-06-01")),
  label = c("K Line Opens", "Regional Connector Opens")
)

ggplot(metro, aes(x = date, y = est_wkday_ridership, color = line_name)) +
  geom_line(linewidth = 0.8) +
  geom_vline(data = events, aes(xintercept = date), linetype = "dotted", linewidth = 0.8) +
  geom_text(data = events, aes(x = date, y = Inf, label = label), 
            vjust = 1.5, hjust = -0.05, inherit.aes = FALSE, size = 3.5) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Weekday Ridership Trends & Major Metro System Developments",
    x = "Date",
    y = "Estimated Weekday Ridership",
    color = "Metro Line"
  ) +
  theme_minimal()

# Compare years Side by Side
ggplot(metro, aes(x = month, y = est_wkday_ridership, color = as.factor(year))) +
  geom_line(linewidth = 1) +
  facet_wrap(~ line_name, scales = "free_y") +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Yearly Trajectory Comparison by Month",
    x = "Month",
    y = "Estimated Weekday Ridership",
    color = "Year"
  ) +
  theme_bw()
