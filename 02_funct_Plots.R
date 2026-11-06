
metro <- read.csv("metro_ridership.csv")
head(metro)

# 1. Load the required package
install.packages("ggplot2")
library(ggplot2)

# 2. Read the data (if not already loaded)
metro <- read.csv("metro_ridership.csv")

# 3. Create the date column so line plots work properly
metro$date <- as.Date(paste(metro$year, metro$month, "01", sep = "-"))

# 4. Create the graph
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
install.packages("dplyr")
library(dplyr)

# 1. Calculate average 2019 baseline per line
base_2019 <- metro %>%
  filter(year == 2019) %>%
  group_by(line_name) %>%
  summarize(base_wkday = mean(est_wkday_ridership, na.rm = TRUE))

# 2. Join baseline and calculate recovery %
metro_indexed <- metro %>%
  left_join(base_2019, by = "line_name") %>%
  mutate(recovery_pct = (est_wkday_ridership / base_wkday) * 100)

# 3. Plot indexed recovery trend
ggplot(metro_indexed, aes(x = date, y = recovery_pct, color = line_name)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray40") +
  labs(
    title = "LA Metro Rail Recovery Relative to 2019 Pre-Pandemic Baseline",
    subtitle = "Dashed line (100%) represents full pre-COVID recovery",
    x = "Date",
    y = "Ridership (% of 2019 Baseline)",
    color = "Metro Line"
  ) +
  theme_minimal()

# Define major milestone dates
# October 2022: K line opening
# June 2023: Regional Connecter opening (Re-links of A, E, and L lines)
events <- data.frame(
  date = as.Date(c("2022-10-01", "2023-06-01")),
  label = c("K Line Opens", "Regional Connector Opens")
)

ggplot(metro, aes(x = date, y = est_wkday_ridership, color = line_name)) +
  geom_line(linewidth = 0.8) +
  geom_vline(data = events, aes(xintercept = date), linetype = "dotted", size = 0.8) +
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

metro <- metro %>%
  mutate(weekend_avg = (est_sat_ridership + est_sun_ridership) / 2,
         weekend_ratio = weekend_avg / est_wkday_ridership)

ggplot(metro, aes(x = date, y = weekend_ratio, color = line_name)) +
  geom_line(linewidth = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Weekend vs. Weekday Ridership Share Over Time",
    subtitle = "Higher ratios highlight strong weekend leisure/non-commute recovery",
    x = "Date",
    y = "Weekend Avg as % of Weekday Ridership",
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
