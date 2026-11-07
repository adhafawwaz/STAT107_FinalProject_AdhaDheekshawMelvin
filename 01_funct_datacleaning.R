
# Adding a function to make the date column using the year & month
# Input: dataset with year and month columns
# Output: same dataset with a new date column

add_date <- function(data) {
  data$date <- as.Date(paste(data$year, data$month, "01", sep = "-"))
  return(data)
}


# Checking how many zero values are in the ridership data
# Input: the metro dataset
# Output: the number of zeros for each type of ridership

check_zeros <- function(data) {

  # the weekdays
  print(sum(data$est_wkday_ridership == 0))

  # Saturdays
  print(sum(data$est_sat_ridership == 0))

  # Sundays
  print(sum(data$est_sun_ridership == 0))

}
