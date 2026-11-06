
# Adding a function to make the date column using the year & month

add_date <- function(data) {
  data$date <- as.Date(paste(data$year, data$month, "01", sep = "-"))
  return(data)
}
