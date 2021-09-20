########################
## Summarize Services ##
########################


## Function for aggregating service utilization

## data:      data frame or tibble
## id:        ID column to group by when summarizing
## cols:      numeric columns to summarize
## date_col:  date column for determining which rows to summarize
## ref_date:  date in YMD format or date column to use in datediff with date_col
## range_min: minimum number of days in date range to summarize
## range_max: maximum number of days in date range to summarize
## breaks:    optional argument specifying lower bound for breaks; creates categorical columns if specified
## den:       optional argument specifying denominator to divide summed services by
## mult:      multiplier for rate per X days if den is specified

summarize_services <- function(data, id = NULL, cols = NULL, date_col = NULL,
                               ref_date, range_min = -365, range_max = -1,
                               breaks = NULL, den = NULL, mult = 1) {
  require(dplyr)
  require(lubridate)
  require(purrr)
  require(tidyr)
  
  
  
  ## Tests ##
  
  # Test whether id to column is null
  if(is.null(id)) {stop("id must be specified")}
  
  # Columns to summarize (defaults to all columns in data)
  cols <- if(is.null(cols)) {colnames(data)} else {cols}
  
  # Test whether date_col is null
  if(is.null(date_col)) {stop("date_col must be specified")}
  
  # Test whether selected columns are numeric
  test_numeric <- data %>%
    dplyr::select(cols) %>%
    map(is.numeric)
  
  if (any(!unlist(test_numeric))) {stop("Not all columns are numeric")}
  
  
  ## Prepare data for summarizing ##
  
  # Create object for summarizing data
  sum_data <- data
  
  # Update data with ref_date
  if (is.character(ref_date)) {
    names(sum_data)[names(sum_data) == ref_date] <- "ref_date" #if column name supplied, then rename column
    } else {sum_data$ref_date <- ymd(ref_date)} #else add ref_date as column

  # Rename id, date_col, and den columns for manipulation below...
  names(sum_data)[names(sum_data) == id] <- "id"
  names(sum_data)[names(sum_data) == date_col] <- "date_col"
  if (!is.null(den)) {names(sum_data)[names(sum_data) == den] <- "den"}
  
  
  
  ## Summarize data ##
  
  if (is.null(den)) {
    # Sum numeric columns for each id
    sum_data <- sum_data %>%
      mutate(date_diff = date_col - ref_date) %>% #date difference for filtering
      filter(date_diff >= range_min, date_diff <= range_max) %>% #only rows in date range
      dplyr::select(id, cols) %>%
      group_by(id) %>%
      summarize(across(everything(), sum))
  } else {
    # Function to sum and divide
    sum_divide <- function(x, d) {
      if (max(d) == 0) { 0 } else { sum(x)/max(d) * mult } #den should be the same for each id; max returns scalar value 
    }
    
    # Sum numeric columns and divide by denominator for each id
    sum_data <- sum_data %>%
      mutate(date_diff = date_col - ref_date) %>% #date difference for filtering
      filter(date_diff >= range_min, date_diff <= range_max) %>% #only rows in date range
      dplyr::select(id, den, cols) %>%
      group_by(id) %>%
      summarize(across(cols, ~sum_divide(.x, den)))
  }
  
  # All unique ids
  out <- tibble(unique(data[,id]))
  
  # Rename column for joining
  names(out)[1] <- "id"
  
  # Join ids and summarized data
  out <- out %>%
    left_join(sum_data, by = "id") %>%
    mutate(across(everything(), ~replace_na(.x, 0)))

  # Output
  if (is.null(breaks)) {
    
    # Order output by id column
    out <- out %>% arrange(id)
    
    # Rename id column to original name
    names(out)[names(out) == "id"] <- id
    
    # Return output
    return(out) 
    
    } else {
      
      # Test if breaks is numeric
      if (!is.numeric(breaks)) {stop("breaks must be numeric vector")}
      
      # Ensure breaks contains zero and 10000, sorted in ascending order
      breaks <- sort(unique(c(0, 10000, breaks)))
      
      # Labels for breaks
      labels <- c()
      for (i in 1:(length(breaks) - 1)) {
        labels[i] <- if (breaks[i] == breaks[i + 1] - 1) {
          breaks[i] } else {
            if (breaks[i + 1] < 10000) {
              paste(breaks[i], breaks[i + 1] - 1, sep = "-") } else {
                paste0(breaks[i], "+")
              }
          }
      }
      
      # Create categorical columns according to specified breaks
      out <- out %>%
        group_by(id) %>%
        mutate(across(where(is.numeric), 
                      list(cat = ~factor(
                        cut(.x, breaks = breaks, labels = labels, right = FALSE), 
                        levels = labels)))) %>%
        arrange(id)
      
      # Rename id column to original name
      names(out)[names(out) == "id"] <- id
      
      # Return output
      return(out) 
      
  }
}


## Example ##

# library(dplyr)
# 
# # Test Data
# set.seed(1088)
# appts <- tibble(ClientId = sample(1:50, 1000, replace = TRUE),
#                 CompletedAppointment = 1,
#                 OtherNumericColumn = sample(1:5, 1000, replace = TRUE),
#                 AppointmentType = sample(c("PC", "MH", "SUDS"), 1000, replace = TRUE),
#                 AppointmentDate = sample(seq(as.Date('2020/01/01'), as.Date('2020/12/31'), by="day"), 1000, replace = TRUE)) %>%
#   arrange(AppointmentDate, ClientId)
# 
# enroll <- tibble(ClientId = 1:50,
#                  StartDate = sample(seq(as.Date('2020/07/01'), as.Date('2020/10/31'), by="day"), 50, replace = TRUE)) %>%
#   mutate(Denominator = as.numeric(as.character(StartDate - ymd(20200701))))
# 
# data <- appts %>%
#   inner_join(enroll, by = "ClientId")
# 
# # Summarize data with function
# summarized_data <- data %>%
#   summarize_services(id = "ClientId",
#                      cols = c("CompletedAppointment", "OtherNumericColumn"),
#                      date_col = "AppointmentDate",
#                      ref_date = "StartDate",
#                      range_min = -30,
#                      range_max = -1,
#                      breaks = c(1,3,5,10),
#                      den = "Denominator")