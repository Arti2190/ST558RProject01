library(httr)
library(jsonlite)
library(tibble)
library(dplyr)



# Helper function to process API response
process_api_response <- function(api_response) {
  # Check if the response was successful
  if (api_response$status_code != 200) {
    stop("Error: API request failed with status code ", api_response$status_code)
  }
  
  # Convert raw content to character string
  content_string <- rawToChar(api_response$content)
  
  # Parse JSON data
  parsed_data <- fromJSON(content_string)
  
  # First row contains column names, the rest is the data
  column_names <- parsed_data[1, ]
  data_rows <- parsed_data[-1, ]
  
  # Convert to tibble and set column names
  tibble_data <- as_tibble(data_rows)
  colnames(tibble_data) <- column_names
  
  return(tibble_data)
}

# 2. Function to validate the year
validate_year <- function(year) {
  if (year < 2010 || year > 2022) {
    stop("Invalid year. Year must be between 2010 and 2022.")
  }
}

# 3. Function to validate and process numeric variables
process_numeric_vars <- function(numeric_vars = c("AGEP", "PWGTP")) {
  valid_numeric_vars <- c("AGEP", "GASP", "GRPIP", "JWAP", "JWDP", "JWMNP", "PWGTP")
  
  # Ensure PWGTP is always included
  if (!"PWGTP" %in% numeric_vars) {
    numeric_vars <- c(numeric_vars, "PWGTP")
  }
  
  # Validate numeric variables
  if (!all(numeric_vars %in% valid_numeric_vars)) {
    stop("Invalid numeric variables. Valid options: AGEP, GASP, GRPIP, JWAP, JWDP, JWMNP, PWGTP.")
  }
  
  # Ensure at least one numeric variable other than PWGTP
  if (!any(numeric_vars %in% valid_numeric_vars[valid_numeric_vars != "PWGTP"])) {
    stop("At least one numeric variable other than PWGTP must be returned.")
  }
  
  # Process time variables (JWAP, JWDP) into middle time value
  # Example: Convert JWAP (start time) and JWDP (end time) to average time (simplified)
  time_vars <- c("JWAP", "JWDP")
  
  numeric_data <- list()
  for (var in numeric_vars) {
    if (var %in% time_vars) {
      numeric_data[[var]] <- function(x) { mean(c(9, 17)) } 
    } else {
      numeric_data[[var]] <- function(x) { as.numeric(x) }
    }
  }
  return(numeric_vars)
}

# 4. Function to validate and process categorical variables

process_categorical_vars <- function(categorical_vars = c("SEX")) {
  valid_categorical_vars <- c("FER", "HHL", "HISPEED", "JWAP", "JWDP", "JWTRNS", "SCH", "SCHL", "SEX")
  
  # Validate categorical variables
  if (!all(categorical_vars %in% valid_categorical_vars)) {
    stop("Invalid categorical variables. Valid options: FER, HHL, HISPEED, JWAP, JWDP, JWTRNS, SCH, SCHL, SEX.")
  }
  
  return(categorical_vars)
}

## TODO
##Specify the geography level: All, Region, Division, or State (with the default of All)
##Check that the value specified by the user is one of the above values


# 5. Main function: Query the API
query_census_pums <- function(
    year = 2023,
    numeric_vars = c("GASP", "PWGTP"),
    categorical_vars = c("SEX"),
    geography_level = "All",
    geography_subset = NULL
) {
  # Validate the year
  validate_year(year)
  
  # Validate and process numeric variables
  numeric_vars <- process_numeric_vars(numeric_vars)
  
  # Validate and process categorical variables
  queryparams <- process_categorical_vars(categorical_vars)
  
  # Construct API URL (placeholder, replace with actual API endpoint)
  base_url <- "https://api.census.gov/data"
  pathparam <- "acs/acs1/pums"  # Change this to the appropriate survey type if necessary
  
  # Construct the full URL
  url <- paste0(base_url, "/", year, "/", pathparam, "?get=", 
                paste(c(numeric_vars, categorical_vars), collapse = ","),"&SCHL=24")
  
  # If a geography subset is provided, add it to the API call
  if (!is.null(geography_subset)) {
    url <- paste0(url, "&for=", geography_level, ":", geography_subset)
  }
  print(url)
  
  # API call using httr::GET
  api_response <- GET(url)
  
  # Check if the request was successful
  if (http_error(api_response)) {
    stop("API request failed: ", status_code(api_response))
  }
  
  # Process the response into a tibble
  data <- process_api_response(api_response)
  
  # Return the final data tibble
  return(data)
}
print(url)

# 6. Function for multiple years
query_multiple_years <- function(years, numeric_vars, categorical_vars, geography_level = "All", geography_subset = NULL) {
  # Validate years
  lapply(years, validate_year)
  print ("test1")
  
  # Query the API for each year and combine results
  result_list <- lapply(years, function(yr) {
    query_census_pums(
      year = yr,
      numeric_vars = numeric_vars,
      categorical_vars = categorical_vars,
      geography_level = geography_level,
      geography_subset = geography_subset
    ) %>%
      mutate(year = yr)  # Add a year column
  })
  
  print ("test2")
  print (result_list)
  
  # Combine all years into one tibble
  final_data <- bind_rows(result_list)
  
  return(final_data)
}

# Example of querying the PUMS data for the year 2022 with numeric and categorical variables
result <- query_census_pums(
  year = 2021,
  numeric_vars = c("AGEP", "PWGTP"),
  categorical_vars = c("SEX", "HISPEED"),
  geography_level = "state",
  geography_subset = "10"
)

# Example of querying multiple years
#multi_year_result <- query_multiple_years(
#  years = c(2020, 2021, 2022),
#  numeric_vars = c("AGEP", "PWGTP"),
#  categorical_vars = c("SEX", "HISPEED"),
#  geography_level = "state",
#  geography_subset = "10"
#)


# View the results
print ("end of function")
print(result)
#print(multi_year_result)