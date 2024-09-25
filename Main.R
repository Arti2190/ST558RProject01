#API key - d116b830d71b6bc7b93f5ad62fbce8f95e8a5b59
#API -   https://api.census.gov/data/2022/acs/acs1/pums?get=SEX,PWGTP,MAR&SCHL=24&key=YOUR_KEY
#https://api.census.gov/data/2022/acs/acs1/pums?get=SEX,PWGTP,MAR&SCHL=24&key=d116b830d71b6bc7b93f5ad62fbce8f95e8a5b59

library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
#apikey <- 'd116b830d71b6bc7b93f5ad62fbce8f95e8a5b59'
url <- "https://api.census.gov/data/2022/acs/acs1/pums?get=SEX,PWGTP,MAR&SCHL=24"
census_data <- httr::GET(url)
head(census_data)
str(census_data)

# write a helper function 
census_data_response <- function(api_result){
  # Check whether we get the response or not
  if(api_result$status_code == 200) {
    print ("Data retrieved successfully")
    
  } else {
    print ("error, try again")
  } 
  parsed_census_data <- fromJSON(rawToChar(census_data$content))
  head(parsed_census_data)
  str(parsed_census_data)
  # Data Processing
  
  census_tibble <- as_tibble(parsed_census_data[-1,])
  colnames(census_tibble) <- parsed_census_data[1,]
  return(census_tibble)
}

# response data into tibble

#census_tibble_data <- census_data_response(census_data)
#head(census_tibble_data)


# write a function to query the API 
query_api_census <- function(year = 2022,
                             numeric_vars = c("AGEP", "PWGTP"),
                             categorical_vars = c("SEX"),
                             geography_level = "All",
                             geography_subset = NULL
) {
  # Validate year should be between 2010 and 2022
 
  if (year < 2010 || year > 2022) {
    stop("Year must be between 2010 and 2022")
  }

  # Ensure that PWGTP is always returned in numeric variables
  if (!"PWGTP" %in% numeric_vars) {
    numeric_vars <- c(numeric_vars, "PWGTP")
  }
  
  # Validate numeric variables (must be from the allowed set)
  allowed_numeric_vars <- c("AGEP", "GASP", "GRPIP", "JWAP", "JWDP", "JWMNP", "PWGTP")
  if (!all(numeric_vars %in% allowed_numeric_vars)) {
    stop("Numeric variables must be one or more of the following: ", 
         paste(allowed_numeric_vars, collapse = ", "))
  }
  
  # Ensure at least one numeric variable other than PWGTP is selected
  if (length(numeric_vars) <= 1) {
    stop("You must select at least one numeric variable other than PWGTP.")
  }
  
  # Validate categorical variables (must be from the allowed set)
  allowed_categorical_vars <- c("FER", "HHL", "HISPEED", "JWAP", "JWDP", "JWTRNS", "SCH", "SCHL", "SEX")
  if (!all(categorical_vars %in% allowed_categorical_vars)) {
    stop("Categorical variables must be one or more of the following: ", 
         paste(allowed_categorical_vars, collapse = ", "))
  }
  
  # Ensure at least one categorical variable is selected
  if (length(categorical_vars) < 1) {
    stop("You must select at least one categorical variable.")
  }
  
  # Validate geography level (must be one of the specified options)
  allowed_geo_levels <- c("All", "Region", "Division", "State")
  if (!geography_level %in% allowed_geo_levels) {
    stop("Geography level must be one of: All, Region, Division, or State.")
  }
  
  # Construct API query
  base_url <- paste0("https://api.census.gov/data/", year, "/acs/acs1/pums?")
  get_vars <- paste(c(numeric_vars, categorical_vars), collapse = ",")
  
  # If geography subset is provided, add it to the query
  if (!is.null(geography_subset)) {
    subset_query <- paste0("&", geography_level, "=", geography_subset)
  } else {
    subset_query <- ""
  }
  
  # Full API URL
  api_url <- paste0(base_url, "get=", get_vars, subset_query)
  
  # Make the GET request to the API
  api_response <- httr::GET(api_url)
  
  # Process the API response and return as tibble
  return(census_data_response(api_response))
}

# Example usage:
census_data <- query_api_census(
  year = 2022, 
  numeric_vars = c("AGEP", "PWGTP"), 
  categorical_vars = c("SEX", "SCHL"), 
  geography_level = "State", 
  geography_subset = "06" # California
)

# Preview the data
head(census_data)
                           



















