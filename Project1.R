# Retrieve Data from URL

#install.packages(c("httr", "jsonlite"))
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
#apikey <- 'd116b830d71b6bc7b93f5ad62fbce8f95e8a5b59'
url <- "https://api.census.gov/data/2022/acs/acs1/pums?get=SEX,PWGTP,MAR&SCHL=24"
census_data <- httr::GET(url)
head(census_data)

str(census_data)

if (census_data$status_code == 200) {
  print ("Data retrieved successfully")
} else {
  print ("error, try again")
}

#print(content_string)
#content_string <- rawToChar(census_data$content)

parsed_census_data <- fromJSON(rawToChar(census_data$content))
head(parsed_census_data)
str(parsed_census_data)
# Data Processing

census_tibble <- as_tibble(parsed_census_data[-1,])
colnames(census_tibble) <- parsed_census_data[1,]
head(census_tibble)



# Write a function to query the API and return the tibble
# Function to query the API with options
query_census_api <- function(year = 2022, numeric_vars = c("AGEP", "PWGTP"), 
                             categorical_vars = c("SEX"), geography = "All", 
                             subset = NULL, apikey) {
  
  # Validate the year
  if (year < 2010 || year > 2022) {
    stop("Invalid year. Must be between 2010 and 2022.")
  }
  
  # Validate numeric variables
  valid_numeric_vars <- c("AGEP", "GASP", "GRPIP", "JWAP", "JWDP", "JWMNP")
  if (!all(numeric_vars %in% valid_numeric_vars)) {
    stop("Invalid numeric variables. Must be one of: AGEP, GASP, GRPIP, JWAP, JWDP, JWMNP")
  }
  
  # Ensure PWGTP is always included
  if (!"PWGTP" %in% numeric_vars) {
    numeric_vars <- c(numeric_vars, "PWGTP")
  }
  
  # Validate categorical variables
  valid_categorical_vars <- c("FER", "HHL", "HISPEED", "JWAP", "JWDP", "JWTRNS", "SCH", "SCHL", "SEX")
  if (!all(categorical_vars %in% valid_categorical_vars)) {
    stop("Invalid categorical variables. Must be one of: FER, HHL, HISPEED, JWAP, JWDP, JWTRNS, SCH, SCHL, SEX")
  }
  
  # Validate geography
  valid_geographies <- c("All", "Region", "Division", "State")
  if (!geography %in% valid_geographies) {
    stop("Invalid geography level. Must be one of: All, Region, Division, State")
  }
  
  # Build the query string
  get_vars <- paste(c(numeric_vars, categorical_vars), collapse = ",")
  query_params <- list(
    get = get_vars,
    key = apikey
  )
  
  # Add optional subsetting
  if (!is.null(subset)) {
    query_params <- c(query_params, subset)
  }
  
  # Construct URL
  url <- paste0("https://api.census.gov/data/", year, "/acs/acs1/pums")
  
  # Make GET request
  census_data <- httr::GET(url, query = query_params)
  
  # Convert response to tibble
  formatted_data <- response_to_tibble(census_data)
  
  return(formatted_data)
}

# Example usage
apikey <- 'd116b830d71b6bc7b93f5ad62fbce8f95e8a5b59'
data <- query_census_api(year = 2022, numeric_vars = c("AGEP"), 
                         categorical_vars = c("SEX", "SCHL"), geography = "State", apikey = apikey)
print(data)




query_multiple_years <- function(years = 2022:2021, numeric_vars = c("AGEP", "PWGTP"), 
                                 categorical_vars = c("SEX"), geography = "All", 
                                 subset = NULL, apikey) {
  
  # Loop over the years and combine the results
  all_data <- lapply(years, function(y) {
    data <- query_census_api(year = y, numeric_vars = numeric_vars, 
                             categorical_vars = categorical_vars, geography = geography, 
                             subset = subset, apikey = apikey)
    data <- data %>% mutate(Year = y)  # Add year column
    return(data)
  })
  
  # Bind all the data together
  combined_data <- bind_rows(all_data)
  
  return(combined_data)
}

# Example usage
data_multiple_years <- query_multiple_years(years = 2021:2022, numeric_vars = c("AGEP"), 
                                            categorical_vars = c("SEX"), apikey = apikey)
print(data_multiple_years)

