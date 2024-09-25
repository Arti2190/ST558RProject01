library(httr)
library(jsonlite)
library(dplyr)

# Helper function to query the API
query_pums_api <- function(year = 2022, 
                           numeric_vars = c("AGEP", "PWGTP"), 
                           cat_vars = c("SEX"), 
                           geo_level = "All", 
                           subset_geo = NULL) {
  
  # Validating the year
  if (year < 2010 || year > 2022) stop("Year must be between 2010 and 2022")
  
  # Making sure that PWGTP is included in numeric variables
  if (!"PWGTP" %in% numeric_vars) numeric_vars <- c(numeric_vars, "PWGTP")
  
  # Validate that the variables are allowed
  valid_numeric_vars <- c("AGEP", "PWGTP", "GASP", "GRPIP", "JWAP", "JWDP", "JWMNP")
  valid_cat_vars <- c("SEX", "FER", "HHL", "HISPEED", "JWTRNS", "SCH", "SCHL")
  
  if (!all(numeric_vars %in% valid_numeric_vars)) stop("Invalid numeric variable(s) provided")
  if (!all(cat_vars %in% valid_cat_vars)) stop("Invalid categorical variable(s) provided")
  
  # Constructing the URL
  query_vars <- paste(c(numeric_vars, cat_vars), collapse = ",")
  base_url <- paste0("https://api.census.gov/data/", year, "/acs/acs1/pums?get=", query_vars)
  
  # If geographic subsetting is requested
  if (!is.null(subset_geo)) base_url <- paste0(base_url, "&for=", geo_level, ":", subset_geo)
  
  # Querying the API
  response <- httr::GET(base_url)
  
  # Error handling
  if (response$status_code != 200) stop("Error retrieving data. Check the API call.")
  
  # Parsing the data
  content <- rawToChar(response$content)
  parsed_data <- fromJSON(content)
  
  # Converting to a tibble and setting column names
  tibble_data <- as_tibble(parsed_data[-1, ])
  colnames(tibble_data) <- parsed_data[1, ]
  
  return(tibble_data)
}

# Example call
census_data <- query_pums_api(year = 2022, numeric_vars = c("AGEP", "PWGTP"), cat_vars = c("SEX", "SCHL"))
head(census_data)

# Function to summarize the numeric and categorical variables
summary_census <- function(census_tibble, numeric_vars = NULL, cat_vars = NULL) {
  
  # If no specific variables provided, use all except PWGTP
  numeric_vars <- numeric_vars %||% setdiff(names(census_tibble), "PWGTP")
  cat_vars <- cat_vars %||% names(Filter(is.factor, census_tibble))
  
  # Calculate weighted mean and standard deviation for numeric variables
  numeric_summary <- census_tibble %>%
    summarize(across(all_of(numeric_vars), 
                     list(mean = ~weighted.mean(as.numeric(.), census_tibble$PWGTP),
                          sd = ~sqrt(weighted.mean((as.numeric(.) - mean(.))^2, census_tibble$PWGTP)))))
  
  # Count occurrences of each level for categorical variables
  cat_summary <- lapply(cat_vars, function(var) {
    as.data.frame(table(census_tibble[[var]], census_tibble$PWGTP))
  })
  
  return(list(numeric = numeric_summary, categorical = cat_summary))
}

# Example
summary_census(census_data)

# Function to query and combine multiple years
query_multiple_years <- function(years, numeric_vars = c("AGEP", "PWGTP"), cat_vars = c("SEX"), geo_level = "All", subset_geo = NULL) {
  
  all_data <- lapply(years, function(yr) {
    data <- query_pums_api(year = yr, numeric_vars = numeric_vars, cat_vars = cat_vars, geo_level = geo_level, subset_geo = subset_geo)
    data <- data %>% mutate(year = yr)
    return(data)
  })
  
  combined_data <- bind_rows(all_data)
  return(combined_data)
}

# Example for multiple years
years_data <- query_multiple_years(years = 2020:2022)
head(years_data)



