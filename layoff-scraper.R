# WARN Notice Scraper
# Alexandra Harris


# Set up ----
library(tidyverse)
library(rvest)
library(janitor)
library(xml2)
library(tidyr)
library(pdftools)
library(lubridate)
library(stringr)
library(tokencodr)
library(googlesheets4)
library(tidycensus)


# Scraper ----

# Set URL
url <- "https://dol.ny.gov/warn-notices"

# Read HTML
webpage <- read_html(url) 

# Read table
notices <- html_table(webpage)[[1]]

# Clean names
notices <- clean_names(notices)

# Select company name from notices
company_name <- notices %>% 
  select(company_name)

# Split
company_name <- separate(company_name, company_name, into = c("company_name", "region"), sep = "-\\s+", extra = "merge")

# Get PDF links
links <- webpage %>% 
  html_nodes("td a") %>% 
  html_attr("href") %>% 
  as.data.frame

# Rename
colnames(links)[1] = "pdf_link"

# Check for absolute versus relative links
has_pdf <- str_detect(links$pdf_link, ".pdf")

# Add domain if relative
links$pdf_link <- ifelse(has_pdf, links$pdf_link, paste0("https://dol.ny.gov", links$pdf_link))

# Read PDF links
pdf_texts <- data.frame(pdf_link = character(0), text = character(0))

for (i in 1:nrow(links)) {
  pdf_link <- links$pdf_link[i]
  text <- tryCatch(pdf_text(pdf_link), error = function(e) NA)
  pdf_texts <- rbind(pdf_texts, data.frame(pdf_link, text))
}

# Clean whitespace and change into single space
pdf_data <- pdf_texts
pdf_data$text <- gsub("\\s+", " ", pdf_data$text)

# Remove header
pdf_data$text <- gsub(" NEW YORK STATE DEPARTMENT OF LABOR OFFICE OF DISLOCATED WORKERS PROGRAM ", "", pdf_data$text)

# Extract
date_of_notice <- str_trim(str_extract(pdf_data$text, "(?<=Date of Notice:).*?(?=Event Number)"))
event_number <- str_trim(str_extract(pdf_data$text, "(?<=Event Number:).*?(?=Rapid Response Specialist)"))
rapid_response_specialist <- str_trim(str_extract(pdf_data$text, "(?<=Rapid Response Specialist:).*?(?=Reason Stated for Filing)"))
reason_stated_for_filing <- str_trim(str_extract(pdf_data$text, "(?<=Reason Stated for Filing:).*?(?=Company)"))
company <- str_trim(str_extract(pdf_data$text, "(?<=Company:).*?(?=County)"))
location <- str_to_title(str_trim(str_extract(pdf_data$text, "(?<=County:).*?(?=Contact)")))
county <- str_trim(str_extract(pdf_data$text, "(?<=County:).*?(?=\\|WDB Name)"))
wdb_name <- str_to_title(str_trim(str_extract(pdf_data$text, "(?<=WDB Name:).*?(?=\\|)")))
region <- str_trim(str_extract(pdf_data$text, "(?<=Region:).*?(?=Contact)"))
contact <- str_trim(str_extract(pdf_data$text, "(?<=Contact:).*?(?=Phone)"))
phone <- str_trim(str_extract(pdf_data$text, "(?<=Phone:).*?(?=Business Type)"))
business_type <- str_trim(str_extract(pdf_data$text, "(?<=Business Type:).*?(?=Number Affected)"))
number_affected <- str_trim(str_extract(pdf_data$text, "(?<=Number Affected:).*?(?=Total Employees)"))
total_employees <- str_trim(str_extract(pdf_data$text, "(?<=Total Employees:).*?(?=Layoff Date)"))
layoff_date <- str_trim(str_extract(pdf_data$text, "(?<=Layoff Date:).*?(?=Closing Date)"))
closing_date <- str_trim(str_extract(pdf_data$text, "(?<=Closing Date:).*?(?=Reason for Dislocation)"))
reason_for_dislocation <- str_trim(str_extract(pdf_data$text, "(?<=Reason for Dislocation:).*?(?=FEIN NUM)"))
fein_num <- str_trim(str_extract(pdf_data$text, "(?<=FEIN NUM:).*?(?=Union)"))
union <- str_trim(str_extract(pdf_data$text, "(?<=Union:).*?(?=Classification)"))
classification_and_additional_info <- str_trim(str_extract(pdf_data$text, "(?<=Classification:).*"))

# Combine into a data frame
layoff_data <- data.frame(date_of_notice, event_number, rapid_response_specialist, 
                 reason_stated_for_filing, company, location, county, wdb_name, region, 
                 contact, phone, business_type, number_affected, total_employees, 
                 layoff_date, closing_date, reason_for_dislocation, fein_num, 
                 union, classification_and_additional_info)


# Clean and format ----

# Format and clean notice dates
layoff_data <- layoff_data %>%
  mutate(date_of_notice_additional_info = str_extract(date_of_notice, "(?<=[A-Za-z0-9])\\s(.*)")) %>%
  mutate(date_of_notice = gsub("\\s.*", "", date_of_notice)) %>% 
  select(date_of_notice, date_of_notice_additional_info, everything()) %>%
  mutate(date_of_notice = trimws(date_of_notice),
         date_of_notice_additional_info = trimws(date_of_notice_additional_info)) %>%
  mutate(date_of_notice = mdy(date_of_notice)) %>%
  mutate(date_of_notice_additional_info = gsub("&", "and", date_of_notice_additional_info)) %>%
  mutate(date_of_notice_additional_info = str_to_title(date_of_notice_additional_info)) %>%
  arrange(desc(date_of_notice))

# Add month column
layoff_data$month_year <- format(layoff_data$date_of_notice, "%B %Y")
layoff_data <- layoff_data %>% 
  select(date_of_notice, month_year, date_of_notice_additional_info, event_number, rapid_response_specialist, reason_stated_for_filing, company, location, county, wdb_name, region, contact, phone, business_type, number_affected, total_employees, layoff_date, closing_date, reason_for_dislocation, fein_num, union, classification_and_additional_info)

# Format number affected
layoff_data <- layoff_data %>% 
  mutate(raw_number_affected = number_affected) %>%
  mutate(raw_number_affected = str_extract(raw_number_affected, "^\\S+")) %>% 
  mutate(raw_number_affected = str_replace(raw_number_affected, ",", "")) %>% 
  mutate(raw_number_affected = as.numeric(raw_number_affected)) %>% 
  select(date_of_notice, month_year, date_of_notice_additional_info, event_number, rapid_response_specialist, reason_stated_for_filing, company, location, county, wdb_name, region, contact, phone, business_type, number_affected, raw_number_affected, total_employees, layoff_date, closing_date, reason_for_dislocation, fein_num, union, classification_and_additional_info)

# Format total employees
layoff_data <- layoff_data %>% 
  mutate(raw_total_employees = total_employees) %>%
  mutate(raw_total_employees = str_extract(raw_total_employees, "^\\S+")) %>% 
  mutate(raw_total_employees = str_replace(raw_total_employees, ",", "")) %>% 
  mutate(raw_total_employees = as.numeric(raw_total_employees)) %>% 
  select(date_of_notice, month_year, date_of_notice_additional_info, event_number, rapid_response_specialist, reason_stated_for_filing, company, location, county, wdb_name, region, contact, phone, business_type, number_affected, raw_number_affected, total_employees, raw_total_employees, layoff_date, closing_date, reason_for_dislocation, fein_num, union, classification_and_additional_info)

# Calculate not affected
layoff_data <- layoff_data %>% 
  mutate(not_affected = (raw_total_employees - raw_number_affected)) %>% 
  select(date_of_notice, month_year, date_of_notice_additional_info, event_number, rapid_response_specialist, reason_stated_for_filing, company, location, county, wdb_name, region, contact, phone, business_type, number_affected, raw_number_affected, total_employees, raw_total_employees, not_affected, layoff_date, closing_date, reason_for_dislocation, fein_num, union, classification_and_additional_info)

# Count occurrences of event number
count <- layoff_data %>% 
  group_by(event_number) %>% 
  summarize(count = n())

# Add handling for new rescission notices, setting affected employees to zero
layoff_data$rescission_amend <- NA

for (i in 1:nrow(layoff_data)) {
  if (grepl("Rescission[:\\s]?", layoff_data$date_of_notice_additional_info[i])) {
    layoff_data$rescission_amend[i] <- 0
  } else {
    layoff_data$rescission_amend[i] <- layoff_data$raw_number_affected[i]
  } 
  if (is.na(layoff_data$date_of_notice_additional_info[i])) {
    layoff_data$rescission_amend[i] <- layoff_data$raw_number_affected[i]
  }
}

layoff_data <- select(layoff_data, date_of_notice, month_year, date_of_notice_additional_info, event_number, rapid_response_specialist, reason_stated_for_filing, company, location, county, wdb_name, region, contact, phone, business_type, number_affected, raw_number_affected, rescission_amend, total_employees, raw_total_employees, not_affected, layoff_date, closing_date, reason_for_dislocation, fein_num, union, classification_and_additional_info)

# Locate old and now rescinded notice by event number and also set to zero
for (i in 1:nrow(layoff_data)) {
  if (grepl("Rescission[:\\s]?", layoff_data$date_of_notice_additional_info[i])) {
    layoff_data$rescission_amend[i] <- 0
    event_number <- layoff_data$event_number[i]
    for (j in 1:nrow(layoff_data)) {
      if (!is.na(layoff_data$event_number[j]) && layoff_data$event_number[j] == event_number) {
        layoff_data$rescission_amend[j] <- 0
      }
    }
  }
}

# Add handling for amendments — newest total affected is posted while old notice is set to 0
for (i in 1:nrow(layoff_data)) {
  
  if (grepl("(?i)amend", layoff_data$date_of_notice_additional_info[i])) {
    layoff_data$rescission_amend[i] <- layoff_data$raw_number_affected[i]
    event_number <- layoff_data$event_number[i]
    
    for (j in 1:nrow(layoff_data)) {
      if (!grepl("(?i)amend", layoff_data$date_of_notice_additional_info[j]) && !is.na(layoff_data$event_number[j]) && layoff_data$event_number[j] == event_number) {
        layoff_data$rescission_amend[j] <- 0
      }
    }
  }
}          
          
                   
# Graphics ----

# Create new_york dataset
new_york <- layoff_data %>% 
  summarize(`Companies` = n_distinct(company),
            `Employees` = sum(raw_number_affected, na.rm = TRUE))

# Create capital_region dataset
capital_region <- layoff_data %>% 
  filter(region == "Capital Region") %>% 
  summarize(`Companies` = n_distinct(company),
            `Employees` = sum(raw_number_affected, na.rm = TRUE))

# Create mid_hudson dataset
mid_hudson <- layoff_data %>%
  filter(region == "Mid-Hudson Region") %>% 
  summarize(`Companies` = n_distinct(company),
            `Employees` = sum(raw_number_affected, na.rm = TRUE))

# Create donut_one dataset
donut_one <- layoff_data %>% 
  filter(row_number() == 1) %>% 
  mutate(`Affected` = sum(raw_number_affected)) %>% 
  mutate(`Not affected` = sum(not_affected)) %>% 
  select(`Affected`, `Not affected`) %>% 
  stack()
names(donut_one)[2] <- "Impact"
names(donut_one)[1] <- "Employees"
donut_one <- donut_one %>% 
  select(Impact, Employees)

# Create donut_two dataset
donut_two <- layoff_data %>% 
  filter(row_number() == 2) %>% 
  mutate(`Affected` = sum(raw_number_affected)) %>% 
  mutate(`Not affected` = sum(not_affected)) %>% 
  select(`Affected`, `Not affected`) %>% 
  stack()
names(donut_two)[2] <- "Impact"
names(donut_two)[1] <- "Employees"
donut_two <- donut_two %>% 
  select(Impact, Employees)

# Create donut_three dataset
donut_three <- layoff_data %>% 
  filter(row_number() == 3) %>% 
  mutate(`Affected` = sum(raw_number_affected)) %>% 
  mutate(`Not affected` = sum(not_affected)) %>% 
  select(`Affected`, `Not affected`) %>% 
  stack()
names(donut_three)[2] <- "Impact"
names(donut_three)[1] <- "Employees"
donut_three <- donut_three %>% 
  select(Impact, Employees)
  
# Create line dataset
line <- layoff_data %>% 
  group_by(month_year) %>% 
  summarize(total_employees_laid_off = sum(raw_number_affected)) %>% 
  rename(Date = month_year,
         "Total employees laid off" = total_employees_laid_off) %>% 
  na.omit()
  
# Create map dataset
map <- layoff_data %>% 
  group_by(county) %>% 
  summarize(total_employees_laid_off = sum(raw_number_affected)) %>% 
  rename(County = county,
         "Total employees laid off" = total_employees_laid_off) %>% 
  na.omit()

# Find tidycensus variables
v20 <- load_variables(2020, "acs5", cache = TRUE)

# Get working age population (age 18-64) -- currently total population
working_age_pop = get_acs(geography = "county",
                          state = "NY",
                          variables = "B01001_001",
                          year = 2021,
                          output = "wide")

# Clean names
working_age_pop <- clean_names(working_age_pop)
working_age_pop$name <- substr(working_age_pop$name, 1, (regexpr(" County", working_age_pop$name)-1))
colnames(working_age_pop)[colnames(working_age_pop) == "name"] <- "County"
colnames(working_age_pop)[colnames(working_age_pop) == "b01001_001e"] <- "Population"

# Join population data
map <- left_join(working_age_pop, map, "County")

# Select and filter
map <- map %>%
  select("County", "Population", "Total employees laid off") %>% 
  filter(`Total employees laid off` != "NA")
 
# Calculate rate
map <- map %>% 
  mutate(Rate = `Total employees laid off`/`Population`*100000)


# Create bar dataset
bar <- layoff_data %>% 
  group_by(business_type) %>% 
  summarize(total_employees_laid_off = sum(raw_number_affected)) %>% 
  na.omit() %>% 
  arrange(desc(total_employees_laid_off)) %>% 
  head(10)

bar$short_name <- str_extract(bar$business_type, "^\\w[\\w-]*( \\w[\\w-]*)?( \\w[\\w-]*)?")

bar <- bar %>% 
select(business_type, short_name, total_employees_laid_off) %>% 
rename("Business type" = business_type,
       "Short name" = short_name,
       "Total employees laid off" = total_employees_laid_off)

             
# Authorize for actions ----
auth_google <- function(email, service, token_path) {
  googlesheets4::gs4_auth(email = "alexandra.harris@timesunion.com", path = tokencodr::decrypt_token(service = gsheet_layoffs,
                                                                                                      path = '.secret/gsheet_layoffs',
                                                                                                      complete = TRUE))
}
                   
                   
# Export ----

# Google Sheets export
sheet_write(layoff_data, ss = "https://docs.google.com/spreadsheets/d/10ccdzjb9OtuXKKow1j1oSdk7LXZb_5Q7x1Z0SMWv79g/edit#gid=723526670", sheet = "pdf_data")
sheet_write(pdf_texts, ss = "https://docs.google.com/spreadsheets/d/10ccdzjb9OtuXKKow1j1oSdk7LXZb_5Q7x1Z0SMWv79g/edit#gid=723526670", sheet = "pdf_texts")
sheet_write(new_york, ss = "https://docs.google.com/spreadsheets/d/10ccdzjb9OtuXKKow1j1oSdk7LXZb_5Q7x1Z0SMWv79g/edit#gid=723526670", sheet = "new_york")
sheet_write(capital_region, ss = "https://docs.google.com/spreadsheets/d/10ccdzjb9OtuXKKow1j1oSdk7LXZb_5Q7x1Z0SMWv79g/edit#gid=723526670", sheet = "capital_region")
sheet_write(mid_hudson, ss = "https://docs.google.com/spreadsheets/d/10ccdzjb9OtuXKKow1j1oSdk7LXZb_5Q7x1Z0SMWv79g/edit#gid=723526670", sheet = "mid_hudson")
sheet_write(donut_one, ss = "https://docs.google.com/spreadsheets/d/10ccdzjb9OtuXKKow1j1oSdk7LXZb_5Q7x1Z0SMWv79g/edit#gid=723526670", sheet = "donut_one")
sheet_write(donut_two, ss = "https://docs.google.com/spreadsheets/d/10ccdzjb9OtuXKKow1j1oSdk7LXZb_5Q7x1Z0SMWv79g/edit#gid=723526670", sheet = "donut_two")
sheet_write(donut_three, ss = "https://docs.google.com/spreadsheets/d/10ccdzjb9OtuXKKow1j1oSdk7LXZb_5Q7x1Z0SMWv79g/edit#gid=723526670", sheet = "donut_three")
sheet_write(line, ss = "https://docs.google.com/spreadsheets/d/10ccdzjb9OtuXKKow1j1oSdk7LXZb_5Q7x1Z0SMWv79g/edit#gid=723526670", sheet = "line")
sheet_write(map, ss = "https://docs.google.com/spreadsheets/d/10ccdzjb9OtuXKKow1j1oSdk7LXZb_5Q7x1Z0SMWv79g/edit#gid=723526670", sheet = "map")
sheet_write(bar, ss = "https://docs.google.com/spreadsheets/d/10ccdzjb9OtuXKKow1j1oSdk7LXZb_5Q7x1Z0SMWv79g/edit#gid=723526670", sheet = "bar")
