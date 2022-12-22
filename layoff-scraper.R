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
library(googlesheets4)


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
date_of_notice <- str_trim(str_extract(pdf_data$text, "(?<=Date of Notice:).*(?=Event Number)"))
event_number <- str_trim(str_extract(pdf_data$text, "(?<=Event Number:).*(?=Rapid Response Specialist)"))
rapid_response_specialist <- str_trim(str_extract(pdf_data$text, "(?<=Rapid Response Specialist:).*(?=Reason Stated for Filing)"))
reason_stated_for_filing <- str_trim(str_extract(pdf_data$text, "(?<=Reason Stated for Filing:).*(?=Company)"))
company <- str_trim(str_extract(pdf_data$text, "(?<=Company:).*(?=County)"))
county <- str_trim(str_extract(pdf_data$text, "(?<=County:).*(?=\\|WDB Name)"))
wdb_name <- str_to_title(str_trim(str_extract(pdf_data$text, "(?<=WDB Name:).*(?=\\|)")))
region <- str_trim(str_extract(pdf_data$text, "(?<=Region:).*(?=Contact)"))
contact <- str_trim(str_extract(pdf_data$text, "(?<=Contact:).*(?=Phone)"))
phone <- str_trim(str_extract(pdf_data$text, "(?<=Phone:).*(?=Business Type)"))
business_type <- str_trim(str_extract(pdf_data$text, "(?<=Business Type:).*(?=Number Affected)"))
number_affected <- str_trim(str_extract(pdf_data$text, "(?<=Number Affected:).*(?=Total Employees)"))
total_employees <- str_trim(str_extract(pdf_data$text, "(?<=Total Employees:).*(?=Layoff Date)"))
layoff_date <- str_trim(str_extract(pdf_data$text, "(?<=Layoff Date:).*(?=Closing Date)"))
closing_date <- str_trim(str_extract(pdf_data$text, "(?<=Closing Date:).*(?=Reason for Dislocation)"))
reason_for_dislocation <- str_trim(str_extract(pdf_data$text, "(?<=Reason for Dislocation:).*(?=FEIN Num)"))
fein_num <- str_trim(str_extract(pdf_data$text, "(?<=FEIN Num:).*(?=Union)"))
union <- str_trim(str_extract(pdf_data$text, "(?<=Union:).*(?=Classification)"))
classification <- str_trim(str_extract(pdf_data$text, "(?<=Classification:).*"))

# Combine into a data frame
layoff_data <- data.frame(date_of_notice, event_number, rapid_response_specialist, 
                 reason_stated_for_filing, company, county, wdb_name, region, 
                 contact, phone, business_type, number_affected, total_employees, 
                 layoff_date, closing_date, reason_for_dislocation, fein_num, 
                 union, classification)


# Export ----

# Authorize
gs4_auth("my_email")

# Google Sheets export
sheet_write(layoff_data, ss = "link", sheet = "pdf_data")
sheet_write(pdf_texts, ss = "link", sheet = "pdf_texts")



# Schedule with Launchd (Mac) ----

# Save as .plist:
# <?xml version="1.0" encoding="UTF-8"?>
# <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
# <plist version="1.0">
# <dict>
# <key>Label</key>
# <string>com.layoff-scraper</string>
# <key>ProgramArguments</key>
# <array>
# <string>/usr/local/bin/Rscript</string>
# <string>my_path/layoff-scraper.R</string>
# </array>
# <key>StartInterval</key>
# <integer>60</integer>
# <key>KeepAlive</key>
# <true/>
# <key>PowerType</key>
# <string>ACPower</string>
# </dict>
# </plist>

# Save to /Libary/LaunchDaemons

# Run in terminal:
# sudo launchctl load /Library/LaunchDaemons/com.layoff-scraper.plist

# Check:
# launchctl list
