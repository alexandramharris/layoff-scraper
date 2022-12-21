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

# Split
pdf_data <- separate(pdf_texts, text, into = c("Date of Notice", "Event Number", "Rapid Response Specialist", "Reason Stated for Filing", "Company", "County", "WDB Name", "Region", "Contact", "Phone", "Business Type", "Number Affected", "Total Employees", "Layoff Date", "Closing Date", "Reason for Dislocation", "FEIN NUM", "Union", "Classification"), sep = ":\\s+", extra = "merge")

# Extract
date_of_notice <- str_extract(pdf_data$text, "Date of Notice:\\s*\\d{1,2}/\\d{1,2}/\\d{4}")
event_number <- str_extract(pdf_data$text, "Event Number:\\s*\\d{4}-\\d{4}")
rapid_response_specialist <- str_extract(pdf_data$text, "Rapid Response Specialist:\\s*[A-Za-z ]+")
reason_stated_for_filing <- str_extract(pdf_data$text, "Reason Stated for Filing:\\s*[A-Za-z ]+")
company <- str_extract(pdf_data$text, "Company:\\s*[A-Za-z0-9, .&-]+")
county <- str_extract(pdf_data$text, "County:\\s*[A-Za-z ]+")
wdb_name <- str_extract(pdf_data$text, "WDB Name:\\s*[A-Za-z ]+")
region <- str_extract(pdf_data$text, "Region:\\s*[A-Za-z ]+")
contact <- str_extract(pdf_data$text, "Contact:\\s*[A-Za-z ]+")
phone <- str_extract(pdf_data$text, "Phone:\\s*[0-9() -]+")
business_type <- str_extract(pdf_data$text, "Business Type:\\s*[A-Za-z0-9, .&-]+")
number_affected <- str_extract(pdf_data$text, "Number Affected:\\s*\\d+")
total_employees <- str_extract(pdf_data$text, "Total Employees:\\s*\\d+")
layoff_date <- str_extract(pdf_data$text, "Layoff Date:\\s*[A-Za-z0-9, .&-]+")
closing_date <- str_extract(pdf_data$text, "Closing Date:\\s*[A-Za-z0-9, .&-]+")
reason_for_dislocation <- str_extract(pdf_data$text, "Reason for Dislocation:\\s*[A-Za-z0-9, .&-]+")
fein_num <- str_extract(pdf_data$text, "FEIN NUM:\\s*[0-9-]+")
union <- str_extract(pdf_data$text, "Union:\\s*(.*)")
classification <- str_extract(pdf_data$text, "Classification:\\s*(\\S+)")

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
sheet_write(layoff_data, ss = "link", sheet = "scraper")
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
