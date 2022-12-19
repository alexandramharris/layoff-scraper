# WARN Notice Scraper
# Alexandra Harris


# Set up ----
library(tidyverse)
library(rvest)
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

# Get PDF links
links <- webpage %>% 
  html_nodes("td a") %>% 
  html_attr("href") %>% 
  as.data.frame

# Rename
colnames(links)[1] = "pdf_link"

# Check for absolute versus relative links
has_pdf <- str_detect(links$pdf_link, ".pdf")

# Get dates
dates <- webpage %>% 
  html_nodes("td:nth-child(2)") %>% 
  html_text() %>% 
  as.data.frame 

# Rename
colnames(dates)[1] = "Date"

# Format dates
dates$Date <- mdy(dates$Date)

# Split
dates <- dates %>% 
  mutate(year = year(Date),
         month = formatC(month(Date), width = 2, format = "d", flag = "0"),
         pdf_url_date = paste0(year, "/", month))


# Add date if .pdf is not present
links$pdf_link <- ifelse(has_pdf, links$pdf_link, paste0(dates$pdf_url_date, links$pdf_link))

# Add .pdf if not present
links$pdf_link <- ifelse(has_pdf, links$pdf_link, paste0(links$pdf_link, ".pdf"))

# Add base URL if .pdf is not present
links$pdf_link <- ifelse(has_pdf, links$pdf_link, paste0("https://dol.ny.gov/system/files/documents/", links$pdf_link))

# Test dataset
pdf_file <- pdf_text("https://dol.ny.gov/system/files/documents/2022/11/warn-avon-products-mid-hudson-2022-0049-11-4-2022.pdf")

# Split text into lines
lines <- strsplit(pdf_file, "\n")[[1]]

# Create dataframe
data_df <- data.frame()

# Create vector
data_names <- c()

# Loop through text
for (line in lines) {
  # Check if  contains colon
  matches <- regexpr(":\\s+(.+)", line)
  if (matches > 0) {
    data_name <- substr(line, 1, matches-1)
    data_names <- c(data_names, data_name)
    data_value <- substr(line, matches+2, nchar(line))
  }
}

# Create data frame
data_df <- data.frame(matrix(ncol = length(data_names), nrow = 1))
colnames(data_df) <- data_names

# Loop through text
for (line in lines) {
  matches <- regexpr(":\\s+(.+)", line)
  if (matches > 0) {
    data_name <- substr(line, 1, matches-1)
    data_value <- substr(line, matches+2, nchar(line))
    data_df[1, data_name] <- data_value
  }
}


# Export ----

# Authorize
gs4_auth("my_email")

# CSV export
write.csv(final, "final.csv", row.names = FALSE)

# Google Sheets export
sheet_write(data_df, ss = "my_link", sheet = "scraper")


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
# <string>/Users/my_path/layoff-scraper.R</string>
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
