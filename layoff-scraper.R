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

# Add domain if relative
links$pdf_link <- ifelse(has_pdf, links$pdf_link, paste0("https://dol.ny.gov", links$pdf_link))

# Read PDF links
pdf_texts <- data.frame(pdf_link = character(0), text = character(0))

for (i in 1:nrow(links)) {
  pdf_link <- links$pdf_link[i]
  text <- tryCatch(pdf_text(pdf_link), error = function(e) NA)
  pdf_texts <- rbind(pdf_texts, data.frame(pdf_link, text))
}

# Split text into lines
lines <- strsplit(pdf_texts$text, "\n")

# Create empty df to hold data
data_df <- data.frame(text = character(0))

# Create vector
data_names <- character(0)

# Loop
for (i in 1:nrow(pdf_texts)) {
  lines <- strsplit(pdf_texts$text[i], "\n")
  new_row <- data.frame(matrix(NA, nrow = 1, ncol = length(data_names)))
  colnames(new_row) <- data_names
  new_row$text <- pdf_texts$text[i]
  for (line in lines[[1]]) {
    matches <- regexpr(":\\s+(.+)", line)
    if (matches > 0) {
      data_name <- substr(line, 1, matches-1)
      data_value <- substr(line, matches+2, nchar(line))
      new_row[data_name] <- data_value
      if (!data_name %in% data_names) {
        data_names <- c(data_names, data_name)
      }
    }
  }
  missing_cols <- setdiff(colnames(data_df), colnames(new_row))
  if (length(missing_cols) > 0) {
    new_row[missing_cols] <- NA
  }
  data_df <- rbind(data_df, new_row)
}

# Remove text column
data_df <- data_df %>% 
  select(-text)

# Export ----

# Authorize
gs4_auth("my_email")

# Remove text column
data_df <- data_df %>% 
  select(-text)

# Google Sheets export
sheet_write(data_df, ss = "LINK", sheet = "scraper")
sheet_write(pdf_texts, ss = "LINK", sheet = "pdf_texts")

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
