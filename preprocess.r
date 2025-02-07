
# Import necessary libraries
library(dplyr)  # For data manipulation
library(tidyr)  # For data reshaping
library(scales) # For data scaling
library(googledrive)

# Authenticate with Google account
drive_auth()

folder_id <- drive_find(pattern = "psrs-machine-learning", type = "folder")$id

# Create a data directory
dir.create("/content/data/", recursive = TRUE)

# Download the dataset if it's not already present
if (!file.exists("/content/data/global-historical-climatology-network.zip")) {
  download.file(
    "https://www.kaggle.com/api/v1/datasets/download/noaa/global-historical-climatology-network", # URL of the dataset on Kaggle
    destfile = "/content/data/global-historical-climatology-network.zip", # Local destination for the downloaded file
    method = "auto",  # Let R choose the appropriate download method
    mode = "wb"  # Write the file in binary mode
  )
}

# Set the working directory to the data directory
setwd("/content/data")

# Read the data from the zip file
rawdata <- read.csv(unz("global-historical-climatology-network.zip",
                        "ghcn-m-v1.csv"), header = T) # Read the 'ghcn-m-v1.csv' file within the zip archive

head(rawdata)

# Tidy the data
procdata <- rawdata %>%
  gather(lon, temp, -(year:lat)) %>%  # Reshape the data from wide to long format
  filter(temp != -9999)  # Remove rows with missing temperature values (-9999)

# Extract latitude and longitude signs and values
procdata$lat_sign <- 1  # Initialize latitude sign to 1 (North)
procdata$lat_sign[grep("S", procdata$lat)] <- -1  # Set latitude sign to -1 for South
procdata$lon_sign <- 1  # Initialize longitude sign to 1 (East)
procdata$lon_sign[grep("W", procdata$lon)] <- -1  # Set longitude sign to -1 for West

# Clean latitude and longitude values
procdata$lat <- sub("[NS]", "", procdata$lat)  # Remove N/S from latitude
procdata$lon <- sub("[EW]", "", procdata$lon)  # Remove E/W from longitude
procdata$lon <- sub("lon_", "", procdata$lon)  # Remove 'lon_' prefix from longitude

# Calculate latitude, longitude, and time
procdata <- procdata %>%
  separate(lat, c("lat1", "lat2"), convert = T) %>%  # Split latitude into two parts
  separate(lon, c("lon1", "lon2"), convert = T) %>%  # Split longitude into two parts
  mutate(lat = lat_sign * (lat1 + lat2)/2,  # Calculate latitude
         lon = lon_sign * (lon1 + lon2)/2,  # Calculate longitude
         time = year + (month-1)/12) %>%  # Calculate time as year + fraction of year
  select(year, month, lat, lon, temp)  # Select the desired columns # time

head(procdata)

# Combine year and month into a date column
procdata$date <- make_date(year = procdata$year, month = procdata$month, day = 1) # Assumes day 1 for each month

write.csv(procdata, file = "/content/data/climate_data.csv", row.names = FALSE)

# Upload the dataframe to Google Drive
drive_upload(
  media = "/content/data/climate_data.csv", # Path to the CSV file
  path = as_id(folder_id), # Upload to the specified folder ID
  name = "climate_data.csv", # Name of the file on Google Drive
  overwrite = TRUE # Overwrite the file if it already exists
)