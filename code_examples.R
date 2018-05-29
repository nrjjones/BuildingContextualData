################################################################################
### Environment setup
################################################################################

library(httr)          # cURL-like tools, command line web interface
library(jsonlite)      # Parses output from APIs
library(ggmap)         # Mapping and geocoding tools
library(tidyverse)     # Lots of data tools, https://www.tidyverse.org/packages/
library(exifr)         # Scrape lat/lon from cell phone photos
library(rvest)         # Scrape web pages

source("./secrets.R")          # Loads my Google API Key
source("./fun/get20results.R") # Loads function to get first 20 Google results
source("./fun/buildzipdf.R")   # Loads function to scrape zip code data

################################################################################
### Example 1: Scrape table from HTML
################################################################################

# Initialize data with first page

zip.df <- build.zip.df("http://zipatlas.com/us/wi/zip-code-comparison/population-below-poverty-level.htm")

# Loop through rest of pages
     # Structure of page link after page 1
     # http://zipatlas.com/us/wi/zip-code-comparison/population-below-poverty-level.2.htm

for(i in 2:8) {

  # For testing
  # i <- 2

  link <- paste0("http://zipatlas.com/us/wi/zip-code-comparison/population-below-poverty-level.", i, ".htm")
  tmp <- build.zip.df(link)
  zip.df <- bind_rows(zip.df, tmp)
}

# Reduce to main data
pov <- zip.df[c(2, 6)]
names(pov) <- c("zip", "poverty")
pov <- pov %>% filter(!is.na(poverty))

# Write it to a file
# write_csv(pov, "./out/poverty.csv")
read_csv("./out/poverty.csv")

# Merge with main data
df <- left_join(df, pov)

# Convert char variable to numeric and convert to real value
df$pov <- as.numeric(str_replace(df$poverty, " %", ""))/100

# Check the output
df %>% select(id, pov)


################################################################################
### Example 2: Get list of search results, map, calculate distances
################################################################################

df <- read_csv("./raw/dummydata.csv")

# ll <- geocode(df$address)

mdata <- get_map(location = "Dane County",
                 color = "color",
                 source = "google",
                 maptype = "terrain",
                 zoom = 9)

 df <- bind_cols(df, ll)

write_csv(df, "./out/geocodedummy.csv")
df <- read_csv("./out/geocodedummy.csv")

# Create zip5 version, but retain zip+4 version
df$zip.full <- df$zip
df$zip <- str_extract(df$zip, "^[0-9]{5}")
df %>% select(zip, zip.full)

ggmap(mdata,
      extent = "device",
      ylab = "Latitude",
      xlab = "Longitude") +
  geom_point(data=df, aes(x = lon, y = lat), color="blue", size=3, show.legend = FALSE)

co <- get20results("Coop grocery in Dane County")

write_csv(co, "./out/coop results.csv")

ggmap(mdata,
      extent = "device",
      ylab = "Latitude",
      xlab = "Longitude") +
  geom_point(data=df, aes(x = lon, y = lat), color="red", size=3, show.legend = FALSE) +
  geom_point(data=co, aes(x = lon, y = lat), color="blue", size=3, show.legend = FALSE)

# Set from to all found high schools, to to all first found Planned Parenthood

# Initialize
from <- df$address
to <- co$name[1]

# Mapdist to calculate distance and estimated duration
# First search hit
dist1 <- mapdist(
  from,
  to,
  mode="bicycling",
  output="simple"
)

dist <- dist1

# Mapdist to calculate distance and estimated duration
# 2 - N search hits
# Appends to end of the first hit to create a dataset

for(i in 2:length(co$name)) {

  to <- co$name[i]

  disti <- mapdist(
    from,
    to,
    mode="bicycling",
    output="simple"
  )
  dist <- bind_rows(dist, disti)
}

write_csv(dist, "./out/co_distances.csv")

dist <- read_csv("./out/co_distances.csv")

# Sort by originating address, then distance in miles (ascending)
# Group by orginating address
# Keep just the top entry creating a dataset of the shortest distances
# Add
closest <- dist %>%
  arrange(from, miles) %>%
  group_by(from) %>%
  filter(row_number()==1) %>%
  ungroup()

cl_coop <- closest %>%
  select(from, to, miles) %>%
  transmute(
    address = from,
    closest = to,
    miles = miles
  )

coop_dist <- left_join(df, cl_coop)

table(coop_dist$closest)


ggmap(mdata,
      extent = "device",
      ylab = "Latitude",
      xlab = "Longitude") +
  geom_point(data=df, aes(x = lon, y = lat), color="red", size=1, show.legend = FALSE) +
  geom_point(data=co, aes(x = lon, y = lat), color="blue", size=3, show.legend = FALSE)

# Map
ggmap(mdata,
      extent = "device",
      ylab = "Latitude",
      xlab = "Longitude") +
  geom_point(data=df, aes(x = lon, y = lat), size=2) +
  scale_color_continuous(low="green", high="red") +
  geom_point(data=uc, aes(x = lon, y = lat), color="orange", size=2, show.legend = FALSE)

################################################################################
### Example 3: Get list and count by criteria
################################################################################

uc <- get20results("Urgent care clinic in Dane County")

write_csv(uc, "./out/uc results.csv")

# Initialize
from <- df$address
to <- uc$address[1]

# Mapdist to calculate distance and estimated duration
# First search hit
dist1 <- mapdist(
  from,
  to,
  mode="driving",
  output="simple"
)

dist <- dist1

# Mapdist to calculate distance and estimated duration
# 2 - N search hits
# Appends to end of the first hit to create a dataset

for(i in 2:length(uc$name)) {

  to <- uc$address[i]

  disti <- mapdist(
    from,
    to,
    mode="driving",
    output="simple"
  )
  dist <- bind_rows(dist, disti)
}

write_csv(dist, "./out/uc distances.csv")

ggmap(mdata,
      extent = "device",
      ylab = "Latitude",
      xlab = "Longitude") +
  geom_point(data=df, aes(x = lon, y = lat), color="blue", size=3, show.legend = FALSE) +
  geom_point(data=uc, aes(x = lon, y = lat), color="red", size=3, show.legend = FALSE)

dist <- read_csv("./out/uc distances.csv")

# Sort by originating address, then distance in miles (ascending)
# Group by orginating address
# Keep just the top entry creating a dataset of the shortest distances
# Add
within25mins <- dist %>%
  arrange(from, minutes) %>%
  group_by(from) %>%
  filter(minutes<25) %>%
  transmute(
    uc_num = n()
    ) %>%
  group_by(from) %>%
  filter(row_number()==1)

write_csv(within25mins, "./out/uc_density.csv")



