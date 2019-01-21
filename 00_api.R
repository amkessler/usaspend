# https://datalab.usaspending.gov/api-guide.html

library(jsonlite)
library(dplyr)

# Set the url to the endpoint which will be the target of the request and attempt to retrieve the first page of data. 
# The first page is requested to check the connection and data quality, we will have to request the first page 
# again in the loop below
# 
# In this script the limit of records per request (aka a page) is set to 100. You may change this setting depending on your connection strength and the level of sophistication in the query you may be running

url <- "https://api.usaspending.gov/api/v1/awards/?limit=20"
pages <- list()
API_data <- fromJSON(url, flatten=TRUE)
API_data$page_metadata$has_next_page=TRUE


# Now paginate through the rest of the data, this may take some time…

i<-1
while(API_data$page_metadata$has_next_page==TRUE) {
  API_data <- fromJSON(paste0(url, "&page=", i), flatten=TRUE)
  message("Retrieving page ",i)
  pages[[i]] <- API_data$results
  i<-i+1
}

# Now bind the data into a dataframe and write it to a csv in your working directory, 
# congratulations you’ve requested and retrieved data from an endpoint!
  
USAspendingData <- bind_rows(pages)
write.csv(USAspendingData, file="USAspendingData.csv", row.names = FALSE)



# To apply filters to the GET request they can be included directly into the target url

# For example, if we only need records that are of award type “A” and has a piid that equals “LB01” we can use 
# the following url to filter on those two field/value combinations. The rest of the script remains unchanged

# url <- "https://api.usaspending.gov/api/v1/awards/?limit=100&type=A&piid=LB01"