# https://rpubs.com/cenuno/USAspendingAPI

# load necessary packages
library( httr )
library( jsonlite )
library( compiler )
library( purrr )
# compilePKGS()
setCompilerOptions(suppressAll = TRUE )
# enableJIT enables or disables just-in-time (JIT) compilation. 
# JIT is disabled if the argument is 0. 
# If level is 3, all top level loops are compiled before they are executed.
enableJIT(3) # 3 will appear


# The DATA ACT API website api.usaspending.gov. The website hosts a variety of paths, labeled as “endpoints”. 
# These endpoints host different categories of data tables. This tutorial uses the “/api/v1/transactions/” endpoint.




################### General Purpose GET Function ############################################
#############################################################################################
GET_all_pages <- function( PATH, QUERY ) {
  # Create empty list
  pages <- list()
  # Create initial API url
  url <- modify_url("https://api.usaspending.gov"
                    , path = PATH
                    , query = QUERY
  )
  # Get API url
  raw.resp <- GET(url)
  if (http_type(raw.resp) != "application/json") {
    stop("API did not return json. Check 'status code = 200'"
         , call. = FALSE)
  }
  this.char.resp <- rawToChar( raw.resp$content) # convert from raw to char
  # convert JSON object into R object
  this.clean.resp <- fromJSON(this.char.resp
                              , flatten = TRUE
  ) 
  # Set initial page number
  page_number <- 1
  
  # conditional element selection
  # if results page is does not have a next page
  # return a data frame for these results
  if( this.clean.resp$page_metadata$has_next_page == FALSE ){
    return( this.clean.resp$results)
  }
  # while loop with boolean condition
  # if the results page contains a next page
  # call the next page and bind the results to a data frame
  # return the data frame with all the page results
  while( this.clean.resp$page_metadata$has_next_page == TRUE ) {
    # identify current page url
    current.page.url <- this.clean.resp$page_metadata$current
    # subsitute "&page=XX" with "&page='page_number'"
    next.page.url <- gsub( pattern = "&page=[[:digit:]]+"
                           , replacement = paste0( "&page=", page_number)
                           , x = current.page.url
    )
    # Get new API url
    raw.resp <- GET( url = next.page.url )
    # Convert raw vector to character vector
    this.char.resp <- rawToChar( raw.resp$content )
    # Convert JSON object into R object
    this.clean.resp <- fromJSON( this.char.resp
                                 , flatten = TRUE
    )
    # For every page number (1, 2, 3...), insert that page's "results" inside the list
    pages[[ page_number ]] <- this.clean.resp$results
    # Add to the page number and restart the loop
    page_number <- page_number + 1
  }
  # once all the pages have been collected,
  data_api_data <- rbind_pages(pages) # rbind.pages() is deprecated
  # return what we've collected
  return( data_api_data )
  
  
  # Turn API errors into R errors
  if (http_error( raw.resp )) {
    stop(
      sprintf(
        "USASpending.gov API request failed [%s]\n%s\n<%s>", 
        status_code( raw.resp),
        this.clean.resp$message,
        this.clean.resp$documentation_url
      ),
      call. = FALSE
    )
  }
  # add some structure stuff 
  structure(
    list(
      content = this.clean.resp
      , path = PATH
      , response = raw.resp
    )
    , class = "usa_spending_api"
  )
} # end of function



################### City, State API Function #####################################
city_state <- function( CITY_NAME, STATE_NAME ) {
  api_data <- GET_all_pages( PATH = "/api/v1/transactions/"
                             , QUERY = list(place_of_performance__city_name = CITY_NAME
                                            , place_of_performance__state_name = STATE_NAME
                             )
  )
  # replace character(0) with NAs functions
  char0_to_NA <- function( list_object ) {
    # set counter
    i <- 1
    # create empty list vector
    emptyList <- list()
    # while loop
    while( i <= length( list_object )) {
      if (identical(list_object[[i]], character(0))) {
        emptyList[[i]] <- NA
      } else {
        emptyList[[i]] <- list_object[[i]]
      } # end of if statement
      # add to counter
      i <- i + 1
    } # end of while statement
    # return empty character vector
    return( emptyList )
  } # end of function
  #
  # Replace the character(0) values with NA
  # in the 'recipient.business_categories' column
  #
  api_data$recipient.business_categories <- char0_to_NA( api_data$recipient.business_categories)
  # 
  # Replace the list with character vector 
  # of the first element of the list
  api_data$recipient.business_categories_1 <- unlist( purrr::map( api_data$recipient.business_categories, 1) )
  # Create a placeholder variable
  api_data$recipient.business_categories_2 <- NA 
  # Create a dataframe with rows that contain a list
  # with only 2 elements
  ad_2 <- api_data[ lapply(api_data$recipient.business_categories
                           , length) == 2
                    ,  ]
  # Fill in the placeholder variable with the second element of the list
  ad_2$recipient.business_categories_2 <- unlist( purrr::map( ad_2$recipient.business_categories, 2) )
  # create a dataframe with rows that DO NOT contain a list
  # with only 2 elements
  ad_3 <- api_data[ !lapply(api_data$recipient.business_categories
                            , length) == 2
                    ,  ]
  # Rename api_data to be the result of an rbind.data.frame
  # from ad_2 and ad_3
  api_data_final <- rbind.data.frame( ad_2, ad_3)
  # relabel the row names
  rownames( api_data_final ) <- as.character(1:nrow(api_data_final))
  # make the list variable 'recipient.business_categories' NULL
  # hereby removing it from the returned dataframe
  api_data_final <- subset( x = api_data_final
                            , select = -recipient.business_categories
  )
  # convert na to "marked_unknown/other"
  # for recipient.business_categories_1
  api_data_final <- within(api_data_final, recipient.business_categories_1 <-
                             ifelse( test = is.na(recipient.business_categories_1)
                                     , yes = "marked_unknown/other"
                                     , no = recipient.business_categories_1
                             )
  )
  # convert na to "marked_unknown/other"
  # for recipient.business_categories_2
  api_data_final <- within(api_data_final, recipient.business_categories_2 <-
                             ifelse( test = is.na(recipient.business_categories_2)
                                     , yes = "marked_unknown/other"
                                     , no = recipient.business_categories_2
                             )
  )
  # check if column "funding_agency"
  if( "funding_agency" %in% names(api_data_final) == TRUE ) {
    # delete the column "funding_agency"
    api_data_final <- subset( x = api_data_final
                              , select = -funding_agency
    )
  }
  return( api_data_final )
} # end of function




##### NOW FOR PULLING THE DATA #####

### For Austin, TX ####
austin_tx <- city_state("AUSTIN", "TEXAS")
dim(austin_tx)
