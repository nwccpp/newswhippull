# load libraries
library(httr)
library(jsonlite)
library(devtools)
library(urltools)
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
source_gist(4205477)

#####################################
#####upload thenecessary files#######
# read in the MBFC_V3 updated data
load("XXXXX/MBFC_V3.Rda")
###
# read in the state media list
###
load("XXXXX/state_media.Rda")
###
# read in the list from Lia on other questionable sites
###
load("XXXXXXX/other_fn.Rda")

#######################################
# script to get climate change related# 
# articles from                       #
# newswhip database using API         #
# first we call the API to get the    #
# articles we are interested in       #
#######################################

##########################

#######################
# variables to change #
#######################

# below is the variable that needs to be changed
# the only change that needs to be made is the
# date range in the format 'YYYY-MM-DD'
# number of maximum articles is 5000

###########################################
# calling the newship API to get articles #
###########################################

# API key: XXXXXXXX
# setup API key and set the URL of the endpoint
api_key <- 'XXXXXXX'
# create a variable that tells the API where to go
# this is from the NewsWhip help file
api_endpoint <- paste0('https://api.newswhip.com/v1/articles?key=', api_key)

# make a function to get articles matching a given query
get_newswhip_articles <- function(api_key, limit, start_time, end_time) {
  api_endpoint <- paste0('https://api.newswhip.com/v1/articles?key=', api_key)          
  data <- paste0('{\"filters\": [\"language:en AND 
  country_code:us AND ((climate change) 
  OR (climate crisis) OR (climate effects) 
  OR (climate hoax) OR (climate policy) 
  OR (climate resilience) OR (climate science) 
  OR (climate summit) OR (global warming) 
  OR (greenhouse gas) OR (greenhouse gases) 
  OR (IPCC) OR (green energy) OR (climate hypocrisy) 
  OR (paris agreement) OR (paris climate) OR (net zero) 
  OR (net-zero) OR (COP26) OR (climate conversation) 
  OR (climate test) OR (climate gap) OR (climate activists) 
  OR (climate activist) OR (clean energy) 
  OR (climate negotiations) OR (climate deal) 
  OR (green new deal) OR (climate conference) 
  OR (green technology) OR (green tech) 
  OR (climate fearmongering) OR (climate fears) 
  OR (climate anxiety) OR (carbon capture))\"],
                           \"size\": ', limit, ', 
                           \"from\": ', start_time, ',
                           \"to\": ', end_time, ',
                           \"search_full_text\": false,
                           \"find_related\": false}')
  r <- httr::POST(api_endpoint, body = data)
  httr::stop_for_status(r)         
  jsonlite::fromJSON(httr::content(r, "text", encoding = "UTF-8"), flatten = TRUE)$articles          
}

#### variables to change 
#### YYYY-MM-DD format
days<-as.character(as.Date(as.Date("2021-01-01"):as.Date("2022-03-30"), origin="1970-01-01"))

### number of articles pull per day maximum is 5000
num_articles <- 5000

#### run the loop for daily pulls in the time frame
#### to change daily to any other time frame change 
#### end time to + (# of days)
mylist <- list()
for (i in days) {
  print("now running days:")
  print (i)
  start_time <- as.numeric(as.POSIXct(paste(i, "00:00:00 EST", sep=" "))) * 1000
  end_time <- as.numeric(as.POSIXct(paste(as.Date(paste(i))+1,  "00:00:00 EST", sep=" "))) * 1000 - 1
  data_temp <- get_newswhip_articles(api_key = api_key, limit = num_articles, start_time = start_time, end_time = end_time)
  data_temp$date_time <- as.POSIXct(round(data_temp$publication_timestamp/1000), origin="1970-01-01")
  data_temp$date <- as.Date(as.POSIXct(round(data_temp$publication_timestamp/1000), origin="1970-01-01"))
  data_temp$relatedStories <- NULL
  data_temp$topics <- NULL
  data_temp$authors <- NULL
  data_temp$entities <- NULL
  data_temp$videos <- NULL
  try(data_temp<- data_temp %>% dplyr::select(delta_time, 
                                              recent_fb_counts, 
                                              recent_tw_counts, 
                                              predicted_interactions, 
                                              predicted_timestamp, 
                                              uuid, 
                                              publication_timestamp, 
                                              link, 
                                              headline, 
                                              excerpt, 
                                              keywords, 
                                              image_link, 
                                              has_video, 
                                              nw_score, 
                                              max_nw_score, 
                                              fb_data.total_engagement_count, 
                                              fb_data.likes, 
                                              fb_data.comments, 
                                              fb_data.shares, 
                                              fb_data.total_count_delta, 
                                              fb_data.delta_period, 
                                              fb_data.delta_period_unit, 
                                              tw_data.tw_count, 
                                              tw_data.total_count_delta, 
                                              tw_data.delta_period, 
                                              tw_data.delta_period_unit, 
                                              li_data.li_count, 
                                              li_data.total_count_delta, 
                                              li_data.delta_period, 
                                              li_data.delta_period_unit, 
                                              pi_data.pi_count, 
                                              pi_data.delta_period, 
                                              pi_data.delta_period_unit, 
                                              source.publisher, 
                                              source.domain, 
                                              source.link, 
                                              source.country, 
                                              source.country_code, 
                                              source.language, 
                                              date_time, 
                                              date))
  mylist[[i]] <- data_temp
}

data_temp <- do.call("rbind",mylist)%>%data.frame()
write.csv(data_temp, file="XXXXX.csv")
save(data_temp, file="XXXXXX.Rda")

#####to code for MBFC and other media bias follow below steps
data_temp$url_to_test <- paste(suffix_extract(domain(data_temp$link))$domain, suffix_extract(domain(data_temp_sam$link))$suffix, sep = ".")

# ####tests
data2 <- left_join(data_temp, MBFC_V3 %>% select(Media, MBFC_category), by = c("source.publisher" = "Media"))
data3 <- left_join(data2, MBFC_V3 %>% select(domain, MBFC_category), by = c("source.publisher" = "domain"), suffix = c(".1", ".2"))
data4 <- left_join(data3, MBFC_V3 %>% select(Media, MBFC_category), by = c("url_to_test" = "Media"))
data5 <- left_join(data4, MBFC_V3 %>% select(domain, MBFC_category), by = c("source.publisher" = "domain"), suffix = c(".3", ".4"))

data5$MBFC_category.1[is.na(data5$MBFC_category.1)] <- "not_listed"
data5$MBFC_category.2[is.na(data5$MBFC_category.2)] <- "not_listed"
data5$MBFC_category.3[is.na(data5$MBFC_category.3)] <- "not_listed"
data5$MBFC_category.4[is.na(data5$MBFC_category.4)] <- "not_listed"

data5$MBFC_category <- "not_listed"
data5$MBFC_category[data5$MBFC_category.1 != "not_listed"] <- data5$MBFC_category.1[data5$MBFC_category.1 != "not_listed"]
data5$MBFC_category[data5$MBFC_category == "not_listed" & data5$MBFC_category.2 != "not_listed"] <- data5$MBFC_category.2[data5$MBFC_category == "not_listed" & data5$MBFC_category.2 != "not_listed"]
data5$MBFC_category[data5$MBFC_category == "not_listed" & data5$MBFC_category.3 != "not_listed"] <- data5$MBFC_category.3[data5$MBFC_category == "not_listed" & data5$MBFC_category.3 != "not_listed"]
data5$MBFC_category[data5$MBFC_category == "not_listed" & data5$MBFC_category.4 != "not_listed"] <- data5$MBFC_category.4[data5$MBFC_category == "not_listed" & data5$MBFC_category.4 != "not_listed"]

data5$MBFC_category.1 <- NULL
data5$MBFC_category.2 <- NULL
data5$MBFC_category.3 <- NULL
data5$MBFC_category.4 <- NULL

# ########################################################
# # creating a variable for whether or not               #
# # NewsWhip has marked the story as related to politics #
# ########################################################

data5$is_politics <- FALSE

for(i in 1:dim(data5)[1]){
  to_test <- data5$topics[[i]]
  if(dim(to_test)[1] >0) data5$is_politics[i] <- 20%in%to_test$id
}


# #######################################################################
# # testing the NewsWhip data against state_media to categorize stories #
# #######################################################################

# # test if the newswhip publisher is in the URL list for all of the various categories
data5$state_media <- "no"
data5$state_media[data5$source.publisher %in% state_media$site] <- "yes"
data5$state_media[data5$state_media == "no" & data5$source.publisher %in% state_media$domain] <- "yes"

# # test if the newswhip publisher is in the domain list for all of the various categories

data5$state_media[data5$state_media == "no" & data5$url_to_test %in% state_media$site] <- "yes"
data5$state_media[data5$state_media == "no" & data5$url_to_test %in% state_media$domain] <- "yes"


# #####################################################################################
# # testing the NewsWhip data against aggregated fake news list to categorize stories #
# #####################################################################################


# # test if the newswhip publisher is in the URL list for all of the various categories
data5$other_fake <- "no"
data5$other_fake[data5$source$publisher %in% other_fn$domain[other_fn$fakes_union==1]] <- "yes"

# # test if the newswhip publisher is in the domain list for all of the various categories
data5$other_fake[data5$other_fake == "no" & data5$url_to_test %in% other_fn$domain[other_fn$fakes_union==1]] <- "yes"

# #####################
# # create a comprehensive category
# #####################

data5$main_category <- "no category"
data5$main_category[data5$main_category=="no category" & data5$state_media=="yes"] <- "State media"
data5$main_category[data5$main_category=="no category" & data5$MBFC_V3_category!="not_listed"] <- data5$MBFC_V3_category[data5$main_category=="no category" & data5$MBFC_V3_category!="not_listed"]
data5$main_category[data5$main_category=="no category" & data5$other_fake=="yes"] <- "Other fake"

write.csv(data5, file="XXXX.csv")
save(data5, file="XXXXX.Rda")

