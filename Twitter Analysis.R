#Twitter Sentiment Analysis Code for BU311
#start by installing required packages
# install.packages("rtweet")
# install.packages("stopwords")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("tidytext")
# install.packages("wordcloud")
# install.packages("devtools")
# install.packages("tidyverse")
# install.packages("stringr")
# install.packages("textdata")
# install.packages("googlesheets4")
# install.packages("sentimentr")
#load the libraries
library(rtweet)
library(stopwords)
library(dplyr)
library(tidyr)
library(tidytext)
library(wordcloud)
library(devtools)
library(tidyverse)
library(stringr)
library(textdata)
library(googlesheets4)
library(sentimentr)

#how big is the sample size per player?
n = 1000 #use caution with this, Twitter API limits to 18,000 total inquiries per 15 minutes so n = 1000 will take just over 15 minutes to complete

#read google spreadsheet
NFL_1 <- read_sheet('https://docs.google.com/spreadsheets/d/1fCas4eTkTEBQSt3a37o4BHIm_OQfgEvHi3lLHRrSlaA/edit?usp=sharing')
#separate first and last name for ease of search
NFL <- extract(NFL_1, Starting_QB, c("FirstName", "LastName"), "([^ ]+) (.*)")
#twitter authorization
auth_setup_default()
#make a list to store the sentiment scores in
sent_list = list()
sent_list = vector("list", length = 0)
# for qb in data set, search twitter for that qb and then store the tweets in a new csv file with the team abbreviation as the name.
for (Team in rownames((NFL))) {
  #these first variables are going through the NFL dataset for each iteration of the loop and getting renamed to the appropriate team.
  qbfirst <- (NFL[Team, "FirstName"])
  qblast <- (NFL[Team, "LastName"])
  team <- (NFL[Team, "Team"])
  name <- (NFL[Team, "Name"])
  #next we create a variable that stores our search term which is made up of the components we derive on a per team basis
  search <- paste(qbfirst, qblast, "OR", qblast , name, "-filter:retweets", "-filter:mentions", sep = " ")
  #search_tweets calls the twitter API and we do a search for n number of tweets. We store the tweets dataset for each team in a variable called tweets
  tweets <- search_tweets(q = search ,
                      n = n ,type = "recent",
                      includes_rts = FALSE, lang = "en", retryonratelimit = TRUE)
  #Twitter API stores a LOT of info so we remove all except the text. This also protects the users identity. 
  short_tweets <- subset.data.frame(tweets, select = c("full_text"))
  #Next we perform sentiment analysis which requires us to parse through each tweet and identify all words.
  tweet_sentiment <- sentiment((short_tweets$full_text), missing_value = 0)  %>% 
    group_by(element_id) %>% 
    summarise(meanSentiment = mean(sentiment))
  
  #we attatch the sentiment score for each tweet to the same dataframe
  short_tweets <- data.frame(short_tweets, tweet_sentiment)
  #Take the mean of each player's sentiment
  Sentiment_Score <- mean(short_tweets$meanSentiment)
  #Store the mean sentiments in order with the list we created earlier
  sent_list[[Team]] <- Sentiment_Score
  #Add the tweets and their scores to a new .csv for each team.
  write.csv(short_tweets, file = paste(team,".csv",sep = ""))

}
#Close the for loop and then call the list we created with all scores and append it to the existing NFL dataframe.
Sentiment_Score = do.call(rbind, sent_list)
NFL <- data.frame(NFL, Sentiment_Score)

write.csv(NFL, file = "Player_Sentiment.csv")

