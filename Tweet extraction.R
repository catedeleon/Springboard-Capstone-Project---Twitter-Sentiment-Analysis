library(twitteR)

# Extract tweets
# Save as .csv

requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumer_key <- 'bcYjokHoaGcYnKqGisZDhT6Pw'
consumer_secret <- 'rj72U5PnPqpcMRmeRRIz1a8p61kid3KN8XhWIgqsiUEaopnZj2'
access_token <- '32861039-x0q6cNKXyciSjxO3HDNr6L4KK29AJXI7JKRtDZYSl'
access_secret <- 'YIzxg6YMsJf6spQvQuH8vJnWjoLJhDw32OusxJ5KEygI7'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

tweets <- searchTwitteR("cosrx",n=10000,lang='en')
tweetsDF <- twListToDF(tweets)
write.csv(tweetsDF, "tweetsDF.csv")