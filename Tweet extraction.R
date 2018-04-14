library(twitteR)

# Extract tweets
# Save as .csv

tweets <- searchTwitteR("cosrx",n=10000,lang='en')
tweetsDF <- twListToDF(tweets)
write.csv(tweetsDF, "tweetsDF.csv")