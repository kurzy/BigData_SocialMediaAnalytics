# Social Media Data Collection and Analysis on topic of 'Amazon Alexa'.
# Jack Kearsley, jack.kearsley@outlook.com, 2018.

# Note: Set the working directory to the directory of this file (data_analysis.r).
# Note: Obtain Twitter and Facebook credentials to perform data collection.

# Install and load the required R packages.
install.packages("SocialMediaLab")
install.packages("slam")
install.packages("Rmpfr")
install.packages("tm")
install.packages("igraph")
install.packages("stringr")
install.packages("twitteR")
install.packages("SnowballC")
install.packages("Rfacebook")
install.packages("magrittr")
install.packages("gender")
install.packages("ggplot2")
install.packages('topicmodels')
install.packages('wordcloud')
install.packages('devtools')
install.packages('ggmap')
install.packages("pkg/Rstem_0.4-1.tar.gz", repos=NULL)
install.packages("pkg/sentiment_0.2.tar.gz", repos=NULL)

require("SocialMediaLab")
require("slam")
require("Rmpfr")
require("tm")
require("igraph")
require("stringr")
require("twitteR")
require("SnowballC")
require("plyr")
require("dplyr")
require("Rfacebook")
require("magrittr")
require("gender")
require("Rstem")
require("sentiment")
require("ggplot2")
require('topicmodels')
require('wordcloud')
require('devtools')
require('ggmap')
require('maps')

############################################ Start Data Collection ####################################################
# Please obtain your own Twitter and Facebook credentials to collect information from these social media sites.
# Otherwise, import previously collected data from the included .RData file.

# # Twitter credentials.
# tw_api_key <- ""
# tw_api_secret <- ""
# tw_access_token <- ""
# tw_access_token_secret <- ""
# 
# # Facebook credentials.
# fb_app_id <- ""
# fb_app_secret <- ""

# Authenticate with Twitter, and collect tweets containing relevant phrases.
# tw_cred <- Authenticate("twitter", apiKey=tw_api_key, apiSecret=tw_api_secret,
#                          accessToken=tw_access_token, accessTokenSecret=tw_access_token_secret)
# 
# search_query <- '"Amazon Echo" OR "Amazon Alexa"'
# tw_data <- Collect(credential=tw_cred,
#               searchTerm=search_query,
#                      numTweets=10, writeToFile=FALSE,
#                      verbose=TRUE, language="en")
# PLEASE NOTE: The Twitter data could not be saved as csv due to its list elements.
# The Twitter data 'tw_data' must be imported from the .RData file.

# Authenticate with Facebook, and collect data on the most recent posts by the Facebook page.
# fb_cred <- Authenticate("Facebook", appID=fb_app_id, appSecret=fb_app_secret)
# fb_data <- Collect(credential=fb_cred, pageName="AmazonEcho", writeToFile=FALSE, verbose=TRUE,
#                    rangeFrom="2017-11-01", rangeTo="2017-11-23")
# write.csv(fb_data, "FB_Data_Amazon_23-11-17.csv")

############################################ End Data Collection ####################################################

# Create an actor network for Twitter data.
tw_actor_net <- tw_data %>% Create("Actor")

# List the top 5 most influential Twitter users.
pageRank_tw_actor_net <- sort(page.rank(tw_actor_net)$vector, decreasing=TRUE)
head(pageRank_tw_actor_net, n=5)

# Function to remove unwanted text or characters from the tweets.
clean_text <- function(x) {
  x<-gsub("RT @\\w+: ", "", x)                          # Remove RT
  x<-gsub("@\\w+ ", "", x)                              # Remove @
  x<-gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", x)  # Remove http link
  x<-gsub("[ |\t]{2,}", "", x)                          # Remove tab
  x<-gsub("[[:digit:]]", "", x)                         # Remove numbers/digits
  x<-gsub("^ ", "", x)                                  # Remove blank spaces at the beginning
  x<-gsub(" $", "", x)                                  # Remove blank spaces at the end
  x<-gsub("https", "", x)                               # Remove RT
  x<-tolower(x)
  return(x)
}

# Create a corpus using the tweet text data, and filter out unwanted characters/strings.
# Remove irrelevant terms with removeWords.
tw_text <- iconv(tw_data$text, to="utf-8")
tw_text <- iconv(tw_text, "latin1", "ASCII", sub="")
docCorpus <- VCorpus(VectorSource(tw_text))
doctext <- docCorpus %>%
  tm_map(removeNumbers) %>%
  tm_map(content_transformer(clean_text)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeWords, stopwords(kind="en")) %>%
  tm_map(stemDocument) %>%
  tm_map(removeWords, c("Amazon", "Echo", "amazon", "echo", "alexa"))

# Find the 10 most common words used in the tweets.
dtm <- as.matrix(TermDocumentMatrix(doctext))
v <- sort(rowSums(dtm), decreasing=TRUE)
df <- data.frame(word=names(v), freq=v)
head(df, n=10)

# Count the number of retweets and original tweets.
paste0(sum(tw_data$isRetweet), " retweets")
paste0(sum(!tw_data$isRetweet), " original tweets")
tw_data$text <- iconv(tw_data$text, "latin1", "ASCII", sub="")

# Create semantic network for Twitter data.
tw_sem_net <- tw_data %>% Create("Semantic", removeTermsOrHashtags=c("Amazon", "Alexa", "Echo", "#amazon"))
# Save as graphml file for Gephi.
write.graph(tw_sem_net, "gephi/tw_sem_net.graphml", format="graphml")

# Part 2A - Question 3
# Find the 10 most important terms in the semantic network.
pageRank_tw_sem <- sort(page.rank(tw_sem_net)$vector, decreasing=TRUE)
head(pageRank_tw_sem, n=10)

# Create a bimodal graph for the Facebook data.
fb_bimodal <- Create(fb_data, "Bimodal")

# Part 2A - Question 4
# Count the number of likes/comments for each post.
fb_likes <- fb_data[fb_data$edgeType == "Like"]
fb_likes_count <- count(fb_likes, fb_likes$to)
# Remove the 'foo' row from the
fb_likes_count <- fb_likes_count[1:12,]

fb_comm <- fb_data[fb_data$edgeType == "Comment"]
fb_comm_count <- count(fb_comm, fb_comm$to)
fb_comm_count <- fb_comm_count[1:12,]

# Total likes and total comments.
length(E(fb_bimodal)$edgeType[which(E(fb_bimodal)$edgeType=="Like")])
length(E(fb_bimodal)$edgeType[which(E(fb_bimodal)$edgeType=="Comment")])

# Twitter community analysis with infomap (igraph)
imc <- infomap.community(tw_actor_net, nb.trials=3)
com_membership <- membership(imc)
com_distribution <- summary(as.factor(com_membership))
tail(sort(com_distribution), n=3)
com_list <- communities(imc)
# List the 4 largest communities
com_list[names(tail(sort(com_distribution),n=3))]
length(com_list)

# Community analysis using Louvain algorithm
tw_bimodal <- tw_data %>% Create("bimodal")
tw_undirected <- as.undirected(tw_bimodal, mode="collapse")
tw_louvain <- cluster_louvain(tw_undirected, weights=NA)
communities(tw_louvain)
sort(sizes(tw_louvain), decreasing=T)

# Community analysis using Girvan-Newman algorithm (edge betweenness)
tw_edgebetween <- cluster_edge_betweenness(tw_undirected)
communities(tw_edgebetween)
sort(sizes(tw_edgebetween), decreasing=T)
length(tw_edgebetween)

# Centrality analysis
cc <- clusters(tw_bimodal)
g3 <- induced_subgraph(tw_bimodal, which(cc$membership == which.max(cc$csize)))

# Degree measure of centrality.
sort(degree(g3, mode="in"), decreasing=T)[1:30]
sort(degree(g3, mode="out"), decreasing=T)[1:30]

# Closeness measure of centrality.
sort(closeness(g3), decreasing=F)[1:30]
sort(closeness(g3), decreasing=T)[1:30]

# Betweenness measure of centrality.
sort(betweenness(g3, directed=FALSE), decreasing=T)[1:30]

# Facebook actors gender analysis.
userNames <- V(fb_bimodal)$name
# Remove last names.
firstNames <- sub(" .*", "", userNames)
# Remove non-alphanumeric characters.
firstNames <- gsub("[^[:alnum:]-]", "", firstNames)
gender_predictions <- gender(firstNames, method="ssa")
summary(as.factor(gender_predictions$gender))

# Expressed as percentages:
# Female percentage.
paste0("Female: ", round(length(as.factor(which(gender_predictions$gender=="female"))) /
                           length(gender_predictions$gender) * 100, 1), "%")
# Male percentage.
paste0("Male: ", round(length(as.factor(which(gender_predictions$gender=="male"))) /
                         length(gender_predictions$gender) * 100, 1), "%")

# Get the followers & friends of the @Amazon Twitter page.
# This data has not been included in the .RData file because it requires a lot of memory.
# Retrieval of information for the large number of followers will take several minutes.  
setup_twitter_oauth(tw_api_key, tw_api_secret, tw_access_token, tw_access_token_secret)
amazon_user <- getUser('Amazon')
amazon_followers <- amazon_user$getFollowers()
amazon_friends <- amazon_user$getFriends()
amazon_neighbors <- union(amazon_followers, amazon_friends)
amazon_neighbors.df = twListToDF(amazon_neighbors)
View(amazon_neighbors.df)

# Plot the log of the followers vs friends for each account associated with @Twitter.
ggplot(data=amazon_neighbors.df, aes(x=log(followersCount), y=log(friendsCount))) + 
  geom_point(stat="identity") +
  xlab("Log Followers Count") + 
  ylab("Log Friends Count") +
  ggtitle("Friends vs Followers,\nfor Users Associated with the @Amazon Twitter (Log 10 Scale)")

# Assign log value to new variables, and put them in a new data frame.
logFriendsCount <- log(amazon_neighbors.df$friendsCount)
logFollowersCount <- log(amazon_neighbors.df$followersCount)
kObject.log <- data.frame(amazon_neighbors.df$name, logFollowersCount, logFriendsCount)

# Remove "-Inf" values.
kObject.log <- subset(kObject.log, kObject.log$logFriendsCount != "-Inf")
kObject.log <- subset(kObject.log, kObject.log$logFollowersCount != "-Inf")

# Run the k-means algorithm
user2Means.log <- kmeans(kObject.log[,2:3], centers=6, iter.max=10, nstart=100)
kObject.log$cluster = factor(user2Means.log$cluster)

# Plot the graph with coloured clusters.
ggplot(data=kObject.log, aes(x=logFollowersCount, y=logFriendsCount, colour=cluster)) +
  geom_point(stat="identity") +
  xlab("Log Followers Count") +
  ylab("Log Friends Count") +
  ggtitle("Friends vs Followers,\nfor Users Associated with the @Amazon Twitter (Log 10 Scale)") + 
  guides(color = guide_legend(override.aes = list(size=7)))

# Perform sentiment analysis on the Twitter data.
# Clean Twitter text first.
tw_text_clean <- tw_text
tw_text_clean <- clean_text(tw_text_clean)

# Classify tweets by emotion.
class_emo = classify_emotion(tw_text_clean, algorithm="bayes", prior=1.0)
emotion = class_emo[,7]

# Classify tweets by polarity.
class_pol = classify_polarity(tw_text_clean, algorithm="bayes")
polarity = class_pol[,4]
summary(as.factor(polarity))

# Data frame with sentiment results.
sent_df = data.frame(text=tw_text_clean, emotion=emotion, polarity=polarity, stringsAsFactors=FALSE)
# Sort data frame.
sent_df = within(sent_df, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=T))))
# Discard the 'unkown' classifications.
final_plot <- subset(sent_df, sent_df$emotion != "unknown")
summary(as.factor(final_plot$emotion))

# Plot the sentiment data.
ggplot(final_plot, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="Emotion Categories", y="Number of Tweets") +
  ggtitle("Sentiment Analysis of Tweets \n(Classification by emotion)")

# Plot the polarity data.
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdYlGn") +
  labs(x="Polarity Categories", y="Number of Tweets") +
  ggtitle("Sentiment Analysis of Tweets \n(classification by polarity)")

# LDA topic modelling to identify related terms.
tweetCorpus <- Corpus(VectorSource(tw_text_clean)) %>%
  tm_map(removeWords, stopwords(kind="en"))
dtmTopicModelling <- DocumentTermMatrix(tweetCorpus)
dtmTopicModelling <- removeSparseTerms(dtmTopicModelling, sparse=0.98)
term_tfidf <- tapply(dtmTopicModelling$v/row_sums(dtmTopicModelling)[dtmTopicModelling$i],
                     dtmTopicModelling$j, mean) * log2(nDocs(dtmTopicModelling) / col_sums(dtmTopicModelling > 0))
median_tfidf <- summary(term_tfidf)[3]
dtmTopicModelling <- dtmTopicModelling[, term_tfidf >= median_tfidf]
toRemove <- which(row_sums(dtmTopicModelling) == 0)
dtmTopicModelling <- dtmTopicModelling[row_sums(dtmTopicModelling) > 0,]

# Find the number of topics to generate (k).
harmonicMean <- function(logLikelihoods, precision=200L) {
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods, prec=precision) + llMed))))
}

# LDA parameters.
burnin = 1000
iter = 1000
keep = 50
sequ <- seq(2, 100, 10)
fitted_many <- lapply(sequ, function(k) LDA(dtmTopicModelling, k=k, method="Gibbs",
                                            control=list(burnin=burnin, iter=iter, keep=keep)))
logLiks_many <- lapply(fitted_many, function(L) L@logLiks[-c(1:(burnin/keep))])
hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))
plot(sequ, hm_many, type="l")
k <- sequ[which.max(hm_many)]
seedNum <- 42
lda <- LDA(dtmTopicModelling, k=k, method="Gibbs", control=list(burnin=burnin, iter=iter, keep=keep, seed=seedNum))
topTenTermsEachTopic <- terms(lda, 15)
View(topTenTermsEachTopic)

# Get some sample tweets from a topic number 'tNo' specified by a column in 'topTenTermsEachTopic'.
tNo <- 12
topicsProb <- topics(lda, 1)
topTenTermsEachTopic[,tNo]
topicTweets <- which(topicsProb==tNo)
tweetCorpus_LDA <- tweetCorpus[-toRemove]
topicTweetText <- as.list(tweetCorpus_LDA[topicTweets])
sampleTweets <- sample(topicTweetText, 30)

# Group tweets by day.
data_by_day <- tw_data %>% group_by(day=format(as.Date(created_at), format="%d")) 
sum(data_by_day$day==22)
sum(data_by_day$day==23)
data_by_day_counts = data.frame(day=c("22 Nov", "23 Nov"), count=c(sum(data_by_day$day==22), sum(data_by_day$day==23)))

# Plot tweets by day.
ggplot(data_by_day_counts, aes(x=day, y=count)) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette="RdGy") +
  ggtitle("Tweets About Amazon Echo/Alexa per Day") +
  xlab("Day") +
  ylab("Number of Tweets")

# Geographic analysis with spatial distribution of tweets.
# Get location information on users that made tweets, from each user's profile.
# Must wait a few seconds between calls to avoid Twitter rate limiting.
tw_data_users_loc <- twListToDF(lookupUsers(tw_data[1:100,11], includeNA=FALSE))$location
Sys.sleep(100)
tw_data_users_loc <- c(tw_data_users_loc, twListToDF(lookupUsers(tw_data[101:200, 11], includeNA=FALSE))$location)
Sys.sleep(100)
tw_data_users_loc <- c(tw_data_users_loc, twListToDF(lookupUsers(tw_data[201:300, 11], includeNA=FALSE))$location)
Sys.sleep(100)
tw_data_users_loc <- c(tw_data_users_loc, twListToDF(lookupUsers(tw_data[301:400, 11], includeNA=FALSE))$location)

# Remove empty entries and illegal characters.
tw_data_users_loc <- tw_data_users_loc[tw_data_users_loc != ""]
tw_data_users_loc <- iconv(tw_data_users_loc, to="utf-8")
tw_data_users_loc <- iconv(tw_data_users_loc, "latin1", "ASCII", sub="")
tw_data_users_loc_copy <- tw_data_users_loc
tw_data_users_loc_copy <- gsub("#\\w+", "", tw_data_users_loc_copy)
tw_data_users_loc_copy <- gsub("*@*", "", tw_data_users_loc_copy)
tw_data_users_loc_copy <- lapply(tw_data_users_loc_copy, clean_text)

# Get latitude/longitude data for each location, from Google Maps, may take a few minutes.
lonlat <- geocode(unlist(tw_data_users_loc_copy))

# Map of north/south america.
map <- get_map(location=c(long=-100, lat=20.0), zoom=2, maptype="terrain", source="google")
ggmap(map) + geom_point(data = lonlat, aes(x=lon, y=lat), color="pink", size=1, alpha=0.5)

# Map of europe/asia/africa/australia
map <- get_map(location=c(long=80, lat=20.0), zoom=2, maptype="terrain", source="google")
ggmap(map) + geom_point(data = lonlat, aes(x=lon, y=lat), color="red", size=1, alpha=0.5)


