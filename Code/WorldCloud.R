library("twitteR")
library("tm")
library("SnowballC")
library("wordcloud")
library("readr")
library("NLP")
library("RColorBrewer")

#Read the dataset into a variable
dataset <- read_csv("~\\Datasets\\dataset.csv")
#Store the 'text' column in a separate variable
tweet = dataset$text
#Clean the tweets using gsub:
#Remove control characters
tweet = gsub("[[:cntrl:]]", " ", tweet)
#Remove retweets
tweet <- gsub("(RT|via)((?:\\b\\W*@\\W+)+)", " ", tweet, ignore.case = T)
# Remove "@ person'
tweet <- gsub('@\\w+', '', tweet)
#Remove punctuations
tweet <- gsub("[[:punct:]]"," ", tweet)
#Remove digits
tweet <- gsub("[[:digit:]]"," ", tweet)
#Remove links
tweet <- gsub("http[s]?\\w+", " ", tweet)
#Remove unwanted spaces
tweet <- gsub("[ \t]{2,}", " ", tweet)
tweet <- gsub("^\\s+|\\s+$", " ", tweet)
#Remove NAs
tweet <- tweet[!is.na(tweet)]
#Remove all otheer insignificant symbols
tweet = gsub("^ ", "", tweet)
tweet = gsub(" $", "", tweet)
tweet = gsub("[^[:alnum:] ]", " ", tweet)
#Convert the text to lowercase
tweet = tolower(tweet)
#Store the text in a Corpus
tweet_corpus = Corpus(VectorSource(tweet))
#Remove stopwords
tweet_corpus = tm_map(tweet_corpus, removeWords, c("demonetization","demonetisation","rt","amp", stopwords("english")))
#Stem the text
tweet_corpus = tm_map(tweet_corpus, stemDocument)
#Generate the wordcloud
wordcloud(tweet_corpus, min.freq = 150, max.words = 300, random.order = 'F', rot.per = 0.2, colors = brewer.pal(8, "Dark2"), random.color = TRUE, scale = c(3,.3))
