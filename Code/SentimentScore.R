library("twitteR")
library("tm")
library("SnowballC")
library("ggplot2")
library("readr")
library("NLP")
library("plyr")
library("stringr")
library("RCurl")

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

score.sentiment <- function(sentences, positive_words, negative_words, .progress='none')
{
  require(plyr)   #forlaply
  require(stringr)  #for str_split
  #list+array+apply=laply
  scores = laply(sentences, function(sentence, positive_words, negative_words) {
    #Split into words
    wordlist = str_split(sentence, '\\s+')
    #Unlist and have as separate words to work on
    word = unlist(wordlist)
    #Compare the words in our file with the list of positive and negative words
    negativematches = match(word, negative_words)
    positivematches = match(word, positive_words)
    #The above match() function returns only the position of the matched terms
    #So, store the words which are not NA, that is, store only the matched words
    positivematches <- !is.na(positivematches)
    negativematches <- !is.na(negativematches)
    #Net score is the differrence between positive and negative matches
    score = sum(positivematches)-sum(negativematches)
    return(score)
  }, positive_words, negative_words, .progress=.progress )
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}
positive <- scan('~OpinionLexicon\\positive-words.txt', what='character', comment.char=';')
negative <- scan('~OpinionLexicon\\negative-words.txt', what='character', comment.char=';')

#Retrieve scores
tweetanalysis <- score.sentiment(tweet, positive, negative, .progress="none")
#Add a component called 'sentiment' to 'tweetanalysis' which stores corresponding sentiment based on score
tweetanalysis$sentiment[tweetanalysis$score == 0] = "Neutral"
tweetanalysis$sentiment[tweetanalysis$score > 0] = "Positive"
tweetanalysis$sentiment[tweetanalysis$score < 0] = "Negative"
tweetanalysis$sentiment <- factor(tweetanalysis$sentiment)
#Make a table of the score and count
scoretable = table(tweetanalysis$score)
score = tweetanalysis$score
#Calculate basic statistical measures of central tendency
mean = mean(score)
median = median(score)
summary(tweetanalysis$sentiment)
#Plot the bar graph with suitable titles
ggplot(data = tweetanalysis, aes(x = score, fill = sentiment)) + 
   geom_bar()+labs(title = "Sentiment Score Bar Plot", x = "Sentiment Score", y = "Tweet Count") +
  scale_x_continuous(breaks = seq(-6,6,1)) + scale_y_continuous(breaks = seq(0,4000,500)) + 
  scale_fill_manual(guide = guide_legend("Sentiment"), values = c("#f91400","#042bdd","#04dd28"))

