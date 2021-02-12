# Install
print("start")
install.packages("tm", repos = "http://cran.us.r-project.org")  # for text mining
print("second")
install.packages('plyr', repos = "http://cran.us.r-project.org")
print("third")
install.packages("SnowballC", repos = "http://cran.us.r-project.org") # for text stemming
print("fourth")
install.packages("wordcloud", repos = "http://cran.us.r-project.org") # word-cloud generator 
print("fifth")
install.packages("RColorBrewer", repos = "http://cran.us.r-project.org") # color palettes
print("sixth")
install.packages("syuzhet", repos = "http://cran.us.r-project.org") # for sentiment analysis
print("seventh")
install.packages("ggplot2", repos = "http://cran.us.r-project.org") # for plotting graphs
print("done")
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")
print("Hello")
#text <- readLines(file.choose())
# Load the data as a corpus
#TextDoc <- Corpus(VectorSource(text))
text <- readLines("C:/Users/akhil/Downloads/commitmsgs.csv")
TextDoc <- Corpus(VectorSource(text))
size <- (length(text)) -1
#Replacing "/", "@" and "|" with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")
# Convert the text to lower case
TextDoc <- tm_map(TextDoc, content_transformer(tolower))
# Remove numbers
TextDoc <- tm_map(TextDoc, removeNumbers)
# Remove english common stopwords
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
# Remove your own stop word
# specify your custom stopwords as a character vector
TextDoc <- tm_map(TextDoc, removeWords, c("commit","update","readm","readmemd", "team")) 
# Remove punctuations
TextDoc <- tm_map(TextDoc, removePunctuation)
# Eliminate extra white spaces
TextDoc <- tm_map(TextDoc, stripWhitespace)
# Text stemming - which reduces words to their root form
TextDoc <- tm_map(TextDoc, stemDocument)


print("cleaned")

# Build a term-document matrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)
# Sort by descearing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 5 most frequent words
head(dtm_d, 5)

# Plot the most frequent words
barplot(dtm_d[1:5,]$freq, las = 2, names.arg = dtm_d[1:5,]$word,
        col ="lightgreen", main ="Top 5 most frequent words",
        ylab = "Word frequencies")

#generate word cloud
set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))


# regular sentiment score using get_sentiment() function and method of your choice
# please note that different methods may have different scales
syuzhet_vector <- get_sentiment(text, method="syuzhet")
# see the first row of the vector
head(syuzhet_vector,10)
# see summary statistics of the vector
summary(syuzhet_vector)

# bing
bing_vector <- get_sentiment(text, method="bing")
head(bing_vector)
summary(bing_vector)
#affin
afinn_vector <- get_sentiment(text, method="afinn")
head(afinn_vector)
summary(afinn_vector)

# run nrc sentiment analysis to return data frame with each row classified as one of the following
# emotions, rather than a score: 
# anger, anticipation, disgust, fear, joy, sadness, surprise, trust 
# It also counts the number of positive and negative emotions found in each row
d<-get_nrc_sentiment(text)
# head(d,10) - to see top 10 lines of the get_nrc_sentiment dataframe
head (d,10)
#angry_items <- which(d$anger > 0)
#text[angry_items]
#
#trust_items <- which(d$trust > 0)
#trust <- text[trust_items]
#print(trust[10:20])
#
#
#fear_items <- which(d$fear > 0)
#text[fear_items]
#
#joy_items <- which(d$joy > 0)
#text[joy_items]
#
#disgust_items <- which(d$disgust > 0)
#text[disgust_items]
#
#
#sadness_items <- which(d$sadness > 0)
#text[sadness_items]
#
#anticipation_items <- which(d$anticipation > 0)
#text[anticipation_items]
#
#surprise_items <- which(d$surprise > 0)
#text[surprise_items]
#

td<-data.frame(t(d))
#The function rowSums computes column sums across rows for each level of a grouping variable.
td_new <- data.frame(rowSums(td[2:size]))
#Transformation and cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
#Plot One - count of words associated with each sentiment
quickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Commit Message Sentiments")

print(colSums(prop.table(d[, 1:8])))
#Plot two - count of words associated with each sentiment, expressed as a percentage
barplot(
  sort(colSums(prop.table(d[, 1:8]))), 
  horiz = FALSE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Text", xlab="Emotions", ylab="Percentage"
) 


