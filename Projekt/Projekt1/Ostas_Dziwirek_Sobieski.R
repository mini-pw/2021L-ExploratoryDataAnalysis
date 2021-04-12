library(dplyr)
library(gutenbergr)

# Install
#install.packages("tm")  # for text mining
#install.packages("SnowballC") # for text stemming
#install.packages("wordcloud") # word-cloud generator 
#install.packages("RColorBrewer") # color palettes
#install.packages("syuzhet") # for sentiment analysis
#install.packages("ggplot2") # for plotting graphs

# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")

bible <- gutenberg_download(30)
bible <- bible[,2]
View(bible)

# 1.

TextDoc <- Corpus(VectorSource(bible))
View(TextDoc)

# 2. 

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
TextDoc <- tm_map(TextDoc, removeWords, c("s", "company", "team")) 
# Remove punctuations
TextDoc <- tm_map(TextDoc, removePunctuation)
# Eliminate extra white spaces
TextDoc <- tm_map(TextDoc, stripWhitespace)
# Text stemming - which reduces words to their root form
TextDoc <- tm_map(TextDoc, stemDocument)

# 3. 

# Build a term-document matrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)
# Sort by descearing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 5 most frequent words
head(dtm_d,10)

# Wordcloud

#generate word cloud
set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))

# 4. 

# Find associations 
findAssocs(TextDoc_dtm, terms = c("lord","god","father","offering"), corlimit = 0.05)
?findAssocs
# 5. 
findAssocs(TextDoc_dtm, terms = findFreqTerms(TextDoc_dtm, lowfreq = 1000), corlimit = 0.1)

# 6. 

# regular sentiment score using get_sentiment() function and method of your choice
# please note that different methods may have different scales
syuzhet_vector <- get_sentiment(bible, method="syuzhet")
# see the first row of the vector
head(syuzhet_vector)
# see summary statistics of the vector
summary(syuzhet_vector)

# bing
bing_vector <- get_sentiment(bible, method="bing")
head(bing_vector)
summary(bing_vector)
#affin
afinn_vector <- get_sentiment(bible, method="afinn")
head(afinn_vector)
summary(afinn_vector)

#compare the first row of each vector using sign function
rbind(
  sign(head(syuzhet_vector)),
  sign(head(bing_vector)),
  sign(head(afinn_vector))
)

# 7. 

# run nrc sentiment analysis to return data frame with each row classified as one of the following
# emotions, rather than a score: 
# anger, anticipation, disgust, fear, joy, sadness, surprise, trust 
# It also counts the number of positive and negative emotions found in each row
d<-get_nrc_sentiment(bible)
# head(d,10) - to see top 10 lines of the get_nrc_sentiment dataframe
head (d,10)

#transpose
td<-data.frame(t(d))
#The function rowSums computes column sums across rows for each level of a grouping variable.
td
td_new <- data.frame(rowSums(td))
#Transformation and cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
#Plot One - count of words associated with each sentiment
quickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Survey sentiments")

#Plot two - count of words associated with each sentiment, expressed as a percentage

## zmienic 10 na 8 zeby wyrzucic "positive, negative"
barplot(
  sort(colSums(prop.table(d[, 1:8])), decreasing=TRUE), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Text", xlab="Percentage"
)
percent <- as.numeric(sort(colSums(prop.table(d[, 1:8])), decreasing=TRUE))
percent
emotions <- factor(c("Trust","Fear","Anticipation","Joy","Disgust","Sadness","Anger","Surprise"),levels=
                     c("Trust","Fear","Anticipation","Joy","Disgust","Sadness","Anger","Surprise"))
emotions_dt <- data.frame(emotions,percent)

###### Wykres srodkowy
emotions_dt %>%
  ggplot(aes(x=emotions,y=percent))+
  geom_col()+
  labs(x="Emotions",y="Percent value")+
  scale_y_continuous(breaks = c(seq(0,0.35,0.05)))+
  coord_flip()+
  theme_bw()

# Amount of fear in the whole book

###################################################################
###################################################################

## wyrazy powiazane z zaufaniem

library(janeaustenr)
library(dplyr)
library(stringr)
library(tidyr)
library(tidytext)
library(textdata)

tidy_books <- bible %>% 
  mutate(linenumber=row_number()) %>% 
  ungroup() %>% 
  unnest_tokens(word,text)

nrc_trust <- get_sentiments("nrc") %>% 
  filter(sentiment=="trust")

alfa <- tidy_books %>% 
  inner_join(nrc_trust) %>% 
  count(word, sort = TRUE)
head(alfa,20)
## Natezenie emocjonalne na przestrzeni calej ksiazki

bible_sentiment <- tidy_books %>%
  inner_join(get_sentiments("bing"))%>%
  count(index = linenumber %/% 100, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

########## Wykres dolny

ggplot(bible_sentiment, aes(x=index, y=sentiment))+
  geom_col()+
  labs(x="Index of the part", y="Sentiment")+
  scale_y_continuous(breaks = c(seq(-60,60,10)))+
  scale_x_continuous(breaks=c(seq(0,1000,100)))+
  theme_bw()

### inne leksykony

afinn <- tidy_books %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 100) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(
  tidy_books %>% 
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  tidy_books %>% 
    inner_join(get_sentiments("nrc") %>% 
                 filter(sentiment %in% c("positive", 
                                         "negative"))
    ) %>%
    mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 100, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")
