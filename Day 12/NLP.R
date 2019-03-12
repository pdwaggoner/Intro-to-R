# Intro to Natural Language Processing

# Load libraries
library(tm)
library(ggplot2)
library(grid)
library(wordcloud) # should also install dependencies: "SnowballC" used later

#
# LOADING TEXTS
#

## FOR MAC:
texts <- file.path("~", "Desktop", "textsR")
texts

dir(texts) # Verify everything is loaded properly

## FOR PC:
texts <- file.path("C:", "texts")
texts

dir(texts) # Verify everything is loaded properly

# Now we can create our raw corpus, which we will "preprocess" in a moment
docs <- VCorpus(DirSource(texts))
summary(docs)

# For details about documents in the corpus, use the inspect(docs) command
inspect(docs[1])

# For more detailed descriptions (i.e., to read the document)
writeLines(as.character(docs[1]))


#
# PREPROCESSING (cleaning the data) using "tm_map"
#


docs <- tm_map(docs, removePunctuation)
writeLines(as.character(docs[1])) # Check the corpus


### SIDE NOTE: Email cleaning - for those interested in QTA on emails, you will need to manually remove specific characters
for (j in seq(docs)) {
  docs[[j]] <- gsub("/", " ", docs[[j]])
  docs[[j]] <- gsub("@", " ", docs[[j]])
  docs[[j]] <- gsub("\\|", " ", docs[[j]])
  docs[[j]] <- gsub("\u2028", " ", docs[[j]])  # an ascii character that did not translate
}
writeLines(as.character(docs[1])) # You can check a document (in this case the first) to see if it worked

# We can now update our recent update (minus punctuation) by respecifying the object "docs" to remove numbers, 
# if you want to (because numbers aren't letters)
docs <- tm_map(docs, removeNumbers)

# writeLines(as.character(docs[1])) # as always, check to make sure it did what you said it should

# For consistency, we may also want to remove captialization
docs <- tm_map(docs, tolower)
docs <- tm_map(docs, PlainTextDocument)
DocsCopy <- docs # now, just rename the document object to save this version of "docs" for later

# Another key aspect of preprocessing text documents for analysis is to remove superfluous words like articles or words 
# with no value for analysis (stopwords, i.e.)

## For a list of the stopwords, run: stopwords("english")  
#  ** and you can use this to see how many there are (174): length(stopwords("english"))
# stopwords("english")
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, PlainTextDocument) # continue respecifying as a .txt for later use

docs <- tm_map(docs, removeWords, c("syllogism", "tautology")) # manually removing words too for your specific purposes (if desired)

# There are some words that R pulls apart that should stay together. 
# We can use "gsub" to manually specify these terms across each document, j:
for (j in seq(docs))
{
  docs[[j]] <- gsub("fake news", "fake_news", docs[[j]])
  docs[[j]] <- gsub("inner city", "inner-city", docs[[j]])
  docs[[j]] <- gsub("politically correct", "politically_correct", docs[[j]])
}
docs <- tm_map(docs, PlainTextDocument)

# We can also omit certain "stems" or common English word endings (e.g., ing, es)
docs_st <- tm_map(docs, stemDocument) # note that we are storing this in a new corpus to give ourselves some options for analysis later
docs_st <- tm_map(docs_st, PlainTextDocument) # this line, repeated many times above tells R to treat your preprocessed documents as text documents to be used in analysis

# Importantly, in preprocessing texts, we can often leave behind a lot of white space, 
# or extra spaces between words. To get rid of this:
docs <- tm_map(docs, stripWhitespace)

# Final step: treat preprocessed documents as text documents for analysis and interpretability downstream (one more time to make sure)
docs <- tm_map(docs, PlainTextDocument)


#
# STAGING using "DocumentTermMatrix" from the tm package
#

# with our text documents cleaned, we now need to turn the words into something useful mathematically for other things we want to do
# in the NLP world, this "thing" is called a document term matrix (or its inverse: term document matrix), and is exactly what it sounds like: 
# --> a matrix of terms by document
# Another package, qdap, allows for creation of a word frequency matrix (function: "wfm"), which is slightly different and less common

# dtm: each document is a row, and each term is a column
dtm <- DocumentTermMatrix(docs)
dtm

# Now transpose to a term document matrix, which is the inverse of the dtm
# --> tdm: each corpus word represents a row (frequency of wordds in document, j), with documents as columns
tdm <- TermDocumentMatrix(docs)
tdm


#
# EXPLORE numerically
#

# with our matrices created, we can now descriptively explore our data

frequency <- sort(colSums(as.matrix(dtm)), decreasing=TRUE) 
# adding up the number of times each term is used, and sorting based on frequency of usage

frequency # view all terms
head(table(frequency), 10) # observe first 10 obs
tail(table(frequency), 10) # observe last 10 obs

# what is the most common term used?
frequency[1]

# We can also tailor the search of terms by frequency used
findFreqTerms(dtm, lowfreq = 100) # narrows by words used more than 100 times (or whatever threshold you set), which are 15 words

# same thing, another way - verify that there are 15 words used over 100 times
wf <- data.frame(word = names(frequency), freq = frequency)
head(wf, 16)  # sure enough, the 16th word is used less than 100 times

# Further, we can order by frequency of use, using "order" function in base R
ordered.terms <- order(frequency)
ordered.terms

# Export the corpus to a .csv if you don't want to repeat the steps above, and wish to use this in the future
trump.speech.corpus <- as.matrix(dtm)
# dim(trump.speech.corpus) # inspect the dimensions of the matrix to ensure we specified it properly: recall, 11 documents, with 3659 unique terms in our preprocessed, cleaned corpus

write.csv(trump.speech.corpus, "trump.speech.corpus.csv")
trump.speech.corpus

# We can also explore relationships between word usage - e.g., what is the correlation of words being used together
findAssocs(dtm, "great", corlimit=0.90) # manually locate terms, and then specify the correlation threshold

findAssocs(dtm, c("great" , "america"), corlimit = 0.90) # multiple words

findAssocs(dtm, c("great" , "america"), corlimit = c(0.85, 0.90, 0.95, 1)) # multiple correlation thresholds


#
# EXPLORE visually
#

# recall we are working with frequency matrices, so a useful place to being is to visualize the 
# frequencies of the terms in our documents to see descriptively what Trump is talking about

# let's start by focusing on those 15 wowrds used more than 100 times

words.100 <- ggplot(subset(wf, freq > 100), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=55, hjust=1)) +
  labs(x = "Term")
words.100

words.75 <- ggplot(subset(wf, freq > 75), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=55, hjust=1)) +
  labs(x = "Term")
words.75

words.50 <- ggplot(subset(wf, freq > 50), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  labs(x = "Term")
words.50

words.35 <- ggplot(subset(wf, freq > 35), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  labs(x = "Term")
words.35

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

# 4 figures arranged in 2 rows and 2 columns
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))
print(words.100, vp = vplayout(1, 1))
print(words.75, vp = vplayout(1, 2))
print(words.50, vp = vplayout(2, 1))
print(words.35, vp = vplayout(2, 2))


# Another visual technique beyond histograms is a wordcloud, which makes terms larger based on frequency of use

# back to our 15 words over 100 times - not to useful though, as wordclouds are better with many terms to see patterns
set.seed(2345) # specifies start/end, making configuration consistent for each plot - can be any number you want
wordcloud(names(frequency), frequency, min.freq = 100)

# thus, let's visualize the 150 most frequently used terms
set.seed(2345)
wordcloud(names(frequency), frequency, max.words = 150)

# and of course, some color is good
# using brewer palette, we can first choose the color scheme we like best, and then specify it below in our word cloud
display.brewer.all(n=NULL, type="all", select=NULL, exact.n=TRUE,
                   colorblindFriendly=FALSE)

# And to manually add a title
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "Trump Speeches Term Frequency Wordcloud")
# Now, generate and overlay the fully formatted wordcloud to result in a clean, professional visualization
wordcloud(names(frequency), frequency,
          min.freq = 1, # terms used at least once
          max.words = 300, # 300 most frequently used terms
          random.order = FALSE, # centers cloud by frequency, > = center
          rot.per = 0.30, # sets proportion of words oriented horizontally
          main = "Title",
          colors = brewer.pal(8, "Dark2"))  # for colors: first arg is how many colors to use from the scheme, and second is the name of the scheme
names(frequency)


# And finally, the coolest one
library(wordcloud2)
wordcloud2(wf,
           shape = "cardioid")



## NOW YOU GO: load, preprocess, stage, and explore each of the major party pltforms from 2016
## Describe the patterns you see and find



dir(texts) # Verify everything is loaded properly

# Now we can create our raw corpus, which we will "preprocess" in a moment
docs <- VCorpus(DirSource(texts))
summary(docs)

# For details about documents in the corpus, use the inspect(docs) command
inspect(docs[1])

# For more detailed descriptions (i.e., to read the document)
writeLines(as.character(docs[1]))


#
# PREPROCESSING (cleaning the data) using "tm_map"
#


docs <- tm_map(docs, removePunctuation)
writeLines(as.character(docs[1])) # Check the corpus


### SIDE NOTE: Email cleaning - for those interested in QTA on emails, you will need to manually remove specific characters
for (j in seq(docs)) {
  docs[[j]] <- gsub("/", " ", docs[[j]])
  docs[[j]] <- gsub("@", " ", docs[[j]])
  docs[[j]] <- gsub("\\|", " ", docs[[j]])
  docs[[j]] <- gsub("\u2028", " ", docs[[j]])  # an ascii character that did not translate
}
writeLines(as.character(docs[1])) # You can check a document (in this case the first) to see if it worked

# We can now update our recent update (minus punctuation) by respecifying the object "docs" to remove numbers, 
# if you want to (because numbers aren't letters)
docs <- tm_map(docs, removeNumbers)

# writeLines(as.character(docs[1])) # as always, check to make sure it did what you said it should

# For consistency, we may also want to remove captialization
docs <- tm_map(docs, tolower)
docs <- tm_map(docs, PlainTextDocument)
DocsCopy <- docs # now, just rename the document object to save this version of "docs" for later

# Another key aspect of preprocessing text documents for analysis is to remove superfluous words like articles or words 
# with no value for analysis (stopwords, i.e.)

## For a list of the stopwords, run: stopwords("english")  
#  ** and you can use this to see how many there are (174): length(stopwords("english"))
# stopwords("english")
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, PlainTextDocument) # continue respecifying as a .txt for later use

docs <- tm_map(docs, removeWords, c("syllogism", "tautology")) # manually removing words too for your specific purposes (if desired)

# There are some words that R pulls apart that should stay together. 
# We can use "gsub" to manually specify these terms across each document, j:
for (j in seq(docs))
{
  docs[[j]] <- gsub("fake news", "fake_news", docs[[j]])
  docs[[j]] <- gsub("inner city", "inner-city", docs[[j]])
  docs[[j]] <- gsub("politically correct", "politically_correct", docs[[j]])
}
docs <- tm_map(docs, PlainTextDocument)

# We can also omit certain "stems" or common English word endings (e.g., ing, es)
docs_st <- tm_map(docs, stemDocument) # note that we are storing this in a new corpus to give ourselves some options for analysis later
docs_st <- tm_map(docs_st, PlainTextDocument) # this line, repeated many times above tells R to treat your preprocessed documents as text documents to be used in analysis

# Importantly, in preprocessing texts, we can often leave behind a lot of white space, 
# or extra spaces between words. To get rid of this:
docs <- tm_map(docs, stripWhitespace)

# Final step: treat preprocessed documents as text documents for analysis and interpretability downstream (one more time to make sure)
docs <- tm_map(docs, PlainTextDocument)


#
# STAGING using "DocumentTermMatrix" from the tm package
#

# with our text documents cleaned, we now need to turn the words into something useful mathematically for other things we want to do
# in the NLP world, this "thing" is called a document term matrix (or its inverse: term document matrix), and is exactly what it sounds like: 
# --> a matrix of terms by document
# Another package, qdap, allows for creation of a word frequency matrix (function: "wfm"), which is slightly different and less common

# dtm: each document is a row, and each term is a column
dtm <- DocumentTermMatrix(docs)
dtm

# Now transpose to a term document matrix, which is the inverse of the dtm
# --> tdm: each corpus word represents a row (frequency of wordds in document, j), with documents as columns
tdm <- TermDocumentMatrix(docs)
tdm


#
# EXPLORE numerically
#

# with our matrices created, we can now descriptively explore our data

frequency <- sort(colSums(as.matrix(dtm)), decreasing=TRUE) 
# adding up the number of times each term is used, and sorting based on frequency of usage

frequency # view all terms
head(table(frequency), 10) # observe first 10 obs
tail(table(frequency), 10) # observe last 10 obs

# what is the most common term used?
frequency[1]

# We can also tailor the search of terms by frequency used
findFreqTerms(dtm, lowfreq = 100) # narrows by words used more than 100 times (or whatever threshold you set), which are 15 words

# same thing, another way - verify that there are 15 words used over 100 times
wf <- data.frame(word = names(frequency), freq = frequency)
head(wf, 16)  # sure enough, the 16th word is used less than 100 times

# Further, we can order by frequency of use, using "order" function in base R
ordered.terms <- order(frequency)
ordered.terms

# Export the corpus to a .csv if you don't want to repeat the steps above, and wish to use this in the future
trump.speech.corpus <- as.matrix(dtm)
# dim(trump.speech.corpus) # inspect the dimensions of the matrix to ensure we specified it properly: recall, 11 documents, with 3659 unique terms in our preprocessed, cleaned corpus

write.csv(trump.speech.corpus, "trump.speech.corpus.csv")
trump.speech.corpus

# We can also explore relationships between word usage - e.g., what is the correlation of words being used together
findAssocs(dtm, "great", corlimit=0.90) # manually locate terms, and then specify the correlation threshold

findAssocs(dtm, c("great" , "america"), corlimit = 0.90) # multiple words

findAssocs(dtm, c("great" , "america"), corlimit = c(0.85, 0.90, 0.95, 1)) # multiple correlation thresholds


#
# EXPLORE visually
#

# recall we are working with frequency matrices, so a useful place to being is to visualize the 
# frequencies of the terms in our documents to see descriptively what Trump is talking about

# let's start by focusing on those 15 wowrds used more than 100 times

words.100r <- ggplot(subset(wf, freq > 100), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=55, hjust=1)) +
  labs(x = "Term")
words.100r


vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

# 4 figures arranged in 2 rows and 2 columns
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
print(words.100d, vp = vplayout(1, 1))
print(words.100r, vp = vplayout(1, 2))



words.75 <- ggplot(subset(wf, freq > 75), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=55, hjust=1)) +
  labs(x = "Term")
words.75

words.50 <- ggplot(subset(wf, freq > 50), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  labs(x = "Term")
words.50

words.35 <- ggplot(subset(wf, freq > 35), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  labs(x = "Term")
words.35

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

# 4 figures arranged in 2 rows and 2 columns
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))
print(words.100, vp = vplayout(1, 1))
print(words.75, vp = vplayout(1, 2))
print(words.50, vp = vplayout(2, 1))
print(words.35, vp = vplayout(2, 2))


# Another visual technique beyond histograms is a wordcloud, which makes terms larger based on frequency of use

# back to our 15 words over 100 times - not to useful though, as wordclouds are better with many terms to see patterns
set.seed(2345) # specifies start/end, making configuration consistent for each plot - can be any number you want
wordcloud(names(frequency), frequency, min.freq = 100)

# thus, let's visualize the 150 most frequently used terms
set.seed(2345)
wordcloud(names(frequency), frequency, max.words = 150)

# and of course, some color is good
# using brewer palette, we can first choose the color scheme we like best, and then specify it below in our word cloud
display.brewer.all(n=NULL, type="all", select=NULL, exact.n=TRUE,
                   colorblindFriendly=FALSE)

# And to manually add a title
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "Trump Speeches Term Frequency Wordcloud")
# Now, generate and overlay the fully formatted wordcloud to result in a clean, professional visualization
wordcloud(names(frequency), frequency,
          min.freq = 1, # terms used at least once
          max.words = 300, # 300 most frequently used terms
          random.order = FALSE, # centers cloud by frequency, > = center
          rot.per = 0.30, # sets proportion of words oriented horizontally
          main = "Title",
          colors = brewer.pal(8, "Dark2"))  # for colors: first arg is how many colors to use from the scheme, and second is the name of the scheme
names(frequency)


# And finally, the coolest one
library(wordcloud2)
wordcloud2(wf,
           shape = "cardioid")



