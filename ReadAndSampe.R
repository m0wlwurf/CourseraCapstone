library(tm)
library(RWeka)
library(slam)
library(word2vec)

## read data to memory

setwd("~/R-Course/CourseraCapstone")

con <- file("./final/en_US/en_US.blogs.txt")
en_US.blogs <- readLines(con)
close(con)

con <- file("./final/en_US/en_US.news.txt")
en_US.news <- readLines(con)
close(con)

con <- file("./final/en_US/en_US.twitter.txt")
en_US.twitter <- readLines(con)
close(con)

##########################################
## 1. Analysis on Single Word Basis:
##########################################

corp.en_US.blogs <- Corpus(VectorSource(en_US.blogs)) #VCorpus() for more advanced operations, but memory limitations kick in..
corp.en_US.news <- Corpus(VectorSource(en_US.news))
corp.en_US.twitter <- Corpus(VectorSource(en_US.twitter))

## Calculate TDMs
tdm.en_US.blogs <- TermDocumentMatrix(corp.en_US.blogs)
tdm.en_US.news <- TermDocumentMatrix(corp.en_US.news)
tdm.en_US.twitter <- TermDocumentMatrix(corp.en_US.twitter)

#corp.all <- c(corp.en_US.blogs, corp.en_US.news, corp.en.US.twitter)
# functions to consider for further processing:
#     removePunctuation()
#     stopwords()
#     tm_combine (use c(,))
#     termFreq()
#     stripWhiteSpaces()

# profanity filter: https://github.com/zacanger/profane-words
# to consider: remove whitespace, remove punctuation
# foreign words: check against word-list from https://github.com/dwyl/english-words/

## term_freq: frequency-sorted for each corpus 
term_freq.en_US.blogs <- sort(slam::row_sums(tdm.en_US.blogs), decreasing = TRUE)
term_freq.en_US.blogs <- term_freq.en_US.blogs / sum(term_freq.en_US.blogs)

term_freq.en_US.news <- sort(slam::row_sums(tdm.en_US.news), decreasing = TRUE)
term_freq.en_US.news <- term_freq.en_US.news / sum(term_freq.en_US.news)

term_freq.en_US.twitter <- sort(slam::row_sums(tdm.en_US.twitter), decreasing = TRUE)
term_freq.en_US.twitter <- term_freq.en_US.twitter / sum(term_freq.en_US.twitter)


## How many words do we need to cover 50% / 90% of the total frequency?
cum_term_freq.en_US.blogs <- cumsum(term_freq.en_US.blogs)
i50.en_US.blogs <- max(which(cum_term_freq.en_US.blogs < .5)) #how many words to cover 50% of coverage?
i90.en_US.blogs <- max(which(cum_term_freq.en_US.blogs < .9)) #how many words to cover 90% of coverage?

cum_term_freq.en_US.news <- cumsum(term_freq.en_US.news)
i50.en_US.news <- max(which(cum_term_freq.en_US.news < .5))
i90.en_US.news <- max(which(cum_term_freq.en_US.news < .9))

cum_term_freq.en_US.twitter <- cumsum(term_freq.en_US.twitter)
i50.en_US.twitter <- max(which(cum_term_freq.en_US.twitter < .5))
i90.en_US.twitter <- max(which(cum_term_freq.en_US.twitter < .9))

plot(cum_term_freq.en_US.blogs[seq(1, length(cum_term_freq.en_US.blogs), by = length(cum_term_freq.en_US.blogs) / 1000)], type = "l", lwd = 2, col = "red", ylab = "cumulative sum over frequency", xlab = "Index/1000", main = "en_US.blogs")
abline(h = .5, col = "green", lwd = 2)
abline(h = .9, col = "blue", lwd = 2)

plot(cum_term_freq.en_US.news[seq(1, length(cum_term_freq.en_US.news), by = length(cum_term_freq.en_US.news) / 1000)], type = "l", lwd = 2, col = "red", ylab = "cumulative sum over frequency", xlab = "Index/1000", main = "en_US.news")
abline(h = .5, col = "green", lwd = 2)
abline(h = .9, col = "blue", lwd = 2)

plot(cum_term_freq.en_US.twitter[seq(1, length(cum_term_freq.en_US.twitter), by = length(cum_term_freq.en_US.twitter) / 1000)], type = "l", lwd = 2, col = "red", ylab = "cumulative sum over frequency", xlab = "Index/1000", main = "en_US.twitter")
abline(h = .5, col = "green", lwd = 2)
abline(h = .9, col = "blue", lwd = 2)

barplot(term_freq.en_US.blogs[1:50], main = "en_US.blogs", las = 2, cex.names = .8)
barplot(term_freq.en_US.news[1:50], main = "en_US.news", las = 2, cex.names = .8)
barplot(term_freq.en_US.twitter[1:50], main = "en_US.twitter", las = 2, cex.names = .8)

################################################
## 2. Analysis on ngram-basis:
################################################

## 2a.Create a Random Sample of the overall data; 
all.raw <- c(en_US.blogs, en_US.news, en_US.twitter)

rsample <- function(fulldat, prob){
    ## returns a random subsample of fulldat; prob is the probability of a entry to be included in the subdat
    
    l <- length(fulldat)
    r <- runif(l)
    sel <- vector("logical", length = l)
    sel <- ifelse(r < prob, TRUE, FALSE)
    subdat <- fulldat[sel]
}

## try to split corpus in 10 pieces, calculate 3gram frequency and summarize
tfm.3gram.sub <- TermDocumentMatrix(VCorpus(VectorSource(NULL)))

for (i in 1:10){
    l <- length(all.raw)
    it <- 1:(l/10) + (i-1)*(l/10)
    corp.sub <- VCorpus(VectorSource(all.raw[it]))
    
    btm <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
    tfm.3gram.sub <- c(tfm.3gram.sub, TermDocumentMatrix(corp.sub, control = list(tokenize = btm)))
}

## sub <- rsample(all.raw, .1) ## reasonable calculation time for prob = .1 (~ 10 min)
## corp.sub <- VCorpus(VectorSource(sub))



## 2b Term-Frequency-Matrix with 3Gram:

# btm <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
# tfm.3gram.sub <- TermDocumentMatrix(corp.sub, control = list(tokenize = btm))
# 
# term_freq.3gram.sub <- sort(slam::row_sums(tfm.3gram.sub), decreasing = TRUE)
# term_freq.3gram.sub <- term_freq.3gram.sub / sum(term_freq.3gram.sub)
# 
# cum_term_freq.3gram.sub <- cumsum(term_freq.3gram.sub)
# plot(cum_term_freq.3gram.sub[seq(1, length(cum_term_freq.3gram.sub), by = length(cum_term_freq.3gram.sub) / 1000)], type = "l", lwd = 2, col = "red", ylab = "cumulative sum over frequency", xlab = "Index/1000", main = "3gram.sub")
# 
# barplot(term_freq.3gram.sub[1:50], main = "3gram.sub", las = 2, cex.names = .8)
