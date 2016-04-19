# Module 10 - Data Science Capstone
# Copyright (c) 2016, Data Science Coursera
# Data Science Specialization, Johns Hopkins University 

#install.packages("tm")
#install.packages("SnowballC")
#install.packages("RWeka")
library(tm)
library(SnowballC)
library(RWeka)

# Read the datasets
twitter <- readLines("en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE)
#news <- readLines("en_US.news.txt", encoding = "UTF-8", skipNul = TRUE)

# Converting
twitter <- iconv(twitter, 'UTF-8', 'ASCII', "byte")
#news <- iconv(news, 'UTF-8', 'ASCII', "byte")
Encoding(twitter)  <- "UTF-8"#magic line to make the tolower function work

# Saving the file
save(twitter, file="twitter.RData")
#save(news, file="news.RData")

all <- c(twitter,news)
save(all, file="all.RData")

# Load whichever is appropriate
# Load() will read and restore the variable also
load("all.RData")
#load("twitter.RData")
#load("news.RData")

# All data has more than 4 million entries, this will kill" my poor
# little machine, just choose whichever is appropriate for your
# machine.
#smpl <- sample(all,10000)
#save(smpl, file="sample-10k.RData")
#smpl <- sample(all,5000)
#save(smpl, file="sample-5k.RData")
#smpl <- sample(all,2000)
#save(smpl, file="sample-2k.RData")
#smpl <- sample(all,1000)
#save(smpl, file="sample-1k.RData")
#smpl <- sample(all,100)
#save(smpl, file="sample-100.RData")

#length(all)
# To use 1% of the available data, which is around 40k (incresing to 14%)
#smpl <- sample(all,round(0.01*length(all)))
smpl <- sample(all,round(0.30*length(all)))

save(smpl, file="sample1p.RData")



# To use 0.1% of the available data, which is around 4k
#smpl <- sample(all,round(0.001*length(all)))
#save(smpl, file="sample-0.1p.RData")

# Just load() it and it will load very quickly into the appropriate variable
load("sample1p.RData")

# Clean up the R Workspace
rm(all)
#rm(news)
rm(twitter)
gc()

# Creating the Corpus
# You can create a file with the profanity words and read it into a vector of char

#profanity <- c("bad", "words", "that", "cannot", "display", "here")

corpus <- Corpus(VectorSource(smpl))
corpus <- tm_map(corpus, removePunctuation)#ok
corpus <- tm_map(corpus, removeNumbers)#ok
corpus <- tm_map(corpus, tolower)#ok
#corpus <- tm_map(corpus, stemDocument)#seriously consider not doing this as stemming affects the input
corpus <- tm_map(corpus, stripWhitespace)#ok
corpus <- tm_map(corpus, removeWords, stopwords("english"))#runs long time...but#ok
profanity <- read.csv("profanity.csv", header=FALSE, stringsAsFactors=FALSE)
profanity <- profanity$V1#get column identifier
corpus <- tm_map(corpus, removeWords, profanity)#remove google's list of bad words

####code works, but takes huge amount of time, a weekend run reduced the corpus from 62.8 MB to 57.7 MB
#docs<-corpus
#for (j in seq(docs))
#{
#  docs[[j]] = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", docs[[j]])
#  docs[[j]] = gsub("@\\w+", "", docs[[j]])
#  docs[[j]] = gsub("http\\w+", "", docs[[j]])
#  docs[[j]] = gsub("[ \t]{2,}", "", docs[[j]])
#  docs[[j]] = gsub("^\\s+|\\s+quot;", "", docs[[j]])
#  docs[[j]] = gsub("[^\x20-\x7E]", "", docs[[j]])
#}


corpus <- tm_map(corpus, PlainTextDocument)#last one to be done

# Save the corpus for quick loading.  This will be useful for your final product.
save(corpus,file="sampleCorpus.RData")

# Let's create a few n-gram functions
# We will use the RWeka library. I focused up to trigram for this demo

#library(RWeka)
unigram_token <- function(x)
  NGramTokenizer(x, Weka_control(min = 1, max = 1))
bigram_token <- function(x)
  NGramTokenizer(x, Weka_control(min = 2, max = 2))
trigram_token <- function(x)
  NGramTokenizer(x, Weka_control(min = 3, max = 3))
fourgram_token <- function(x) 
  NGramTokenizer(x, Weka_control(min = 4, max = 4))
fivegram_token <- function(x)
 NGramTokenizer(x, Weka_control(min = 5, max = 5))

# Create n-gram files which you should keep for your final product.

load("sampleCorpus.RData")
#library(tm)
unigram <- TermDocumentMatrix(corpus, control=list(tokenize=unigram_token))
save(unigram,file="unigram.RData")

bigram <- TermDocumentMatrix(corpus, control=list(tokenize=bigram_token))
save(bigram,file="bigram.RData")

trigram <- TermDocumentMatrix(corpus, control=list(tokenize=trigram_token))
save(trigram,file="trigram.RData")


#Skipping fourgram and fivegram
fourgram <- TermDocumentMatrix(corpus, control=list(tokenize=fourgram_token))
save(fourgram,file="fourgram.RData")

fivegram <- TermDocumentMatrix(corpus, control=list(tokenize=fivegram_token))
save(trigram,file="fivegram.RData")

# Let's look at n-grams manually and do the backoff algorithm manually
# Note: tf = Term Frequency
#Count frequency of uni, bi and trigrams
library(tm)
library(slam)
freq <- rowapply_simple_triplet_matrix(unigram,sum)
freqbi <- rowapply_simple_triplet_matrix(bigram,sum)
freqtri <- rowapply_simple_triplet_matrix(trigram,sum)
save(freq,file="freq.RData")
save(freqbi,file="freqbi.RData")
save(freqtri ,file="freqtri.RData")

#sort n-grams by frequency
freq_uni <- sort(freq, decreasing = T)
freq_bi <- sort(freqbi, decreasing = T)
freq_tri <- sort(freqtri, decreasing = T)

#to visualize the frequency
barplot(freq_uni[1:20], width = 0.1, main = "Freqency of Top 20 words in unigram")
barplot(freq_bi[1:20], width = 0.1, main = "Freqency of Top 20 words in unigram")
barplot(freq_tri[1:20], width = 0.1, main = "Freqency of Top 20 words in unigram")

#Split joined strings
firstname <- sapply(strsplit(names(freqbi), ' '), function(a) a[1])
secname <- sapply(strsplit(names(freqbi), ' '), function(a) a[2])
firsttriname <- sapply(strsplit(names(freqtri), ' '),function(a) a[1])
sectriname <- sapply(strsplit(names(freqtri), ' '),function(a) a[2])
tritriname <- sapply(strsplit(names(freqtri), ' '),function(a) a[3])

names(freqbi)[1] #view first entry in the bi-gram model
firstname[1]#view the split of the first entry in the bi-gram model into the first word
secname[1]#view the split of the first entry in the bi-gram model into the second  word

#save uni, bi and trigrams as dataframes
unigramDF <- data.frame(names(freq),freq,stringsAsFactors = F)
bigramDF <- data.frame(names(freqbi),freqbi,firstname,secname,stringsAsFactors = F)
trigramDF <- data.frame(names(freqtri),freqtri,paste(firsttriname,sectriname),tritriname,stringsAsFactors = F)
names(unigramDF) <- c('unigram','freq')
names(bigramDF) <- c('bigram','freq','unigram','name')
names(trigramDF) <- c('trigram','freq','bigram','name')
save(unigramDF,bigramDF,trigramDF,file = 'ngram.RData')
load('ngram.RData')

View(unigramDF)
View(bigramDF)
View(unigramDF)


