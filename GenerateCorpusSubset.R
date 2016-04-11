# set paths to data files


# N:\Users\brian\Desktop\Files\DSS\docs
# blogfile <- "Coursera-SwiftKey/final/en_US/en_US.blogs.txt"
# newsfile <- "Coursera-SwiftKey/final/en_US/en_US.news.txt"
# twitfile <- "Coursera-SwiftKey/final/en_US/en_US.twitter.txt"
blogfile <- "../docs/en_US.blogs.txt"
newsfile <- "../docs/en_US.news.txt"
twitfile <- "../docs/en_US.twitter.txt"

# open connections to the three files
blogcon <- file(blogfile, open="rb")
newscon <- file(newsfile, open = "rb")
twitcon <- file(twitfile, open = "r")

# read the lines from the three files
blogLines <- readLines(blogcon, encoding = "UTF-8") # 250MB
blogLines <- iconv(blogLines, "UTF-8", "ascii", sub = " ")

newsLines <- readLines(newscon, encoding = "UTF-8") # 250MB
newsLines <- iconv(newsLines, "UTF-8", "ascii", sub = " ")

twitLines <- readLines(twitcon, encoding = "UTF-8") # 300MB
twitLines <- iconv(twitLines, "UTF-8", "ascii", sub = " ")


# close the connectinos to the three files
close(blogcon)
close(newscon)
close(twitcon)

# subset the data from the three files
#subblog <- blogLines[1:10]

#sampleSize <- 0.1 # grab 10% of each file
sampleSize <- 0.2 # grab 10% of each file
set.seed(11111)

subblog <- sample(blogLines, length(blogLines)*sampleSize, replace = FALSE) #25MB
subnews <- sample(newsLines, length(newsLines)*sampleSize, replace = FALSE) #25MB
subtwit <- sample(twitLines, length(twitLines)*sampleSize, replace = FALSE) #30MB

rm(blogLines)
rm(newsLines)
rm(twitLines)

# combine the samples from the three files
# each row is a record from the original files
subSet <- c(subnews, subblog, subtwit) # 80MB

subSet2 <-paste0(subSet, collapse = " ") #55MB
#subSet <- subblog  ## temp while debugging

rm(blogcon)
rm(newscon)
rm(twitcon)
rm(blogfile)
rm(newsfile)
rm(twitfile)

# ~2 minutes

library("RWeka")
library("tm")
library("quanteda")
library("data.table")
library("dplyr")

#totaltext <- paste0(twitLines, blogLines, newsLines, collapse = " ")#
#corpustotal <- corpus(totaltext)#

# Used in the report to determine total size of the files ... not needed here.
# corpustwitter <- corpus(paste0(twitLines, collapse = " "))
# corpusblogs <- corpus(paste0(blogLines, collapse = " "))
# corpusnews <- corpus(paste0(newsLines, collapse = " "))
# 
# nt <- ntoken(corpustwitter)
# nb <- ntoken(corpusblogs)
# nn <- ntoken(corpusnews)
# nlt <- length(twitLines)
# nlb <- length(blogLines)
# nln <- length(newsLines)
#ntoken(corpustotal)

# Create a corpus of the one-long-line subset and then count the words...
subsetcorpus <- corpus(subSet2)
num_words_in_subset2 <- ntoken(subsetcorpus)


profile <- "profanewords.txt"
profcon <- file(profile, open="r")
profanewords <- readLines(profcon)
close(profcon)
rm(profcon)
rm(profile)

# create the corpus from the samples
SubCorp <- VCorpus(VectorSource(subSet2))
SubCorp <- tm_map(SubCorp, PlainTextDocument)
SubCorp <- tm_map(SubCorp, content_transformer(tolower))
SubCorp <- tm_map(SubCorp, removeNumbers) 
SubCorp <- tm_map(SubCorp, removePunctuation)
SubCorp <- tm_map(SubCorp, stripWhitespace)
SubCorp <- tm_map(SubCorp, removeWords, profanewords)
#-------------------------------------------------
rm(profanewords)
#SubCorp is a "clean" corpus -- 52MB

# cleaned corpus
#thissubcorp <- corpus(SubCorp)








###########################################
saveRDS(SubCorp, "TwentyPercentsubcorpbackup.rds")