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
sampleSize <- 0.3 # grab 10% of each file
set.seed(11111)

subblog <- sample(blogLines, length(blogLines)*sampleSize, replace = FALSE) #25MB
subnews <- sample(newsLines, length(newsLines)*sampleSize, replace = FALSE) #25MB
subtwit <- sample(twitLines, length(twitLines)*sampleSize, replace = FALSE) #30MB


# Save some files here

# saveRDS(blogLines, "blogs.rds")
# saveRDS(newsLines, "news.rds")
# saveRDS(twitLines, "twit.rds")

rm(blogLines)
rm(newsLines)
rm(twitLines)
rm(blogcon)
rm(newscon)
rm(twitcon)
rm(blogfile)
rm(newsfile)
rm(twitfile)

gc()

# combine the samples from the three files
# each row is a record from the original files
subSet <- c(subnews, subblog, subtwit) # 80MB

#subSet1 <- c("don't", subSet)


subSet2 <-paste0(subSet, collapse = " ") #55MB
#subSet <- subblog  ## temp while debugging


# ~2 minutes

library("RWeka")
library("tm")
library("quanteda")
library("data.table")
library("dplyr")

# ################################################################################
# #
# # Create a corpus of the one-long-line subset and then count the words...
# # Appears that this is never used other than to compute word count
# # of full corpus.
# # 
# # 
# subsetcorpus <- corpus(subSet2) # this is a QUANTEDA corpus
# num_words_in_subset2 <- ntoken(subsetcorpus) # 24222136
# #
# #
# ################################################################################

profile <- "profanewords.txt"
profcon <- file(profile, open="r")
profanewords <- readLines(profcon)
close(profcon)
rm(profcon)
rm(profile)
rm(subblog)
rm(subnews)
rm(subtwit)
gc()

# create the corpus from the samples
SubCorp <- VCorpus(VectorSource(subSet2)) # this is a TM corpus
SubCorp <- tm_map(SubCorp, PlainTextDocument)
SubCorp <- tm_map(SubCorp, content_transformer(tolower))
SubCorp <- tm_map(SubCorp, removeNumbers) 
SubCorp <- tm_map(SubCorp, removePunctuation)
SubCorp <- tm_map(SubCorp, stripWhitespace)
SubCorp <- tm_map(SubCorp, removeWords, profanewords) # a little bit slow ...
#-------------------------------------------------
rm(profanewords)
#SubCorp is a "clean" corpus -- 52MB

##########################################

saveRDS(SubCorp, "ThirtyPercentsubcorpbackup.rds")