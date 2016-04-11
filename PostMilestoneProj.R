# user defined functions
btokens_2 <- function(str_in) {
     str_in <- tolower(str_in)
     v_str_in <- strsplit(str_in, "\\s+")[[1]] # vector of words
     
     numw <- length(v_str_in)
     
     i <- numw
     j <- 0
     result <- NULL
     while (i > 0) {
          # do something with ith val
          result <- rbind(result, v_str_in[i])
          
          i <- i - 1
          j <- j + 1
     }
     return (result)
}

# Function that returns an array of the last n n-grams with n at most equal to 3
btokens <- function(str_in) 
{
     str_in <- tolower(str_in)
     
     # TODO: Remove puctuation, etc
     
     v_str_in <- strsplit(str_in, "\\s+")[[1]] # vector of words
     
     result <- NULL
     for ( i in 1:3 ) 
     {
          grams <- ngrams(v_str_in, n = i, concatenator = " ")
          result <- rbind(result, grams[length(grams)])
     }
     return (result)
}     

ngp <- function(ngv) # ngv = n-gram vector
{
     result <- NULL
     for ( i in 1 : length(ngv) )
     {
          result[i] <- match(ngv[i], sources[[i]]$V1, nomatch = 0) 
     }
     return (result)
     
}

ftoken <- function(str_in) 
{
     # must be more than 1 word ... 2, 3, etc ....
     str_in <- tolower(str_in)
     
     # TODO: Remove puctuation, etc
     
     v_str_in <- strsplit(str_in, "\\s+")[[1]] # vector of words
     
     # need two strings ... first (n-1) words, nth word
     leftside <- ngrams(v_str_in, n = length(v_str_in)-1, concatenator = " ")[1]
     rightside<- v_str_in[length(v_str_in)]
     return(c(leftside, rightside))
     
     
}

Ltoken <- function(str_in) 
{
     # must be more than 1 word ... 2, 3, etc ....
     str_in <- tolower(str_in)
     
     # TODO: Remove puctuation, etc
     
     v_str_in <- strsplit(str_in, "\\s+")[[1]] # vector of words
     
     # need two strings ... first (n-1) words, nth word
     leftside <- ngrams(v_str_in, n = length(v_str_in)-1, concatenator = " ")[1]
     #rightside<- v_str_in[length(v_str_in)]
     return(as.character(leftside))
     
     
}

Rtoken <- function(str_in) 
{
     # must be more than 1 word ... 2, 3, etc ....
     str_in <- tolower(str_in)
     
     # TODO: Remove puctuation, etc
     
     v_str_in <- strsplit(str_in, "\\s+")[[1]] # vector of words
     
     # need two strings ... first (n-1) words, nth word
     #leftside <- ngrams(v_str_in, n = length(v_str_in)-1, concatenator = " ")[1]
     rightside<- v_str_in[length(v_str_in)]
     return(as.character(rightside))
     
     
}


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
sampleSize <- 0.00001 # grab 10% of each file
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
saveRDS(SubCorp, "subcorpbackup.rds")
#
#
#
SubCorp <- readRDS("subcorpbackup.rds")
###########################################









mycleanqdcorpus <- corpus(SubCorp)
num_words_in_clean_subset2 <- ntoken(mycleanqdcorpus)

# tokenize to words (1-grams)
mycleanqdtokens <- tokenize(mycleanqdcorpus) # don't need options since it's already cleaned. (n1g)
n1g <- mycleanqdtokens                                  # 87 MB
n2g <- ngrams(mycleanqdtokens, n=2, concatenator = " ") # 234.7 MB
n3g <- ngrams(mycleanqdtokens, n=3, concatenator = " ") # 504 MB
n4g <- ngrams(mycleanqdtokens, n=4, concatenator = " ")



#my1dfm <- dfm(mycleanqdcorpus) # features of base doc
# my1dfm <- dfm(n1g) # features of base doc
# my2dfm <- dfm(n2g) 
# my3dfm <- dfm(n3g)

my1dfm <- trim(dfm(n1g), minCount = 2) # features of base doc
my2dfm <- trim(dfm(n2g), minCount = 2)
my3dfm <- trim(dfm(n3g), minCount = 2)
my4dfm <- trim(dfm(n4g), minCount = 2)


################################################
saveRDS(my1dfm, "my1dfm.rds")
saveRDS(my2dfm, "my2dfm.rds")
saveRDS(my3dfm, "my3dfm.rds")
saveRDS(my4dfm, "my4dfm.rds")
#
#
#
my1dfm <- readRDS("my1dfm.rds")
my2dfm <- readRDS("my2dfm.rds")
my3dfm <- readRDS("my3dfm.rds")
my4dfm <- readRDS("my4dfm.rds")
################################################

# create ranked n-gram frequencies
my1dfmFeatures <- topfeatures(my1dfm, length(my1dfm))
my2dfmFeatures <- topfeatures(my2dfm, length(my2dfm))
my3dfmFeatures <- topfeatures(my3dfm, length(my3dfm))
my4dfmFeatures <- topfeatures(my4dfm, length(my4dfm))

# wordProb:
ng1_Prob <- my1dfmFeatures / sum(my1dfmFeatures)
ng2_Prob <- my2dfmFeatures / sum(my2dfmFeatures)
ng3_Prob <- my3dfmFeatures / sum(my3dfmFeatures)

# Convert my probabilities to data tables
pdt1 <- data.table(cbind(names(ng1_Prob), ng1_Prob))
pdt2 <- data.table(cbind(names(ng2_Prob), ng2_Prob))
pdt3 <- data.table(cbind(names(ng3_Prob), ng3_Prob))

#sources <- list(pdt1, pdt2, pdt3)

# transform f1 to consistent format
f1 <- tbl_df(pdt1)
f1 <- f1 %>% transmute(phrase = V1, prob = ng1_Prob)%>%
     arrange(phrase, desc(prob))

# create word table for 2-grams
f2 <- tbl_df(pdt2)
f2 <- f2 %>% transmute(phrase = V1, prob = ng2_Prob) %>%
     mutate(lefty = sapply(phrase, Ltoken)) %>%
     mutate(righty = sapply(phrase, Rtoken)) %>%
     arrange(lefty, desc(prob))

# create word table for 3-grams
f3 <- tbl_df(pdt3)
f3 <- f3 %>% transmute(phrase = V1, prob = ng3_Prob) %>%
     mutate(lefty = sapply(phrase, Ltoken)) %>%
     mutate(righty = sapply(phrase, Rtoken)) %>%
     arrange(lefty, desc(prob))

# create word table for 4-grams
f4 <- as.data.frame(my4dfmFeatures)
f4 <- add_rownames(f4, var = "phrase")
f4 <- tbl_df(f4)
f4 <- f4 %>% 
     mutate(num = my4dfmFeatures) %>% 
     select(phrase, num) %>% 
     mutate(prob = num/sum(num)) %>%
     mutate(lefty = sapply(phrase, Ltoken)) %>%
     mutate(righty = sapply(phrase, Rtoken)) %>%
     arrange(lefty, desc(prob))

sources <- list(f1, f2, f3, f4)

saveRDS(sources, "datasources.rds")
#
#
#
sources <- readRDS("datasources.rds")


# f4 <- f4 %>% 
#      mutate(num = my4dfmFeatures) %>% 
#      select(phrase, num) %>% 
#      mutate(prob = num/sum(num)) %>%
#      mutate_each(left = Ltoken(phrase)) %>%
#      mutate(right = Rtoken(phrase)) %>%
#      arrange(left)



# f4 <- f4 %>% 
#      mutate(num = my4dfmFeatures) %>% 
#      select(phrase, num) %>% 
#      mutate(prob = num/sum(num))
# 
# f4<-data.table(phrase = f4$phrase, lefty = sapply(f4$phrase,Ltoken), righty = sapply(f4$phrase,Rtoken), num = f4$num, prob = f4$prob)
# 
# f4<-arrange(f4, lefty, desc(prob))








# barplot(topfeatures(my1dfm, 10), main = "Top 10 1-gram Frequencies", col = "blue")
# barplot(topfeatures(my2dfm, 10), main = "Top 10 2-gram Frequencies", col = "blue")
# barplot(topfeatures(my3dfm, 10), main = "Top 10 3-gram Frequencies", col = "blue")

#----> ~4 minutes

# quanteda functions of use ...
# trim
# features
# docfreq


#match("am to pma", pdt3$V1, nomatch = 0) > 0  ---> FALSE
#match("am to pm", pdt3$V1, nomatch = 0) > 0   ---> TRUE