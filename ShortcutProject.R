library("RWeka")
library("tm")
library("quanteda")
library("data.table")
library("dplyr")



#sources <- readRDS("datasources.rds")
sources <- readRDS("TwentyPercentdatasources.rds")



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
          result[i] <- match(ngv[i], sources[[i+1]]$lefty, nomatch = 0) 
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
bpredict <- function(str_in)
{
     # TODO: need to fix null case
     btoks <- btokens(str_in)
     ngs <- ngp(btoks)
     
     idx <- ngs[ngs>0][length(ngs[ngs>0])]
     
     jdx <- which(ngs == idx)
     
     if(length(jdx) == 0)
     {
          pred <- sources[[1]]$phrase[1]
     }
     else
     {
          pred <- sources[[jdx+1]]$righty[idx]
     }
     
     return(paste(str_in, "...", pred))
}

tester1 <- "this is not" # string known to not be in the corpus
tester2 <- "there is a" #string known to be in the corpus

a1 <- btokens(tester1)
a2 <- btokens(tester2)

b1 <- ngp(a1)
b2 <- ngp(a2)

i1 <- b1[b1>0][length(b1[b1>0])]
i2 <- b2[b2>0][length(b2[b2>0])]
     
j1 <- which(b1 == i1)
j2 <- which(b2 == i2) # j2-gram

pred1 <- 00000
pred2 <- sources[[j2 + 1]]$righty[i2]





