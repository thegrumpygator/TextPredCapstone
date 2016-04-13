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