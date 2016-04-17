library("RWeka")
library("tm")
library("quanteda")
library("data.table")
library("dplyr")

## Ltoken, Rtoken, bmerge, btokens, into1


#sources <- readRDS("datasources.rds")
sources <- readRDS("TwentyPercentdatasources--.rds")



# user defined functions

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

# Function that gets top 4 matches for the i_th source
into1 <- function(str_in, i)
{
     idxn <- which(sources[[i+1]]$lefty == str_in)
     idx4 <- idxn[1:4]
     res <- data.table(
          pre = sources[[i+1]]$lefty[idx4],
          word = sources[[i+1]]$righty[idx4], 
          prob = sources[[i+1]]$prob[idx4])
     return(res)
}

# Function that merges the top n matches by summing their normalized rates
#    and returns 3 top suggestions and their relative strength
bmerge <- function(str_in)
{
     a <- btokens(str_in)
     
     # initialize with HEAVILY penalized single word suggestions
     res <- data.table(
          pre = "NA",
          word = sources[[1]]$phrase[1:4], 
          prob = (.00001^3) * sources[[1]]$prob[1:4])
     
     for (i in 1:length(a))
     {
          # get the i_th result and penalize based on order
          b <- into1(a[i], i) %>%
               mutate(prob = prob * (.01)^(3-i))
          res <- rbind(res, b)
     }
     res_ <- res %>%
          arrange(desc(prob))
     
     res <- res %>% 
          group_by(word) %>% 
          summarize(prob = sum(prob)) %>%
          mutate(n_prob = prob / max(prob, na.rm = TRUE)) %>%
          arrange(desc(n_prob)) %>%
          select(word, n_prob)
     
     
     
     #return(res)
     return(res[1:3])
}

# Function which returns the left n-1 words from n-gram
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

# Function which returns the right word from n-gram
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


#===============================================================================
# some test values ...
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

#===============================================================================
# Unused functions

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

ngp <- function(ngv) # ngv = n-gram vector
{
     result <- NULL
     for ( i in 1 : length(ngv) )
     {
          result[i] <- match(ngv[i], sources[[i+1]]$lefty, nomatch = 0) 
     }
     return (result)
     
}

i3to1 <- function(str_in)
{
     idxn <- which(sources[[4]]$lefty == str_in)
     idx4 <- idxn[1:4]
     res <- data.table(word = sources[[4]]$righty[idx4], prob = sources[[4]]$prob[idx4])
     return(res)
}

i2to1 <- function(str_in)
{
     idxn <- which(sources[[3]]$lefty == str_in)
     idx4 <- idxn[1:4]
     res <- data.table(word = sources[[3]]$righty[idx4], prob = sources[[3]]$prob[idx4])
     return(res)
}

i1to1 <- function(str_in)
{
     idxn <- which(sources[[2]]$lefty == str_in)
     idx4 <- idxn[1:4]
     res <- data.table(word = sources[[2]]$righty[idx4], prob = sources[[2]]$prob[idx4])
     return(res)
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

pmerge <- function(str_in)
{
     # merge the 4 data.tables with their probabilities
     a <- i3to1(str_in)
     
     b <- i2to1(str_in)
     b <- mutate(b, prob = prob * 0.4)
     
     c <- i1to1(str_in)
     c <- mutate(c, prob = prob * 0.4 * 0.4)
     
     d <- data.table(righty = sources[[1]]$phrase[1:4], prob = sources[[1]]$prob[1:4])
     d <- mutate(d, prob = prob * 0.4 * 0.4 * 0.4)
     
     return (rbind(a, b, c, d))
}

bmerge_test <- function(str_in)
{
     ## testing different coefficients and returning full results ...
     a <- btokens(str_in)
     
     res <- NULL
     
     for (i in 1:length(a))
     {
          b <- into1(a[i], i)
          b <- mutate(b, prob = prob * (0.4)^(3-i))
          res <- rbind(res, b)
     }
     # add in the single words ...
     res <- rbind(
          res, 
          data.table(word = sources[[1]]$phrase[1:4], prob = sources[[1]]$prob[1:4])
     )
     
     # collapse
     #res <- res %>% group_by(word) %>% summarize(sum(prob))
     return(res)
     #return(res[1:3])
}
