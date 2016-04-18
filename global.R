library("RWeka")
library("tm")
library("quanteda")
library("data.table")
library("dplyr")



#sources <- readRDS("datasources.rds")
#sources <- readRDS("TwentyPercentdatasources.rds")

# # 30% of twitter and news
sources <- readRDS("ThirtyPercentdatasources--.rds") 

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
     if(missing(str_in))# | nchar(str_in) < 1)
          #if(length(str_in)<2)
     {
          res <- data.table(
               word = c(" ", " ", " "),
               n_prob = c(0, 0, 0)
          )
     }
     else if(nchar(str_in) < 1)
     {
          res <- data.table(
               word = c(" ", " ", " "),
               n_prob = c(0, 0, 0)
          )
     }
     else
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
     }
     
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
