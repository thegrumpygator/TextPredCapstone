SubCorp <- readRDS("TwentyPercentsubcorpbackup.rds")
###########################################


mycleanqdcorpus <- corpus(SubCorp)
#num_words_in_clean_subset2 <- ntoken(mycleanqdcorpus)

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


# ################################################
# saveRDS(my1dfm, "my1dfm.rds")
# saveRDS(my2dfm, "my2dfm.rds")
# saveRDS(my3dfm, "my3dfm.rds")
# saveRDS(my4dfm, "my4dfm.rds")
# #
# #
# #
# my1dfm <- readRDS("my1dfm.rds")
# my2dfm <- readRDS("my2dfm.rds")
# my3dfm <- readRDS("my3dfm.rds")
# my4dfm <- readRDS("my4dfm.rds")
# ################################################

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

saveRDS(sources, "TwentyPercentdatasources.rds")