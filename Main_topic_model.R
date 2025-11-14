
# Load packages
library(lubridate)
library(stringr)
library(tidycensus)
library(knitr)
library(tidyverse)

root_path <- "/Users/jenniezhao/Dev/PPOL6081/replication/dataverse_files"
list.files(root_path)
setwd(root_path)
getwd()

################################################################################
##
## Purpose: This script prepares the final data for NLP scripts.
##
## Author: James Bisbee (james.h.bisbee@vanderbilt.edu)
##
##  - Inputs:
##    - ./data/prepped/finalData_12_25_2021.RData: Prepped data from 1_DATA_merge.R
##    - ./data/prepped/bills/binder_bills_merged.RData: Prepped data from 5_DATA_bill_prep.R
##    - ./data/raw/politicians/Yellen_vote.csv: Raw data from https://www.senate.gov/legislative/LIS/roll_call_lists/roll_call_vote_cfm.cfm?congress=117&session=1&vote=00006
##  - Outputs:
##    - ./data/prepped/finalData_for_NLP.RData
##
##
## See associated log file for compute environment, package versions, 
##  and date of most recent run.
##
################################################################################
rm(list = ls())
gc()
require(tidyverse)

set.seed(123)

# Compute details
print(paste0('Compute environment from ',Sys.Date(),' run by Jiani'))
if(Sys.info()['sysname'] == 'Windows') {
  ram_size = system("wmic MemoryChip get Capacity", intern = TRUE)[-1]
  model_name = system("wmic cpu get name", intern = TRUE)[2] # nocov
  vendor_id = system("wmic cpu get manufacturer", intern = TRUE)[2] # nocov
  
  print(list(ram = stringr::str_squish(ram_size)[1],
             vendor_id = stringr::str_squish(vendor_id),
             model_name = stringr::str_squish(model_name),
             no_of_cores = parallel::detectCores()))
} else if(Sys.info()['sysname'] == 'Linuxs') {
  splitted <- strsplit(system("ps -C rsession -o %cpu,%mem,pid,cmd", intern = TRUE), " ")
  df <- do.call(rbind, lapply(splitted[-1], 
                              function(x) data.frame(
                                cpu = as.numeric(x[2]),
                                mem = as.numeric(x[4]),
                                pid = as.numeric(x[5]),
                                cmd = paste(x[-c(1:5)], collapse = " "))))
  df
} else {
  cat("If not on Linux or Windows, you'll have to figure out your own solution to seeing the compute environment.")
}

sessionInfo()


# -------------------------------------------------------------- Loading inputs data
load('./data/prepped/finalData_12_25_2021.RData')
# Binder bill data: binder_bills_merged.RData created by /Data/Bills/bill_prep.R
load('./data/prepped/bills/binder_bills_merged.RData')

# Yellen nomination vote data: Yellen_vote.csv from https://www.senate.gov/legislative/LIS/roll_call_lists/roll_call_vote_cfm.cfm?congress=117&session=1&vote=00006
# reads yellen nomination csv and rename columns as given 
vote <- read_csv('./data/raw/politicians/Yellen_vote.csv',
                 col_names = c('speaker','Party','stab','yellen_vote'))


# Cleaning the data further.
# create unique row index, 
# remove blocks that look like section headers in all caps
# replace newlines/carriage returns with spaces
# collapse multliple spaces
# trim leading/trailing spaces
finalMerge <- finalMerge %>%
  mutate(fullInd = row_number(),
         textclean = trimws(gsub('\\s{2,}',' ',gsub('\\\r|\\\n',' ',gsub('\\\n\\s{3,}[[:upper:]]{2,}.*\\\r','',textclean))))) %>%
  mutate(nchars = nchar(textclean))

# Filling in missing demographic data
finalMerge %>%
  select(opensecretsID,gender,party,nominate_dim1,age,seniority) %>%
  filter(!is.na(opensecretsID),
         !complete.cases(.)) %>% distinct()

# fill party where missing 
# if party is missing, copy from position

table(finalMerge$party)
# initially, the data only has three clear party codes (Democrat, Republican, and Independent)

finalMerge$party[which(is.na(finalMerge$party))] <- finalMerge$position[which(is.na(finalMerge$party))]

table(finalMerge$party)
## after filling the party code with the position, it contains more than 3 parties, admin, expert and fed chair have been added
## fed chair and expert are not actual parties, and the admin will be removed after, so the purpose of this step
## is to remain non-politicians for completeness

# normalize gender for Fed/experts
finalMerge <- finalMerge %>%
  mutate(gender = ifelse(opensecretsID %in% c('FEDGREENSPAN',
                                              'FEDBERNANKE',
                                              'FEDPOWELL'),'M',
                         ifelse(opensecretsID == 'FEDYELLEN','F',gender)))

# remove the administrative placeholders
finalMerge <- finalMerge %>% filter(opensecretsID != 'ADMIN')

table(finalMerge$opensecretsID)
## the numbers represent individual speaking segments from hearings


# check more missing values in electoral and family variables
finalMerge %>%
  select(opensecretsID,stab,gender,party,nominate_dim1,
         votepct_rel,votepct,nKids,nDaughters,nSons,firstDaughter) %>%
  filter(!is.na(opensecretsID),
         !complete.cases(.)) %>% distinct() 

## there are 13 unique speaker IDs that stil have the missing data
## most of them are fed officals/experts, and one real legislator (N00042619)
## since they are not elected politicians, so they don't have state(stab), votepct, votepct_rel


# patch electoral fields for special IDs
## for the particular member N00042619, set vote share and relative votes share to document values
## for fed/experts, set vote shares to 1 and state/abbrev stab to "DC"
finalMerge <- finalMerge %>%
  mutate(votepct = ifelse(opensecretsID == 'N00042619',.549,
                          ifelse(grepl('FED|EXPERT',opensecretsID),1,votepct)),
         votepct_rel = ifelse(opensecretsID == 'N00042619',.549-.44,
                              ifelse(grepl('FED|EXPERT',opensecretsID),1,votepct_rel)),
         stab = ifelse(grepl('FED|EXPERT',opensecretsID),'DC',stab)) # https://ballotpedia.org/Michael_F.Q._San_Nicolas

# impute family composition for Fed/experts
# for each fed official/expert, hard-code the number of children, daughters, sons, and whether the first-born child is a daughter
finalMerge <- finalMerge %>%
  mutate(nKids = ifelse(opensecretsID == 'FEDPOWELL',3,
                        ifelse(opensecretsID == 'FEDBERNANKE',2,
                               ifelse(opensecretsID == 'FEDGREENSPAN',0,
                                      ifelse(opensecretsID == 'FEDYELLEN',1,
                                             ifelse(opensecretsID == 'EXPERTKOHN',2,
                                                    ifelse(opensecretsID == 'EXPERTKOO',2,
                                                           ifelse(opensecretsID == 'EXPERTMELTZER',2,
                                                                  ifelse(opensecretsID == 'EXPERTTAYLOR',2,
                                                                         ifelse(opensecretsID == 'EXPERTMCCLOSKEY',2,
                                                                                ifelse(grepl('EXPERT',opensecretsID),0,nKids)))))))))),
         nDaughters = ifelse(opensecretsID == 'FEDPOWELL',2,
                             ifelse(opensecretsID == 'FEDBERNANKE',0,
                                    ifelse(opensecretsID == 'FEDGREENSPAN',0,
                                           ifelse(opensecretsID == 'FEDYELLEN',0,
                                                  ifelse(opensecretsID == 'EXPERTKOHN',1,
                                                         ifelse(opensecretsID == 'EXPERTKOO',0,
                                                                ifelse(opensecretsID == 'EXPERTMELTZER',0,
                                                                       ifelse(opensecretsID == 'EXPERTTAYLOR',1,
                                                                              ifelse(opensecretsID == 'EXPERTMCCLOSKEY',1,
                                                                                     ifelse(grepl('EXPERT',opensecretsID),0,nDaughters)))))))))),
         nSons = ifelse(opensecretsID == 'FEDPOWELL',1,
                        ifelse(opensecretsID == 'FEDBERNANKE',2,
                               ifelse(opensecretsID == 'FEDGREENSPAN',0,
                                      ifelse(opensecretsID == 'FEDYELLEN',1,
                                             ifelse(opensecretsID == 'EXPERTKOHN',1,
                                                    ifelse(opensecretsID == 'EXPERTKOO',2,
                                                           ifelse(opensecretsID == 'EXPERTMELTZER',2,
                                                                  ifelse(opensecretsID == 'EXPERTTAYLOR',1,
                                                                         ifelse(opensecretsID == 'EXPERTMCCLOSKEY',1,
                                                                                ifelse(grepl('EXPERT',opensecretsID),0,nSons)))))))))),
         firstDaughter = ifelse(opensecretsID == 'FEDPOWELL',1,
                                ifelse(opensecretsID == 'FEDBERNANKE',0,
                                       ifelse(opensecretsID == 'FEDGREENSPAN',0,
                                              ifelse(opensecretsID == 'FEDYELLEN',0,
                                                     ifelse(opensecretsID == 'EXPERTKOHN',1,
                                                            ifelse(opensecretsID == 'EXPERTKOO',0,
                                                                   ifelse(opensecretsID == 'EXPERTMELTZER',0,
                                                                          ifelse(opensecretsID == 'EXPERTTAYLOR',0,
                                                                                 ifelse(opensecretsID == 'EXPERTMCCLOSKEY',0,
                                                                                        ifelse(grepl('EXPERT',opensecretsID),0,firstDaughter)))))))))))




# -------------------------------------------------------------- Merging Step 1
# Merging with the bills
## use opensecretsID as a key, join two bills and select variables that match tot or Bill 
finalMerge %>%
  left_join(finalBills) %>%
  select(opensecretsID,matches('_tot|Bill')) %>%
  drop_na()

names(finalMerge)

finalMerge <- finalMerge %>%
  left_join(finalBills)


# Finally merging with the vote records of who voted against Yellen
finalMerge <- finalMerge %>%
  left_join(finalMerge %>%
              # keep only senate speeches from 2014???
              filter(chamber == "Senate",
                     year == 2014) %>% 
              select(opensecretsID,speaker,party,stab,chamber) %>%
              distinct() %>%
              # remove prefixes like chairman, senator to match names in the vote dataset
              mutate(speaker = gsub('Chairman |Chairwoman |Senator |\\.','',speaker)) %>%
              left_join(vote %>% mutate(party = Party)) %>%
              # create a variable(yellen_vote), if senator voted NAY or NV, then coded 1 otherwise coded 0
              mutate(yellen_vote = ifelse(yellen_vote %in% c('NAY','NV'),1,0)) %>%
              select(opensecretsID,yellen_vote) %>%
              filter(!is.na(yellen_vote))) %>%
  mutate(yellen_vote = ifelse(is.na(yellen_vote),0,yellen_vote))

table(finalMerge$yellen_vote)

# Save for toxicity prep and topic models
save(finalMerge,file = './data/prepped/finalData_for_NLP.RData')

# EOF



################################################################################
##
## Purpose: This script estimates a range of topic models.
##
## Author: James Bisbee (james.h.bisbee@vanderbilt.edu)
##
##  - Inputs:
##    - ./data/prepped/finalData_for_NLP.RData: Prepped data from 6_DATA_intermediate_build.R
##  - Outputs:
##    - ./data/prepped/hearings/topic_robustness.RData
##    - ./data/prepped/hearings/topic_models_100.RData
##    - ./data/prepped/hearings/topic_robustness_JOPRR1_Grped.RData
##    - ./data/prepped/hearings/topic_robustness_JOPRR1_spkr.RData
##    - ./data/prepped/hearings/stm_results.RData
##    - ./data/prepped/finalData_70kGrped.RData
##
##
## See associated log file for compute environment, package versions, 
##  and date of most recent run.
##
################################################################################
rm(list = ls())
gc()
set.seed(12345)
require(text2vec)
require(textstem)
require(stopwords)
require(tidyverse)
require(textmineR)
require(stm)
require(tm)

# Compute details
print(paste0('Compute environment from ',Sys.Date(),' run by Jiani'))
if(Sys.info()['sysname'] == 'Windows') {
  ram_size = system("wmic MemoryChip get Capacity", intern = TRUE)[-1]
  model_name = system("wmic cpu get name", intern = TRUE)[2] # nocov
  vendor_id = system("wmic cpu get manufacturer", intern = TRUE)[2] # nocov
  
  print(list(ram = stringr::str_squish(ram_size)[1],
             vendor_id = stringr::str_squish(vendor_id),
             model_name = stringr::str_squish(model_name),
             no_of_cores = parallel::detectCores()))
} else if(Sys.info()['sysname'] == 'Linuxs') {
  splitted <- strsplit(system("ps -C rsession -o %cpu,%mem,pid,cmd", intern = TRUE), " ")
  df <- do.call(rbind, lapply(splitted[-1], 
                              function(x) data.frame(
                                cpu = as.numeric(x[2]),
                                mem = as.numeric(x[4]),
                                pid = as.numeric(x[5]),
                                cmd = paste(x[-c(1:5)], collapse = " "))))
  df
} else {
  cat("If not on Linux or Windows, you'll have to figure out your own solution to seeing the compute environment.")
}

sessionInfo()

# text preprocessing
## lowercase, remove ', remove non-letter, collapse spaces
prep_fun = function(x) {
  x = str_to_lower(x)
  x = str_replace_all(x, "'", "")
  x = str_replace_all(x, "[^[:alpha:]]", " ")
  x = str_replace_all(x, "\\s+", " ")
}

# Topic model estimated on speaker-hearing concatenated text
load('./data/prepped/finalData_for_NLP.RData')

text <- finalMerge %>%
  mutate(textclean = lemmatize_strings(prep_fun(textclean))) %>%
  filter(!is.na(speaker)) %>%
  select(fullInd,textclean)

# train/test split for model selection 
## first 6000 docs for trainng, remaining for test ???
trainText <- text$textclean[1:6000]
testText <- text$textclean[6001:nrow(text)]

# build vocab/DTM/TMC on train
## Token iterator over training docs ???
it = itoken(trainText,ids = text$fullInd[1:6000],progressbar = FALSE)
## Build vocabulary with stopwords removed.
v = create_vocabulary(it,stopwords = c(gsub("'",'',stopwords())))
## drop very rare terms and too common terms
v = prune_vocabulary(v,doc_proportion_max = .2,term_count_min = 20)
## Create DTM (document–term matrix) and TCM (term co-occurrence) for coherence.
vectorizer = vocab_vectorizer(v)
dtm = create_dtm(it,vectorizer)
tcm <- create_tcm(it,vectorizer,skip_grams_window = 10L)

# Apply the same vocabulary to the held-out test docs.
it = itoken(testText,ids = text$fullInd[6001:nrow(text)],progressbar = FALSE)
dtmTest = create_dtm(it,vectorizer)

# Model selection over k
perplx <- cohres <- NULL
for(k in c(10,20,30,50,100,150,200,300,400,500,750,1000)) {
  set.seed(123)
  lda_model = LDA$new(n_topics = k, doc_topic_prior = 0.1, topic_word_prior = 0.01)
  doc_topic_distr = lda_model$fit_transform(x = dtm, n_iter = 1000,convergence_tol = 0.001, n_check_convergence = 25,progressbar = FALSE)
  tw = lda_model$get_top_words(n = 10, lambda = 1)
  
  # Coherence
  ## computes topic coherence (using TCM & textmineR::coherence).
  cohTmp <- coherence(tw, tcm, n_doc_tcm = attr(v, 'document_count'))
  cohres <- bind_rows(data.frame(cohTmp) %>%
                        mutate(topic = row.names(.),
                               k = k),cohres)
  
  # Perplexity
  ## evaluates on held-out test DTM.
  new_doc_topic_distr = lda_model$transform(dtmTest)
  perplx <- bind_rows(perplx,data.frame(k = k,
                                        perplexity = perplexity(dtmTest, 
                                                                topic_word_distribution = lda_model$topic_word_distribution, 
                                                                doc_topic_distribution = new_doc_topic_distr)))
  cat(k,'\n')
}

save(perplx,cohres,file = './data/prepped/hearings/topic_robustness.RData')

perplx %>%
  ggplot(aes(x = k,y = perplexity)) + 
  geom_point() + 
  geom_line() + 
  geom_vline(xintercept = 100)
## At low k (e.g., 10), topics are too few → model can’t represent diversity → high perplexity.
## As k increases (50, 100, 200…), perplexity falls → better predictive power.
## After ~100–200 topics, improvement slows → overfitting risk.

# Looking at the results
cohres %>%
  filter(!is.infinite(mean_logratio),
         k > 0) %>%
  group_by(k) %>%
  # slice_max(mean_logratio,n = 100) %>%
  summarise_all(mean,na.rm=T) %>%
  ggplot(aes(x = k,y = mean_logratio)) + 
  geom_point() + 
  geom_line() + 
  geom_vline(xintercept = 100) + 
  ylab('Coherence (avg. log ratio)')

## Measures how related the top words of a topic are, based on co-occurrence statistics (from the TCM).
## Coherence first increases as k grows (topics get more specific and distinct).
## After a point (~k = 100–150), coherence drops because topics start splitting into very narrow, noisy clusters.

# Choosing the best
it = itoken(text$textclean,ids = text$fullInd,progressbar = FALSE)
## Rebuild vocabulary on all docs with unigrams+bigrams
v = create_vocabulary(it,ngram = c(1,2),stopwords = c(gsub("'",'',stopwords())))
## looser pruning (terms in ≤50% of docs; min count 10).??? 
v = prune_vocabulary(v,doc_proportion_max = .5,term_count_min = 10)
## build the DTM on all docs
vectorizer = vocab_vectorizer(v)
dtm = create_dtm(it,vectorizer)
## fit the final LDA Model
set.seed(123)
lda_model = LDA$new(n_topics = 100, doc_topic_prior = 0.1, topic_word_prior = 0.01)
doc_topic_distr = lda_model$fit_transform(x = dtm, n_iter = 1000,convergence_tol = 0.001, n_check_convergence = 25,progressbar = FALSE)

# Convert the doc–topic matrix into long format
doc_topic_distr <- doc_topic_distr %>%
  data.frame() %>%
  mutate(id =  text$fullInd) %>%
  as_tibble() %>%
  gather(topic,theta,-id) %>%
  mutate(topic = gsub('X','',topic))


save(doc_topic_distr,lda_model,file = './data/prepped/hearings/topic_models_100.RData')




# STM version
# data preparation
text <- finalMerge %>%
  # marks utterances ending with -- as interrupted (1), otherwise coded as 0
  # lemmatize and clean the data
  mutate(interrupted = ifelse(grepl('--$',textclean),1,0),
         textclean = lemmatize_strings(prep_fun(textclean))) %>%
  filter(!is.na(speaker))  %>% 
  # keep only relevant demographic meta data
  select(textclean,chamber,opensecretsID,position,party,nKids,nDaughters,nSons,votepct_rel,gender,age,seniority,nominate_dim1,fullInd,
         interrupted)

text %>%
  count(interrupted)

# party missing value fix
text$party[which(is.na(text$party))] <- text$position[which(is.na(text$party))]
text <- text %>%
  mutate(comparisons = ifelse(grepl('FED',opensecretsID),opensecretsID,
                              ifelse(chamber == 'Senate',paste0('Senate: ',party),
                                     paste0('House: ',party))))
# STM fitting
## use STM helper function from stm package to preprocess the data
processed <- textProcessor(documents = text$textclean,metadata = text)
## convert the processed list into STM input format
out <- prepDocuments(processed$documents,processed$vocab,processed$meta)
## STM traning
fit <- stm(documents = out$documents,vocab = out$vocab,
           K = 100,prevalence = ~ comparisons + interrupted + s(nominate_dim1) + age + seniority + gender,
           max.em.its = 75,data = out$meta,init.type = 'LDA')

# estimate covariate effects
prep <- estimateEffect(1:100 ~ comparisons + interrupted + s(nominate_dim1) + age + seniority + gender,
                       fit,metadata = out$meta,uncertainty = 'Global')


save(prep,fit,out,file = './data/prepped/hearings/stm_results.RData')


# RR1 work
## purpose: run multiple LDA-based topic models (robustness check)
## to see whether the topic patterns are stable under different aggregation levels

# define the function, it runs the loop of LDA topic models for diff # of k
topicModel_fun <- function(train,test,text,id,ks = c(10,20,30,50,70,100,150,200,300,400,500,750,1000)) {
  set.seed(1234)
  it = itoken(train[[text]],ids = train[[id]],progressbar = FALSE)
  v = create_vocabulary(it,stopwords = c(gsub("'",'',stopwords())))
  v = prune_vocabulary(v,doc_proportion_max = .2,term_count_min = 20)
  vectorizer = vocab_vectorizer(v)
  dtm = create_dtm(it,vectorizer)
  tcm <- create_tcm(it,vectorizer,skip_grams_window = 20L)
  
  it = itoken(test[[text]],ids = test[[id]],progressbar = FALSE)
  dtmTest = create_dtm(it,vectorizer)
  
  perplx <- cohres <- NULL
  for(k in ks) {
    lda_model = LDA$new(n_topics = k, doc_topic_prior = 0.1, topic_word_prior = 0.01)
    doc_topic_distr = lda_model$fit_transform(x = dtm, n_iter = 1000,convergence_tol = 0.001, n_check_convergence = 25,progressbar = FALSE)
    tw = lda_model$get_top_words(n = 10, lambda = 1)
    
    # Coherence
    cohTmp <- coherence(tw, tcm, n_doc_tcm = attr(v, 'document_count'))
    cohres <- bind_rows(data.frame(cohTmp) %>%
                          mutate(topic = row.names(.),
                                 k = k),cohres)
    
    # Perplexity
    new_doc_topic_distr = lda_model$transform(dtmTest)
    perplx <- bind_rows(perplx,data.frame(k = k,perplexity = perplexity(dtmTest, topic_word_distribution = lda_model$topic_word_distribution, doc_topic_distribution = new_doc_topic_distr)))
    cat(k,'\n')
  }
  return(list(perplexity = perplx,
              coherence = cohres))
}

# Topic model estimated on speaker-hearing concatenated text (JOP RR1)
load('./data/prepped/finalData_for_NLP.RData')

# combine smaller utterances so each doc has enough text length
text <- finalMerge %>%
  select(docID,fullInd,opensecretsID,textclean,nchars) %>%
  group_by(opensecretsID) %>%
  arrange(opensecretsID,fullInd) %>%
  mutate(over = nchars > 1000) %>%
  group_by(opensecretsID) %>%
  mutate(rwID = cumsum(over != lag(over, default = over[1]))) %>%
  group_by(opensecretsID,rwID) %>%
  mutate(cumchars = cumsum(nchars)) %>%
  ungroup() %>%
  mutate(group_id = cumsum(cumchars > 1000) + 1) %>%
  mutate(rwID = ifelse(lag(cumchars) < 1000,lag(rwID),rwID)) %>%
  group_by(opensecretsID,rwID) %>%
  mutate(combText = paste(textclean,collapse = '. ')) %>%
  mutate(rwID = ifelse(is.na(rwID),-1,rwID)) %>%
  ungroup()

# text2 has uniqueID,textclean
text2 <- text %>%
  ungroup() %>%
  select(docID,opensecretsID,rwID,combText) %>%
  distinct() %>%
  mutate(textclean = lemmatize_strings(prep_fun(combText))) %>%
  filter(!is.na(opensecretsID)) %>%
  mutate(nchars = nchar(textclean)) %>%
  mutate(uniqueID = paste0(docID,opensecretsID,rwID)) %>%
  select(uniqueID,textclean)

# split 50/50 into train/test sets
inds <- sample(1:nrow(text2),size = round(nrow(text2)*.5),replace = F)
train <- text2 %>%
  slice(inds)
test <- text2 %>%
  slice(-inds)

tmRes_Grped <- topicModel_fun(train,test,text = 'textclean',id = 'uniqueID')


save(tmRes_Grped,file = './data/prepped/hearings/topic_robustness_JOPRR1_Grped.RData')


# Choosing the best
it = itoken(text2$textclean,ids = text2$uniqueID,progressbar = FALSE)
v = create_vocabulary(it,ngram = c(1,2),stopwords = c(gsub("'",'',stopwords())))
v = prune_vocabulary(v,doc_proportion_max = .5,term_count_min = 10)
vectorizer = vocab_vectorizer(v)
dtm = create_dtm(it,vectorizer)
lda_model = LDA$new(n_topics = 70, doc_topic_prior = 0.1, topic_word_prior = 0.01)
doc_topic_distr = lda_model$fit_transform(x = dtm, n_iter = 1000,convergence_tol = 0.001, n_check_convergence = 25,progressbar = FALSE)

# reshape results from long to wide format
doc_topic_distr <- doc_topic_distr %>%
  data.frame() %>%
  mutate(id =  text2$uniqueID) %>%
  as_tibble() %>%
  gather(topic,theta,-id) %>%
  mutate(topic = gsub('X','',topic))

text %>%
  mutate(uniqueID = paste0(docID,opensecretsID,rwID)) %>%
  count(uniqueID)


doc_topic_distr %>%
  spread(topic,theta,sep = '70Grped_') %>%
  rename(uniqueID = id) %>%
  count(uniqueID)

# merge topic back
## result: Each row = one document with all topics.
textToMerge <- text %>%
  mutate(uniqueID = paste0(docID,opensecretsID,rwID)) %>%
  left_join(doc_topic_distr %>%
              spread(topic,theta,sep = '70Grped_') %>%
              rename(uniqueID = id))

# merge to main dataset
## purpose: can later analyze interruptions, toxicity, etc. 
## while controlling for topic composition.
utterance_level <- finalMerge %>%
  left_join(textToMerge %>%
              select(docID,opensecretsID,fullInd,matches('topic70Grped')))

# Calculate speaker topics instead
## aggregate by speaker within a hearing
text <- utterance_level %>%
  select(docID,fullInd,opensecretsID,textclean,nchars) %>%
  group_by(docID,opensecretsID) %>%
  summarise(textclean = paste(textclean,collapse = '. ')) %>%
  ungroup() %>%
  mutate(textclean = lemmatize_strings(prep_fun(textclean)),
         uniqueID = paste0(docID,opensecretsID))

# split train/test split (8:2)
inds <- sample(1:nrow(text),size = round(nrow(text)*.8),replace = F)
train <- text %>%
  slice(inds)
test <- text %>%
  slice(-inds)

tmRes_spkr <- topicModel_fun(train,test,text = 'textclean',id = 'uniqueID')

save(tmRes_spkr,file = './data/prepped/hearings/topic_robustness_JOPRR1_spkr.RData')


# Choosing the best
it = itoken(text$textclean,ids = text$uniqueID,progressbar = FALSE)
v = create_vocabulary(it,ngram = c(1,2),stopwords = c(gsub("'",'',stopwords())))
v = prune_vocabulary(v,doc_proportion_max = .5,term_count_min = 10)
vectorizer = vocab_vectorizer(v)
dtm = create_dtm(it,vectorizer)
lda_model = LDA$new(n_topics = 70, doc_topic_prior = 0.1, topic_word_prior = 0.01)
doc_topic_distr = lda_model$fit_transform(x = dtm, n_iter = 1000,convergence_tol = 0.001, n_check_convergence = 25,progressbar = FALSE)


doc_topic_distr <- doc_topic_distr %>%
  data.frame() %>%
  mutate(id =  text$uniqueID) %>%
  as_tibble() %>%
  gather(topic,theta,-id) %>%
  mutate(topic = gsub('X','',topic))

textToMerge <- text %>%
  left_join(doc_topic_distr %>%
              spread(topic,theta,sep = '70Spkr_') %>%
              rename(uniqueID = id))

# merge both topic sets
## topic70Grped_* (grouped-document topics)
## topic70Spkr_* (speaker-level topics)
utterance_level <- utterance_level %>%
  left_join(textToMerge %>%
              select(docID,opensecretsID,matches('topic70Spkr')))


topics_toMerge <- utterance_level %>%
  select(docID,opensecretsID,ind,matches('topic'))

save(topics_toMerge,file = './data/prepped/hearings/topic_models_grp_spkr.RData')


# EOF



################################################################################
##
## Purpose: This script assembles the final dataset and does some final cleaning.
##
## Author: James Bisbee (james.h.bisbee@vanderbilt.edu)
##
##  - Inputs:
##    - ./data/prepped/finalData_for_NLP.RData: Prepped data from 6_DATA_intermediate_build.R
##    - ./data/prepped/hearings/toxicity_resultsFull.RData: Prepped data from 8_DATA_toxicity_prep.R (not run)
##    - ./data/prepped/hearings/topic_models_100.RData: Prepped data from 7_DATA_topic_model_prep.R
##    - ./data/prepped/hearings/topic_models_grp_spkr.RData: Prepped data from 7_DATA_topic_model_prep.R
##  - Outputs:
##    - ./data/prepped/finalData.RData
##
##
## See associated log file for compute environment, package versions, 
##  and date of most recent run.
##
################################################################################
rm(list = ls())
gc()
require(tidyverse)
set.seed(123)

# Compute details
print(paste0('Compute environment from ',Sys.Date(),' run by Jiani'))
if(Sys.info()['sysname'] == 'Windows') {
  ram_size = system("wmic MemoryChip get Capacity", intern = TRUE)[-1]
  model_name = system("wmic cpu get name", intern = TRUE)[2] # nocov
  vendor_id = system("wmic cpu get manufacturer", intern = TRUE)[2] # nocov
  
  print(list(ram = stringr::str_squish(ram_size)[1],
             vendor_id = stringr::str_squish(vendor_id),
             model_name = stringr::str_squish(model_name),
             no_of_cores = parallel::detectCores()))
} else if(Sys.info()['sysname'] == 'Linuxs') {
  splitted <- strsplit(system("ps -C rsession -o %cpu,%mem,pid,cmd", intern = TRUE), " ")
  df <- do.call(rbind, lapply(splitted[-1], 
                              function(x) data.frame(
                                cpu = as.numeric(x[2]),
                                mem = as.numeric(x[4]),
                                pid = as.numeric(x[5]),
                                cmd = paste(x[-c(1:5)], collapse = " "))))
  df
} else {
  cat("If not on Linux or Windows, you'll have to figure out your own solution to seeing the compute environment.")
}

sessionInfo()



# -------------------------------------------------------------- Merging Step 2
# Picking back up here after the toxicity and topic models have been prepared
load('./data/prepped/finalData_for_NLP.RData')

# Loading in the toxicity data: Prepared by ./code/0_DATA_toxicity_prep.R
load('./data/prepped/hearings/toxicity_resultsFull.RData')

# Loading in the topic model data: Prepared by ./code/0_DATA_topic_model_prep.R
load('./data/prepped/hearings/topic_models_100.RData')

# Merging with toxicity
## Summarize multiple toxicity dimensions into three composites 
## and join to each utterance back to utterance dataset by matching fullInd.
toxic_resFull <- toxic_resFull %>% 
  rowwise() %>%
  mutate(combAttack = sum(ATTACK_ON_AUTHOR,IDENTITY_ATTACK,INSULT,THREAT,na.rm=T),
         combToxic = sum(TOXICITY,PROFANITY,SEXUALLY_EXPLICIT,FLIRTATION,INFLAMMATORY,OBSCENE,na.rm=T),
         combIncoh = sum(INCOHERENT,UNSUBSTANTIAL,na.rm=T))

colnames(toxic_resFull) <- paste0('SENT_',colnames(toxic_resFull))

finalMerge <- finalMerge %>%
  left_join(toxic_resFull,by = c('fullInd' = 'SENT_text_id'))


# Merging with topic model loadings
## Spread to wide and join topic proportions back to each utterance.
finalMerge <- finalMerge %>%
  left_join(doc_topic_distr %>% spread(topic,theta,sep = '_'),by = c('fullInd' = 'id'))

colnames(finalMerge)

# Preparing measures
## build utterance-level features
utterance_level <- finalMerge %>%
  mutate(date = as.Date(gsub('fed|\\.txt','',docID))) %>%
  arrange(docID,fullInd,ind) %>%
  mutate(interruptor = ifelse(grepl('--$',lag(textclean)),1,0),  # Interruptions are characterized by two or more hyphens, followed by an end of line
         interrupted = ifelse(grepl('--$',textclean),1,0)) %>%
  mutate(respondingTo = lag(opensecretsID),
         interruptedBy = lag(opensecretsID)) %>%
  group_by(opensecretsID) %>%
  mutate(any = sum(interrupted) > 0) %>% 
  ungroup() %>%
  mutate(opensecretsID = relevel(factor(opensecretsID),ref = 'FEDBERNANKE')) %>%
  mutate(fed = ifelse(grepl('^FED',opensecretsID),1,0), # Indicator for whether the speaker is a Fed chair
         fedResp = ifelse(grepl('^FED',respondingTo),1,0), # Indicator for whether the person being spoken to is a Fed chair
         yellen = ifelse(grepl('YELLEN',respondingTo),1,0), # Indicator for whether the person being spoken to Yellen
         yellenTime = year %in% 2014:2017) %>% # Indicator for whether the hearing is attended by Yellen
  group_by(fed,docID) %>%
  mutate(mind = ifelse(nchars == max(nchars*fed),ind,NA)) %>% # A shortcut to identify the opening statement by the Fed chair (slice out the longest utterance by the Fed chair...NB THIS MIGHT NOT ALWAYS BE CORRECT)
  ungroup() %>%
  group_by(docID) %>%
  arrange(ind) %>%
  fill_('mind',.direction = 'updown') %>%
  # ungroup() %>%
  mutate_at(vars(matches('topic_|nchars|SENT_|yellen_vote|constrain_|cent_decent|oversight_'),
                 age,seniority,votepct,gender,party,nominate_dim1),list(lag = ~lag(.))) %>% # Want nchars, topic loadings, and toxicity measures for the preceding utterance, as well as the covariates of whoever interrupted
  ungroup() %>%
  group_by(opensecretsID) %>%
  mutate(all = n()) %>% 
  ungroup() %>%
  mutate_at(vars(matches('lag')),function(x) ifelse(is.na(x),0,x)) %>% # If the lagged measure is NA, replace with zero.
  mutate(DEM = ifelse(party == 'D',1,0), # Dummies for party of speaker
         GOP = ifelse(party == 'R',1,0),
         Male = ifelse(gender == 'M',1,0)) %>% # Dummy for gender of speaker
  group_by(opensecretsID,docID) %>%
  mutate(tot_utterances = n()) %>%
  ungroup() %>%
  group_by(docID) %>%
  arrange(ind) %>%
  mutate(tot_utterances_lag = lag(tot_utterances)) %>%
  ungroup()

utterance_level <- utterance_level %>%
  mutate(name = ifelse(opensecretsID == 'EXPERTKOHN','DONALD KOHN',
                       ifelse(opensecretsID == 'N00003218','HAROLD E. FORD JR.',
                              ifelse(opensecretsID == 'N00006267','MICHAEL CRAPO',
                                     ifelse(opensecretsID == 'N00026627','PATRICK T. MCHENRY',
                                            ifelse(opensecretsID == 'N00029070','JAMES A. HIMES',
                                                   ifelse(opensecretsID == 'N00029277','GARY C. PETERS',name)))))))


# Create a speaker-by-hearing version of the dataset
speaker_level <- utterance_level %>%
  filter(ind > mind) %>% # Drop the utterances up to the opening statement by the Fed chair
  group_by(opensecretsID,party,gender,speaker,respondingTo,fedResp,yellenTime,
           stab,docID,chamber,nDaughters,nKids,firstDaughter,nSons,
           constrain_empower_tot,oversight_indep_tot,yellen_vote,votepct,
           seniority,age,nominate_dim1,fed,year,date,tot_utterances)  %>%
  summarise(denom = n(), # Total number of utterances by speaker by hearing
            interrupted = sum(interrupted,na.rm=T), # Total number of utterances that are interrupted by speaker by hearing
            interruptor = sum(interruptor,na.rm=T), # Total number of utterances that interrupt by speaker by hearing
            interruptedPct = interrupted*100/denom, # Proportions of utterances that are interrupted by speaker by hearing
            interruptorPct = interruptor*100/denom, # Proportions of utterances that interrupt by speaker by hearing
            combAttack = sum(SENT_combAttack,na.rm=T)/denom, # The proportion of all utterances that are an attack, by speaker by hearing
            combIncoh = sum(SENT_combIncoh,na.rm=T)/denom, # The proportion of all utterances that are incoherent, by speaker by hearing
            combToxic = sum(SENT_combToxic,na.rm=T)/denom) %>% # The proportion of interruptions that are toxic, by speaker by hearing
  ungroup() %>%
  distinct() %>%
  mutate(respondingTo = relevel(factor(respondingTo),ref = 'FEDBERNANKE'))


# Adding in the new topics from the revisions
load('./data/prepped/hearings/topic_models_grp_spkr.RData')
utterance_level <- utterance_level %>%
  left_join(topics_toMerge)

save(utterance_level,speaker_level,file = './data/prepped/finalData.RData')

# Check topic columns
grep('^topic(70Grped|70Spkr)_', names(utterance_level), value = TRUE)[1:10]

# Check toxicity columns
grep('^SENT_comb', names(utterance_level), value = TRUE)

# Check interruption variables
colnames(utterance_level)[grepl('interrupt', names(utterance_level))]

# Speaker-level sample
head(speaker_level)

# EOF

