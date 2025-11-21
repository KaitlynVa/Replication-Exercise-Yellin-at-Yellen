
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
## Purpose: This script creates Figure 1, along with SI figure 7
##
## Author: James Bisbee (james.h.bisbee@vanderbilt.edu)
##
## Input Files:
##  - ./data/prepped/finalData.RData: Prepped data from 9_DATA_final_build.R
##
## Output Files:
##  - ./Figures/MS_figure_1.pdf
##  - ./Figures/SI_figure_7.pdf
##
##
## See associated log file for compute environment, package versions, 
##  and date of most recent run.
##
################################################################################
rm(list = ls())
gc()
require(tidyverse)
require(ggridges)

set.seed(123)

# Compute details
print(paste0('Compute environment from ',Sys.Date(),' run by Bisbee'))
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


load('./output_data/finalData.RData')

toplot1 <- utterance_level %>%
  group_by(position,party,gender,name) %>%
  summarise(all = n(),
            interrupted = sum(interrupted)) %>%
  ungroup() %>%
  mutate(propInterrupt = interrupted/all,
         all =  all - interrupted) %>%
  filter(all > 100) %>%
  mutate(id = paste0(str_to_title(name),' (',position,': ',party,'-',gender,')')) %>%
  select(all,interrupted,propInterrupt,id) %>%
  gather(key,value,-propInterrupt,-id)

toplot2 <- utterance_level %>%
  mutate(interruptor = ifelse(interruptor == 1 & interrupted == 1,1,0)) %>%
  group_by(position,party,gender,name) %>%
  summarise(all = n(),
            interruptor = sum(interruptor)) %>%
  ungroup() %>%
  mutate(propInterrupt = interruptor/all,
         all =  all - interruptor) %>%
  mutate(id = paste0(str_to_title(name),' (',position,': ',party,'-',gender,')')) %>%
  select(all,interruptor,propInterrupt,id) %>%
  gather(key,value,-propInterrupt,-id) %>%
  filter(id %in% toplot1$id)

toplot <- toplot1 %>%
  mutate(type = 'Interrupted') %>%
  bind_rows(toplot2 %>%
              mutate(type = 'Interruptor'))

pdf('./Figures/MS_figure_1.pdf',width = 8,height = 7)


toplot1 %>%
  mutate(key = ifelse(grepl('interrupt',key),'Interruption','All')) %>%
  ggplot(aes(x = value,y = reorder(id,value),group = key,
             color = key,
             fill = key,label = propInterrupt)) + 
  geom_bar(position = 'stack',stat = 'identity') + 
  geom_text(data = toplot1 %>%
              group_by(id,propInterrupt) %>%
              summarise(value = sum(value)) %>%
              mutate(propInterruptedLab = paste0(round(propInterrupt*100),'%'),
                     key = 'All'),
            color = 'black',nudge_x = 80,hjust = 0,
            aes(size = propInterrupt,label = propInterruptedLab),
            alpha = 1) +
  xlab('Total Utterances') + ylab('') + 
  scale_size_continuous(guide = 'none',range = c(2,7)) + 
  scale_x_continuous(expand = c(.15,0)) + 
  scale_fill_manual(name = '',values = c('grey80','grey30')) +
  scale_alpha_manual(name = 'Type',values = c(.5,1)) +
  scale_color_manual(name = '',values = c('white','black')) +
  theme_ridges() + 
  theme(legend.position = 'bottom',
        strip.background = element_rect(fill = 'white',color = 'black',size = .5,linetype = 'solid'),
        axis.text.y = element_text(size = 10,vjust = .5))
dev.off()

# SI
toplot1 <- utterance_level %>%
  group_by(position,party,gender,name) %>%
  summarise(all = n(),
            interrupted = sum(interrupted)) %>%
  ungroup() %>%
  mutate(propInterrupt = interrupted/all,
         all =  all - interrupted) %>%
  filter(all > 30) %>%
  mutate(id = paste0(str_to_title(name),' (',position,': ',party,'-',gender,')')) %>%
  select(all,interrupted,propInterrupt,id) %>%
  gather(key,value,-propInterrupt,-id)

pdf('./Figures/SI_figure_7.pdf',width = 8,height = 10)
toplot1 %>%
  mutate(key = ifelse(grepl('interrupt',key),'Interruption','All')) %>%
  ggplot(aes(x = value,y = reorder(id,value),group = key,
             color = key,
             fill = key,label = propInterrupt)) + 
  geom_bar(position = 'stack',stat = 'identity') + 
  geom_text(data = toplot1 %>%
              group_by(id,propInterrupt) %>%
              summarise(value = sum(value)) %>%
              mutate(propInterruptedLab = paste0(round(propInterrupt*100),'%'),
                     key = 'All'),
            color = 'black',nudge_x = 80,hjust = 0,
            aes(size = propInterrupt,label = propInterruptedLab),
            alpha = 1) +
  xlab('Total Utterances') + ylab('') + 
  scale_size_continuous(guide = 'none',range = c(2,7)) + 
  scale_x_continuous(expand = c(.15,0)) + 
  scale_fill_manual(name = '',values = c('grey80','grey30')) +
  scale_alpha_manual(name = 'Type',values = c(.5,1)) +
  scale_color_manual(name = '',values = c('white','black')) +
  theme_ridges() + 
  theme(legend.position = 'bottom',
        strip.background = element_rect(fill = 'white',color = 'black',size = .5,linetype = 'solid'),
        axis.text.y = element_text(size = 8,vjust = .5))
dev.off()

# EOF

################################################################################
##
## Purpose: This script creates Figure 2
##
## Author: James Bisbee (james.h.bisbee@vanderbilt.edu)
##
## Input Files:
##  - ./data/prepped/finalData.RData: Prepped data from 9_DATA_final_build.R
##
## Output Files:
##  - ./output/figures/MS_figure_2.pdf
##
##
## See associated log file for compute environment, package versions, 
##  and date of most recent run.
##
################################################################################
rm(list = ls())
gc()
require(tidyverse)
require(ggridges)

set.seed(123)

# Compute details
print(paste0('Compute environment from ',Sys.Date(),' run by Bisbee'))
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


load('./output_data/finalData.RData')

toplot <- utterance_level %>%
  filter(chamber == 'House') %>%
  mutate(fed = ifelse(fed == 0 & GOP == 1 & Male == 1,'GOP - Male',
                      ifelse(fed == 0 & GOP == 1 & Male == 0,'GOP - Female',
                             ifelse(fed == 0 & DEM == 1 & Male == 1,'DEM - Male',
                                    ifelse(fed == 0 & DEM == 1 & Male == 0,'DEM - Female',
                                           ifelse(fed == 0 & GOP == 0 & DEM == 0,'Expert','Fed Chair')))))) %>%
  filter(fed != 'Expert') %>%
  mutate(fed = ifelse(grepl('Male',fed),'Male',
                      ifelse(grepl('Female',fed),'Female',fed))) %>%
  group_by(fed,date,yellenTime) %>%
  summarise(interrupted = mean(interrupted),
            nchars = mean(nchars,na.rm=T),
            n = n(),
            interruptor = mean(interruptor)) %>%
  gather(type,proportion,-fed,-date,-yellenTime,-nchars,-n) %>%
  bind_rows(utterance_level %>%
              filter(chamber == 'House') %>%
              mutate(fed = ifelse(fed == 0 & GOP == 1 & Male == 1,'GOP - Male',
                                  ifelse(fed == 0 & GOP == 1 & Male == 0,'GOP - Female',
                                         ifelse(fed == 0 & DEM == 1 & Male == 1,'DEM - Male',
                                                ifelse(fed == 0 & DEM == 1 & Male == 0,'DEM - Female',
                                                       ifelse(fed == 0 & GOP == 0 & DEM == 0,'Expert','Fed Chair')))))) %>%
              filter(fed != 'Expert') %>%
              mutate(fed = ifelse(grepl('GOP',fed),'GOP',
                                  ifelse(grepl('DEM',fed),'DEM',fed))) %>%
              group_by(fed,date,yellenTime) %>%
              summarise(interrupted = mean(interrupted),
                        nchars = mean(nchars,na.rm=T),
                        n = n(),
                        interruptor = mean(interruptor)) %>%
              gather(type,proportion,-fed,-date,-yellenTime,-nchars,-n))

toplot <- utterance_level %>%
  arrange(docID,ind) %>%
  mutate(fed = ifelse(fed == 0,'Legislators','Fed Chair')) %>%
  mutate(interruptor2 = ifelse(interruptor == 1 & lag(interrupted,2) == 0,1,0)) %>%
  group_by(fed,date,yellenTime,chamber) %>%
  summarise(interrupted = mean(interrupted),
            nchars = mean(nchars,na.rm=T),
            n = n(),
            interruptor = mean(interruptor2)) %>%
  gather(type,proportion,-fed,-date,-yellenTime,-nchars,-n,-chamber) %>%
  mutate(grp = paste0(ifelse(date < as.Date('2014-01-01'),'preYellen',
                             ifelse(date < as.Date('2018-01-01'),'Yellen','postYellen')),fed)) %>%
  mutate(type = ifelse(type == 'interrupted','Speaker is being interrupted',
                       'Speaker is interrupting someone else')) 

pdf('./Figures/MS_figure_2.pdf',width = 7,height = 5)

toplot %>%
  ggplot(aes(x = date,y = proportion,weight = n,size = n,
             label = grp,
             shape = factor(fed),
             color = factor(fed),
             linetype = factor(fed),
             group = factor(grp))) + 
  geom_point(alpha = .5) + 
  scale_y_continuous(labels = scales::percent,limits = c(0,.45)) + 
  geom_vline(xintercept = as.Date(c('2014-01-01','2018-01-01'))) + 
  annotate(geom = 'rect',xmin = as.Date('2014-01-01'),xmax = as.Date('2018-01-01'),
           ymin = -Inf,ymax = Inf,
           alpha = .1,fill = 'grey50') +
  geom_smooth(show.legend = F,se = F,method = 'lm',formula = 'y ~ poly(x,1)') +
  facet_grid(chamber~type) + 
  scale_color_manual(values = c('grey10','grey50')) + 
  scale_shape_manual(values = c(19,15)) + 
  scale_linetype_manual(values = c('solid','dashed'))  + 
  geom_text(data = toplot %>%
              filter(chamber == 'House',
                     grp == 'YellenFed Chair',
                     date == as.Date('2016-02-10')) %>%
              mutate(grp = "Yellen",
                     date = as.Date('2016-01-01'),
                     proportion = Inf),vjust = 1,show.legend = FALSE) + 
  annotate(geom = 'rect',xmin = as.Date('2014-01-01'),xmax = as.Date('2018-01-01'),
           ymin = -Inf,ymax = Inf,alpha = .2) +
  theme_bw() + 
  theme(legend.position = 'bottom') + 
  labs(x = 'Date',y = 'Proportion of utterances',
       color = 'Speaker',shape = 'Speaker',size = '# of utterances',
       linetype = 'Speaker') + 
  guides(shape = guide_legend(override.aes = list(size = 5)))
dev.off()

#EOF


################################################################################
##
## Purpose: This script creates Figure 3, along with SI figure 8 and SI table 3.
##
## Author: James Bisbee (james.h.bisbee@vanderbilt.edu)
##
## Input Files:
##  - ./data/prepped/finalData.RData: Prepped data from 9_DATA_final_build.R
##
## Output Files:
##  - ./output/figures/MS_figure_3.pdf
##  - ./output/tables/SI_table_3.tex
##  - ./output/figures/SI_figure_8.pdf
##
##
## See associated log file for compute environment, package versions, 
##  and date of most recent run.
##
################################################################################
rm(list = ls())
gc()
require(tidyverse)
require(ggridges)
require(fixest)
library(fixest)

set.seed(123)

# Compute details
print(paste0('Compute environment from ',Sys.Date(),' run by Bisbee'))
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


load('./output_data/finalData.RData')

dims <- colnames(utterance_level %>% select(matches('SENT_'),-matches('_lag|error')) %>% select(-matches('SEVERE|AUTHOR|LIKELY')))

# Speaker intercepts, controlling for document, with minimum utterances > 100
mod1 <- feols(as.formula(paste0('interrupted ~ factor(opensecretsID)',
                                ' + poly(scale(log(nchars)),3) + interruptor +', # U-level covariates
                                paste(paste0('topic_',1:100),collapse = ' + '),
                                ' + ',
                                paste(paste0('scale(',dims[-which(grepl('comb',dims))],'_lag)'),collapse = ' + '),
                                ' + poly(scale(log(tot_utterances_lag)),3) + scale(votepct_lag) + gender_lag + scale(seniority_lag)', # X-level covariates
                                ' + scale(age_lag) + scale(nominate_dim1_lag) + constrain_empower_tot_lag + yellen_vote_lag',
                                '| docID')),
              utterance_level %>% 
                filter(all >= 100 & !grepl('EXPERT',opensecretsID) & ind > mind) %>%
                mutate(opensecretsID = paste0(opensecretsID,position)),
              cluster = 'interruptedBy')

toplot <- mod1$coeftable %>%
  data.frame() %>%
  rename_all(function(x) gsub('Estimate','est',gsub('Std..Error','se',gsub('t.value','tstat',gsub('Pr...t..','pval',x))))) %>%
  mutate(covs =rownames(.)) %>%
  filter(grepl('opensecrets',covs),
         !is.na(est)) %>%
  mutate(covs =  gsub('factor\\(opensecretsID\\)','',covs)) %>%
  as_tibble() %>%
  left_join(utterance_level %>% select(opensecretsID,position,party,gender,stab,speaker,name,all) %>% distinct() %>%
              mutate(opensecretsID = paste0(opensecretsID,position)),
            by = c('covs' = 'opensecretsID')) %>%
  mutate(stab = ifelse(grepl('Chairperson',covs),'DC',stab),
         id = gsub(' Chair','',gsub('Legislator','MC',gsub('Committee','Comm',gsub('Fed Chair: ','',paste0(str_to_title(name),' [',position,': ',party,'-',stab,' (',gender,')]'))))),
         fedID = ifelse(grepl('FED',covs),'Fed Chair','Not'))

cols <- ifelse(toplot$position[order(toplot$est)] == 'Committee Chair','blue',
               ifelse(toplot$position[order(toplot$est)] == 'Fed Chair','red','grey40'))

pdf('./Figures/MS_figure_3.pdf',width = 7,height = 7)
toplot %>%
  ggplot(aes(x = est,y = reorder(id,est),color = position,shape = position)) + 
  geom_vline(xintercept = 0,linetype = 'dashed') + 
  geom_errorbarh(aes(xmin = est-2*se,xmax = est+2*se),height = 0,linewidth = .1) + 
  geom_point(aes(size = all),fill = 'white') + 
  theme_ridges() + 
  scale_color_manual(name = '',values = c('grey50','black','grey40')) +
  scale_shape_manual(name = '',values = c(15,19,21)) + 
  scale_size_continuous(name = 'Total Utterances',breaks = c(100,500,1000),range = c(2,7)) + 
  scale_y_discrete(expand = c(.05,0)) + 
  geom_text(data = toplot %>%
              filter(position == 'Fed Chair',
                     est < 0),show.legend  = FALSE,
            aes(x = est+2*se,y = reorder(id,est),label = gsub('\\[Fed-DC |\\]','',id)),size = 5,hjust = 0) + 
  geom_text(data = toplot %>%
              filter(position == 'Fed Chair',
                     est > 0),show.legend  = FALSE,
            aes(x = est-2*se,y = reorder(id,est),label = gsub('\\[Fed-DC |\\]','',id)),size = 5,hjust = 1.1,vjust = .5) + 
  geom_text(data = toplot %>%
              filter(position == 'Legislator'),show.legend  = FALSE,
            aes(x = est,y = reorder(id,est),label = gsub('.*\\(|\\).*','',id)),size = 2.5,hjust = .5,vjust = .5) + 
  geom_text(data = toplot %>%
              filter(position == 'Committee Chair'),show.legend  = FALSE,
            aes(x = est,y = reorder(id,est),label = gsub('.*\\(|\\).*','',id)),size = 2.5,hjust = .5,vjust = .5,color = 'white') + 
  xlab('Fixed Effect Intercept (Reference = Bernanke)') + 
  theme(legend.position = 'right',legend.box="vertical",panel.grid.major.y = element_blank(),
        axis.text.y = element_blank()) + 
  ylab('')
dev.off()


# EOF

################################################################################
##
## Purpose: This script creates Figure 4, along with SI table 4
##
## Author: James Bisbee (james.h.bisbee@vanderbilt.edu)
##
## Input Files:
##  - ./data/prepped/finalData.RData: Prepped data from 9_DATA_final_build.R
##
## Output Files:
##  - ./output/figures/MS_figure_4.pdf
##  - ./output/tables/SI_table_4.tex
##
##
## See associated log file for compute environment, package versions, 
##  and date of most recent run.
##
################################################################################
rm(list = ls())
gc()
require(tidyverse)
require(ggridges)
require(fixest)

set.seed(123)

# Compute details
print(paste0('Compute environment from ',Sys.Date(),' run by Bisbee'))
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


load('./output_data/finalData.RData')
dims <- colnames(utterance_level %>% select(matches('SENT_'),-matches('_lag|error')) %>% select(-matches('SEVERE|AUTHOR|LIKELY')))

# Speaker intercepts, controlling for document, with minimum utterances > 100
modSenateInted <- feols(as.formula(paste0('interrupted ~ factor(opensecretsID)',
                                          ' + poly(scale(log(nchars)),3) + interruptor +', # U-level covariates
                                          paste(paste0('topic_',1:100),collapse = ' + '),
                                          ' + ',
                                          paste(paste0('scale(',dims[-which(grepl('comb',dims))],'_lag)'),collapse = ' + '),
                                          ' + poly(scale(log(tot_utterances_lag)),3) + scale(votepct_lag) + gender_lag + scale(seniority_lag)', # X-level covariates
                                          ' + scale(age_lag) + scale(nominate_dim1_lag) + constrain_empower_tot_lag + yellen_vote_lag',
                                          '| docID')),
                        utterance_level %>% 
                          filter(all > 30,
                                 !grepl('EXPERT',opensecretsID),
                                 chamber == 'Senate',
                                 ind > mind),
                        cluster = 'interruptedBy')

toplotChamber <- modSenateInted$coeftable %>%
  data.frame() %>%
  rename_all(function(x) gsub('Estimate','est',gsub('Std..Error','se',gsub('t.value','tstat',gsub('Pr...t..','pval',x))))) %>%
  mutate(covs =rownames(.)) %>%
  filter(grepl('FED',covs)) %>%
  mutate(covs =  gsub('factor\\(opensecretsID\\)','',covs)) %>%
  as_tibble() %>%
  left_join(utterance_level %>% select(opensecretsID,gender,name) %>% distinct(),
            by = c('covs' = 'opensecretsID')) %>%
  mutate(stab = ifelse(grepl('FED',covs),'DC',stab),
         id = paste0(str_to_title(name),' (',gender,')'),
         fedID = ifelse(grepl('FED',covs),'Fed Chair','Not'))

modHouseInted <- feols(as.formula(paste0('interrupted ~ factor(opensecretsID)',
                                         ' + poly(scale(log(nchars)),3) + interruptor +', # U-level covariates
                                         paste(paste0('topic_',1:100),collapse = ' + '),
                                         ' + ',
                                         paste(paste0('scale(',dims[-which(grepl('comb',dims))],'_lag)'),collapse = ' + '),
                                         ' + poly(scale(log(tot_utterances_lag)),3) + scale(votepct_lag) + gender_lag + scale(seniority_lag)', # X-level covariates
                                         ' + scale(age_lag) + scale(nominate_dim1_lag) + constrain_empower_tot_lag + yellen_vote_lag',
                                         '| docID')),
                       utterance_level %>% 
                         filter(all > 30,
                                !grepl('EXPERT',opensecretsID),
                                chamber == 'House',
                                ind > mind),
                       cluster = 'interruptedBy')

toplotChamber <- toplotChamber %>%
  mutate(chamber = 'Senate') %>%
  bind_rows(modHouseInted$coeftable %>%
              data.frame() %>%
              rename_all(function(x) gsub('Estimate','est',gsub('Std..Error','se',gsub('t.value','tstat',gsub('Pr...t..','pval',x))))) %>%
              mutate(covs =rownames(.)) %>%
              filter(grepl('FED',covs)) %>%
              mutate(covs =  gsub('factor\\(opensecretsID\\)','',covs)) %>%
              as_tibble() %>%
              left_join(utterance_level %>% select(opensecretsID,gender,name) %>% distinct(),
                        by = c('covs' = 'opensecretsID')) %>%
              mutate(stab = ifelse(grepl('FED',covs),'DC',stab),
                     id = paste0(str_to_title(name),' (',gender,')'),
                     fedID = ifelse(grepl('FED',covs),'Fed Chair','Not'),
                     chamber = 'House'))




# Interruptors
modSenateIntor <- feols(as.formula(paste0('interruptor ~ factor(opensecretsID)',
                                          ' + poly(scale(log(nchars)),3) + interrupted +', # U-level covariates
                                          paste(paste0('topic_',1:100),collapse = ' + '),
                                          ' + ',
                                          paste(paste0('scale(',dims[-which(grepl('comb',dims))],'_lag)'),collapse = ' + '),
                                          ' + poly(scale(log(tot_utterances_lag)),3) + scale(votepct_lag) + gender_lag + scale(seniority_lag)', # X-level covariates
                                          ' + scale(age_lag) + scale(nominate_dim1_lag) + constrain_empower_tot_lag + yellen_vote_lag',
                                          '| docID')),
                        utterance_level %>% 
                          filter(all > 30,
                                 !grepl('EXPERT',opensecretsID),
                                 chamber == 'Senate',
                                 ind > mind),
                        cluster = 'opensecretsID')

modHouseIntor <- feols(as.formula(paste0('interruptor ~ factor(opensecretsID)',
                                         ' + poly(scale(log(nchars)),3) + interrupted +', # U-level covariates
                                         paste(paste0('topic_',1:100),collapse = ' + '),
                                         ' + ',
                                         paste(paste0('scale(',dims[-which(grepl('comb',dims))],'_lag)'),collapse = ' + '),
                                         ' + poly(scale(log(tot_utterances_lag)),3) + scale(votepct_lag) + gender_lag + scale(seniority_lag)', # X-level covariates
                                         ' + scale(age_lag) + scale(nominate_dim1_lag) + constrain_empower_tot_lag + yellen_vote_lag',
                                         '| docID')),
                       utterance_level %>% 
                         filter(all > 30,
                                !grepl('EXPERT',opensecretsID),
                                chamber == 'House',
                                ind > mind),
                       cluster = 'opensecretsID')

toplotChamber <- toplotChamber %>%
  mutate(outcome = 'Interrupted') %>%
  bind_rows(modSenateIntor$coeftable %>%
              data.frame() %>%
              rename_all(function(x) gsub('Estimate','est',gsub('Std..Error','se',gsub('t.value','tstat',gsub('Pr...t..','pval',x))))) %>%
              mutate(covs =rownames(.)) %>%
              filter(grepl('FED',covs)) %>%
              mutate(covs =  gsub('factor\\(opensecretsID\\)','',covs)) %>%
              as_tibble() %>%
              left_join(utterance_level %>% select(opensecretsID,gender,name) %>% distinct(),
                        by = c('covs' = 'opensecretsID')) %>%
              mutate(stab = ifelse(grepl('FED',covs),'DC',stab),
                     id = paste0(str_to_title(name),' (',gender,')'),
                     fedID = ifelse(grepl('FED',covs),'Fed Chair','Not')) %>%
              mutate(outcome = 'Interruptor',
                     chamber = 'Senate'))


toplotChamber <- toplotChamber %>%
  bind_rows(modHouseIntor$coeftable %>%
              data.frame() %>%
              rename_all(function(x) gsub('Estimate','est',gsub('Std..Error','se',gsub('t.value','tstat',gsub('Pr...t..','pval',x))))) %>%
              mutate(covs =rownames(.)) %>%
              filter(grepl('FED',covs)) %>%
              mutate(covs =  gsub('factor\\(opensecretsID\\)','',covs)) %>%
              as_tibble() %>%
              left_join(utterance_level %>% select(opensecretsID,gender,name) %>% distinct(),
                        by = c('covs' = 'opensecretsID')) %>%
              mutate(stab = ifelse(grepl('FED',covs),'DC',stab),
                     id = paste0(str_to_title(name),' (',gender,')'),
                     fedID = ifelse(grepl('FED',covs),'Fed Chair','Not')) %>%
              mutate(outcome = 'Interruptor',
                     chamber = 'House'))


pdf('./Figures/MS_figure_4.pdf',width = 7,height = 4)

# EOF


################################################################################
##
## Purpose: This script creates Figure 5
##
## Author: James Bisbee (james.h.bisbee@vanderbilt.edu)
##
## Input Files:
##  - ./data/prepped/finalData.RData: Prepped data from 9_DATA_final_build.R
##
## Output Files:
##  - ./output/figures/MS_figure_5.pdf
##
##
## See associated log file for compute environment, package versions, 
##  and date of most recent run.
##
################################################################################
rm(list = ls())
gc()
require(tidyverse)
require(ggridges)
require(fixest)

set.seed(123)

# Compute details
print(paste0('Compute environment from ',Sys.Date(),' run by Bisbee'))
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


load('./output_data/finalData.RData')

# data prep
keeps <- utterance_level %>%
  filter(grepl('FED',respondingTo)) %>%
  filter(!grepl('FED',opensecretsID)) %>%
  group_by(opensecretsID,respondingTo) %>%
  summarise(denom = n(),
            interrupted = sum(interrupted,na.rm=T)/all,
            interruptor = sum(interruptor,na.rm=T)/all) %>%
  ungroup() %>%
  distinct() %>%
  group_by(opensecretsID) %>%
  mutate(n=n()) %>%
  filter(n > 1) %>%
  filter(denom >= 20,
         opensecretsID != respondingTo)

toplot <- utterance_level %>%
  filter(opensecretsID %in% (keeps %>% filter(grepl('YELLEN',respondingTo)) %>% .$opensecretsID),
         ind > mind) %>%
  group_by(opensecretsID,party,gender,name,respondingTo,stab) %>%
  summarise(denom = n(),
            interrupted = sum(interrupted,na.rm=T),
            interruptor = sum(interruptor,na.rm=T),
            interruptedPct = interrupted*100/all,
            interruptorPct = interruptor*100/all) %>%
  ungroup() %>%
  filter(grepl('FED',respondingTo)) %>%
  mutate(id = paste0(str_to_title(name),' [',party,'-',stab,' (',gender,')]')) %>%
  distinct()


pdf('./Figures/MS_figure_5.pdf',width = 7,height = 7)
toplot %>%
  mutate(id = factor(id,levels = toplot %>% filter(grepl('YELLEN',respondingTo)) %>%
                       arrange(interruptorPct) %>% .$id),
         Interrupting = ifelse(respondingTo == 'FEDYELLEN','Yellen','Male Fed Chairs')) %>%
  filter(!is.na(id)) %>%
  ggplot(aes(x = interruptorPct,y = id,fill = Interrupting,size = denom,shape = Interrupting)) + 
  geom_point(alpha = .6) + 
  theme_ridges() + 
  scale_size_continuous(name = '# Interactions') + 
  scale_shape_manual(values = c(21,22,24,25)) + 
  xlab('% Interrupting out of Total Utterances') + ylab('') + 
  guides(shape = guide_legend(override.aes = list(size = 4))) + 
  theme(axis.text.y = element_text(size = 8))
dev.off()

# EOF

################################################################################
##
## Purpose: This script creates Figure 6, along with SI table 5
##
## Author: James Bisbee (james.h.bisbee@vanderbilt.edu)
##
## Input Files:
##  - ./data/prepped/finalData.RData: Prepped data from 9_DATA_final_build.R
##
## Output Files:
##  - ./output/figures/MS_figure_6.pdf
##  - ./output/tables/SI_table_5.tex
##
##
## See associated log file for compute environment, package versions, 
##  and date of most recent run.
##
################################################################################
rm(list = ls())
gc()
require(tidyverse)
require(ggridges)
require(fixest)
require(ggrepel)
require(marginaleffects)

set.seed(123)

# Compute details
print(paste0('Compute environment from ',Sys.Date(),' run by Bisbee'))
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


load('./output_data/finalData.RData')



# Rigorous fixed effects
dyadToAnal <- utterance_level %>%
  mutate_at(vars(matches('lag')),function(x) ifelse(is.na(x),0,x)) %>%
  mutate(votepct_rel = ifelse(is.infinite(votepct_rel),1,votepct_rel),
         respondingTo = relevel(factor(respondingTo),ref = 'FEDBERNANKE'))

dims <- colnames(utterance_level %>% select(matches('SENT_'),-matches('_lag|error')) %>% select(-matches('SEVERE|AUTHOR|LIKELY')))

# diff-in-diff
summary(mod <- feols(as.formula(paste0('interruptor ~ fedResp*yellenTime + poly(year,3) + ',
                                       'poly(tot_utterances_log,3) + poly(nchars_lag_log,3) + ',
                                       paste(paste0('topic_',1:100,'_lag'),collapse = ' + '),
                                       ' + ',
                                       paste(paste0('scale(',dims[which(grepl('comb',dims))],'_lag)'),collapse = ' + '),
                                       '| opensecretsID + chamber')),
                     utterance_level %>%
                       mutate(yellenTime = ifelse(yellenTime,1,0),
                              tot_utterances_log = log(tot_utterances),
                              clust = paste0(opensecretsID,docID),
                              nchars_lag_log = log(nchars_lag)) %>%
                       filter(ind > mind),
                     cluster = c('clust')))

summary(mod)

# toplotP <- plot_cme(mod,variables = 'fedResp',condition = (c('yellenTime','party')),draw = F)
toplot <- plot_comparisons(mod,variables = 'fedResp',condition = (c('yellenTime','party')),draw = F)

toadd <- toplot %>%
  select(estimate,std.error) %>%
  summarise(diff = diff(estimate),
            se = sqrt(sum(std.error^2)))

toplot <- toplot %>%
  bind_cols(toadd)

pFull <- toplot %>%
  ggplot(aes(x = factor(yellenTime),y = estimate,group = term)) + 
  geom_point() + 
  geom_line() + 
  geom_errorbar(aes(ymin = conf.low,ymax = conf.high),width = 0) + 
  theme_ridges() + 
  scale_x_discrete(labels = c('Male Fed Chairs','Yellen')) + 
  geom_hline(yintercept = 0,linetype = 'dashed') + 
  geom_label(data = toplot %>%
               filter(yellenTime == 1),x = 1.5,
             aes(y = estimate - diff/2,label = paste0(round(diff,3),' (',round(se,3),')')),
             hjust = .5) + 
  labs(x = NULL,y = 'Increase in Interruptions\nAssociated with Fed Chair')


# Moderator by Party
summary(modP <- feols(as.formula(paste0('interruptor ~ fedResp*yellenTime*party + poly(year,3) + ',
                                        'poly(tot_utterances_log,3) + poly(nchars_lag_log,3) + ',
                                        paste(paste0('topic_',1:100,'_lag'),collapse = ' + '),
                                        ' + ',
                                        paste(paste0('scale(',dims[which(grepl('comb',dims))],'_lag)'),collapse = ' + '),
                                        '| opensecretsID + chamber')),
                      utterance_level %>%
                        mutate(yellenTime = ifelse(yellenTime,1,0),
                               tot_utterances_log = log(tot_utterances),
                               clust = paste0(opensecretsID,docID),
                               nchars_lag_log = log(nchars_lag)) %>%
                        filter(ind > mind,
                               party %in% c('D','R')),
                      cluster = c('clust')))

toplotP <- plot_comparisons(modP,variables = 'fedResp',condition = (c('yellenTime','party')),draw = F)

toplotP <- toplotP %>%
  left_join(toplotP %>%
              group_by(party) %>%
              summarise(diff = diff(estimate),
                        se = sqrt(sum(std.error^2))) %>%
              ungroup() %>%
              mutate(pval = 2*pt(diff/se,df = 1000,lower.tail = F),
                     stars = ifelse(pval < .001,'***',ifelse(pval < .01,'**',
                                                             ifelse(pval < .05,'*','')))))



pParty <- toplotP %>%
  ggplot(aes(x = factor(yellenTime),y = estimate,group = party,color = party,shape = party)) + 
  geom_point(position = position_dodge(width = 0)) + 
  geom_errorbar(aes(ymin = conf.low,ymax = conf.high),width = 0,position = position_dodge(width = 0)) + 
  geom_line(position = position_dodge(width = 0)) + 
  theme_ridges() + 
  scale_x_discrete(labels = c('Male Chairs','Yellen'),expand = c(.1,.1,.1,.7)) + 
  geom_hline(yintercept = 0,linetype = 'dashed') + 
  geom_label(data = toplotP %>%
               filter(yellenTime == 1),
             aes(y = estimate,label = paste0(party,': ',round(diff,3),'\n     (',round(se,2),')')),
             hjust = -.1) + 
  scale_color_manual(guide = 'none',name = 'Party',values = c('blue','red')) + 
  scale_shape_discrete(guide = 'none',name = 'Party') + 
  labs(x = NULL,y = NULL,
       subtitle = 'By Party')

pParty

summary(modG <- feols(as.formula(paste0('interruptor ~ fedResp*yellenTime*gender + poly(year,3) + ',
                                        'poly(tot_utterances_log,3) + poly(nchars_lag_log,3) + ',
                                        paste(paste0('topic_',1:100,'_lag'),collapse = ' + '),
                                        ' + ',
                                        paste(paste0('scale(',dims[which(grepl('comb',dims))],'_lag)'),collapse = ' + '),
                                        '| opensecretsID + chamber')),
                      utterance_level %>%
                        mutate(yellenTime = ifelse(yellenTime,1,0),
                               tot_utterances_log = log(tot_utterances),
                               clust = paste0(opensecretsID,docID),
                               nchars_lag_log = log(nchars_lag)) %>%
                        filter(ind > mind),
                      cluster = c('clust')))

toplotG <- plot_comparisons(modG,variables = 'fedResp',condition = (c('yellenTime','gender')),draw = F)


toplotG <- toplotG %>%
  left_join(toplotG %>%
              group_by(gender) %>%
              summarise(diff = diff(estimate),
                        se = sqrt(sum(std.error^2))) %>%
              ungroup() %>%
              mutate(pval = 2*pt(diff/se,df = 1000,lower.tail = F),
                     stars = ifelse(pval < .001,'***',ifelse(pval < .01,'**',
                                                             ifelse(pval < .05,'*','')))))

pGender <- toplotG %>%
  ggplot(aes(x = factor(yellenTime),y = estimate,group = gender,color = gender,shape = gender)) + 
  geom_point(position = position_dodge(width = 0)) + 
  geom_errorbar(aes(ymin = conf.low,ymax = conf.high),width = 0,position = position_dodge(width = 0)) + 
  geom_line(position = position_dodge(width = 0)) + 
  theme_ridges() + 
  scale_x_discrete(labels = c('Male Chairs','Yellen'),expand = c(.1,.1,.1,.7)) + 
  geom_hline(yintercept = 0,linetype = 'dashed') + 
  geom_label_repel(data = toplotG %>%
                     filter(yellenTime == 1),direction = 'y',min.segment.length = 100,nudge_x = .1,
                   aes(y = estimate,label = paste0(gender,': ',round(diff,3),'\n     (',round(se,2),')'))) + 
  scale_color_manual(guide = 'none',name = 'Party',values = c('darkgreen','tomato')) + 
  scale_shape_discrete(guide = 'none',name = 'Party') + 
  labs(x = NULL,y = NULL,
       subtitle = 'By Gender')


modG$coeftable['fedResp:yellenTime',1] - modG$coeftable[1,1]
modG$coeftable['fedResp',1]
sum(modG$coeftable[c('fedResp','fedResp:genderM'),1])
sum(modG$coeftable[c('fedResp','fedResp:yellenTime'),1])
sum(modG$coeftable[c('fedResp','fedResp:genderM','fedResp:yellenTime','fedResp:yellenTime:genderM'),1])


sum(modG$coeftable[c(1,2,3),1])
(modG$coeftable[1,1] + modG$coeftable['fedResp:genderM',1] + modG$coeftable['yellenTime:genderM',1] + 
    modG$coeftable['fedResp:yellenTime:genderM',1] + modG$coeftable['fedResp:yellenTime',1]) - 
  (modG$coeftable[1,1] + modG$coeftable['fedResp:genderM',1])



require(patchwork)
pdf('./Figures/MS_figure_6.pdf',width = 7,height = 5)
(pFull & labs(subtitle = 'Overall DiD')) + ((pParty & theme(axis.text.x = element_blank())) / pGender) +
  plot_layout(widths = c(4,2))
dev.off()

etable(mod,modP,modG,
       extralines = list('100 LDA Topic Loadings' = c('Yes','Yes','Yes')),
       drop = 'opensecretsID\\)N|topic',replace = T,headers = c('Overall DiD','By Party','By Gender'),
       file = './output/tables/SI_table_5.tex')

#EOF

################################################################################
##
## Purpose: This script creates Figure 7, along with SI figure 11 and SI tables
##          6 and 7.
##
## Author: James Bisbee (james.h.bisbee@vanderbilt.edu)
##
## Input Files:
##  - ./data/prepped/finalData.RData: Prepped data from 9_DATA_final_build.R
##
## Output Files:
##  - ./output/figures/MS_figure_7.pdf
##  - ./output/tables/SI_table_6.tex
##  - ./output/tables/SI_table_7.tex
##  - ./output/figures/SI_figure_11.pdf
##
##
## See associated log file for compute environment, package versions, 
##  and date of most recent run.
##
################################################################################
rm(list = ls())
gc()
require(tidyverse)
require(ggridges)
require(fixest)

set.seed(123)

# Compute details
print(paste0('Compute environment from ',Sys.Date(),' run by Bisbee'))
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



load('./output_data/finalData.RData')

# Data prep
thresholds <- utterance_level %>%
  filter(grepl('FED',opensecretsID)) %>%
  group_by(docID,chamber) %>%
  summarise(n=n(),
            Greenspan = grepl('GREENSPAN',opensecretsID),
            Yellen = grepl('YELLEN',opensecretsID),
            Bernanke = grepl('BERNANKE',opensecretsID),
            Powell = grepl('POWELL',opensecretsID)) %>%
  distinct() %>%
  ungroup() %>%
  data.frame()


toAnalDyad <- speaker_level %>%
  mutate(respondingTo = relevel(factor(respondingTo),ref = 'FEDBERNANKE'))


BernYellRes <- list()
dates <- list(as.Date(c('2006-01-01','2018-01-01')),
              as.Date(c('2008-01-01','2016-01-01')),
              as.Date(c('2013-01-01','2015-01-01')))

for(ds in dates) {
  for(cham in c('House','Senate')) {
    # stop()
    BernYellRes[[paste(ds,collapse = ' to ')]][[cham]] <- feols(as.formula(paste0('interruptorPct ~ factor(respondingTo)',
                                                                                  '| opensecretsID')),
                                                                toAnalDyad %>%
                                                                  filter(date > as.Date(ds[1]) & date < as.Date(ds[2]),
                                                                         chamber == cham) %>%
                                                                  filter(grepl('FED',respondingTo),
                                                                         !grepl('FED',opensecretsID)) %>%
                                                                  group_by(opensecretsID) %>%
                                                                  mutate(n = n(),
                                                                         keep = any(grepl('YELLEN',respondingTo)) & any(grepl('BERNANKE',respondingTo))) %>%
                                                                  ungroup() %>%
                                                                  filter(n > 1,
                                                                         keep)) 
  }
}

toAnalDyad <- toAnalDyad %>%
  mutate(respondingTo = relevel(factor(respondingTo),ref = 'FEDYELLEN'))
YellPowRes <- list()
dates <- list(as.Date(c('2014-01-01','2021-01-01')),
              as.Date(c('2016-01-01','2020-01-01')),
              as.Date(c('2017-01-01','2019-01-01')))
for(ds in dates) {
  for(cham in c('House','Senate')) {
    YellPowRes[[paste(ds,collapse = ' to ')]][[cham]] <- feols(as.formula(paste0('interruptorPct ~ factor(respondingTo)',
                                                                                 '| opensecretsID')),
                                                               toAnalDyad %>%
                                                                 filter(date > as.Date(ds[1]) & date < as.Date(ds[2]),
                                                                        chamber == cham) %>%
                                                                 filter(grepl('FED',respondingTo),
                                                                        !grepl('FED',opensecretsID)) %>%
                                                                 group_by(opensecretsID) %>%
                                                                 mutate(n = n(),
                                                                        keep = any(grepl('YELLEN',respondingTo)) & any(grepl('POWELL',respondingTo))) %>%
                                                                 ungroup() %>%
                                                                 filter(n > 1,
                                                                        keep)) 
  }
}



toplot <- NULL
for(i in 1:length(YellPowRes)) {
  toplot <- bind_rows(toplot,as_tibble(data.frame(summary(YellPowRes[[i]]$House)$coeftable) %>%
                                         rename_all(.funs = function(x) gsub('Estimate','est',gsub('Std..Error','se',gsub('t.value','tstat',gsub('Pr...t..','pval',x))))) %>%
                                         mutate(ref = rownames(.))) %>%
                        mutate(period = names(YellPowRes)[i],
                               chamber = 'House',
                               chairComp = 'Powell'))
  toplot <- bind_rows(toplot,as_tibble(data.frame(summary(YellPowRes[[i]]$Senate)$coeftable) %>%
                                         rename_all(.funs = function(x) gsub('Estimate','est',gsub('Std..Error','se',gsub('t.value','tstat',gsub('Pr...t..','pval',x))))) %>%
                                         mutate(ref = rownames(.))) %>%
                        mutate(period = names(YellPowRes)[i],
                               chamber = 'Senate',
                               chairComp = 'Powell'))
}
for(i in 1:length(BernYellRes)) {
  toplot <- bind_rows(toplot,as_tibble(data.frame(summary(BernYellRes[[i]]$House)$coeftable) %>%
                                         rename_all(.funs = function(x) gsub('Estimate','est',gsub('Std..Error','se',gsub('t.value','tstat',gsub('Pr...t..','pval',x))))) %>%
                                         mutate(ref = rownames(.))) %>%
                        mutate(period = names(BernYellRes)[i],
                               chamber = 'House',
                               chairComp = 'Bernanke'))
  toplot <- bind_rows(toplot,as_tibble(data.frame(summary(BernYellRes[[i]]$Senate)$coeftable) %>%
                                         rename_all(.funs = function(x) gsub('Estimate','est',gsub('Std..Error','se',gsub('t.value','tstat',gsub('Pr...t..','pval',x))))) %>%
                                         mutate(ref = rownames(.))) %>%
                        mutate(period = names(BernYellRes)[i],
                               chamber = 'Senate',
                               chairComp = 'Bernanke'))
}

pdf('./Figures/MS_figure_7.pdf',width = 8,height = 5)
toplot %>%
  mutate(ref = gsub('factor.*?FED','',ref)) %>%
  filter((chairComp == 'Powell' & ref == 'POWELL') | (chairComp == 'Bernanke' & ref == 'YELLEN')) %>%
  mutate(period = gsub('-01-01','',gsub('to','to\n',period)),
         chairComp = ifelse(chairComp == 'Bernanke','Bernanke -> Yellen','Yellen -> Powell')) %>%
  ggplot(aes(x = period,y = est,color = chamber,shape = chamber)) + 
  geom_hline(yintercept = 0,linetype = 'dashed') + 
  scale_shape_manual(name = 'Chamber',values = c(21,22)) + 
  geom_errorbar(aes(ymin = est - 2*se,ymax = est + 2*se),position = position_dodge(width = .2),width = 0) + 
  geom_point(position = position_dodge(width = .2),fill = 'white',size = 3) + 
  facet_grid(~chairComp,scales = 'free') + 
  theme_ridges() + 
  scale_color_manual(name = 'Chamber',values = c('grey30','grey60')) + 
  theme(strip.background = element_rect(fill = 'white',size = .5,linetype = 'solid')) + 
  xlab('Comparison Window') + ylab('Change in interruptions\nassociated with change in Fed chair')
dev.off()



# Tables
mList <- list()
for(chamb in c('House')) { 
  for(d in names(BernYellRes)) {
    mList[[paste0(chamb,': ',d)]] <- BernYellRes[[d]][[chamb]]
  }
  for(d in names(YellPowRes)) {
    mList[[paste0(chamb,': ',d)]] <- YellPowRes[[d]][[chamb]]
  }
}

dict <- c('factor(respondingTo)FEDYELLEN' = 'Yellen (ref. Bernanke)',
          'factor(respondingTo)FEDPOWELL' = 'Powell (ref. Yellen)',
          'opensecretsID' = 'Speaker')
etable(mList,dict = dict,depvar = F,digits = 3,
       signif.code = c('***' = .001,'**' = .01,'*' = .05,'\\dag' = .1),
       replace = T,
       file = './output/tables/SI_table_6.tex')


mList <- list()
for(chamb in c('Senate')) { 
  for(d in names(BernYellRes)) {
    mList[[paste0(chamb,': ',d)]] <- BernYellRes[[d]][[chamb]]
  }
  for(d in names(YellPowRes)) {
    mList[[paste0(chamb,': ',d)]] <- YellPowRes[[d]][[chamb]]
  }
}

etable(mList,dict = dict,depvar = F,digits = 3,
       signif.code = c('***' = .001,'**' = .01,'*' = .05,'\\dag' = .1),
       replace = T,
       file = './output/tables/SI_table_7.tex')


# RR1: placebo comparing first terms of Bernanke, and first term of powell relative to Bernanke
GreenBernRes <- list()


dates <- list(as.Date(c('2001-01-01','2014-01-01')),
              as.Date(c('2002-01-01','2010-01-01')),
              as.Date(c('2005-01-01','2007-01-01')))

for(ds in dates) {
  for(cham in c('House','Senate')) {
    # stop()
    GreenBernRes[[paste(ds,collapse = ' to ')]][[cham]] <- feols(as.formula(paste0('interruptorPct ~ factor(respondingTo)',
                                                                                   '| opensecretsID')),
                                                                 toAnalDyad %>%
                                                                   filter(date > as.Date(ds[1]) & date < as.Date(ds[2]),
                                                                          chamber == cham) %>%
                                                                   filter(grepl('FED',respondingTo),
                                                                          !grepl('FED',opensecretsID)) %>%
                                                                   group_by(opensecretsID) %>%
                                                                   mutate(n = n(),
                                                                          keep = any(grepl('GREENSPAN',respondingTo)) & any(grepl('BERNANKE',respondingTo))) %>%
                                                                   ungroup() %>%
                                                                   filter(n > 1,
                                                                          keep)) 
  }
}


BernPowRes <- list()
dates <- list(as.Date(c('2010-01-01','2022-01-01')),
              as.Date(c('2012-01-01','2020-01-01')),
              as.Date(c('2013-01-01','2019-01-01')))

for(ds in dates) {
  for(cham in c('House','Senate')) {
    # stop()
    BernPowRes[[paste(ds,collapse = ' to ')]][[cham]] <- feols(as.formula(paste0('interruptorPct ~ factor(respondingTo)',
                                                                                 '| opensecretsID')),
                                                               toAnalDyad %>%
                                                                 filter(date > as.Date(ds[1]) & date < as.Date(ds[2]),
                                                                        chamber == cham) %>%
                                                                 filter(grepl('FED',respondingTo),
                                                                        !grepl('FED',opensecretsID),
                                                                        !yellenTime) %>%
                                                                 group_by(opensecretsID) %>%
                                                                 mutate(n = n(),
                                                                        keep = any(grepl('POWELL',respondingTo)) & any(grepl('BERNANKE',respondingTo))) %>%
                                                                 ungroup() %>%
                                                                 filter(n > 1,
                                                                        keep)) 
  }
}


toplot <- NULL
for(i in 1:length(GreenBernRes)) {
  toplot <- bind_rows(toplot,as_tibble(data.frame(summary(GreenBernRes[[i]]$House)$coeftable) %>%
                                         rename_all(.funs = function(x) gsub('Estimate','est',gsub('Std..Error','se',gsub('t.value','tstat',gsub('Pr...t..','pval',x))))) %>%
                                         mutate(ref = rownames(.))) %>%
                        mutate(period = names(GreenBernRes)[i],
                               chamber = 'House',
                               chairComp = 'Greenspan vs. Bernanke'))
  toplot <- bind_rows(toplot,as_tibble(data.frame(summary(GreenBernRes[[i]]$Senate)$coeftable) %>%
                                         rename_all(.funs = function(x) gsub('Estimate','est',gsub('Std..Error','se',gsub('t.value','tstat',gsub('Pr...t..','pval',x))))) %>%
                                         mutate(ref = rownames(.))) %>%
                        mutate(period = names(GreenBernRes)[i],
                               chamber = 'Senate',
                               chairComp = 'Greenspan vs. Bernanke'))
}
for(i in 1:length(BernPowRes)) {
  toplot <- bind_rows(toplot,as_tibble(data.frame(summary(BernPowRes[[i]]$House)$coeftable) %>%
                                         rename_all(.funs = function(x) gsub('Estimate','est',gsub('Std..Error','se',gsub('t.value','tstat',gsub('Pr...t..','pval',x))))) %>%
                                         mutate(ref = rownames(.))) %>%
                        mutate(period = names(BernPowRes)[i],
                               chamber = 'House',
                               chairComp = 'Bernanke vs. Powell'))
  toplot <- bind_rows(toplot,as_tibble(data.frame(summary(BernPowRes[[i]]$Senate)$coeftable) %>%
                                         rename_all(.funs = function(x) gsub('Estimate','est',gsub('Std..Error','se',gsub('t.value','tstat',gsub('Pr...t..','pval',x))))) %>%
                                         mutate(ref = rownames(.))) %>%
                        mutate(period = names(BernPowRes)[i],
                               chamber = 'Senate',
                               chairComp = 'Bernanke vs. Powell'))
}

pdf('./output/figures/SI_figure_11.pdf',width = 8,height = 5)
toplot %>%
  mutate(ref = gsub('factor.*?FED','',ref)) %>%
  mutate(chairComp = factor(gsub(' vs. ','->',chairComp),
                            levels = c('Greenspan->Bernanke','Bernanke->Powell'))) %>%
  mutate(period = gsub('-01-01','',gsub('to','to\n',period))) %>%
  ggplot(aes(x = period,y = est,color = chamber,shape = chamber)) + 
  geom_hline(yintercept = 0,linetype = 'dashed') + 
  scale_shape_manual(name = 'Chamber',values = c(21,22)) + 
  geom_errorbar(aes(ymin = est - 2*se,ymax = est + 2*se),position = position_dodge(width = .2),width = 0) + 
  geom_point(position = position_dodge(width = .2),fill = 'white',size = 3) + 
  facet_grid(~chairComp,scales = 'free') + 
  theme_ridges() + 
  scale_color_manual(name = 'Chamber',values = c('grey30','grey60')) + 
  theme(strip.background = element_rect(fill = 'white',size = .5,linetype = 'solid')) + 
  xlab('Comparison Window') + ylab('Change in interruptions\nassociated with change in Fed chair')
dev.off()

# EOF

################################################################################
##
## Purpose: This script creates Figure 8
##
## Author: James Bisbee (james.h.bisbee@vanderbilt.edu)
##
## Input Files:
##  - ./data/prepped/finalData.RData: Prepped data from 9_DATA_final_build.R
##
## Output Files:
##  - ./output/figures/MS_figure_8.pdf
##
##
## See associated log file for compute environment, package versions, 
##  and date of most recent run.
##
################################################################################
rm(list = ls())
gc()
require(tidyverse)
require(ggridges)
require(fixest)
require(ggrepel)
require(patchwork)
require(marginaleffects)

set.seed(123)

# Compute details
print(paste0('Compute environment from ',Sys.Date(),' run by Bisbee'))
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


load('./output_data/finalData.RData')


# Rigorous fixed effects
dyadToAnal <- utterance_level %>%
  mutate_at(vars(matches('lag')),function(x) ifelse(is.na(x),0,x)) %>%
  mutate(votepct_rel = ifelse(is.infinite(votepct_rel),1,votepct_rel),
         respondingTo = relevel(factor(respondingTo),ref = 'FEDBERNANKE'))

dims <- colnames(utterance_level %>% select(matches('SENT_'),-matches('_lag|error')) %>% select(-matches('SEVERE|AUTHOR|LIKELY')))

summary(modAny <- feols(as.formula(paste0('interruptor ~ fedResp*yellenTime*anyDaughters',
                                          ' + nSons + interrupted + poly(year,3) + log(tot_utterances) + log(nchars_lag)',
                                          '| opensecretsID + chamber')),
                        utterance_level %>%
                          mutate(yellenTime = ifelse(yellenTime,1,0),
                                 anyDaughters = ifelse(nDaughters > 0,1,0)) %>%
                          filter(ind > mind,all > 30),
                        cluster = c('opensecretsID')))

summary(modAnyM <- feols(as.formula(paste0('interruptor ~ fedResp*yellenTime*anyDaughters',
                                           ' + nSons + interrupted + poly(year,3) + log(tot_utterances) + log(nchars_lag)',
                                           '| opensecretsID + chamber')),
                         utterance_level %>%
                           mutate(yellenTime = ifelse(yellenTime,1,0),
                                  anyDaughters = ifelse(nDaughters > 0,1,0)) %>%
                           filter(ind > mind,
                                  all > 30,
                                  gender == 'M'),
                         cluster = c('opensecretsID')))

summary(modAnyF <- feols(as.formula(paste0('interruptor ~ fedResp*yellenTime*anyDaughters',
                                           ' + nSons + interrupted + poly(year,3) + log(tot_utterances) + log(nchars_lag)',
                                           '| opensecretsID + chamber')),
                         utterance_level %>%
                           mutate(yellenTime = ifelse(yellenTime,1,0),
                                  anyDaughters = ifelse(nDaughters > 0,1,0)) %>%
                           filter(ind > mind,
                                  all > 30,
                                  gender == 'F'),
                         cluster = c('opensecretsID')))



toplotAny <- plot_cme(modAny,variables = 'fedResp',
                      condition = (c('yellenTime','anyDaughters')),draw = F)

toplotAny <- toplotAny %>%
  left_join(toplotAny %>%
              group_by(anyDaughters) %>%
              summarise(diff = diff(estimate),
                        se = sqrt(sum(std.error^2))) %>%
              ungroup()) %>%
  bind_cols(modAny$coeftable %>%
              data.frame() %>%
              mutate(vars = row.names(.)) %>%
              filter(grepl('.*:.*:.*',vars)) %>%
              as_tibble() %>%
              select(est = Estimate,pval = Pr...t..,stdErr = Std..Error))

pAny <- toplotAny %>%
  mutate(anyDaughters = ifelse(anyDaughters == 0,'None','1+')) %>%
  ggplot(aes(x = factor(yellenTime),y = estimate,group = anyDaughters,color = anyDaughters,shape = anyDaughters)) + 
  geom_errorbar(aes(ymin = conf.low,ymax = conf.high),width = 0,position = position_dodge(width = 0)) + 
  geom_line(position = position_dodge(width = 0)) + 
  geom_point(position = position_dodge(width = 0)) + 
  theme_ridges() + 
  scale_x_discrete(labels = c('Male Chairs','Yellen'),expand = c(.1,.1,.1,.7)) + 
  geom_hline(yintercept = 0,linetype = 'dashed') + 
  geom_label(data = toplotAny %>%
               mutate(anyDaughters = ifelse(anyDaughters == 0,'None','1+')) %>%
               filter(yellenTime == 1),#direction = 'y',min.segment.length = 100,
             aes(y = estimate,label = paste0(anyDaughters,': ',round(diff,3),'\n     (',round(se,2),')')),
             hjust = 1.1,
             direction = 'y',min.segment.length = Inf,
             size = 3) + 
  geom_segment(data = toplotAny %>% 
                 filter(yellenTime == 1) %>%
                 select(estimate) %>%
                 distinct() %>%
                 mutate(mn = min(estimate),
                        mx = max(estimate)),
               inherit.aes = F,
               aes(y = mn,yend = mx),x = 2.15,xend = 2.15) + 
  geom_segment(data = toplotAny %>% 
                 filter(yellenTime == 1) %>%
                 select(estimate,est,pval) %>%
                 distinct() %>%
                 summarise(yMid = min(estimate) + abs((diff(estimate)/2))),
               inherit.aes = F,
               x = 2.15,aes(y = yMid,yend = yMid),xend = 2.2) + 
  geom_label(data = toplotAny %>% 
               filter(yellenTime == 1) %>%
               select(estimate,est,pval,stdErr) %>%
               distinct() %>%
               summarise(yMid = min(estimate) + abs((diff(estimate)/2)),
                         est = abs(mean(est)),
                         pval = mean(pval),
                         se = mean(stdErr)),
             inherit.aes = F,
             x = 2.2,aes(y = yMid,label = paste0('Diff: ',round(est,3),'\n       (',round(se,2),')')),
             hjust = 0,size = 3) + 
  scale_color_manual(guide = 'none',name = 'Party',values = c('orange','brown')) + 
  scale_shape_discrete(guide = 'none',name = 'Party') + 
  labs(x = NULL,y = NULL,
       subtitle = 'Any Daughters')

toplotAnyM <- plot_cme(modAnyM,variables = 'fedResp',
                       condition = (c('yellenTime','anyDaughters')),draw = F)

toplotAnyM <- toplotAnyM %>%
  left_join(toplotAnyM %>%
              group_by(anyDaughters) %>%
              summarise(diff = diff(estimate),
                        se = sqrt(sum(std.error^2))) %>%
              ungroup()) %>%
  bind_cols(modAnyM$coeftable %>%
              data.frame() %>%
              mutate(vars = row.names(.)) %>%
              filter(grepl('.*:.*:.*',vars)) %>%
              as_tibble() %>%
              select(est = Estimate,pval = Pr...t..,stdErr = Std..Error))

pAnyM <- toplotAnyM %>%
  mutate(anyDaughters = ifelse(anyDaughters == 0,'None','1+')) %>%
  ggplot(aes(x = factor(yellenTime),y = estimate,group = anyDaughters,color = anyDaughters,shape = anyDaughters)) + 
  geom_errorbar(aes(ymin = conf.low,ymax = conf.high),width = 0,position = position_dodge(width = 0)) + 
  geom_line(position = position_dodge(width = 0)) + 
  geom_point(position = position_dodge(width = 0)) + 
  theme_ridges() + 
  scale_x_discrete(labels = c('Male Chairs','Yellen'),expand = c(.1,.1,.1,.7)) + 
  geom_hline(yintercept = 0,linetype = 'dashed') + 
  geom_label(data = toplotAnyM %>%
               mutate(anyDaughters = ifelse(anyDaughters == 0,'None','1+')) %>%
               filter(yellenTime == 1),#direction = 'y',min.segment.length = 100,
             aes(y = estimate,label = paste0(anyDaughters,': ',round(diff,3),'\n     (',round(se,2),')')),
             hjust = 1.1,size = 3) + 
  geom_segment(data = toplotAnyM %>% 
                 filter(yellenTime == 1) %>%
                 select(estimate) %>%
                 distinct() %>%
                 mutate(mn = min(estimate),
                        mx = max(estimate)),
               inherit.aes = F,
               aes(y = mn,yend = mx),x = 2.15,xend = 2.15) + 
  geom_segment(data = toplotAnyM %>% 
                 filter(yellenTime == 1) %>%
                 select(estimate,est,pval) %>%
                 distinct() %>%
                 summarise(yMid = min(estimate) + abs((diff(estimate)/2))),
               inherit.aes = F,
               x = 2.15,aes(y = yMid,yend = yMid),xend = 2.2) + 
  geom_label(data = toplotAnyM %>% 
               filter(yellenTime == 1) %>%
               select(estimate,est,pval,stdErr) %>%
               distinct() %>%
               # group_by(est,pval) %>%
               summarise(yMid = min(estimate) + abs((diff(estimate)/2)),
                         est = abs(mean(est)),
                         pval = mean(pval),
                         se = mean(stdErr)),
             inherit.aes = F,
             x = 2.2,aes(y = yMid,label = paste0('Diff: ',round(est,3),'\n       (',round(se,2),')')),
             hjust = 0,size = 3) + 
  scale_color_manual(guide = 'none',name = 'Party',values = c('orange','brown')) + 
  scale_shape_discrete(guide = 'none',name = 'Party') + 
  labs(x = NULL,y = NULL,
       subtitle = 'Any Daughters: Men')


toplotAnyF <- plot_cme(modAnyF,variables = 'fedResp',
                       condition = (c('yellenTime','anyDaughters')),draw = F)

toplotAnyF <- toplotAnyF %>%
  left_join(toplotAnyF %>%
              group_by(anyDaughters) %>%
              summarise(diff = diff(estimate),
                        se = sqrt(sum(std.error^2))) %>%
              ungroup()) %>%
  bind_cols(modAnyF$coeftable %>%
              data.frame() %>%
              mutate(vars = row.names(.)) %>%
              filter(grepl('.*:.*:.*',vars)) %>%
              as_tibble() %>%
              select(est = Estimate,pval = Pr...t..,stdErr = Std..Error))

pAnyF <- toplotAnyF %>%
  mutate(anyDaughters = ifelse(anyDaughters == 0,'None','1+')) %>%
  ggplot(aes(x = factor(yellenTime),y = estimate,group = anyDaughters,color = anyDaughters,shape = anyDaughters)) + 
  geom_errorbar(aes(ymin = conf.low,ymax = conf.high),width = 0,position = position_dodge(width = 0)) + 
  geom_line(position = position_dodge(width = 0)) + 
  geom_point(position = position_dodge(width = 0)) + 
  theme_ridges() + 
  scale_x_discrete(labels = c('Male Chairs','Yellen'),expand = c(.1,.1,.1,.7)) + 
  geom_hline(yintercept = 0,linetype = 'dashed') + 
  geom_label_repel(data = toplotAnyF %>%
                     mutate(anyDaughters = ifelse(anyDaughters == 0,'None','1+')) %>%
                     filter(yellenTime == 1),#direction = 'y',min.segment.length = 100,
                   aes(y = estimate,label = paste0(anyDaughters,': ',round(diff,3),'\n     (',round(se,2),')')),
                   nudge_x = -.1,hjust = 1,direction = 'y',size = 3,min.segment.length = Inf) + 
  geom_segment(data = toplotAnyF %>% 
                 filter(yellenTime == 1) %>%
                 select(estimate) %>%
                 distinct() %>%
                 mutate(mn = min(estimate),
                        mx = max(estimate)),
               inherit.aes = F,
               aes(y = mn,yend = mx),x = 2.15,xend = 2.15) + 
  geom_segment(data = toplotAnyF %>% 
                 filter(yellenTime == 1) %>%
                 select(estimate,est,pval) %>%
                 distinct() %>%
                 summarise(yMid = min(estimate) + abs((diff(estimate)/2))),
               inherit.aes = F,
               x = 2.15,aes(y = yMid,yend = yMid),xend = 2.2) + 
  geom_label(data = toplotAnyF %>% 
               filter(yellenTime == 1) %>%
               select(estimate,est,pval,stdErr) %>%
               distinct() %>%
               # group_by(est,pval) %>%
               summarise(yMid = min(estimate) + abs((diff(estimate)/2)),
                         est = abs(mean(est)),
                         pval = mean(pval),
                         se = mean(stdErr)),
             inherit.aes = F,
             x = 2.2,aes(y = yMid,label = paste0('Diff: ',round(est,3),'\n       (',round(se,2),')')),
             hjust = 0,size = 3) + 
  scale_color_manual(guide = 'none',name = 'Party',values = c('orange','brown')) + 
  scale_shape_discrete(guide = 'none',name = 'Party') + 
  labs(x = NULL,y = NULL,
       subtitle = 'Any Daughters: Women')

pdf('./Figures/MS_figure_8.pdf',width = 8,height = 5)
pAny + (pAnyM / pAnyF) + plot_layout(widths = c(4,2))
dev.off()


summary(modAny3 <- feols(as.formula(paste0('interruptor ~ fedResp*yellenTime*anyDaughters*gender',
                                           ' + nSons + interrupted + poly(year,3) + log(tot_utterances) + log(nchars_lag)',
                                           '| opensecretsID + chamber')),
                         utterance_level %>%
                           mutate(yellenTime = ifelse(yellenTime,1,0),
                                  anyDaughters = ifelse(nDaughters > 0,1,0)) %>%
                           filter(ind > mind,
                                  all > 30),
                         cluster = c('opensecretsID')))

etable(modAny,modAnyF,modAnyM,modAny3,depvar = F,digits = 3,
       signif.code = c('***' = .001,'**' = .01,'*' = .05,'\\dag' = .1),
       replace = T,headers = c('Combined','Female Subset','Male Subset','4-way'),
       file = './output/tables/SI_table_8.tex')

# EOF
################################################################################
##
## Purpose: This script creates Figure 9
##
## Author: James Bisbee (james.h.bisbee@vanderbilt.edu)
##
## Input Files:
##  - ./data/prepped/finalData.RData: Prepped data from 9_DATA_final_build.R
##  - ./data/prepped/hearings/topic_models_100.RData: Prepped data from 7_DATA_topic_model_prep.R
##
## Output Files:
##  - ./output/figures/MS_figure_9.pdf
##
##
## See associated log file for compute environment, package versions, 
##  and date of most recent run.
##
################################################################################
rm(list = ls())
gc()
require(tidyverse)
require(ggridges)
require(fixest)
require(ggrepel)

set.seed(123)

# Compute details
print(paste0('Compute environment from ',Sys.Date(),' run by Bisbee'))
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



# Loading Data
load('./output_data/finalData.RData')
load('./output_data/topic_models_100.RData')
topWord <- lda_model$get_top_words(n = 50) %>%
  data.frame() %>%
  rename_all(function(x) gsub('X','topic_',x)) %>%
  mutate(top_word = row_number()) %>%
  as_tibble()


# Looking at which topics are interrupted the most
toplot <- utterance_level %>%
  filter(nchars > 0,
         ind > mind,
         !grepl("Yellen",speaker)) %>%
  group_by(interrupted) %>%
  summarise_at(vars(matches('topic_')),mean,na.rm=T) %>%
  gather(key,value,-interrupted) %>%
  filter(!grepl('lag',key)) %>%
  left_join(topWord %>%
              gather(key,term,-top_word) %>%
              group_by(key) %>%
              arrange(top_word) %>%
              slice(1:3) %>%
              summarise(terms = paste(term,collapse = ', ')) %>%
              ungroup() %>%
              mutate(terms = paste0(gsub('topic_','',key),': ',terms)))

# Who uses these topics most?
toplot2 <- utterance_level %>%
  mutate(tmpSpk = ifelse(grepl('Yellen',speaker),'Yellen',
                         ifelse(grepl('Bernanke|Greenspan|Powell',speaker),'Other Fed Chairs','All Others'))) %>%
  filter(nchars > 0,
         ind > mind) %>%
  select(tmpSpk,fullInd,matches('topic_\\d+$')) %>%
  gather(topic,theta,-tmpSpk,-fullInd) %>%
  group_by(tmpSpk,topic) %>%
  summarise(m = mean(theta),
            sd = sd(theta),
            n=n()) %>%
  left_join(topWord %>%
              gather(topic,term,-top_word) %>%
              group_by(topic) %>%
              arrange(top_word) %>%
              slice(1:3) %>%
              summarise(terms = paste(term,collapse = ', ')) %>%
              ungroup() %>%
              mutate(terms = paste0(gsub('topic_','',topic),': ',terms))) 

toplot2 <- toplot %>%
  spread(interrupted,value) %>%
  mutate(diffInt = `1` - `0`) %>%
  left_join(toplot2) 

toplot3 <- toplot2 %>%
  left_join(toplot2 %>%
              filter(tmpSpk != 'All Others') %>%
              select(tmpSpk,terms,m) %>%
              spread(tmpSpk,m))

toplot4 <- toplot3 %>%
  left_join(doc_topic_distr %>%
              mutate(topic = paste0('topic_',topic)) %>%
              group_by(topic) %>%
              summarise(theta = mean(theta))) %>%
  mutate(diffTop = Yellen - `Other Fed Chairs`) %>%
  select(terms,diffInt,diffTop,theta,topic) %>% distinct()

lda_model$get_top_words(n = 10) %>%
  data.frame() %>%
  rename_all(function(x) gsub('X','topic_',x)) %>%
  mutate(top_word = row_number()) %>%
  as_tibble() %>%
  t()

insubs <- c(1, 
            3,
            5, 
            9,
            10,
            11, 
            12,
            19,
            20,
            22,
            24,
            27,
            29,
            31,
            37,
            39,
            41,
            42,
            46,
            47,
            49,
            51,
            54,
            55,
            57,
            69,
            71,
            76,
            83,
            85,
            88,
            90,
            91,
            92,
            93,
            96,
            97)

substantive <- paste0('topic_',setdiff(1:100,insubs))

pdf('./Figures/MS_figure_9.pdf',width = 7,height = 7)
toplot4 %>%
  filter(terms != 'issue, get, type') %>%
  ggplot(aes(x = diffTop,y = diffInt,label = terms,size = theta,weight= theta)) + 
  geom_point(shape = 21) + 
  geom_smooth(show.legend = F) + 
  geom_vline(xintercept = 0,linetype = 'dashed') + 
  geom_hline(yintercept = 0,linetype = 'dashed') +  
  ylab(bquote('' %<-% ' Topic Interrupted Less ... Topic Interrupted More ' %->% '')) + 
  xlab(bquote('' %<-% ' Topic Used More by Fed Chairs ... Topic Used More by Yellen ' %->% '')) + 
  theme_ridges() + 
  scale_size_continuous(name = 'Prevalence') + 
  theme(axis.title.x = element_text(hjust = .5,vjust = 0),
        axis.title.y = element_text(hjust = .5,vjust = 0),
        legend.position = 'bottom') + 
  geom_text_repel(data = toplot4 %>%
                    filter(topic %in% substantive | abs(diffInt) > .01),
                  size = 3)
dev.off()

# EOF

################################################################################
##
## Purpose: This script creates Figure 10, along with SI figure 19.
##
## Author: James Bisbee (james.h.bisbee@vanderbilt.edu)
##
## Input Files:
##  - ./data/prepped/finalData.RData: Prepped data from 9_DATA_final_build.R
##  - ./data/prepped/hearings/topic_models_100.RData: Prepped data from 7_DATA_topic_model_prep.R
##
## Output Files:
##  - ./output/figures/MS_figure_10.pdf
##  - ./output/figures/SI_figure_19.pdf
##
##
## See associated log file for compute environment, package versions, 
##  and date of most recent run.
##
################################################################################
rm(list = ls())
gc()
require(tidyverse)
require(ggridges)
require(fixest)

set.seed(123)

# Compute details
print(paste0('Compute environment from ',Sys.Date(),' run by Bisbee'))
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



# Loading Data
load('./output_data/finalData.RData')
load('./output_data/topic_models_100.RData')
topWord <- lda_model$get_top_words(n = 50) %>%
  data.frame() %>%
  rename_all(function(x) gsub('X','topic_',x)) %>%
  mutate(top_word = row_number()) %>%
  as_tibble()

lda_model$get_top_words(n = 10) %>%
  data.frame() %>%
  rename_all(function(x) gsub('X','topic_',x)) %>%
  mutate(top_word = row_number()) %>%
  as_tibble() %>%
  t()

insubs <- c(1, 
            3,
            5, 
            9,
            10,
            11, 
            12,
            19,
            20,
            22,
            24,
            27,
            29,
            31,
            37,
            39,
            41,
            42,
            46,
            47,
            49,
            51,
            54,
            55,
            57,
            69,
            71,
            76,
            83,
            85,
            88,
            90,
            91,
            92,
            93,
            96,
            97)

substantive <- paste0('topic_',setdiff(1:100,insubs))


toplot <- utterance_level %>% 
  filter(ind > mind) %>%
  mutate(tmpSpk = ifelse(grepl('FED',opensecretsID),as.character(opensecretsID),'Others')) %>%
  mutate(tmpSpk = ifelse(grepl('YELLEN',tmpSpk),'Yellen',
                         ifelse(grepl('FED',tmpSpk),'Male Fed Chairs','Others'))) %>%
  arrange(fullInd) %>%
  select(fullInd,interrupted,nchars,tmpSpk,matches('topic_\\d+$')) %>%
  gather(topic,theta,-fullInd,-interrupted,-nchars,-tmpSpk) %>%
  group_by(fullInd,interrupted) %>%
  filter(theta == max(theta)) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(fullInd) %>% distinct() %>%
  group_by(topic,tmpSpk) %>%
  summarise(int = mean(interrupted),
            meanTheta = mean(theta,na.rm=T),
            meanChars = mean(nchars,na.rm=T), 
            n=n()) %>%
  group_by(tmpSpk) %>%
  mutate(tot = sum(n),
         avgInt = mean(int)) %>%
  ungroup() %>%
  mutate(share = n/tot) %>%
  left_join(topWord %>%
              gather(topic,term) %>%
              group_by(topic) %>%
              slice(1:5) %>%
              group_by(topic) %>%
              summarise(terms = paste(term,collapse = ', '))) 

toplot <- toplot %>%
  mutate(terms = factor(terms,levels = toplot %>% filter(tmpSpk == 'Yellen') %>% arrange(int) %>% .$terms),
         topic = factor(topic,levels = toplot %>% filter(tmpSpk == 'Yellen') %>% arrange(int) %>% .$topic)) %>%
  filter(terms != 'NA',
         tmpSpk != 'Others') 

pdf('./Figures/MS_figure_10.pdf',width = 7,height = 8)
toplot %>%
  ggplot(aes(x = int,y = topic,size = share,fill = tmpSpk,shape = tmpSpk)) + 
  geom_point(alpha = 1,size = 1) + 
  geom_vline(data = toplot %>% select(tmpSpk,avgInt) %>% distinct(),aes(xintercept = avgInt,color = tmpSpk)) +
  scale_size_continuous(name = '% of Utterances',range = c(1,10),labels = scales::percent) +
  theme_ridges() + 
  scale_shape_manual(guide = 'none',name = 'Speaker',values = 21:25) + 
  geom_segment(data = toplot %>%
                 select(topic,tmpSpk,int) %>%
                 spread(tmpSpk,int) %>%
                 filter(Yellen > `Male Fed Chairs`),
               aes(x = Yellen,y = topic,xend = `Male Fed Chairs`,yend = topic),
               size = .5,color = 'darkorange',inherit.aes = F) + 
  geom_segment(data = toplot %>%
                 select(topic,tmpSpk,int) %>%
                 spread(tmpSpk,int) %>%
                 filter(Yellen <= `Male Fed Chairs`),
               aes(x = Yellen,y = topic,xend = `Male Fed Chairs`,yend = topic),
               size = .5,color = 'black',inherit.aes = F) + 
  geom_point(alpha = .35) + 
  geom_text(data = toplot %>%
              select(topic,terms,tmpSpk,int) %>%
              spread(tmpSpk,int) %>%
              filter(topic %in% substantive) %>%
              filter(Yellen > `Male Fed Chairs`),
            aes(x = Yellen,y = topic,label = terms),inherit.aes = F,
            size = 2,hjust = -.1) + 
  scale_fill_manual(guide = 'none',name = 'Speaker',values = c('black','darkorange')) + 
  scale_x_continuous(breaks = c(0,.25,.5,.75,1),labels = scales::percent) + 
  theme(legend.position = c(.7,.15),
        axis.title.x = element_text(hjust = .6),
        panel.grid.major.x = element_line(linewidth = .1),
        panel.grid.major.y = element_blank(),
        legend.title = element_text(size = 10),legend.text = element_text(size = 9),
        axis.text.y = element_blank()) + 
  xlab('Proportion Interrupted') + ylab('Topic') + 
  scale_color_manual(guide = 'none',name = 'Speaker',values = c('black','darkorange')) + 
  scale_linetype_discrete(guide = 'none',name = 'Speaker') + 
  geom_text(data = toplot %>% select(tmpSpk,avgInt) %>% distinct(),
            aes(x = avgInt,y = c(100,102),label = tmpSpk,color = tmpSpk),
            inherit.aes = F,hjust = 0,size = 3.5,show.legend = FALSE) +
  coord_cartesian(ylim = c(0,103),xlim = c(0,1.5),clip = 'off')
dev.off()

# EOF

################################################################################
##
## Purpose: This script creates Figure 11, along with SI 9 and 10.
##
## Author: James Bisbee (james.h.bisbee@vanderbilt.edu)
##
## Input Files:
##  - ./data/prepped/finalData.RData: Prepped data from 9_DATA_final_build.R
##
## Output Files:
##  - ./output/figures/MS_figure_11.pdf
##  - ./output/figures/SI_table_9.tex
##  - ./output/figures/SI_table_10.tex
##
##
## See associated log file for compute environment, package versions, 
##  and date of most recent run.
##
################################################################################
rm(list = ls())
gc()
require(tidyverse)
require(ggridges)
require(fixest)

set.seed(123)

# Compute details
print(paste0('Compute environment from ',Sys.Date(),' run by Bisbee'))
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


load('./output_data/finalData.RData')


# Is Yellen more toxic?
dyadToAnalYellen <- utterance_level %>%
  arrange(fullInd) %>%
  mutate_at(vars(matches('lag')),function(x) ifelse(is.na(x),0,x)) %>%
  mutate(votepct_rel = ifelse(is.infinite(votepct_rel),1,votepct_rel),
         yellen = ifelse(grepl('YELLEN',opensecretsID),1,0),
         respondingTo = relevel(factor(respondingTo),ref = 'FEDBERNANKE'),
         opensecretsID = relevel(factor(opensecretsID),ref = 'FEDYELLEN'),
         respGroup = ifelse(grepl('YELLEN',respondingTo),'Yellen',
                            ifelse(grepl('BERNANKE',respondingTo),'Bernanke',
                                   ifelse(grepl('GREENSPAN',respondingTo),'Greenspan',
                                          ifelse(grepl('POWELL',respondingTo),'Powell',lag(party)))))) %>% 
  filter(ind > mind,all > 30)



# Are legislators more toxic toward Yellen?
dyadToAnalLegs <- utterance_level %>%
  arrange(fullInd) %>%
  mutate_at(vars(matches('lag')),function(x) ifelse(is.na(x),0,x)) %>%
  mutate(votepct_rel = ifelse(is.infinite(votepct_rel),1,votepct_rel),
         yellen = ifelse(grepl('YELLEN',respondingTo),1,0),
         respondingTo = relevel(factor(respondingTo),ref = 'FEDYELLEN'),
         opensecretsID = relevel(factor(opensecretsID),ref = 'FEDYELLEN'),
         respGroup = relevel(factor(ifelse(grepl('YELLEN',respondingTo),'Yellen',
                                           ifelse(grepl('BERNANKE',respondingTo),'Bernanke',
                                                  ifelse(grepl('GREENSPAN',respondingTo),'Greenspan',
                                                         ifelse(grepl('POWELL',respondingTo),'Powell',lag(party)))))),ref = 'Yellen')) %>% 
  filter(ind > mind,all > 30)

dims <- colnames(utterance_level %>% select(matches('SENT_'),-matches('_lag|error')) %>% select(-matches('SEVERE|AUTHOR|LIKELY')))

# Is yellen more toxic, attacking, or incoherent compared to other fed chairs?
toplot <- NULL
mList <- mListRev <- list()
for(y in dims) {
  (mList[[y]] <- feols(as.formula(paste0('scale(',y,') ~ yellen + ',
                                         paste(paste0('topic_',1:100,'_lag'),collapse = ' + '),
                                         ' + ',
                                         paste(paste0('scale(',dims[-which(grepl('comb',dims))],'_lag)'),collapse = ' + '),
                                         ' + poly(log(nchars_lag+1),3) + scale(log(tot_utterances)) + interrupted',
                                         '| respondingTo + chamber')),
                       dyadToAnalYellen %>%
                         filter(grepl('FED',opensecretsID),
                                !grepl('FED',respondingTo)),cluster = c('respondingTo','docID')))
  
  toplot <- bind_rows(toplot,mList[[y]]$coeftable %>%
                        data.frame() %>%
                        rename(est = Estimate,se = Std..Error,tstat = t.value,pval = Pr...t..) %>%
                        slice(1) %>%
                        mutate(out = y,
                               model = 'fedTone') %>% as_tibble())
  
  (mListRev[[y]] <- feols(as.formula(paste0('scale(',y,') ~ yellen + ',
                                            paste(paste0('topic_',1:100,'_lag'),collapse = ' + '),
                                            ' + ',
                                            paste(paste0('scale(',dims[-which(grepl('comb',dims))],'_lag)'),collapse = ' + '),
                                            ' + poly(log(nchars_lag+1),3) + scale(log(tot_utterances)) + interrupted',
                                            '| opensecretsID + chamber')),
                          dyadToAnalLegs %>%
                            filter(grepl('FED',respondingTo),
                                   !grepl('FED',opensecretsID)),cluster = c('respondingTo','docID')))
  
  toplot <- bind_rows(toplot,mListRev[[y]]$coeftable %>%
                        data.frame() %>%
                        rename(est = Estimate,se = Std..Error,tstat = t.value,pval = Pr...t..) %>%
                        slice(1) %>%
                        mutate(out = y,
                               model = 'legTone') %>% as_tibble())
}

toplot2 <- toplot %>%
  mutate(sentType = ifelse(out %in% paste0('SENT_',c('ATTACK_ON_AUTHOR','ATTACK_ON_COMMENTER','IDENTITY_ATTACK','INSULT','THREAT','combAttack')),'Aggression',
                           ifelse(out %in% paste0('SENT_',c('TOXICITY','SEVERE_TOXICITY','PROFANITY','SEXUALLY_EXPLICIT','FLIRTATION','OBSCENE','INFLAMMATORY','combToxic')),'Toxicity',
                                  ifelse(out %in% paste0('SENT_',c('INCOHERENT','UNSUBSTANTIAL','combIncoh')),'Incoherence',NA))),
         out = gsub('Attack On Commenter','Attack',
                    gsub('Combattack','INDEX: Aggression',
                         gsub('Combincoh','INDEX: Incoherence',
                              gsub('Combtoxic','INDEX: Toxicity',str_to_title(gsub('_',' ',gsub('SENT_','',out)))))))) %>%
  mutate(index = ifelse(grepl('INDEX',out),'index','dimension')) %>%
  mutate(model = ifelse(model == 'fedTone',"Tone of Fed Chair","Tone of Responses to Fed Chair")) %>%
  filter(!is.na(sentType),
         !out %in% c('Attack On Author','Severe Toxicity'),
         !grepl('INDEX:',out))

pdf('./Figures/MS_figure_11.pdf',width = 8,height = 7)
toplot2 %>%
  mutate(out = factor(out,
                      levels = toplot2 %>%
                        filter(model == 'Tone of Responses to Fed Chair') %>%
                        arrange(est) %>% pull(out))) %>%
  ggplot(aes(x = est,y = out,shape = index,fill = index,size = index)) + 
  geom_errorbarh(aes(xmin = est - 2*se,xmax = est + 2*se),height = .1,size = .5) + 
  geom_point() + 
  geom_vline(xintercept = 0,linetype = 'dashed') + 
  theme_ridges() + 
  scale_shape_manual(values = c(19,21)) + 
  scale_size_manual(values = c(2,4)) + 
  scale_fill_manual(values = c('white','white')) + 
  facet_grid(sentType~model,scales = 'free_y',shrink = TRUE,space = 'free_y') + 
  theme(legend.position = 'none') + 
  xlab("Yellen relative to male Fed chairs") + 
  ylab('Tone Dimension')
dev.off()

# Tables
dict <- mList$SENT_TOXICITY$coeftable %>%
  data.frame() %>%
  mutate(covs = row.names(.)) %>%
  filter(grepl('yellen|year|interrupted|SENT|nchars',covs)) %>%
  pull(covs)

dict <- gsub('_lag\\)','_lag',gsub('scale\\(','',dict))

names(dict) <- mList$SENT_TOXICITY$coeftable %>%
  data.frame() %>%
  mutate(covs = row.names(.)) %>%
  filter(grepl('yellen|year|interrupted|SENT|nchars',covs)) %>%
  pull(covs)

dict <- trimws(gsub('  \\+ 1|, 3','',gsub('ITY|ENTITY|UALLY|ICIT|ATION|ENTER|MATORY|STANTIAL|TAC| ON|ERENT','',gsub('_lag',' ',gsub('SENT_','',dict)))))

etable(mList[-which(grepl('comb',names(mList)))],
       headers = gsub('ITY|ENTITY|UALLY|ICIT|ATION|ENTER|MATORY|STANTIAL|TAC| ON|ERENT','',gsub('_',' ',gsub('SENT_','',names(mList[-which(grepl('comb',names(mList)))])))),
       keep = 'yellen|interrupted|TOXY|ID ATK|INSULT|PROFAN|SEX EXPL|THREAT|FLIRT|ATK|INCOH|INFLAM|OBSCENE|UNSUB|nchars',
       depvar = F,
       dict = dict,drop.section = 'fixef',
       extralines = list('100 Topics'=rep('Y',12),
                         'Chamber FE' = rep('Y',12),
                         'Speaker FE' = rep('Y',12)),
       signif.code = c('***' = .001,'**' = .01,'*' = .05,'$\\dag$' = .1),
       replace = T,
       file = './output/tables/SI_table_9.tex')



etable(mListRev[-which(grepl('comb',names(mListRev)))],
       headers = gsub('ITY|ENTITY|UALLY|ICIT|ATION|ENTER|MATORY|STANTIAL|TAC| ON|ERENT','',gsub('_',' ',gsub('SENT_','',names(mListRev[-which(grepl('comb',names(mListRev)))])))),
       keep = 'yellen|interrupted|TOXY|ID ATK|INSULT|PROFAN|SEX EXPL|THREAT|FLIRT|ATK|INCOH|INFLAM|OBSCENE|UNSUB|nchars',
       depvar = F,
       dict = dict,drop.section = 'fixef',
       extralines = list('100 Topics'=rep('Y',12),
                         'Chamber FE' = rep('Y',12),
                         'Speaker FE' = rep('Y',12)),
       signif.code = c('***' = .001,'**' = .01,'*' = .05,'$\\dag$' = .1),
       replace = T,
       file = './output/tables/SI_table_10.tex')

# EOF
