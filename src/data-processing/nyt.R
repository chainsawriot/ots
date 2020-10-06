#!/usr/bin/env Rscript
#DEPENDS: data/raw/master.RDS, data/raw/liwc_res.RDS, data/raw/quanteda_lexicoder.csv, data/raw/nrc_res.RDS
#CREATES: data/intermediate/nyt.RDS
#TITLE: Munge all sentiment scores together
#PIPE: FALSE  # if process can be run as a pipeline, e.g. program < infile > outfile

require(rvest)
require(tidyverse)
require(lubridate)
require(lmtest)
require(zoo)
require(corrplot)
require(rio)
require(bit64)

###pres <- readRDS("president_approval.RDS")

readRDS(here::here('data/raw/master.RDS')) %>% select(-title) %>% saveRDS(here::here("data/raw/master_no_title.RDS"))

readRDS(here::here('data/raw/master.RDS')) %>% as_tibble() %>% mutate(publication_date = ymd(substr(publication_date, 1, 10))) -> nyt

liwc <- readRDS(here::here("data/raw/liwc_res.RDS")) %>% select(-document) %>% select(aid, affect, posemo, negemo, anx, anger, sad) %>% as_tibble

colnames(liwc) <- c("aid", "liwc_affect", "liwc_posemo", "liwc_negemo", "liwc_anx", "liwc_anger", "liwc_sad")

import(here::here('data/raw/quanteda_lexicoder.csv')) %>% mutate(aid = as.character(aid)) %>% as_tibble() %>% mutate(lsd_nettone= ((positive + neg_negative) / wordcount) - ((negative + neg_positive) / wordcount), lsd_diff = (positive + neg_negative) - (negative + neg_positive)) %>% select(aid, wordcount, lsd_nettone, lsd_diff) -> lexicoder

nyt %>% left_join(lexicoder, by = 'aid') %>% left_join(liwc, by = 'aid') %>% mutate_at(vars(liwc_affect:liwc_sad), ~(./wordcount)*100) -> nyt

nrc <- readRDS(here::here("data/raw/nrc_res.RDS")) %>% distinct %>% as_tibble
colnames(nrc)[2:11] <- paste0("nrc_", colnames(nrc)[2:11], seq = "")

nyt %>% left_join(nrc, by = 'aid') %>%  mutate_at(vars(nrc_anger:nrc_positive), ~(./wordcount)) -> nyt
saveRDS(nyt, here::here("data/intermediate/nyt.RDS"))
