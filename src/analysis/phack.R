#!/usr/bin/env Rscript
#DEPENDS: data/intermediate/president_approval.RDS, data/intermediate/nyt.RDS
#CREATES: report/phack37.csv
#TITLE: p-hacking using all articles
#PIPE: FALSE  # if process can be run as a pipeline, e.g. program < infile > outfile

require(rvest)
require(tidyverse)
require(lubridate)
require(lmtest)
require(zoo)
require(corrplot)
require(rio)

pres <- readRDS(here::here("data/intermediate/president_approval.RDS"))
nyt <- readRDS(here::here("data/intermediate/nyt.RDS"))

nyt_date_range <- range(nyt$publication_date)

tibble(date = seq(as.Date('1980-01-01'), as.Date('2016-12-31'), by = 'day')) %>% left_join(pres, by = c('date' = 'mid_date')) %>% mutate(approval = na.spline(approval, na.rm = FALSE), president = na.spline(president, na.rm = FALSE)) %>% select(date, president, approval) %>% filter(date >= nyt_date_range[1] & date <= nyt_date_range[2]) %>% filter(date >= nyt_date_range[1] & date <= nyt_date_range[2]) %>% mutate(approval_z = abs((approval - mean(approval)) / sd(approval))) -> ts_date


grang <- function(var, data, ts_date, adj = FALSE) {
    ##var_s <- sym(var)
    var_x <- quo(!!sym(var))
    if (!adj) {
        data %>% mutate(bing_liu = bing_liu_pos - bing_liu_neg) %>% group_by(publication_date) %>% summarise(m_sent = mean(!!var_x)) %>% ungroup %>% mutate(volatile = abs((m_sent - mean(m_sent)) / sd(m_sent))) %>% rename(date = publication_date) %>% select(date, volatile) %>% left_join((ts_date %>% select(date, approval_z)), by = 'date') -> res_pres
    } else {
        data %>% mutate(bing_liu = bing_liu_pos - bing_liu_neg) %>% mutate(target = !!var_x / wordcount) %>% group_by(publication_date) %>% summarise(m_sent = mean(target)) %>% ungroup %>% mutate(volatile = abs((m_sent - mean(m_sent)) / sd(m_sent))) %>% rename(date = publication_date) %>% select(date, volatile) %>% left_join((ts_date %>% select(date, approval_z)), by = 'date') -> res_pres
    }
    grangertest(zoo(res_pres$volatile), zoo(res_pres$approval_z), order = 30)$`Pr(>F)`[2]
}

nyt %>% mutate(bing_liu = bing_liu_pos - bing_liu_neg) %>% select(-aid, -content_length, -bing_liu_neg, -bing_liu_pos, -pronouns, -title, -source_name, -publication_date, -mf_moralitygeneral) %>% colnames %>% map_dbl(., grang, data = nyt, ts_date) -> pres_phack

nyt %>% mutate(bing_liu = bing_liu_pos - bing_liu_neg) %>% select(-aid, -content_length, -bing_liu_neg, -bing_liu_pos, -pronouns, -title, -source_name, -publication_date, -mf_moralitygeneral) %>% colnames -> score_names

nyt %>% mutate(bing_liu = bing_liu_pos - bing_liu_neg) %>% select(-aid, -content_length, -bing_liu_neg, -bing_liu_pos, -pronouns, -title, -source_name, -publication_date, -mf_moralitygeneral) %>% cor -> cor_matrix

tibble(score_names, cor_wc = cor_matrix[, "wordcount"], pres_phack) %>% arrange(cor_wc, pres_phack) -> res

### Remove all length-adjusted (e.g. liwc; wordcount)

res %>% filter(pres_phack < 0.05) %>% pull(score_names) %>% discard(~str_detect(., "^liwc") | . == "wordcount") -> sig_scores

sig_scores %>% map_dbl(., grang, data = nyt, ts_date, adj = TRUE) -> pres_phack_adj

res %>% left_join(tibble(score_names = sig_scores, pres_phack_adj)) %>% rio::export(here::here("report/phack37.csv"))

