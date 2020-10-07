#!/usr/bin/env Rscript
#DEPENDS: data/raw/tbmics.csv
#CREATES: data/intermediate/tbmics.RDS
#TITLE: Preprocessing tbmics data (Michigan Consumer Sentiment Index)
#PIPE: FALSE  # if process can be run as a pipeline, e.g. program < infile > outfile

require(rvest)
require(tidyverse)
require(lubridate)
require(lmtest)
require(zoo)
require(corrplot)
require(rio)

tbmics <- rio::import(here::here('data/raw/tbmics.csv'))

tbmics %>% mutate(date = dmy(paste0("20-", Month, "-",YYYY))) -> tbmics

tibble(date = seq(as.Date('1979-12-01'), as.Date('2017-01-31'), by = 'day')) %>% left_join(tbmics, by = c('date')) %>% mutate(ICS_ALL  = na.spline(ICS_ALL, na.rm = FALSE)) %>% filter(date >= as.Date('1980-01-01') & date <= as.Date('2016-12-31')) %>% select(date, ICS_ALL) %>% saveRDS(here::here("data/intermediate/tbmics.RDS"))
