#!/usr/bin/env Rscript
#DEPENDS: data/raw/approval/32.html, data/raw/approval/33.html, data/raw/approval/34.html, data/raw/approval/35.html, data/raw/approval/36.html, data/raw/approval/37.html, data/raw/approval/38.html, data/raw/approval/39.html, data/raw/approval/40.html, data/raw/approval/41.html, data/raw/approval/42.html, data/raw/approval/43.html, data/raw/approval/44.html, data/raw/approval/45.html
#CREATES: data/intermediate/president_approval.RDS
#TITLE: Extract the presidential approve time series data from web scraped htmls
#PIPE: FALSE  # if process can be run as a pipeline, e.g. program < infile > outfile

require(rvest)
require(tidyverse)
require(lubridate)
##require(lmtest)
require(zoo)
##require(corrplot)

### Generation of presidential approval time series data from web scraped htmls
tr2df <- function(tr, pres = 33) {
    tr %>% html_nodes('td') %>% html_text -> res
    data_frame(president = pres, start_date = res[2], end_date = res[3], approval = res[5], disapproval = res[6], other = res[7])
}


extractapproval <- function(pres = 33) {
    read_html(paste0("data/raw/approval/", pres, ".html")) %>% html_node(css = "table[width='600']") %>% html_nodes('tr[bgcolor]') %>% map_dfr(tr2df, pres)
}


president_approval <- map_dfr(32:45, extractapproval)

president_approval %>% mutate(start_date = mdy(start_date), end_date = mdy(end_date), approval = as.numeric(approval), disapproval = as.numeric(disapproval), other = as.numeric(other)) %>% rowwise %>% mutate(mid_date= mean.Date(c(start_date, end_date))) %>% ungroup %>% saveRDS("data/intermediate/president_approval.RDS")
