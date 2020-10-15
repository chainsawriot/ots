#!/usr/bin/env Rscript
#DEPENDS: data/intermediate/president_approval.RDS, data/intermediate/nyt.RDS
#CREATES: report/corrplot_37.png
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
score_names <- rio::import(here::here("data/raw/score_names.csv"))
nyt_date_range <- range(nyt$publication_date)

tibble(date = seq(as.Date('1980-01-01'), as.Date('2016-12-31'), by = 'day')) %>% left_join(pres, by = c('date' = 'mid_date')) %>% mutate(approval = na.spline(approval, na.rm = FALSE), president = na.spline(president, na.rm = FALSE)) %>% select(date, president, approval) %>% filter(date >= nyt_date_range[1] & date <= nyt_date_range[2]) %>% filter(date >= nyt_date_range[1] & date <= nyt_date_range[2]) %>% mutate(approval_z = abs((approval - mean(approval)) / sd(approval))) -> ts_date

nyt %>% mutate(bing_liu = bing_liu_pos - bing_liu_neg) %>% select(-aid, -title, -source_name, -publication_date, -content_length, -pronouns, -mf_moralitygeneral, -bing_liu_pos, -bing_liu_neg, -wordcount) %>% rename(lsd_absolute = "lsd_diff") -> nyt_scores

colnames(nyt_scores) <- score_names$dictionary_name[match(colnames(nyt_scores), score_names$var_name)]

nyt_scores %>% cor -> M
saveRDS(M, here::here("report/M.RDS"))


png(here::here('report/corrplot_37.png'))
corrplot(M, order="hclust", method = 'shade', type = 'upper')
dev.off()

png(here::here('report/corrplot_37_alpha.png'), width = 900, height = 900, res = 100)
corrplot(M, order="alphabet", method = 'shade', type = 'upper', tl.cex = 0.9)
dev.off()



nyt %>% mutate(bing_liu = bing_liu_pos - bing_liu_neg) %>% select(-aid, -title, -source_name, -publication_date, -content_length, -pronouns, -mf_moralitygeneral, -bing_liu_pos, -bing_liu_neg, -wordcount) %>% svd -> pca37



png(here::here('report/pca_37.png'), width = 900, height = 900, res = 100)
plot(y = pca37$u[,1] , x = nyt$wordcount, ylab = "Component 1", xlab = "Word Count", , log = 'x', pch = 16, col = rgb(red=0, green=0, blue=0, alpha=0.01))
lines(lowess(y = pca37$u[,1], x = nyt$wordcount), col = 'red', lwd = 2)
dev.off()

### binnin

pos_sent <- c("mf_harmvirtue", "mf_authorityvirtue", "mf_purityvirtue", "inquirer_pos", "mf_fairnessvirtue", "mf_ingroupvirtue", "anew_valence", "dal_pleasantness", "lsd_nettone", "lsd_absolute", "liwc_posemo", "nrc_joy", "nrc_positive", "bing_liu")

nyt %>% mutate(bing_liu = bing_liu_pos - bing_liu_neg) %>% select(-aid, -title, -source_name, -publication_date, -content_length, -pronouns, -mf_moralitygeneral, -bing_liu_pos, -bing_liu_neg, -wordcount) %>% rename(lsd_absolute = "lsd_diff") %>% select(!!pos_sent) %>% cor -> M_pos

colnames(M_pos) <- score_names$dictionary_name[match(colnames(M_pos), score_names$var_name)]
rownames(M_pos) <- score_names$dictionary_name[match(rownames(M_pos), score_names$var_name)]

saveRDS(M_pos, here::here("report/M_pos.RDS"))


png(here::here('report/corrplot_37_positive.png'), width = 900, height = 900, res = 100)
corrplot(M_pos,method = 'shade', type = 'upper')
dev.off()

nyt %>% mutate(bing_liu = bing_liu_pos - bing_liu_neg) %>% select(-aid, -title, -source_name, -publication_date, -content_length, -pronouns, -mf_moralitygeneral, -bing_liu_pos, -bing_liu_neg, -wordcount) %>% rename(lsd_absolute = "lsd_diff") %>% colnames()

neg_sent <- c("mf_fairnessvice", "mf_harmvice", "inquirer_neg", "mf_authorityvice", "mf_ingroupvice", "mf_purityvice", "liwc_negemo", "liwc_anx", "liwc_anger", "liwc_sad", "nrc_anger", "nrc_disgust", "nrc_fear", "nrc_sadness", "nrc_negative")

nyt %>% mutate(bing_liu = bing_liu_pos - bing_liu_neg) %>% select(-aid, -title, -source_name, -publication_date, -content_length, -pronouns, -mf_moralitygeneral, -bing_liu_pos, -bing_liu_neg, -wordcount) %>% rename(lsd_absolute = "lsd_diff") %>% select(!!neg_sent) %>% cor -> M_neg

colnames(M_neg) <- score_names$dictionary_name[match(colnames(M_neg), score_names$var_name)]
rownames(M_neg) <- score_names$dictionary_name[match(rownames(M_neg), score_names$var_name)]

saveRDS(M_neg, here::here("report/M_neg.RDS"))

png(here::here('report/corrplot_37_negative.png'), width = 900, height = 900, res = 100)
corrplot(M_neg, method = 'shade', type = 'upper')
dev.off()

### Fig 1
library(magick)
all <- image_scale(image_read(here::here("report/corrplot_37_alpha.png")), "1000")
pos <- image_read(here::here("report/corrplot_37_positive.png"))
neg <- image_read(here::here("report/corrplot_37_negative.png"))
img <- c(pos, neg)
image_append(c(all, image_append(image_scale(img, "900"))), stack = TRUE) %>% image_write(here::here("report/fig1.png"), format = "png")

## Fig2
library(tidyverse)
M <- readRDS(here::here("report/M.RDS"))
diag(M) <- NA
all <- tibble(pairs = "All pairs", r = as.vector(M)[!is.na(as.vector(M))])
M_pos <- readRDS(here::here("report/M_pos.RDS"))
diag(M_pos) <- NA
pos <- tibble(pairs = "Positive pairs", r = as.vector(M_pos)[!is.na(as.vector(M_pos))])
M_neg <- readRDS(here::here("report/M_neg.RDS"))
diag(M_neg) <- NA
neg <- tibble(pairs = "Negative pairs", r = as.vector(M_neg)[!is.na(as.vector(M_neg))])
require(ggridges)
bind_rows(all, pos, neg) %>% ggplot(aes(x = r, y = pairs, fill = pairs)) +   geom_density_ridges(stat = "binline", bins = 35, alpha = 0.3, draw_baseline = FALSE) + scale_y_discrete(expand = c(0, 0)) + ylab("Pairs") + theme(legend.position = "none") + scale_fill_brewer(palette="Dark2") -> fig2

ggsave(here::here("report/fig2.png"), fig2, width = 5, height = 5)
