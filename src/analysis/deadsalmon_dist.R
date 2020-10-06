#!/usr/bin/env Rscript
#DEPENDS: report/deadsalmon_pvalues.RDS
#CREATES: report/deadsalmon.png
#TITLE: "dead salmon" test - plotting
#PIPE: FALSE  # if process can be run as a pipeline, e.g. program < infile > outfile

require(tidyverse)

deadsalmon <- readRDS("report/deadsalmon_pvalues.RDS")

data.frame(deadsalmon) %>% ggplot(aes(x = deadsalmon)) + geom_histogram(binwidth = 0.01) + xlab("p-value") + geom_vline(xintercept = 0.05, col = 'red') -> ds

ggsave("report/deadsalmon.png", ds)

