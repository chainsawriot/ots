# ots

This repo contains the scripts for reproducing the analyses in the [paper](https://osf.io/preprints/socarxiv/np5wa/).

This repo is organized as a [Research Compendium Templete CCS Amsterdam](https://github.com/ccs-amsterdam/compendium).

## Instructions

### Prerequisites

1. Make sure you have installed `doit`.

```sh
sudo apt-get install python3-pip python3-venv
pip3 install doit
```

2. This project requires the following R packages.

```r
install.packages(c("rvest", "tidyverse", "lubridate", "lmtest", "zoo", "corrplot", "rio", "here", "rmarkdown", "stringr"))
```

3. Compiling the articles needs to have a LaTeX distribution installed (e.g. TexLive), as well as `pandoc`, `pandoc-citeproc`.

The R package `papaja` is also needed.

```r
install.packages("devtools")
devtools::install_github("crsh/papaja")
```

4. Due to large file size, a few data files are not available in this repo. However, they are available from [osf](https://osf.io/utxs5/).

You can download them manually and then put them into the `data/raw` directoty. This process can also be automated: You can obtain these files using the R package `osfr` and put them into the `data/raw` directory.

```r
install.packages("osfr")
require(osfr)
require(tidyverse)
require(stringr)
osf_retrieve_node("https://osf.io/utxs5/") %>% osf_ls_files  -> utxs5_files

utxs5_files[str_detect(utxs5_files$name, "RDS$|csv$"), ] %>% osf_download(path = here::here("data/raw"), conflicts = "overwrite")
```

### Reproduce

1. Reproduce the analysis

```sh
doit passphraise="geheim"
```

2. Render the article

pdf - everything
ccr - html version (without online appendix)
appendix - appendix only

```sh
cd report
make pdf
make ccr
make appendix
```

## Environment

```
R version 4.0.2 (2020-06-22)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Ubuntu 20.04.1 LTS
```
