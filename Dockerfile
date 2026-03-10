FROM rocker/r-ver:4.5.1
LABEL maintainer="Kjorte Harra <kjorteh@gmail.com>"

RUN apt-get update && apt-get install -y \
    build-essential \
    g++ \
    make \
    git \
    curl \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libomp-dev \
    libglpk-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev

RUN install2.r --error \
    TAM \
    brms \
    BMS \
    loo \
    dplyr \
    tidyr \
    ggplot2 \
    haven \
    mice \
    caret \
    posterior \
    patchwork \
    tidyverse \
    BART \
    LaplacesDemon

RUN R -e "install.packages('cmdstanr', repos = c('https://stan-dev.r-universe.dev', getOption('repos')))"

ENV CMDSTAN_PATH="/opt/cmdstan"

RUN mkdir -p ${CMDSTAN_PATH} && chmod -R 777 ${CMDSTAN_PATH}

RUN R -e "cmdstanr::install_cmdstan(cores=2, dir='/opt/cmdstan')"

RUN R -e "library(cmdstanr); bin_path <- list.dirs('/opt/cmdstan', recursive = FALSE)[1]; set_cmdstan_path(bin_path)"
