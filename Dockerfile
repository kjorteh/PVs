FROM rocker/r-ver:4.5.1
LABEL maintainer="Kjorte Harra <kjorteh@gmail.com>"

# System dependencies needed for brms / cmdstan / tidyverse / mice
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

# Install R packages (excluding cmdstanr) using littler
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


# 1. Install the cmdstanr R PACKAGE
RUN R -e "install.packages('cmdstanr', repos = c('https://stan-dev.r-universe.dev', getOption('repos')))"

# 2. Set the global environment variable
ENV CMDSTAN_PATH="/opt/cmdstan"

# 3. Create directory with open permissions for HTC users
RUN mkdir -p ${CMDSTAN_PATH} && chmod -R 777 ${CMDSTAN_PATH}

# 4. Install CmdStan binaries into that specific directory
RUN R -e "cmdstanr::install_cmdstan(cores=2, dir='/opt/cmdstan')"

# 5. Find the versioned subfolder and set it as the default path
# This ensures that any R session starting in this container knows exactly where the binaries live
RUN R -e "library(cmdstanr); bin_path <- list.dirs('/opt/cmdstan', recursive = FALSE)[1]; set_cmdstan_path(bin_path)"