FROM rocker/tidyverse:3.5.1

# Adapted from rocker-org/rocker-versioned/verse

MAINTAINER Joachim Gassen "gassen@wiwi.hu-berlin.de" 

ENV PATH=$PATH:/opt/TinyTeX/bin/x86_64-linux/

## Add LaTeX, rticles and bookdown support
RUN wget "https://travis-bin.yihui.name/texlive-local.deb" \
  && dpkg -i texlive-local.deb \
  && rm texlive-local.deb \
  && apt-get update \
  && apt-get install -y --no-install-recommends \
    ## for rJava
    default-jdk \
    ## Nice Google fonts
    fonts-roboto \    
    ## used by some base R plots
    ghostscript \
    ## used to build rJava and other packages
    libbz2-dev \
    libicu-dev \
    liblzma-dev \
    ## system dependency of hunspell (devtools)
    libhunspell-dev \
    ## system dependency of hadley/pkgdown
    libmagick++-dev \
    ## rdf, for redland / linked data
    librdf0-dev \
    ## for V8-based javascript wrappers
    libv8-dev \
    ## R CMD Check wants qpdf to check pdf sizes, or throws a Warning 
    qpdf \
    ## For building PDF manuals
    texinfo \
    ## for git via ssh key 
    ssh \
 ## just because
    joe \
	less \
    vim \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/ \
  ## Use tinytex for LaTeX installation
  && install2.r --error tinytex \
  && R -e "tinytex::install_tinytex(dir = '/opt/TinyTeX')" \ 
  && /opt/TinyTeX/bin/*/tlmgr path add \
  && tlmgr install metafont mfware inconsolata tex ae parskip listings \
  && tlmgr path add \
  && Rscript -e "tinytex::r_texmf()" \
  && chown -R root:staff /opt/TinyTeX \
  && chmod -R g+w /opt/TinyTeX \
  && chmod -R g+wx /opt/TinyTeX/bin \
  ## Currently (2017-06-06) need devel PKI for ssl issue: https://github.com/s-u/PKI/issues/19
  && install2.r --error --repo http://rforge.net PKI \
  ## And some nice R packages for publishing-related stuff
  && install2.r --error --deps TRUE bookdown rticles rmdshower 

RUN  R -e "install.packages(c('devtools', 'Quandl', 'gtools', 'ggpubr', 'lfe', \
          'dplyr', 'tidyr', 'lubridate', 'broom', 'tweenr', 'moments', \
          'Hmisc', 'RCurl', 'scales', 'ggridges', 'latex2exp', 'RPostgres', 'DBI', 'rstudioapi'), repos=c(CRAN = 'https://mran.microsoft.com/snapshot/2018-09-19'))" \
  && R -e "devtools::install_github('nteetor/gganimate@71da746')" \
  && R -e "devtools::install_github('joachim-gassen/ExPanDaR@11b4f4b')"

# The below invalidates the cache whenever the Head of the Github repo changes
ADD https://api.github.com/repos/joachim-gassen/ExpAcc/git/refs/heads/master version.json  

RUN cd /home/rstudio \
  && sudo -H -u rstudio git clone https://github.com/joachim-gassen/ExpAcc.git

COPY ExpAcc.Rproj /home/rstudio/ExpAcc

COPY *.tar.gz /home/rstudio/

RUN cd /home/rstudio \
  && sudo -H -u rstudio tar xfvz rstudio_conf.tar.gz \
  && sudo -H -u rstudio tar xfvz rproj_conf.tar.gz \
  && rm *.tar.gz \
  && chown rstudio.rstudio ExpAcc/ExpAcc.Rproj \
  && chown 644 ExpAcc/ExpAcc.Rproj

 