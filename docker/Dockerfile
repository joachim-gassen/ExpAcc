FROM rocker/verse:3.5.1

MAINTAINER Joachim Gassen "gassen@wiwi.hu-berlin.de" 

RUN apt-get update \
  && apt-get install -y --no-install-recommends ffmpeg curl

# 2018-10-09:  
# The package gifski (needed by gganimate) 
# relies on rust being installed but debian does 
# not seem to hold the cargo package
# installing using the default script for now...

RUN curl -sSf https://static.rust-lang.org/rustup.sh | sh
  
RUN  R -e "install.packages(c('Quandl', 'gtools', 'ggpubr', 'lfe',  \
                            'moments', 'Hmisc', 'ggridges', 'latex2exp', \
                            'RPostgres', 'kableExtra', 'shinycssloaders', \
                            'ExPanDaR', 'gganimate', 'transformr'), \
                            repos=c(CRAN = 'https://mran.microsoft.com/snapshot/2019-01-04'))" 

# Install some additional texlife packages
# First make sure that tlmgr is current
# Then install the packages needed to build the paper

RUN tlmgr update --self

RUN tlmgr install multirow xcolor colortbl wrapfig float tabu varwidth threeparttable \
                  threeparttablex environ trimspaces ulem makecell endfloat setspace \
                  footmisc csquotes\
  && tlmgr path add 

# The below invalidates the cache whenever the Head of the Github repo changes

ADD https://api.github.com/repos/joachim-gassen/ExpAcc/git/refs/heads/master version.json  

RUN cd /home/rstudio \
  && sudo -H -u rstudio git clone https://github.com/joachim-gassen/ExpAcc.git

COPY ExpAcc.Rproj /home/rstudio/ExpAcc

COPY *.tar.gz /home/rstudio/

RUN sed -i '/^PATH=/ s/$/:\/opt\/TinyTeX\/bin\/x86_64-linux/' /usr/local/lib/R/etc/Renviron \
  && cd /home/rstudio \
  && sudo -H -u rstudio tar xfvz rstudio_conf.tar.gz \
  && sudo -H -u rstudio tar xfvz rproj_conf.tar.gz \
  && rm *.tar.gz \
  && chown rstudio:rstudio ExpAcc/ExpAcc.Rproj \
  && chown 644 ExpAcc/ExpAcc.Rproj

 