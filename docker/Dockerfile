FROM rocker/verse:3.5.1

MAINTAINER Joachim Gassen "gassen@wiwi.hu-berlin.de" 

RUN  R -e "install.packages(c('Quandl', 'gtools', 'ggpubr', 'lfe', 'tweenr', \
                            'moments', 'Hmisc', 'ggridges', 'latex2exp', 'RPostgres', \
                            'kableExtra', 'shinycssloaders'), \
                            repos=c(CRAN = 'https://mran.microsoft.com/snapshot/2018-09-19'))" \
  && R -e "devtools::install_github('nteetor/gganimate@71da746')" \
  && R -e "devtools::install_github('joachim-gassen/ExPanDaR@f7bb730')"

# Install some additional texlife packages

RUN tlmgr install multirow xcolor colortbl wrapfig float tabu varwidth threeparttable \
                  threeparttablex environ trimspaces ulem makecell endfloat setspace \
                  footmisc \
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

 