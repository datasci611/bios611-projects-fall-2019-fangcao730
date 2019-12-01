FROM python

RUN mkdir /Scripts

RUN pip install pandas numpy 

WORKDIR /Results

FROM rocker/tidyverse

RUN Rscript -e "install.packages('data.table')"

RUN Rscript -e "install.packages('dplyr')"

RUN Rscript -e "install.packages('grid')"

RUN Rscript -e "install.packages('gridExtra')"




