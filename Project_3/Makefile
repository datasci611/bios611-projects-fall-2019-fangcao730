my_images:Dockerfile
        docker build -t my_images -f Dockerfile . && touch $@

pull-pandas:
        docker pull amancevice/pandas:0.25.2-slim && touch $@

UDES.csv:pull-pandas
        docker run -it \
                --rm -v $(shell pwd):/workdir \
                -w /workdir \
                amancevice/pandas:0.25.2-slim \
                        python3 Cleaning.py UDES.csv
insurance.csv:pull-pandas
                docker run -it \
                --rm -v $(shell pwd):/workdir \
                -w /workdir \
                amancevice/pandas:0.25.2-slim \
                        python3 Cleaning.py insurance.csv
wellness.csv:pull-pandas
                docker run -it \
                --rm -v $(shell pwd):/workdir \
                -w /workdir \
                amancevice/pandas:0.25.2-slim \
                        python3 Cleaning.py wellness.csv
Report.html: Report.Rmd my_images \
                        insurance.csv \
                        wellness.csv \
                        UDES.csv
        docker run --rm -ti -v${PWD}:/workdir -w/workdir my_images Rscript -e "library(rmarkdown); rmarkdown::render('Report.Rmd', 'html_document')"    

