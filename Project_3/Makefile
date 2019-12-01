my_images:Dockerfile
        docker build -t my_images -f Dockerfile . && touch $@

UDES.csv:my_images
        docker run -it --rm -v $(pwd):$(pwd) -w $(pwd) my_images python3 clean.py

insurance.csv:my_images
        docker run -it --rm -v $(pwd):$(pwd) -w $(pwd) my_images python3 clean.py

wellness.csv:my_images
        docker run -it --rm -v $(pwd):$(pwd) -w $(pwd) my_images python3 clean.py


myplot.png:UDES_clean.csv r_images
        docker run -it --rm -v $(pwd):$(pwd) -w $(pwd) r_images Rscript R_plot.R

results/Report.html: results/report.Rmd \
			insurance.csv \
			wellness.csv \
			UDES.csv
	docker run -it --rm -v $(shell pwd):/workdir \
	-w $(pwd) my_images \
	Rscript -e "library(rmarkdown); rmarkdown::render('results/Report.Rmd', 'html_document')"

