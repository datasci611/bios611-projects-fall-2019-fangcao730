my_images:Dockerfile.python
        docker build -t my_images -f Dockerfile.python . && touch $@
UDES_clean.csv:my_images
        docker run -it --rm -v $(pwd):$(pwd) -w $(pwd) my_images python3 clean.py
r_images:Dockerfile.Rscript
        docker build -t r_images -f Dockerfile.Rscript . && touch $@
myplot.png:UDES_clean.csv r_images
        docker run -it --rm -v $(pwd):$(pwd) -w $(pwd) r_images Rscript R_plot.R
                                                                                         
