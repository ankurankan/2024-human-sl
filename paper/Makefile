all : plots main.pdf

main.pdf : main.tex
	latexmk -use-make -pdf $<

plots:
	cd ../code/fig3/ && Rscript make_plots.R

clean:
	rm main.pdf
