all : fig3 fig5 main.pdf

main.pdf : main.tex
	latexmk -use-make -pdf $<

fig3:
	cd ../code/fig3/ && Rscript make_plots.R

fig5:
	cd ../code/fig5/ && Rscript make_plots.R

clean:
	rm main.pdf
