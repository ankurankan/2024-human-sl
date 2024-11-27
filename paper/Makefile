all : fig3 fig4 main.pdf

main.pdf : main.tex
	latexmk -use-make -pdf $<

fig3:
	cd ../code/fig3/ && Rscript make_plots.R

fig4:
	cd ../code/fig4/ && Rscript make_plots.R

clean:
	rm main.pdf
