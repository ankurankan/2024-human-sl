all : fig3 fig4 figures.pdf example.pdf main.pdf

figures.pdf : figures.tex
	latexmk -use-make -pdf $<

example.pdf : example.tex
	latexmk -use-make -pdf $<

main.pdf : main.tex
	latexmk -use-make -pdf $<

fig3:
	cd ../code/fig3/ && Rscript make_plots.R

fig4:
	cd ../code/fig4/ && Rscript make_plots.R

fig5:
	cd ../code/fig5/ && python make_plots.py

clean:
	rm main.pdf main.aux main.bbl main.blg main.fdb_latexmk main.fls figures.pdf example.pdf
