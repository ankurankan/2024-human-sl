all : fig3 fig4 fig5.pdf figures.pdf figures_v2.pdf example.pdf main.pdf

ms : main.pdf

figures : fig3 fig4 fig5.pdf figures.pdf example.pdf

fig5.pdf : fig5.tex
	latexmk -use-make -pdf $<

figures.pdf : figures.tex
	latexmk -use-make -pdf $<

figures_v2.pdf : figures_v2.tex
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
	rm main.pdf main.aux main.bbl main.blg main.fdb_latexmk main.fls figures.pdf example.pdf appendix.aux example.aux example.fdb_latexmk example.fls fig5.aux fig5.fdb_latexmk fig5.fls fig5.pdf figures.aux figures.fdb_latexmk figures.fls main.out main.log figures.log
