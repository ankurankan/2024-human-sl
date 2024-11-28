all : fig3 fig4 fig5 main.pdf

main.pdf : main.tex
	latexmk -use-make -pdf $<

fig3:
	cd ../code/fig3/ && Rscript make_plots.R

fig4:
	cd ../code/fig4/ && Rscript make_plots.R

fig5:
	cd ../code/fig5/ && python make_plots.py

clean:
	rm main.pdf
