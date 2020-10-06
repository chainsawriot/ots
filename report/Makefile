# arxiv: with Appendix
# ccr: without Appendix; seperated appendix

# because papaja can mess up the original .rmd files, it is better to make papaja works on a copy.

.FORCE:

arxiv: .FORCE
	cp report.rmd arxiv.rmd
	Rscript -e "rmarkdown::render('arxiv.rmd')"
	rm arxiv.rmd
	rm *.Rmd
	rm appendices.tex
	rm arxiv.tex

ccr: .FORCE
	cp report.rmd ccr.rmd
	Rscript -e "rmarkdown::render('ccr.rmd', output_format = 'word_document')"
	rm *.Rmd

clean:
	rm *.Rmd
	rm arxiv.rmd
	rm appendices.tex
	rm arxiv.tex
