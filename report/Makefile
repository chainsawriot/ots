# because papaja can mess up the original .rmd files, it is better to make papaja works on a copy.

.FORCE:

pdf: .FORCE
	cp report.rmd socarxiv.rmd
	Rscript -e "rmarkdown::render('socarxiv.rmd')"
	rm socarxiv.rmd
	rm *.Rmd
	rm appendices.tex
	rm socarxiv.tex

ccr: .FORCE
	cp report.rmd ccr.rmd
	Rscript -e "rmarkdown::render('ccr.rmd', output_format = 'html_document')"
	rm *.Rmd

appendix: .FORCE
	cat appendix_header.rmd appendices.rmd > online_appendix.rmd
	Rscript -e "rmarkdown::render('online_appendix.rmd')"
	rm online_appendix.tex
	rm online_appendix.rmd

clean:
	rm *.Rmd
	rm arxiv.rmd
	rm appendices.tex
	rm arxiv.tex

