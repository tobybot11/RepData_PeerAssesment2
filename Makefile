all: PA2_template.html

PA2_template.html: PA2_template.Rmd
	R -e "rmarkdown::render('PA2_template.Rmd', 'all')"
	open PA2_template.html 

.PHONY: clean
clean: 
	rm -rf PA2_template.html PA2_template.md PA2_template_files PA2_template_cache repdata-data-StormData.csv
