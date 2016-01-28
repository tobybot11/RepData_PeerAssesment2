all: PA2_template.html

PA2_template.html: PA2_template.Rmd
	R -e "rmarkdown::render('PA2_template.Rmd')"
	open PA2_template.html 

.PHONY: clean
clean: 
	rm -rf PA2_template.html PA2_template.md PA2_template_files 
