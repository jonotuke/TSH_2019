RMDS:=$(wildcard *.Rmd)
HTMLS:=$(RMDS:.Rmd=.html)

all: $(HTMLS)

%.html: %.Rmd
	Rscript -e "rmarkdown::render('$<', output_format = 'html_document')"
