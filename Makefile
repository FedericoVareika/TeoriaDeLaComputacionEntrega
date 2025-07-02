TEXDIR := Documentacion
TEXFILES := $(TEXDIR)/*.tex $(TEXDIR)/Reducciones/*.tex
BIBFILES := $(TEXDIR)/*.bib
TEXSRC := $(TEXDIR)/Solucion.tex
PDFOUT := $(TEXDIR)/Solucion.pdf

.PHONY: all clean

all: $(PDFOUT)

$(PDFOUT): $(TEXFILES) $(BIBFILES)
	latexmk -pdf -shell-escape -output-directory=$(TEXDIR) $(TEXSRC)

clean:
	latexmk -C -output-directory=$(TEXDIR) $(TEXSRC)
