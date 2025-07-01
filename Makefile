TEXDIR := Documentacion
TEXFILES := $(TEXDIR)/*.tex
BIBFILES := $(TEXDIR)/*.bib
TEXSRC := $(TEXDIR)/Solucion.tex
PDFOUT := $(TEXDIR)/Solucion.pdf

.PHONY: all clean

all: $(PDFOUT)

$(PDFOUT): $(TEXFILES) $(BIBFILES)
	latexmk -pdf -output-directory=$(TEXDIR) $(TEXSRC)

clean:
	latexmk -C -output-directory=$(TEXDIR)
