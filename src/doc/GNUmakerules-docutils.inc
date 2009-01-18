

.PHONY: full-doc create-doc regenerate-doc make-doc-repository prepare-hook \
	view-doc to-print \
	clean clean-doc-repository info-doc-local

	
# OVERALL_DOCUMENT_SOURCE and DOCUTILS_TOP should have been defined beforehand.


GENERATOR_LOG_FILE = "rst.log"
	
TMP_RST_REPOSITORY = "tmp-rst"


# OVERALL_DOCUMENT_SOURCE defined in per-project GNUmakefile:
OVERALL_DOCUMENT_TARGET = $(patsubst %.rst,%.pdf,$(OVERALL_DOCUMENT_SOURCE))

RST_FILES = $(wildcard *.rst)

AUX_GENERATED_FILES  = $(patsubst %.rst,%.aux,$(RST_FILES))
TEX_GENERATED_FILES  = $(patsubst %.rst,%.tex,$(RST_FILES))
OUT_GENERATED_FILES  = $(patsubst %.rst,%.out,$(RST_FILES))
LOG_GENERATED_FILES  = $(patsubst %.rst,%.log,$(RST_FILES))
HTML_GENERATED_FILES = $(patsubst %.rst,%.html,$(RST_FILES))
PDF_GENERATED_FILES  = $(patsubst %.rst,%.pdf,$(RST_FILES))


RST_GENERATED_FILES =  $(AUX_GENERATED_FILES) $(TEX_GENERATED_FILES)      \
	$(OUT_GENERATED_FILES) $(LOG_GENERATED_FILES) $(HTML_GENERATED_FILES) \
	$(PDF_GENERATED_FILES) rst.log   


#RST_GENERATOR = update-docutils.sh
RST_GENERATOR = generate-docutils.sh

PDF_VIEWER = /usr/bin/evince


# To trigger the automatic rendering of a created PDF, add 'VIEW_PDF=' 
# at the end of the command line (ex : 'make MyFSM.png VIEW_PDF=')
VIEW_PDF = yes
#VIEW_PDF = no


%.pdf: %.rst
	@echo "     Generating PDF documentation $@"
	@if $(RST_GENERATOR) $< --pdf ; then if [ "$(VIEW_PDF)" != "no" ] ; then $(PDF_VIEWER) $@ & fi; fi
	
%.html: %.rst
	@echo "     Generating HTML documentation $@"
	$(RST_GENERATOR) $< $(CEYLAN_CSS)



# Centralization of documentation sources is necessary, as the LateX generator
# from docutils will not find images in directories otherwise.
full-doc: create-doc


create-doc: regenerate-doc clean-doc-repository make-doc-repository prepare-hook
	@echo "     Generating full (PDF) documentation from $(OVERALL_DOCUMENT_SOURCE) in $(TMP_RST_REPOSITORY) (log in $(GENERATOR_LOG_FILE))"
	@$(MAKE) $(TMP_RST_REPOSITORY)/$(OVERALL_DOCUMENT_TARGET) 2>&1 | tee $(GENERATOR_LOG_FILE)
	

regenerate-doc:
	@echo "     Regenerating all documentation elements"
	@cd $(DOCUTILS_TOP) && $(MAKE) -s doc
	

make-doc-repository:
	@echo "     Copying documentation sources to $(TMP_RST_REPOSITORY)"
	@mkdir -p $(TMP_RST_REPOSITORY)
	@find $(DOCUTILS_TOP) -name $(TMP_RST_REPOSITORY) -prune -o \( -name '*.rst' -o -name '*.png' \) -exec cp -f '{}' $(TMP_RST_REPOSITORY) ';'

        
view-doc: 
	@[ ! -e "$(TMP_RST_REPOSITORY)/$(OVERALL_DOCUMENT_TARGET)" ] || $(PDF_VIEWER) $(TMP_RST_REPOSITORY)/$(OVERALL_DOCUMENT_TARGET) 1>/dev/null 2>&1
	

to-print: create-doc
	@echo "    Transferring document $(OVERALL_DOCUMENT_TARGET) to $(PRINT_SERVER)"
	@scp $(TMP_RST_REPOSITORY)/$(OVERALL_DOCUMENT_TARGET) $(PRINT_LOCATION) 
		
		
clean: clean-doc-repository clean-generated


clean-doc-repository:
	@echo "     Cleaning documentation repository in $(TMP_RST_REPOSITORY)"
	-@rm -rf $(TMP_RST_REPOSITORY)
			
			
clean-generated:
	@echo "     Cleaning generated files"
	-@rm -f $(RST_GENERATED_FILES)
	
	
info: info-doc-local


info-doc-local:
	@echo "OVERALL_DOCUMENT_SOURCE = $(OVERALL_DOCUMENT_SOURCE)"
	@echo "OVERALL_DOCUMENT_TARGET = $(OVERALL_DOCUMENT_TARGET)"
	@echo "PRINT_SERVER = $(PRINT_SERVER)"

	
