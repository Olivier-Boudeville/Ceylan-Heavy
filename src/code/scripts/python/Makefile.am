
SUBDIRS = tests

# These are the subdirectories which will be distributed with 'make dist' :
DIST_SUBDIRS = $(SUBDIRS)

PYTHON_SOURCES_SET = \
	dataUtils.py     \
	fileUtils.py     \
	generalUtils.py  \
	introspection.py \
	startup.py       \
	toolbox.py

EXTRA_DIST = $(PYTHON_SOURCES_SET)

install-data-local:
	@echo; echo "@CEYLAN_INSTALL_STYLE@    Installing python scripts in\
	$(srcdir) to [@CEYLAN_INSTALL_PYTHON_DIR@]@CEYLAN_DEFAULT_STYLE@"
	@mkdir -p @CEYLAN_INSTALL_PYTHON_DIR@
	@for f in $(PYTHON_SOURCES_SET); do \
	cp -f $(srcdir)/$$f @CEYLAN_INSTALL_PYTHON_DIR@/$$f; done
