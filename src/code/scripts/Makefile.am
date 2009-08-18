
# erlang not listed here, as relies on classical makefiles, not using autotools:
SUBDIRS = python shell

# These are the subdirectories which will be distributed with 'make dist':
DIST_SUBDIRS = $(SUBDIRS)


all: all-local


all-local:
	@cd erlang && $(MAKE) -f GNUmakefile all


install-data-local:
	@echo; echo "@CEYLAN_INSTALL_STYLE@    Installing Erlang BEAMS in \
	$(srcdir) to $(DESTDIR)@CEYLAN_INSTALL_ERLANG_DIR@ @CEYLAN_DEFAULT_STYLE@"
	@mkdir -p $(DESTDIR)@CEYLAN_INSTALL_ERLANG_DIR@
	@cd erlang && $(MAKE) install \
	INSTALLATION_PREFIX=$(DESTDIR)@CEYLAN_INSTALL_ERLANG_DIR@
