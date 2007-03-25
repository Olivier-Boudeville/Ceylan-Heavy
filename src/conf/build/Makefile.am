
SUBDIRS=m4

.PHONY: all check-configure download-config-scripts \
	blank-MD5 install-data-local dist-hook clean clean-download real-clean


BUILD_SCRIPTS_SET = \
	autogen.sh                     \
	checkConfigure.sh              \
	cleanGeneratedConfigFiles.sh   \
	build-and-test-package.sh


EXTRA_DIST = \
	$(BUILD_SCRIPTS_SET)           \
	Ceylan.cbp                     \
	MakeConfigure                  \
	CeylanToolsSettings.inc        \
	configure-template.ac
	
	
noinst_SCRIPTS = $(BUILD_SCRIPTS_SET)

pkgconfigdir   = $(libdir)/pkgconfig
pkgconfig_DATA = @CEYLAN_PKGCONFIG_FILE@

DIFF_TOOL      = `which tkdiff 2>/dev/null`

CONFIGURE_FILE = configure.ac

# Was/will be : ftp://ftp.gnu.org/gnu/config
CONFIGURE_SCRIPTS_BASE_URL = \
	http://savannah.gnu.org/cgi-bin/viewcvs/*checkout*/config/config

DOWNLOADED_HELPERS = config.guess config.sub

CONFIG_FILES       = $(DOWNLOADED_HELPERS)


all:
	

		
check-configure: 
	@echo "@CEYLAN_MAKE_STYLE@    Comparing configure \
	files@CEYLAN_DEFAULT_STYLE@"
	@$(srcdir)/checkConfigure.sh $(DIFF_TOOL)
	@echo "@CEYLAN_COMMAND_STYLE@    Comparison finished, \
	$(CONFIGURE_FILE) may be updated now."


download-config-scripts: clean-download
	@echo "@CEYLAN_MAKE_STYLE@    Downloading latest \
	configure helper scripts@CEYLAN_DEFAULT_STYLE@"
	@for f in $(DOWNLOADED_HELPERS); do \
	wget --quiet $(CONFIGURE_SCRIPTS_BASE_URL)/$$f ; done
	@chmod +x $(DOWNLOADED_HELPERS)


blank-MD5:
	@echo "@CEYLAN_INSTALL_STYLE@    Blanking MD5 sum of Ceylan release \
	to avoid MD5 recursive dependency of archive@CEYLAN_DEFAULT_STYLE@"
	@$(srcdir)/../../code/scripts/shell/replaceLineStartingBy.sh Ceylan_MD5 \
	'Ceylan_MD5="(blanked)"' $(srcdir)/CeylanToolsSettings.inc


install-data-local:
	@cp $(srcdir)/CeylanToolsSettings.inc $(DESTDIR)@CEYLAN_INSTALL_SHARED_DIR@
	
	
dist-hook:
	@cp $(srcdir)/build-and-test-package.sh $(top_distdir)
	
	
clean-local:
	@echo "@CEYLAN_CLEAN_STYLE@    Cleaning generated config \
	files@CEYLAN_DEFAULT_STYLE@"
	-@rm -f autoscan*.log autoscan.err configure.scan
	
	
clean-download:
	@echo "@CEYLAN_CLEAN_STYLE@    Cleaning downloaded config \
	files@CEYLAN_DEFAULT_STYLE@"
	-@rm -f $(DOWNLOADED_HELPERS)
	
	
real-clean: clean clean-download
