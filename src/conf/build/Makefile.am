
SUBDIRS=m4

.PHONY: all check-configure download-config-scripts links \
	clean clean-download real-clean

BUILD_SCRIPTS_SET = \
	autogen.sh                     \
	checkConfigure.sh              \
	cleanGeneratedConfigFiles.sh   \
	test-build-against-features.sh

EXTRA_DIST     = \
	$(BUILD_SCRIPTS_SET)   \
	Ceylan.cbp             \
	MakeConfigure          \
	ToolsSettings.inc      \
	configure-template.ac
	
noinst_SCRIPTS = $(BUILD_SCRIPTS_SET)

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = ceylan-0.3.pc

DIFF_TOOL    = `which tkdiff 2>/dev/null`

CONFIGURE_FILE = configure.ac

CONFIGURE_SCRIPTS_BASE_URL = http://savannah.gnu.org/cgi-bin/viewcvs/*checkout*/config/config
# Was/will be : ftp://ftp.gnu.org/gnu/config

DOWNLOADED_HELPERS = config.guess config.sub

CONFIG_FILES = $(DOWNLOADED_HELPERS)


all : links
	
		
check-configure: 
	@echo "@CEYLAN_MAKE_STYLE@    Comparing configure\
	files@CEYLAN_DEFAULT_STYLE@"
	@./checkConfigure.sh $(DIFF_TOOL)
	@echo "@CEYLAN_COMMAND_STYLE@    Comparison finished,\
	$(CONFIGURE_FILE) may be updated now."


download-config-scripts: clean-download
	@echo "@CEYLAN_MAKE_STYLE@    Downloading latest\
	configure helper scripts@CEYLAN_DEFAULT_STYLE@"
	@for f in $(DOWNLOADED_HELPERS); do \
	wget --quiet $(CONFIGURE_SCRIPTS_BASE_URL)/$$f ; done
	@chmod +x $(DOWNLOADED_HELPERS)


links:
	@echo "@CEYLAN_MAKE_STYLE@    Creating links to config\
	files@CEYLAN_DEFAULT_STYLE@"
	@cd ${top_builddir}; for f in $(CONFIG_FILES); do \
	ln -sf conf/build/$$f ; done 
	
	
clean-local:
	@echo "@CEYLAN_CLEAN_STYLE@    Cleaning generated config\
	files@CEYLAN_DEFAULT_STYLE@"
	-@rm -f autoscan*.log autoscan.err configure.scan
	
	
clean-download:
	@echo "@CEYLAN_CLEAN_STYLE@    Cleaning downloaded config\
	files@CEYLAN_DEFAULT_STYLE@"
	-@rm -f $(DOWNLOADED_HELPERS)
	
	
real-clean: clean clean-download
