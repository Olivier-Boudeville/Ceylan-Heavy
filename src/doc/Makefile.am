SUBDIRS = basic

# Whole website will be distributed:
EXTRA_DIST = \
	Ceylan-@CEYLAN_VERSION@-release-notes.txt \
	Ceylan-@CEYLAN_VERSION@-changes.txt       \
	web


# This section takes care of the web documentation.


# What is expected from the html documentation for the Ceylan x.y version:
#	- be accessible from '$BASE_URL'
# (ex: BASE_URL=http://ceylan.sourceforge.net), under '$BASE_URL/Ceylan-x.y'
#	- if x.y is the latest stable version, then it should be accessible
# from '$BASE_URL/Ceylan-latest' as well
#
# The general documentation (not specific to any given version) should be found
# directly from '$BASE_URL'.
#
# The full website must be set to any given ROOT
# (ex: ROOT=/home/groups/c/ce/ceylan/htdocs),


# This documentation generation has been a pain and a mess for months.
#
# The issue is: rsync will not create non already-existing target base
# directories, and doing it so thanks to a remote script is not convenient.

# The only solution: making a perfectly similar tree and rsync'ing it blindly.


# Therefore, for all web roots ${ROOT}, the result of the rsync should lead to a
# tree like:
#
# ${ROOT}/
# |-- (directly files and directories common to all versions)
# |-- Ceylan-latest -> Ceylan-x.y
# |-- Ceylan-u.v
# |   |-- (directly files and directories common to this version)
# |   `-- api
# `-- Ceylan-x.y
#     |-- (directly files and directories common to this version)
#     `-- api

# etc.


# PNG Generation for .graph files thanks to dot, for documentation files:
# (ex: 'make web/Ceylan-latest/anImage.png')
GRAPH_SRC = $(wildcard *.graph)

GRAPH_PNG  = $(GRAPH_SRC:%.graph=%.png)

%.png: %.graph
	@echo "     Generating PNG file $@ from $<"
	@$(DOT) -Tpng $< -o $@


# Content directories.



# Sourceforge settings.

SF_PROJECT_NAME      = @UNIX_PROJECT_NAME@

SF_SHELL_SERVER      = shell.@CEYLAN_SF_WEB_SERVER@
SF_WEB_SERVER        = web.sourceforge.net

SF_WEB_DIR           = htdocs

SF_WEB_URL           = @CEYLAN_SF_URL@/index.html

CEYLAN_DEFAULT_SF_USER              = wondersye
SF_SHELL_HOMEDIR     = /home/users/`echo "@CEYLAN_DEFAULT_SF_USER@" | head -c 1`/`echo "@CEYLAN_DEFAULT_SF_USER@" | head -c 2`/"@CEYLAN_DEFAULT_SF_USER@"


# Mirror settings.
MIRROR_HOSTNAME      = sonata
MIRROR_WEB_URL       = http://@UNIX_PROJECT_NAME@.esperide.com
MIRROR_WEB_USER      = web-srv
MIRROR_WEB_DIR       = /var/www/@PROJECT_NAME@


# Web content should not contain 'common' section, since both doc and api need
# it.

# A lot of complication (the creation of directories then the installation of
# files) comes from distcheck and its setting the whole source tree (including
# the directories) in read-only: it is a mess to handle with cp --parents and/or
# /bin/install.

# These variable are made to be evaluated from $(srcdir)/web:

WEB_CONTENT_FILES    = `find . -path 'common' -prune -a -type f -o ! -path '*.svn*' -a -type f`

WEB_CONTENT_DIRS     = `find . -path 'common' -prune -a -type d -o ! -path '*.svn*' -a -type d`

COMMON_WEB_FILES     = `find common ! -path '*.svn*' -type f -follow`
COMMON_WEB_DIRS      = `find common ! -path '*.svn*' -type d -follow`

HTML_FILES           = `find . -name '*.html' -a -type f`
RST_FILES            = `find . -name '*.rst' -a -type f`



# Tools section

DOXYGEN_ROOT  = @CEYLAN_LOANI_INSTALLATIONS@/doxygen-@DOXYGEN_VERSION@
DOXYGEN       = `PATH=$(DOXYGEN_ROOT)/bin:$$PATH which doxygen 2>/dev/null`

DOXYGEN_GENERATED_CONF = @top_builddir@/src/conf/doc/doxygen.conf

DOT_ROOT      = @CEYLAN_LOANI_INSTALLATIONS@/dot-graphviz-@DOT_VERSION@
DOT           = `PATH=$(DOT_ROOT)/bin:$$PATH which dot 2>/dev/null`

RST2HTML_ROOT = @CEYLAN_LOANI_INSTALLATIONS@/docutils-$(docutils_VERSION)
RST2HTML      = `PATH=$(RST2HTML_ROOT)/bin:$$PATH which rst2html 2>/dev/null`

ASTYLE        = `which astyle 2>/dev/null`
ASTYLE_CONF   = @top_srcdir@/conf/astyle.conf

TIDY_ROOT     = @CEYLAN_LOANI_INSTALLATIONS@/tidy
TIDY		  = `PATH=$(TIDY_ROOT)/bin:$$PATH which tidy 2>/dev/null`

RSYNC		  = `which rsync 2>/dev/null`
RSYNC_OPT     = "-pg -e ssh -vcrlz --cvs-exclude"

SCP           = `which scp 2>/dev/null`
SCP_OPT       = "-p -r"


.PHONY: all install doc build inc web links site common                    \
	update-rst check-rst loani-archives api update-dox-look check-dot      \
	fix-dox-conf  tidy transfer toSF toSF-rsync toSF-scp log-to-SF-web     \
	toMirror toMirror-rsync toMirror-scp check-scp remind-locations        \
	info info-dirs info-files info-sf info-html info-tools                 \
	clean clean-web cleanobjs cleandeps clean-install real-clean



# This section is devoted to local documentation generation only.


# If api is wanted and can be generated, will be built:

install-data-local: site remind-locations

doc: local-web remind-locations

local-web: site api


# The 'site' target takes automatically care of both shared tree and of the tree
# specific to this version:
site: update-rst common
	@echo; echo "@CEYLAN_INSTALL_STYLE@    Copying web source tree to \
	local web repository \
	[$(DESTDIR)@CEYLAN_LOCAL_WEB_DIR@]@CEYLAN_DEFAULT_STYLE@"
	@cd $(srcdir)/web; for d in $(WEB_CONTENT_DIRS); do \
	mkdir -p $(DESTDIR)@CEYLAN_LOCAL_WEB_SHARED_DIR@/$$d ; done ;  \
	for f in $(WEB_CONTENT_FILES); do \
	cp -f $$f $(DESTDIR)@CEYLAN_LOCAL_WEB_SHARED_DIR@/$$f; done
	@echo "@CEYLAN_COMMAND_STYLE@    + you can bookmark local doc site, \
	which is to be found in:"
	@echo "      file://$(DESTDIR)@CEYLAN_LOCAL_WEB_DIR@/index.html@CEYLAN_DEFAULT_STYLE@"
	@cd $(DESTDIR)@CEYLAN_LOCAL_WEB_DIR@; \
	ln -sf @CEYLAN_CURRENT_VERSION_PREFIX@ Ceylan-latest


update-rst: check-rst
	@echo; echo "@CEYLAN_INSTALL_STYLE@    Generating html documentation pages \
	thanks to docutils (rst)@CEYLAN_DEFAULT_STYLE@"
	@#Not used yet: @CEYLAN_UPDATE_RST@ web/common/css/Ceylan-docutils.css


generate-pdf: check-rst
	@echo; echo "@CEYLAN_INSTALL_STYLE@    Generating PDF documentation \
	thanks to docutils (rst)@CEYLAN_DEFAULT_STYLE@"
	@@CEYLAN_UPDATE_RST@ --pdf


check-rst:
	@[ -x "$(RST2HTML)" ] \
	|| ( echo; echo "@CEYLAN_ERROR_STYLE@    Unable to generate \
	html files from docutils files (*.rst): \
	docutils tool (rst2html) not found [docutils homepage: \
	http://docutils.sourceforge.net].@CEYLAN_DEFAULT_STYLE@"; echo; false )


common:
	@echo; echo "@CEYLAN_INSTALL_STYLE@    Copying common web files to \
	local doc repository \
	[$(DESTDIR)@CEYLAN_LOCAL_WEB_SHARED_DIR@]@CEYLAN_DEFAULT_STYLE@"
	@if [ -d "$(DESTDIR)@CEYLAN_LOCAL_DOC_DIR@" ] ; then \
	chmod a+w $(DESTDIR)@CEYLAN_LOCAL_DOC_DIR@; else \
	chmod a+w `dirname $(DESTDIR)@CEYLAN_LOCAL_DOC_DIR@`; \
	mkdir -p $(DESTDIR)@CEYLAN_LOCAL_DOC_DIR@ ; fi
	@mkdir -p $(DESTDIR)@CEYLAN_LOCAL_WEB_DIR@
	@mkdir -p $(DESTDIR)@CEYLAN_LOCAL_WEB_SHARED_DIR@
	@cd $(srcdir)/web; for d in $(COMMON_WEB_DIRS); do \
	mkdir -p $(DESTDIR)@CEYLAN_LOCAL_WEB_SHARED_DIR@/$$d ; done ; \
	for f in $(COMMON_WEB_FILES); do \
	cp -f $$f $(DESTDIR)@CEYLAN_LOCAL_WEB_SHARED_DIR@/$$f; done




uninstall-hook:
	@echo; echo "@CEYLAN_INSTALL_STYLE@    Uninstalling documentation from \
	$(DESTDIR)@CEYLAN_LOCAL_WEB_SHARED_DIR@ @CEYLAN_DEFAULT_STYLE@"
	-@/bin/rm -rf $(DESTDIR)@CEYLAN_LOCAL_DOC_DIR@


# One must prevent doxygen from overwriting the master
# doxygen-@PROJECT_NAME@.css through the symbolic link!
#
# We assume here that 'dot' has been compiled with our default compiler, gcc:

api: check-dot fix-dox-conf
	@[ -x "$(DOXYGEN)" ] || \
	( echo; echo "@CEYLAN_ERROR_STYLE@    \
	Unable to create source documentation: doxygen tool not found \
	[doxygen homepage: http://www.doxygen.org].@CEYLAN_DEFAULT_STYLE@"; \
	echo; false )
	@echo; echo "@CEYLAN_COMMAND_STYLE@    Generating API \
	(doxygen configuration taken from $(DOXYGEN_GENERATED_CONF)) \
	@CEYLAN_DEFAULT_STYLE@"
	@echo
	@mkdir -p $(DESTDIR)@CEYLAN_LOCAL_WEB_CURRENT_VERSION_API_DIR@
	-@PATH=`pwd`/$(DOT_ROOT)/bin:$$PATH \
	LD_LIBRARY_PATH=`pwd`/$(GCC_ROOT)/lib:$$LD_LIBRARY_PATH \
	$(DOXYGEN) $(DOXYGEN_GENERATED_CONF)
	@echo
	@echo "@CEYLAN_COMMAND_STYLE@    + you can bookmark \
	@CEYLAN_CURRENT_VERSION_PREFIX@ API, which is located in: \
file://$(DESTDIR)@CEYLAN_LOCAL_WEB_CURRENT_VERSION_API_DIR@/html/index.html@CEYLAN_DEFAULT_STYLE@"


# Not run by default, as the native look of doxygen is actually better, and more
# appropriate:
update-dox-look:
	@/bin/rm -f $(DESTDIR)@CEYLAN_LOCAL_WEB_CURRENT_VERSION_API_DIR@/html/doxygen.css
	@cp -f $(srcdir)/web/common/css/doxygen-@PROJECT_NAME@.css \
	$(DESTDIR)@CEYLAN_LOCAL_WEB_CURRENT_VERSION_API_DIR@/html/doxygen.css
	@cp -f $(srcdir)/web/common/doxygen/*.png \
	$(DESTDIR)@CEYLAN_LOCAL_WEB_CURRENT_VERSION_API_DIR@/html
	@echo


check-dot:
	@[ -x "$(DOT)" ] \
	|| ( echo; echo "@CEYLAN_ERROR_STYLE@    Unable to generate \
	any useful documentation from source code: \
	dot tool not found (checked dot root was $(DOT_ROOT) [dot homepage: \
	http://www.graphviz.org/].@CEYLAN_DEFAULT_STYLE@"; echo; false )


fix-dox-conf:
	@@CEYLAN_SUBSTITUTE@ '\$${prefix}' "${prefix}" $(DOXYGEN_GENERATED_CONF)


tidy:
	@[ -x "$(TIDY)" ] \
	|| ( echo; echo "@CEYLAN_ERROR_STYLE@    Unable to enforce \
	W3C compliance and formatting of html documentation files \
	according to @PROJECT_NAME@'s conventions: \
	tidy tool not found.@CEYLAN_DEFAULT_STYLE@"; echo; false )
	@echo "@CEYLAN_COMMAND_STYLE@    Tidying all html \
	files@CEYLAN_DEFAULT_STYLE@"
	@for f in $(HTML_FILES); do ( echo "  + tidying $$f"; \
	$(TIDY) -config @top_srcdir@/src/conf/doc/tidy.conf -m $$f ); done



# This section is devoted to remote documentation synchronization only.


transfer: toSF toMirror


# toSF will copy all documentation files (doc and, if available, api) to SF
# webserver.
#
# As doc is in CVS, one would generally only need to transfer api.  Updating doc
# manually is notably useful to instantaneously update the website.
toSF:
	@if [ -x "$(RSYNC)" ]; then $(MAKE) toSF-rsync; \
	else $(MAKE) toSF-scp; fi


toSF-rsync: site
	@echo
	@echo "@CEYLAN_INSTALL_STYLE@    Updating @PROJECT_NAME@ \
	Sourceforge's website from local web repository with rsync \
	[@CEYLAN_LOCAL_WEB_DIR@] using rsync@CEYLAN_DEFAULT_STYLE@"
	@echo
	@opt=$(RSYNC_OPT); $(RSYNC) $$opt @CEYLAN_LOCAL_WEB_DIR@/* \
	@CEYLAN_DEFAULT_SF_USER@,@UNIX_PROJECT_NAME@@$(SF_WEB_SERVER):$(SF_WEB_DIR)
	@echo "@CEYLAN_COMMAND_STYLE@    + you can bookmark \
	@PROJECT_NAME@'s website on Sourceforge at \
	$(SF_WEB_URL)@CEYLAN_DEFAULT_STYLE@"


toSF-scp: check-scp site
	@echo
	@echo "@CEYLAN_INSTALL_STYLE@    Updating @PROJECT_NAME@ \
	Sourceforge's website from local web repository with scp \
	[@CEYLAN_LOCAL_WEB_DIR@] using scp@CEYLAN_DEFAULT_STYLE@"
	@echo
	@opt=$(SCP_OPT); $(SCP) $$opt @CEYLAN_LOCAL_WEB_DIR@/* \
	@CEYLAN_DEFAULT_SF_USER@,@UNIX_PROJECT_NAME@@$(SF_WEB_SERVER):$(SF_WEB_DIR)
	@echo "@CEYLAN_COMMAND_STYLE@    + you can bookmark \
	@PROJECT_NAME@'s website on Sourceforge at \
	$(SF_WEB_URL)@CEYLAN_DEFAULT_STYLE@"


log-to-SF-web:
	@echo
	@echo "@CEYLAN_INSTALL_STYLE@    Logging to Sourceforge's @PROJECT_NAME@ shell@CEYLAN_DEFAULT_STYLE@"
	@ssh -t @CEYLAN_DEFAULT_SF_USER@,@UNIX_PROJECT_NAME@@shell.sourceforge.net create


toMirror:
	@if [ -x "$(RSYNC)" ]; then $(MAKE) toMirror-rsync; \
	else $(MAKE) toMirror-scp; fi


toMirror-rsync: site
	@echo
	@echo; echo "@CEYLAN_INSTALL_STYLE@    Updating @PROJECT_NAME@ \
	$(MIRROR_HOSTNAME)'s website from local web repository \
	[@CEYLAN_LOCAL_WEB_DIR@] using rsync@CEYLAN_DEFAULT_STYLE@"
	@echo
	@opt=$(RSYNC_OPT); $(RSYNC) $$opt @CEYLAN_LOCAL_WEB_DIR@/* \
	$(MIRROR_WEB_USER)@$(MIRROR_HOSTNAME):$(MIRROR_WEB_DIR)
	@echo "@CEYLAN_COMMAND_STYLE@    + you can bookmark \
	@PROJECT_NAME@'s website on $(MIRROR_HOSTNAME) at \
	$(MIRROR_WEB_URL)@CEYLAN_DEFAULT_STYLE@"


toMirror-scp: check-scp site
	@echo
	@echo; echo "@CEYLAN_INSTALL_STYLE@    Updating @PROJECT_NAME@ \
	$(MIRROR_HOSTNAME)'s website from local web repository \
	[@CEYLAN_LOCAL_WEB_DIR@] using scp@CEYLAN_DEFAULT_STYLE@"
	@echo
	@opt=$(SCP_OPT); $(SCP) $$opt @CEYLAN_LOCAL_WEB_DIR@/* _
	$(MIRROR_WEB_USER)@$(MIRROR_HOSTNAME):$(MIRROR_WEB_DIR)
	@echo "@CEYLAN_COMMAND_STYLE@    + you can bookmark \
	@PROJECT_NAME@'s website on $(MIRROR_HOSTNAME) at \
	$(MIRROR_WEB_URL)@CEYLAN_DEFAULT_STYLE@"


check-scp:
	@[ -x "$(SCP)" ] || ( echo; echo "@CEYLAN_ERROR_STYLE@    \
	Unable to transfer html documentation files to remote \
	web server: rsync and its alternative scp not available. \
	Please check src/conf/MakeVars.inc or update \
	PATH environment variable.@CEYLAN_DEFAULT_STYLE@"; echo; false )
	@echo; echo "@CEYLAN_WARNING_STYLE@    Consider installing rsync, \
	which is far more efficient than scp at updating \
	mirrors.@CEYLAN_DEFAULT_STYLE@"


remind-locations:
	@echo; echo "@CEYLAN_INSTALL_STYLE@    + the following \
	documentation repositories are available:"
	@echo "      * local doc directory: \
	$(DESTDIR)@CEYLAN_LOCAL_DOC_DIR@"
	@echo "      * local web site     : \
	$(DESTDIR)@CEYLAN_LOCAL_WEB_DIR@/main/index.html"
	@echo "      * local web API      : \
	$(DESTDIR)@CEYLAN_LOCAL_WEB_CURRENT_VERSION_API_DIR@/html/index.html"
	@echo "      * official website   : @CEYLAN_SF_VERSION_URL@"
	@echo "@CEYLAN_DEFAULT_STYLE@"


show-found-tools:
	@echo; echo "@CEYLAN_INSTALL_STYLE@    Listing found tools \
	@CEYLAN_DEFAULT_STYLE@"; echo
	@if [ -x "$(DOXYGEN)" ]; then echo "  + doxygen found, using <$(DOXYGEN)> \
	with configuration file <$(DOXYGEN_CONF)>"; fi
	@if [ -x "$(DOT)" ]; then  echo "  + dot found, using <$(DOT)>"; fi
	@if [ -x "$(ASTYLE)" ]; then echo "  + astyle found, using <$(ASTYLE)> \
	with configuration file <$(ASTYLE_CONF)>"; fi
	@if [ -x "$(TIDY)" ]; then echo "  + tidy found, using <$(TIDY)>"; fi
	@if [ -x "$(RSYNC)" ]; then echo "  + rsync found, using <$(RSYNC)>"; fi
	@if [ -x "$(SCP)" ]; then echo "  + scp found, using <$(SCP)>"; fi


info-local: info-dirs info-files info-sf


info-dirs:
	@echo "    + prefix is:" ${prefix}
	@echo "    + local documentation directory is: @CEYLAN_LOCAL_DOC_DIR@"
	@echo "    + local web directory is: @CEYLAN_LOCAL_WEB_DIR@"
	@echo "    + local api directory is: \
	@CEYLAN_LOCAL_WEB_CURRENT_VERSION_API_DIR@"


info-files: info-html
	@echo; echo "    + web documentation files are  : "; \
	echo "$(WEB_CONTENT_FILES)"
	@echo; echo "    + common web files are         : "; \
	echo "$(COMMON_WEB_FILES)"
	@echo; echo "    + doxygen configuration file is: \
	$(DOXYGEN_GENERATED_CONF)"


info-sf:
	@echo "    + sourceforge UNIX project name: $(SF_PROJECT_NAME)"
	@echo "    + sourceforge shell server     : $(SF_SHELL_SERVER)"
	@echo "    + sourceforge web server       : $(SF_WEB_SERVER)"
	@echo "    + full sourceforge login       : \
	ssh @CEYLAN_DEFAULT_SF_USER@@$(SF_SHELL_SERVER)"
	@echo "    + sourceforge web directory    : $(SF_WEB_DIR)"
	@echo "    + sourceforge home directory   : $(SF_SHELL_HOMEDIR)"
	@echo "    + SSH target                   : \
	@CEYLAN_DEFAULT_SF_USER@@$(SF_SHELL_SERVER):$(SF_WEB_DIR)"
	@echo "    + files to be copied will be   : \
	scp myFiles @CEYLAN_DEFAULT_SF_USER@@$(SF_WEB_SERVER):$(SF_SHELL_HOMEDIR)"


info-html:
	@echo
	@echo "    + HTML files are   :"; echo "$(HTML_FILES)"
	@echo "    + rsync options are: $(RSYNC_OPT)"
	@echo "    + scp options are  : $(SCP_OPT)"


info-tools:
	@echo "DOXYGEN       = <$(DOXYGEN)>"
	@echo "DOXYGEN_ROOT  = <$(DOXYGEN_ROOT)>"
	@echo "DOT           = <$(DOT)>"
	@echo "DOT_ROOT      = <$(DOT_ROOT)>"


clean-local: clean-web



clean-web:
	@echo "@CEYLAN_CLEAN_STYLE@    Cleaning local doc repository \
	[@CEYLAN_LOCAL_WEB_DIR@]@CEYLAN_DEFAULT_STYLE@"
	-@/bin/rm -rf @CEYLAN_LOCAL_WEB_DIR@


clean-install: clean-web
	@echo "@CEYLAN_CLEAN_STYLE@    Cleaning installed doc repository \
	[@CEYLAN_LOCAL_DOC_DIR@]@CEYLAN_DEFAULT_STYLE@"
	-@/bin/rm -rf @CEYLAN_LOCAL_DOC_DIR@


real-clean: clean-web
