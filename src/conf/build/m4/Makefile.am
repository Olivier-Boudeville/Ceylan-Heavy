# ceylan.m4 is to be found in ${top_srcdir}/test to help Ceylan users
# detecting the Ceylan library appropriately from their programs.

m4dir=${datadir}/@PROJECT_NAME@/aclocal
dist_m4_DATA=acinclude-ceylan.m4

install-data-local: regenerate-m4
	@cp -f $(srcdir)/ceylan.m4 @CEYLAN_INSTALL_SHARED_DIR@
	
regenerate-m4:
	@cp -f $(srcdir)/ceylan.m4 ${top_srcdir}/test
	
	
EXTRA_DIST = \
	axinclude.m4
