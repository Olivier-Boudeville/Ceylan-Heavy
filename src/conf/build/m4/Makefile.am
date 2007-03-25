# ceylan.m4 is to be found in ${top_srcdir}/test to help Ceylan users
# detecting the Ceylan library appropriately from their programs.

m4dir=${datadir}/@PROJECT_NAME@/aclocal
dist_m4_DATA=acinclude-ceylan.m4

install-data-local:
	@mkdir -p $(DESTDIR)@CEYLAN_INSTALL_SHARED_DIR@
	@cp -f $(srcdir)/ceylan.m4 $(srcdir)/pkg.m4 \
	$(DESTDIR)@CEYLAN_INSTALL_SHARED_DIR@
		
	
EXTRA_DIST = \
	axinclude.m4 \
	ceylan.m4    \
	pkg.m4
