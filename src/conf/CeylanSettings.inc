# Basic settings for Ceylan project.

# This is a source file that can be modified to customize settings prior to the
# whole 'autogen.sh' process, whose beginning is the generation of
# 'configure.ac' from the settings defined here and the template
# 'configure-template.ac'.

# More precisely, due to Autoconf limitations (no variable substitution in
# AC_INIT, AC_CONFIG_SRCDIR, AC_CONFIG_HEADERS, etc.) we had to create
# 'configure-template.ac', a template for the expected 'configure.ac', made to
# be filled with the values from this 'CeylanSettings.inc' file.

# It is a means of preserving one-time assignments of key variables: we believe
# that duplication of value definitions leads to nasty pitfalls.

# See FIXME.html for the detailed explanation of the issue.

# The allowed syntax in this file is the one of the Makefile, since this
# settings file will be only included in 'MakeConfigure' to guide the
# substitution process: the ST_* Substitution Targets tags written in
# 'configure-template.ac' will be replaced by their associated value defined
# here, and the result will be the expected 'configure.ac' filled appropriately.

# This settings file is only to be sourced for the substitution process, it
# should not be used afterwards in the building process. Hence no Makefile
# should include it. The proper way for them to access the settings is to
# include the 'Makevars.inc' file, which is specifically generated with the
# correct values by the configuration process.


# Filenames are relative to the 'trunk/src' directory.


# Naming section.

# (tag: ST_PROJECT_NAME)
PROJECT_NAME=Ceylan

# (tag: ST_UNIX_PROJECT_NAME)
UNIX_PROJECT_NAME=ceylan


# Ceylan version numbers for current release.


# Usual versioning.

# (tag: ST_MAJOR_VERSION)
CEYLAN_MAJOR_VERSION=0

# (tag: ST_MINOR_VERSION)
# (note: most be updated just *before* making a release)
CEYLAN_MINOR_VERSION=7
#CEYLAN_MINOR_VERSION=8

# Release number is only for project fine grain milestone,
# for a given major.minor version (ex: release candidate).
# (tag: ST_RELEASE)
CEYLAN_RELEASE=0

# Full actual release date (not generated for better control).
# (quote protection is necessary)
# (tag: ST_RELEASE_DATE)
CEYLAN_RELEASE_DATE='"Saturday, April 4, 2009"'

# (tag: ST_VERSION)
CEYLAN_VERSION=$(CEYLAN_MAJOR_VERSION).$(CEYLAN_MINOR_VERSION)

# (tag: ST_FULL_VERSION)
CEYLAN_FULL_VERSION=$(CEYLAN_VERSION).$(CEYLAN_RELEASE)


# Libtool-style versioning.

# When making releases, modify these version numbers according to
# Libtool-defined rules, as described in:
# http://www.gnu.org/software/libtool/manual.html#Updating-version-info

# Starting from latest public release, labelled 'major:minor:ancestor', version
# becomes:
# (all matching rules should be applied, in turn)
#   1. If the implementation (the source code) changed, then increment minor
#   2. If the API changed (any interface added, removed, or changed), then
# increment major, and set minor to zero (i.e. go to the next major)
#   3. If any interface has been removed or has its contract changed,
# set ancestor to major


# Ancestor corresponds to the first major version whose API is a subset of the
# current one, i.e. starting from this major version, any later one can be used
# instead. Libtool age corresponds to: major minus ancestor.
# (tag: ST_MAJOR_ANCESTOR)
CEYLAN_MAJOR_ANCESTOR=0


# Backward compatibility section.

# Build section.

# As long as the Ceylan library is not almost frozen, managing age is
# cumbersome, the usual major.minor versioning is preferred.

# The major version of the most ancient Ceylan version compatible with this one.
# (tag: ST_OLDEST_SUPPORTED_MAJOR)
CEYLAN_OLDEST_SUPPORTED_MAJOR=0

# The minor version of the most ancient Ceylan version compatible with this one.
# (tag: ST_OLDEST_SUPPORTED_MINOR)
CEYLAN_OLDEST_SUPPORTED_MINOR=6



# (tag: ST_CONFIG_HEADER).
CEYLAN_CONFIG_HEADER="src/code/CeylanTemporaryConfig.h"



# Support section.

# (tag: ST_MAILING_LIST_SUPPORT)
CEYLAN_MAILING_LIST_SUPPORT="ceylan-support@lists.sourceforge.net"

# (tag: ST_MAILING_LIST_BUGTRACKING)
CEYLAN_MAILING_LIST_BUGTRACKING="ceylan-bugs@lists.sourceforge.net"
