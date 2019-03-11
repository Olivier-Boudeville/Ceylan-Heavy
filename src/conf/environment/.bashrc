# Bash main configuration file.

# Created 2002, June 26.
# Author : Olivier Boudeville (olivier (dot) boudeville (at) esperide (dot) com)

# This script triggers in turn (order matters!) the relevant bash configuration
# files.


# Sources specified bash configuration file, if existing.
#
# (helper)
#
source_if_exists()
{

	target_file="${1}"

	# Regular file or symlink allowed:
	#
	if [ -f "${target_file}" ] || [ -h "${target_file}" ] ; then

		#echo "  (sourcing ${target_file})"
		source "${target_file}"

	else

		!
		#echo "  (no '${target_file}' found, hence skipped)" 1>&2

	fi

}



# Retrieves the directory where this file and its helper files are stored:
#
MYHOME="${HOME}"


# From the most specific cases to the least:
#
# (note however that general settings may then overwrite specific ones, if
# blindly settings values)

# Local, host-specific overridden settings:
source_if_exists "${MYHOME}/.bashrc.local"

# Context-specific settings (ex: for a given organisation):
source_if_exists "${MYHOME}/.bashrc.contextual"

# Platform-specific settings:
source_if_exists "${MYHOME}/.bashrc.$(uname)"

# User-specific settings:
source_if_exists "${MYHOME}/.bashrc.$(whoami)"

# General-purpose, universal basic settings:
source_if_exists "${MYHOME}/.bashrc.basics"
