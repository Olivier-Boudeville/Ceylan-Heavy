# Update the environment so that the Ceylan library can be found.

# For Windows (must be sourced from the script directory) :
PATH=`pwd`/../src/Debug:$PATH

# For most UNIX platforms :
LD_LIBRARY_PATH=`pwd`/../src/code:$LD_LIBRARY_PATH
