#!/usr/bin/env python

# Imports standard python modules:
import os, os.path, sys, string, shutil, tempfile, fileUtils, time


__doc__ = """
Usage: tree-file-compare.py [ -h | --help ] [ -v | --verbose ] [ --by-name-only ] --reference APath [--mirror AnotherPath]
Will scan first specified tree, in search of duplicated files (same content, different path). The resulting associations will be stored in ~/*-tree-file-compare.log files. If a second tree is specified (--mirror option), then will look for files whose content is in second tree but not in the first one, to ensure the reference tree is complete.

This script is useful to ensure a reference tree does not lack any content from a mirror and to know whether the mirror is up-to-date.
The script can be used for example for snapshots or archives.

Options:
	-v or --verbose: set verbose mode
	--by-name-only: comparison is done based on names only; no MD5 checksum performed (useful when the names refer clearly to the content, as an archive filename, as opposed to snapshots) 
	--mirror A_PATH: specifies a second tree to compare with
"""


# Examples:  
#
#   - for snapshots:
# ./tree_file_compare.py --by-name-only --reference /media/MacWay-Huge/Archive/ --mirror /home/boudevil/Projects/Generic/Personnel/Archives-personnelles/
#
#   - for archives: 
# ./tree_file_compare.py --reference /home/boudevil/Snapshots-Repository/ --mirror /home/boudevil/Snapshots-Repository-from-rainbow/



# The scanning of a path will result in the storing, in a dictionary, of
# a series of entries identifying the different file contents found.
# Each entry has for key a md5 code, and its associated value is 
# a list of the full relative paths of the files having that md5
# code. 
# Thus duplicates in a given tree can be easily found.


#### Beginning of top-level code. ####

log_file = None

# Home directory should be writable:
base_write_path = os.path.expanduser("~")

file_base_name = time.strftime( "%Y%m%d-tree-file-compare.log", time.gmtime() )

log_filename = os.path.join( base_write_path, file_base_name ) 



def output(message):
	print message
	log_file.write("%s\n" % message)
	
	

def build_file_index_for(path):
	"""Creates two (dictionary-based) file index for specified path."""

	# content_dict: keys are MD5 codes, values are lists of relative paths.
	# name_dict: keys are filenames, values are lists of relative paths.
	
	file_paths = fileUtils.getAllRelativeFilePathsFromRoot(path)

	content_dict={}
	name_dict={}
	
	for f in file_paths:
		
		full_path = os.path.join(path,f)
		
		# First scan the content:
		md5 = fileUtils.getMD5codeFor(full_path)
		
		if content_dict.has_key(md5):
			content_dict[md5] += [f]
		else:
			content_dict[md5] = [f]		
	
		# Then scan the filenames:
		name = os.path.basename(f)

		if name_dict.has_key(name):
			name_dict[name] += [f]
		else:
			name_dict[name] = [f]		
		
	#print "File content index = %s." % (content_dict,)
	#print "File name index = %s." % (name_dict,)
		
	return (content_dict,name_dict)



def build_name_index_for(path):
	"""Creates one (dictionary-based) name index for specified path."""

	# name_dict: keys are filenames, values are lists of relative paths.
	
	file_paths = fileUtils.getAllRelativeFilePathsFromRoot(path)

	name_dict={}
	
	for f in file_paths:
		
		full_path = os.path.join(path,f)
		
		# Scan the filenames:
		name = os.path.basename(f)

		if name_dict.has_key(name):
			name_dict[name] += [f]
		else:
			name_dict[name] = [f]		
		
	#print "File name index = %s." % (name_dict,)
		
	return name_dict



def display_content_duplicates(root_path,content_index):
	"""Displays the duplicates in specified content file index."""
	output( "Displaying duplicated content in tree %s:" % (root_path,))
	for k in content_index.keys():
		file_list = content_index[k]
		if len(file_list) > 1:
			#print "Following files located in path %s have the same content: %s." % (root_path,file_list)
			output( "  + identical content: %s." % (file_list,) )			
	output("")
	
	
			
def display_name_duplicates(root_path,name_index):
	"""Displays the duplicates in specified name file index."""
	output( "Displaying duplicated names in tree %s:" % (root_path,))
	for k in name_index.keys():
		file_list = name_index[k]
		if len(file_list) > 1:
			#print "Following files located in path %s have the same name: %s." % (root_path,file_list)
			output( "  + duplicated names: %s." % (file_list,) )
	output("")



def compare_content_trees(ref_content_index,mirror_content_index):
	"""Compares the reference and mirror trees, based on the file content. Useful to know whether a mirror is complete."""
	output("Comparing reference tree with mirror tree:")
	for k in ref_content_index.keys():
		ref_files = ref_content_index[k]
		if mirror_content_index.has_key(k):
			mirror_files = mirror_content_index[k]
			if mirror_files != ref_files:
				#print "For content whose MD5 code is %s, reference tells: %s, whereas mirror tells: %s." % (k,ref_files,mirror_files)				
				output( "  + identical content for %s in reference and %s in mirror." % (ref_files,mirror_files) )				
		else:
				#print "Content whose MD5 code is %s is in reference as %s, whereas mirror does not have it." % (k,ref_files)	
				output( "  (content corresponding to %s is in reference but not in mirror)" % (ref_files,) )	
	output("")
	
	
def compare_name_trees(ref_name_index,mirror_name_index):
	"""Compares the reference and mirror trees, based on the file name. Useful to know whether a mirror is complete."""
	output("Comparing reference tree with mirror tree:")
	for k in ref_name_index.keys():
		ref_files = ref_name_index[k]
		if mirror_name_index.has_key(k):
			mirror_files = mirror_name_index[k]
			if mirror_files != ref_files:
				#print "For name %s, reference tells: %s, whereas mirror tells: %s." % (k,ref_files,mirror_files)				
				output( "  + identical name for %s in reference and %s in mirror." % (ref_files,mirror_files) )				
		else:
				#print "Name %s is in reference as %s, whereas mirror does not have it." % (k,ref_files)	
				output( "  (name corresponding to %s is in reference but not in mirror)" % (ref_files,) )	
	output("")
	
	
	
def check_content_completeness(ref_content_index,mirror_content_index):
	"""Checks that all content of mirror tree is in reference tree, preferably with the same filenames."""
	output("Checking completeness of reference regarding the mirror:")
	for k in mirror_content_index.keys():
		if not ref_content_index.has_key(k):
			#print "Content whose MD5 code is %s is referenced in the mirror tree, as %s, and not available in reference." % (k,mirror_content_index[k])
			output( "  + content corresponding to %s is in mirror but not in reference." % (mirror_content_index[k],) )	
	output("")
		
		
	
def check_name_completeness(ref_name_index,mirror_name_index):
	"""Checks that all name of mirror tree is in reference tree, preferably with the same filenames."""
	output("Checking completeness of reference regarding the mirror:")
	for k in mirror_name_index.keys():
		if not ref_name_index.has_key(k):
			#print "Content whose MD5 code is %s is referenced in the mirror tree, as %s, and not available in reference." % (k,mirror_name_index[k])
			output( "  + name corresponding to %s is in mirror but not in reference." % (mirror_name_index[k],) )	
	output("")



def write_hashes(log_file,content_index):
	"""Writes specified content index in specified log file."""
	log_file.write("Hashes:\n\n")
	for k in content_index.keys():			
		log_file.write( "  %s %s\n" % (k,content_index[k]))
	log_file.write("\n")
		
		
		
		
if __name__ == '__main__':
		
	help_options = [ '-h', '--help' ]
	verbose_options = [ '-v', '--verbose' ]
	by_name_options = [ '--by-name-only' ]
	
	options = help_options + verbose_options + by_name_options

	# Defaults:
	verbose = False
	compare_by_content = True
		
	#print 'Arguments specified are <%s>.' % ( sys.argv, )

	saved_args = sys.argv[1:]
	
    # Remove executable name:
	sys.argv.pop(0)
    
	item_count = 0
	
	reference_path = None
	mirror_path = None
	
	while len(sys.argv):

		item = sys.argv.pop(0)
		item_understood = False

		#print 'Examining argument %s.' % ( item, )
		item_count += 1
	
		if item in help_options:
			item_understood = True
			print __doc__ 
			sys.exit( 0 )
									
		if item == "--reference":
			item_understood = True
			reference_path = sys.argv.pop(0)            
			#print "Set reference path to %s." % ( reference_path,)

		if item == "--mirror":
			item_understood = True
			mirror_path = sys.argv.pop(0)            
			#print "Set mirror path to %s." % ( mirror_path,)

		if item in verbose_options:
			item_understood = True
			verbose = True          
			print "Verbose mode activated."
    
		if item in by_name_options:
			item_understood = True
			compare_by_content = False          
			print "Comparison will be based on names only, rather than on content too."
    
		if not item_understood:
			print "Error, unexpected parameter: %s, stopping." % (  item, )   
			print __doc__ 
			sys.exit( 1 )
            
	if verbose:
		print "Reference path = %s" % ( reference_path )
		print "Mirror path = %s" % ( mirror_path )
    
	if not reference_path:
		print "Error, no reference path given, stopping."
		print __doc__ 
		sys.exit( 2 )
			
	
	log_file = open(log_filename,"w")
	
	log_file.write( "Report generated on %s.\n" % ( time.strftime("%a, %d %B %Y %H:%M:%S", time.gmtime()),) )

	log_file.write( "Arguments specified: %s" % (saved_args,) )
		
	print "Scanning reference tree..."

	if compare_by_content:
		(ref_content_index,ref_name_index) = build_file_index_for( reference_path )
		log_file.write("\n\n ***** For reference tree %s *****\n\n" % (reference_path,))
		display_content_duplicates(reference_path,ref_content_index)
		display_name_duplicates(reference_path,ref_name_index)
		write_hashes(log_file,ref_content_index)
	else:
		ref_name_index = build_name_index_for( reference_path )
		log_file.write("\n\n ***** For reference tree %s *****\n\n" % (reference_path,))
		display_name_duplicates(reference_path,ref_name_index)

	if mirror_path:
		log_file.write("\n\n ***** For mirror tree %s *****\n\n" % (mirror_path,))
		print "Scanning mirror tree..."
		if compare_by_content:
			(mirror_content_index,mirror_name_index) = build_file_index_for( mirror_path )
			display_content_duplicates(mirror_path,mirror_content_index)
			display_name_duplicates(mirror_path,mirror_name_index)
			write_hashes(log_file,mirror_content_index)
			compare_content_trees( ref_content_index, 
				mirror_content_index )
			check_content_completeness( ref_content_index,
				mirror_content_index )
		else:
			mirror_name_index = build_name_index_for( mirror_path )
			display_name_duplicates(mirror_path,mirror_name_index)
			# Maybe less useful:
			#compare_name_trees( ref_name_index, 
			#	mirror_name_index )
			check_name_completeness( ref_name_index,
				mirror_name_index )
			
		log_file.write("\n\n ***** Tree comparison *****\n\n")

	log_file.close()
	
