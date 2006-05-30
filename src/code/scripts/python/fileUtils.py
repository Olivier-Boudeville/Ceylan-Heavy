#!/usr/bin/env python


_file__         = 'fileUtils.py'
__title__       = 'This module helps managing files and directories.'
__version__     = '0.1'
__author__      = 'Olivier Boudeville (olivier.boudeville@online.fr)'
__project__     = 'Ceylan'
__creationDate__= '2004, February 1'
__comments__    = ""
__source__      = 'Mark Pilgrim, http://diveintopython.org/, and al.'
__doc__         = __title__ + '\n' + __comments__


# Import standard python modules :
import sys, types, string, os, os.path, ConfigParser

# Import self-made modules :
from generalUtils import *


class FileUtilsException( GeneralUtilsException ) :
    """Base class for fileUtils exceptions.""" 


def updateConfigurationFromFile( configDic, configFile ) :
	"""
	Updates configuration dictionary 'configDic' with informations found in file 'configFile'.
	Warning : uppercase letters should not be used in configuration files, since ConfigParser
	downcase them.
	"""
	
	try:
		myParser = ConfigParser.ConfigParser()
		myParser.read( configFile )
		optionsList = myParser.options( 'Options' )
		#print "Option list is %s." % ( optionsList, )
	except:
		print >> sys.stderr, 'Cannot open or read configuration file %s. Using default settings.' % ( configFile, ) 
		return configDic    
	for option in optionsList:
		if configDic.has_key( option ) :
			#print "Setting option %s" % ( option, )
			configDic[ option ] = myParser.get( 'Options', option )    
		else :
			print >> sys.stderr, "Warning : ignoring option <%s>, found in configuration file but not registered." % ( option, )


def findNextNewDirectoryName( destinationPrefix ):
    """
	Returns a unique name : find next possible new directory name by adding 
		increasing numbers to the end of the names of pre existing ones.
        Example : OSDL-data -> OSDL-data-1, OSDL-data-17 -> OSDL-data-18
    """
    dir_count = 0
    dest_dir  = destinationPrefix
    while os.path.exists( dest_dir ) :
        dir_count += 1
        dest_dir = '%s%s%s' % ( destinationPrefix, '-' , str( dir_count ) )    
    return dest_dir


def checkDirectory( directoryName ) :
	"""Checks if corresponding directory exists. If not, raises FileUtilsException."""
	
	if not os.path.exists( directoryName ) :
	    raise FileUtilsException, 'Cannot find directory <%s>.' % ( directoryName, )
	if not os.path.isdir( directoryName ) :
		raise FileUtilsException, '<%s> is not a directory.' % ( directoryName, )
	return directoryName
	
	
def checkFile( filename ) :
    """Checks if corresponding file exists. If not, raises FileUtilsException."""
    if not os.path.exists( filename ) :
        raise FileUtilsException, 'Cannot find file <%s>.' % ( filename, )
	if not os.path.isfile( filename ) :
		raise FileUtilsException, '<%s> is not a file.' % ( filename, )
	return filename


def convertIntoFileName( name ) :
	"""Converts specified name into a valid filename for most file systems."""
	
	return name.replace( '.', '-' ).replace( ' ', '-' )
	
	
def filesAllExist( filenamesList ) :
	"""Returns true if and only if all files in the list exist."""
	for f in filenamesList:
		if not os.path.isfile( f ) :
			display.debug( 'File %s does not exist.' )
			return false
	return True
	
	
def isContent( filename ) :
	"""Returns true if and only if filename corresponds to a content file."""		
	return os.path.isfile( filename ) and ( isGraphics( filename ) or isSound( filename ) )
        
        
def isGraphics( filename ) :
	"""Returns true if and only if filename corresponds to a picture file."""
	graphics_extension_list = [ '.jpg', '.jpeg', '.tiff', '.png', '.gif', '.bmp' ]
	return os.path.splitext( filename )[1].lower() in graphics_extension_list
        
        
def isSound( filename ) :
	"""Returns true if and only if filename corresponds to a sound file."""
	sound_extension_list = [ '.wav', '.mp3', '.ogg', '.raw' ]
	return os.path.splitext( filename )[1].lower() in sound_extension_list
		
	
def getParentDirectory( directoryName ) :
	"""Returns the name of the parent directory of directoryName."""
	return os.path.dirname( directoryName )
	
	
def getChildrenDirectories( directoryName ) :
	"""
	Returns a list made of the (relative) names of the children directories of directoryName.
	"""
	return [ elem for elem in os.listdir( directoryName ) if os.path.isdir( os.path.join( directoryName, elem ) ) ]


def getFilesInDir( directoryName ) :
	"""Returns a list made of the (relative) names of the files in directory directoryName."""
	#return [ elem for elem in os.listdir( directoryName ) if os.path.isfile( os.path.join( directoryName, elem) ) ]
	fileList = []
	for elem in os.listdir( directoryName ) :
		if os.path.isfile( os.path.join( directoryName, elem) ) :
			fileList.append( elem )
	return fileList		


def getDirectoryElements( directoryName ) :
	"""
	Returns a pair, the list of subdirectories of directoryName and the list of 
	the files in directoryName, all of them relatively to that directory, no absolute path.
	"""
	directories_list = []
	files_list       = []
	for elem in os.listdir( directoryName ) :
		if os.path.isdir( os.path.join( directoryName, elem) ) :
			directories_list.append( elem )
		elif os.path.isfile( os.path.join( directoryName, elem) ) :
			files_list.append( elem )			
		else:
			raise FileUtilsException, 'In directory %s, %s was found being neither directory or file.' % (	elem,)
	return directories_list, files_list
	
	
def addPrefixToFileList( prefix, filesList ) :
	"""All files or directories in filesList will be prefixed with prefix."""
	
	return [ os.path.join( prefix, file) for file in filesList] 
	

def scanDirectoryForContent( directoryName ) :
	"""Scans provided directory and return a 3-item tuple with graphics files, sound files and other files that were spotted."""
	
	if not os.path.isdir( directoryName ):
		raise ValueError, "Unable to scan directory %s, for it does not exist." % ( directoryName, )	
	graphics_list = []	
	sounds_list   = []
	unknown_list  = []
	for f in getFilesInDir( directoryName ) :
		if isGraphics( f ) :
			graphics_list.append( f )
		elif isSound( f ) :
			sounds_list.append( f )
		else :
			unknown_list.append( f )
			
	return graphics_list, sounds_list, unknown_list
	        

def backup( fileToBackup ) :
	"""
	Backups a file. Non-existing file will be ignored. Backup file will have the suffix '.bak'.
	"""
        
	backExtension = '.bak'
		
	backupFile = fileToBackup + backExtension
		
	if os.path.exists( backupFile ) :
		try:
			os.remove( backupFile )
		except OSError:
			print >> sys.stderr, 'Cannot remove %s' % ( backupFile, )
		return sys.exc_info()
				
	if os.path.exists( fileToBackup ):
		try:
			os.rename( fileToBackup, backupFile )
		except OSError:
			print >> sys.stderr, 'Cannot rename %s' % ( fileToBackup, )
		return sys.exc_info()



if __name__ == "__main__":        
	print __doc__

		
