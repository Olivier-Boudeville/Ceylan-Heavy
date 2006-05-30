#!/usr/bin/env python


_file__         = 'generalUtils.py'
__title__       = 'This is the general module, which contains everything which is the least specific to any project : all generic and all purpose code should end up here.'
__version__     = '0.1'
__author__      = 'Olivier Boudeville (olivier.boudeville@online.fr)'
__project__     = 'Ceylan'
__creationDate__= '2004, January 31'
__comments__    = "Curiously enough, making an inherited method private does not override base class' one : Display.display could not be Display.__display if subclasses were to be able to redefine it."
__source__      = 'Mark Pilgrim, http://diveintopython.org/, and al.'
__doc__         = __title__ + '\n' + __comments__



# Backward compatibility for previous versions of Python, since True and False did not exist :
true  = 1
false = 0


# Import standard python modules :
import sys, types, string


class GeneralUtilsException( Exception ) :
    """Base class for generalUtils exceptions.""" 


class ApplicationException( Exception ) :
    """Base class for applicative exceptions."""


def activateNameCompletion() :
	"""Activates python interpreter's command line automatic name completion."""
	import rlcompleter
	import readline
	readline.parse_and_bind( "tab: complete" )
	print "Python's name completion activated."
	

def displayDic( aDic ):
	"""Displays key-value pairs of specified dictionnary."""
	# Find longest key :
	max_len = 0
	for k in aDic.keys():
		if len(k) > max_len:
			max_len = len(k)
	# First field shoud have the width of the longest key item :
	for (k,v) in aDic.items():
		print '  %*s : %s' % (max_len, k, v)
		
		 
def displayList( aList ):	
	"""Displays list items, one by line."""
	for i in aList:
		print i
		
	
class Display :	
	"""This class provides abstract display services. It is intended to be subclassed."""
	
	prefixForKeyMessages = "--"
	offsetIncrement = 4
	
	normalPrefix  = ''
	debugPrefix   = '--> Debug : '
	warningPrefix = 'Warning : '
	errorPrefix   = '#### Error : '
	 
	do_debug    = true
	
	def __init__( self, spacing = 10, compression = true, truncate = false, verbosity = 2 ) :
		"""
		Defines the options for this display to perform its tasks.
			- spacing : minimum size for a field,
			- compression : if true, a multi-lined field is displayed only on one line,
			- truncate : if true, truncates a field to the maximum length this values provides,
			- verbosity : sets the level of detail of the display, 
					+ 0 : totally silent,
					+ 1 : only most important messages are displayed, those which starts by prefixForKeyMessages,
					+ 2 : all messages are displayed.
		"""	

		self.esp    = spacing     and ( lambda s, spaceNum = spacing : string.ljust( s, spaceNum ) ) or ( lambda s : s )
		self.comp   = compression and ( lambda s : string.join( s.split(), ' ' ) ) or ( lambda s : s )
		self.trunc  = truncate    and ( lambda s, truncNum = truncate : s[ : truncNum ] ) or ( lambda s : s )
		self.verb   = verbosity
		self.offset = 0
	
	
	def display( self, message, addReturn = true ) :
		"""Displays unconditionnally provided normal message."""
		if addReturn :
			self.innerDisplayStandard( self.normalPrefix + self.offset * ' ' + message + '\n' )
		else :
			self.innerDisplayStandard( self.normalPrefix + self.offset * ' ' + message )
		
	
	def debug( self, message, addReturn = true  ) :
		"""Displays debug message if and only if we are in debug mode."""
		
		if self.do_debug :
			if addReturn :
				self.innerDisplayStandard( self.debugPrefix + message + '\n' )
			else :
				self.innerDisplayStandard( self.debugPrefix + message )


	def warning( self, message, addReturn = true  ) :
		"""Displays provided warning message."""
		if addReturn :
			self.innerDisplayError( self.warningPrefix + message + '\n' )
		else :
			self.innerDisplayError( self.warningPrefix + message )
			
			
	def error( self, message, addReturn = true  ) :
		"""Displays provided error message."""
		if addReturn :
			self.innerDisplayError( self.errorPrefix + message + '\n' )
		else :
			self.innerDisplayError( self.errorPrefix + message )
	
	
	def innerDisplayStandard( self, message ) : 
		"""
		Pure virtual function, to be redefined by implementation classes.
		It should have been private, if it could be overridden.
		"""
		pass
		
		
	def indent( self ) :
		"""Indents one more level for normal messages."""
		self.offset += self.offsetIncrement

		
	def desindent( self ) :
		"""Indents one fewer level for normal messages."""
		self.offset -= self.offsetIncrement
		if self.offset < 0 :
			self.offset = 0	
			
					
	def blankLine( self ) :
		if self.verb == 2 :
			self.innerDisplayStandard( '\n' )
			
		
	def status( self ) :
		self.innerDisplayStandard( 'Verbosity level is %s.' % ( self.verb, ) )
						
						
	def __call__( self, message, addReturn = true ) :
		if type( message ) == types.StringType : 
			if self.verb == 2 or ( ( self.verb == 1 ) and ( message[:2] == self.prefixForKeyMessages ) ) :
				self.display( self.esp( self.trunc( self.comp( message ) ) ), addReturn )
		elif type( message ) in [ types.ListType, types.TupleType ] :
			for item in message :
				self.__call__( item, addReturn )
		else:
			print 'Display : unsupported message type, unable to display it.' 		
		
		
	def setVerbosity( self, newVerbosity = 2 ):
		self.verb = newVerbosity



class ScreenDisplay( Display ) :	
	"""
	This is the Display implementation that uses screen as display output device.
	"""
	
	def __init__( self, spacing = 10, compression = true, truncate = false, verbosity = 2 ) :
		"""Propagates back setting to ancestor class' constructor."""
		Display.__init__( self, spacing, compression, truncate, verbosity )

		
	def innerDisplayStandard( self, message ) :
		"""Displays a message to standard output file descriptor."""
		sys.stdout.write( message )
        sys.stdout.flush()


	def innerDisplayError( self, message ) :
		"""Displays a message to error output file descriptor."""
		sys.stderr.write( message )
        sys.stderr.flush()
	
	
	
class FileDisplay ( Display ) :
	"""This is the Display implementation that uses files as display output device."""

	defaultLogBaseName   = "Log"
	defaultExtension 	 = "txt"


	def __init__( self, logBaseName = defaultLogBaseName, allowOverwrite = true, spacing = 10, compression = true, truncate = false, verbosity = 2) :
		"""
		Implements the Display interface so that messages are output to a log file.
				- logFilename : defines the file where messages should be stored,
				- allowOverwrite : tells whether a previously existing log file could 
				be overwritten,
				- the other parameters have the same semantics as the Display ones.				
		"""
		
		Display.__init__( self, spacing = 10, compression = true, truncate = false, verbosity = 2 ) 
		self.logBaseName    = logBaseName
		self.allowOverwrite = allowOverwrite
		self.logFilename = self.logBaseName + "." + self.defaultExtension
		if self.allowOverwrite :
			self.logFile = open( self.logFilename, 'w' )
		else:
			self.logFile = open( self.logFilename, 'a' )			
		self.status()


	def __del__( self ) :
		"""Destructor shall release the resources."""
		if self.logFile :
				self.logFile.close()	
			
			
	def innerDisplayStandard( self, message ) :
		self.logFile.write( message + '\n' )
		self.logFile.flush()
			
	def innerDisplayError( self, message ) :
		self.logFile.write( message + '\n' )
		self.logFile.flush()



if __name__ == "__main__":        
	if len( sys.argv ) != 2 :	         
		print __doc__
	else :
		temp = ScreenDisplay()
		temp( sys.argv[ 1 ] )

		
