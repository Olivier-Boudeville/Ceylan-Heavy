#!/usr/bin/env python

__file__        = 'test-general.py'
__title__       = 'This is the test of the general module.'
__version__     = '0.1'
__author__      = 'Olivier Boudeville (olivier.boudeville@online.fr)'
__project__     = 'Ceylan'
__creationDate__= '2004, January 31'
__comments__    = 'Testing module.'
__source__      = 'OSDL (http://osdl.sourceforge.net)'
__doc__         = __title__ + '\n' + __comments__

__testTarget__  = 'generalUtils'


from generalUtils import *


print 'Beginning test of module %s.\n\n' % ( __testTarget__, )


print 'Testing basic definitions...'
print

print 'true is ', true

if true :
	print 'general.true is logically true.'
else :
	print 'general.true is logically false.'
	
print 'false is ', false

if false :
	print 'general.false is logically true.'
else :
	print 'general.false is logically false.'
	
print	
print '...done\n'




print 'Testing ScreenDisplay...'

myScreenDisplay = ScreenDisplay()

myScreenDisplay.blankLine()
print '  + testing channels'

myScreenDisplay( 'Hello, screen world !' )
myScreenDisplay( 'Let the sun shine !' )
myScreenDisplay.error( 'I made a mistake.' )
myScreenDisplay.debug( 'I shall debug.' )
myScreenDisplay.warning( 'I shall warn my users.' )

myScreenDisplay.blankLine()
print '  + testing indentation'

myScreenDisplay.indent()
myScreenDisplay( 'Let the sun shine ! (bis)' )
myScreenDisplay( 'Let the sun shine ! (ter)' )
myScreenDisplay.indent()
myScreenDisplay( 'Let the sun shine ! (quattro ?)' )
myScreenDisplay.desindent()
myScreenDisplay.desindent()
myScreenDisplay.desindent()
myScreenDisplay( 'Let the sun shine ! (cinco ?)' )
myScreenDisplay.error( 'I made a mistake.' )

myScreenDisplay.blankLine()
print '  + testing string formatting'

myScreenDisplay.blankLine()
print '      * testing string truncating'

myTruncateScreenDisplay = ScreenDisplay( truncate = 16 )
myTruncateScreenDisplay( 'I am afraid I will be in some way truncated, my friend.' )

myScreenDisplay.blankLine()
print '      * testing string spacing'

mySpacedScreenDisplay = ScreenDisplay( spacing = 15, truncate = 40 )
mySpacedScreenDisplay( 'I', addReturn = false )
mySpacedScreenDisplay( 'am', addReturn = false )
mySpacedScreenDisplay( 'big', addReturn = true )

myScreenDisplay.blankLine()
print '      * testing string compressing'

myCompressedScreenDisplay = ScreenDisplay( compression = true )
myCompressedScreenDisplay( "I \nsuspect \nI \nwon't \nbe \nmultilined \nfor \nlong."  )

myScreenDisplay.blankLine()
print '...done\n'

myScreenDisplay.blankLine()
print '  + testing verbosity management'

myScreenDisplay( 'I am a normal message and should be displayed.' )
myScreenDisplay( Display.prefixForKeyMessages + 'I am an important message and should be displayed.' )

myScreenDisplay.setVerbosity( 1 )
myScreenDisplay( 'I am a normal message and should not be displayed.' )
myScreenDisplay( Display.prefixForKeyMessages + 'I am an important message and should be displayed.' )

myScreenDisplay.setVerbosity( 0 )
myScreenDisplay( 'I am a normal message and should not be displayed.' )
myScreenDisplay( Display.prefixForKeyMessages + 'I am an important message but should not be displayed.' )
myScreenDisplay.blankLine()

print 'Testing FileDisplay...'

myFileDisplay = FileDisplay()

myFileDisplay( 'Hello, file world !' )
myFileDisplay( 'This is a silly message indeed.' )
myFileDisplay.error( 'I made a mistake.' )
myFileDisplay.debug( 'I shall debug.' )
myFileDisplay.warning( 'I shall warn my users.' )

print '...done\n'


print 'End of test for module %s.\n\n' % ( __testTarget__, )


