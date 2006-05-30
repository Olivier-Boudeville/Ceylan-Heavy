#!/usr/bin/env python

__file__        = 'toolbox.py'
__title__       = 'This is the toolbox module, convenient generic functions and classes for python.'
__version__     = '0.1'
__author__      = 'Olivier Boudeville (olivier.boudeville@online.fr)'
__project__     = 'Ceylan'
__creationDate__= '2001, October 12'
__comments__    = 'Set of useful general-purpose python modules.'
__source__      = 'Mark Pilgrim, http://diveintopython.org/, and al.'
__doc__         = __title__ + '\n' + __comments__


# Toolbox's code is being distributed among *Utils.py.


# Home-made ones :
from generalUtils import * 
from fileUtils import *



class ToolboxException( GeneralUtilsException ) :
    """Base class for toolbox exceptions.""" 


if __name__ == "__main__":        
	print __doc__

		
