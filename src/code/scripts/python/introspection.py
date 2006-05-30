#!/usr/bin/env python

__file__        = "introspection.py"
__title__       = "This is the introspection module, made to retrieve easily informations about python objects."
__version__     = "0.2"
__author__      = "Olivier Boudeville (olivier.boudeville@online.fr)"
__project__     = "Ceylan"
__creationDate__= "2001, August 23"
__comments__    = "Use its own introspection capabilities to discover what this module can do !"
__source__      = "Mark Pilgrim, http://diveintopython.org/, and al."
__doc__         = __title__ + '\n' + __comments__


# Original functions translated from Python 2.1 to Python 1.5.2 and, eventually, back in 2.1 !

import sys, string, inspect, toolbox


def showDict(myDict, spacing=10, collapse=1):
    """Affiche un dictionnaire, en plaçant sur la même ligne une clé et l'objet qui lui correspond"""
    #"""Display a dictionnary in a user-friendly fashion, each line showing a key and its related object"""
    tempList = []

    for item in myDict.keys():
		tempList.append("%s %s" % (string.ljust(item,spacing), myDict[item]))
    print string.join(tempList, '\n')

    
def showMethods(object, spacing=15, collapse=1):   
    """Affiche les méthodes de l'objet spécifié et leur documentation __doc__"""
    #"""Print methods and doc strings of object"""
    
    methodList = []
    for method in dir(object):
    	if callable(getattr(object, method)):
    		methodList.append(method)    	
    processFunc = collapse and (lambda s: string.join(s.split(), ' ')) or (lambda s: s)
    tempList = []
    for method in methodList:
        tempList.append("%s %s" % (string.ljust(method,spacing),str(getattr(object, method).__doc__)))        
    print string.join(tempList, '\n')
    

    
def showDataMembers(object, spacing=10, collapse=1):   
    """Affiche les données membres de l'objet spécifié et leur type"""
    #"""Print data members of object, with their type"""
    
    dataList = []
    for member in dir(object):
    	if not callable(getattr(object, member)):
    		dataList.append(member)    	
    processFunc = collapse and (lambda s: string.join(s.split(), ' ')) or (lambda s: s)
    tempList = []
    for member in dataList:
        tempList.append("%s %s %s" % (string.ljust(member,spacing), type(member), str(member)))  
    if len(tempList)==0:
		print "No data attributes"
    else:
		print string.join(tempList, '\n')
    
    
def showLoadedModules(spacing=10, collapse=1):   
    #"""Affiche les modules actuellement chargés dans l'interpréteur"""
    """Print methods and doc strings of object"""
    
    tempList=[]
    for item in sys.modules.keys():
		tempList.append("%s %s" % (string.ljust(item,spacing), sys.modules[item]))
    print string.join(tempList, '\n')


def showObjectSymbolTable(myObject):
    #"""Affiche la table des symboles locaux de l'objet spécifié"""
    """Display local object symbol table """
    showDict(vars(myObject))        


def showCurrentLocalSymbolTable(spacing=10, collapse=1):
    #"""Affiche la table des symboles locaux, avec leur type"""
    """Show current local symbol table, with their type"""
    tempList=[]
    
    for item in dir():
		tempList.append("%s %s" % (string.ljust(item,spacing), type(item)))
    print string.join(tempList, '\n')	


def inspectModule(myModule, spacing=10, collapse=1):
	"""Affiche des informations sur un module"""
	print "Description du module : ", myModule.__doc__
	if myModule.__file__ is not None:
		print "Défini dans le fichier :", myModule.__file__
	tempList = []
	print "Membres du modules : "
	processFunc = collapse and (lambda s: string.join(s.split(), ' ')) or (lambda s: s)
	print "\n".join(["%s %s" % (string.ljust(item[0],spacing), processFunc(str(item[1]))) for item in inspect.getmembers(myModule)])
	    	
def inspectClass(myClass, spacing=10, collapse=1):
	"""Affiche des informations sur une classe"""
	print "Description de la classe : ", myClass.__doc__
	print "Définie dans le module : ", myClass.__module__
	
def inspectMethod(myMethod, spacing=15, collapse=1):
	"""Affiche des informations sur une méthode"""
	print "Nom : ", myMethod.__name__
	print "Description de la méthode : ", myMethod.__doc__
	print "Appartient à la classe : ", myMethod.im_class
	print "Objet-fonction contenant cette méthode : ", myMethod.im_func
	if myMethod.im_self is not None:
		print "Liée à l'instance : ", im_self
	
def inspectFunction(myFunc, spacing=10, collapse=1):
	"""Affiche des informations sur une fonction"""
	print "Nom : ", myFunc.__name__	
	print "Description de la fonction : ", myFunc.__doc__
	print "Arguments par défaut : ", myFunc.func_defaults
	print "Espace de nommage global dans lequel cette fonction est définie : ", myfunc.func_globals


    
if __name__ == "__main__":                 
   print __doc__
