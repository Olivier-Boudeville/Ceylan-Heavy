#!/usr/bin/env python


_file__         = 'dataUtils.py'
__title__       = 'This is the data module, which contains everything which is linked to data structure and management.'
__version__     = '0.1'
__author__      = 'Olivier Boudeville (olivier.boudeville@online.fr)'
__project__     = 'Ceylan'
__creationDate__= '2004, August 6'
__comments__    = ""
__source__      = 'None'
__doc__         = __title__ + '\n' + __comments__



# Import standard python modules :
import sys, types, string

# Import home-made modules :
import generalUtils


class DataUtilsException( generalUtils.GeneralUtilsException ) :
    """Base class for generalUtils exceptions.""" 



class Node:
	"""
	Nodes are trees : they can contain other nodes.
	Each node can carry a content, whose type is NodeContent.
	"""
	
	
	def __init__( self, newContent = None ) :
		"""Creates an empty node with no child node."""
		self.content  = newContent
		self.children = []	

	
	def __cmp__(self, other):
		return self.content != other.content

	
	def __repr__( self ):
		"""Returns a textual representation of this node's state."""
		res = "Node has "
		if self.content:
			res += "content (%s)" % ( self.content, )
		else:
			res += "no content"
		res += " and it has "	
		if self.children:
			res += "%s children." % ( len( self.children ), )
		else:
			res += "no child."
			
		return res
	
			
	def toString( self, offset = 0, nextOffset = 0, isFirstChild = True ):
		"""
		Returns a stringified description of the tree.
	
		__ a __ b
			  |_ c __ d __ e
						|_ f
			  |_ g
		
		offset is the current position where to write
		nexOffset is the position where children should begin 	
		""" 
			
			
		res= ""
		
		# The two branches must have the same length :
		#branchFirst = 'x__x'
		#branchOther = 'y|_y'
		branchFirst = ' __ '
		branchOther = ' |_ '
		branchEnd   = ''	
					
		node_text = string.ljust( '%s' % (self.content,), nextOffset - offset + 1 )
					 
		if isFirstChild:
			res += branchFirst + node_text
		else:
			#res =  offset  * 'z' + branchOther + node_text
			res =  offset  * ' ' + branchOther + node_text
			
		if len( self.children ):
			newOffset = offset + len( branchFirst ) + len(node_text)
			
			# Compute max child content total length :
			extraLength = 0
			for child in self.children:
				child_size = len( child.content )
				if child_size > extraLength:
					extraLength = child_size
			newNextOffset = offset + len( branchFirst ) +	extraLength
			res +=  self.children[0].toString( newOffset, newNextOffset, True )
			for c in self.children[1:]:
				res += '\n' + c.toString( newOffset, newNextOffset, False )			
				res += branchEnd
		return res
				
		
	def addChild( self, aChild ):
		"""Adds a child to current node."""	
		self.children.append( aChild )

	
	def removeChild( self, child ):
		"""Removes specified child, raises an exception if child not found."""
		self.children.remove( child )

							
	def removeAllChildren( self ):
		self.children = None	

		
	def getChildren( self ):
		"""Returns this node's children."""
		return self.children
		
		
	def setContent( self, nodeContent ):
		"""Sets a new content to current node, which must not have already a content."""	
		if self.content:
			raise ValueError, "Node::setContent : content already assigned."
		self.content = nodeContent
		
		
	def getContent( self ):
		"""Returns this node's current content."""	
		return self.content
				
				
	def dropContent( self ):
		"""Removes this node's content."""	
		self.content = None


	def searchChildren( self, content ):
		"""Searches through node's children the first, if any, that has specified content."""
		for c in self.children:
			if c.content == content:
				return c
		return None		
		
		
	def listDepthFirst( self ):
		"""
		Walks the tree depth-first, returns the list of encountered nodes.
		"""
		res = [ self ]
		for c in self.children:
			res += c.listDepthFirst()
		return res	
		#return [ self ] + c.listDepthFirst() for c in self.children ]
		
		
	def listByHeight( self, first = True ):
		"""
		Walks the tree height by height, starting from root node, and returns the list of 
		encountered nodes.
		"""
		res = []
		if first:
			res = [ self ]
		res += self.children
		for c in self.children:
			res += c.listByHeight( False )
		return res
		
		
	def searchContent( self, content ):
		"""
		Searches through internal content and then recursively through children for 
		specified content.Returns the first node found having the content, if any.
		Otherwise, returns None.
		content -> list of nodes
		"""
		#outputDevice.debug( "Comparing target (<%s>) with content <%s>." % ( content, self.content ) )
		if content == self.content:
			return self
		else:
			for c in self.children:
				res = c.searchContent( content )
				if res:
					return res
		return None

		
	def searchPathToContent( self, content ):
		"""
		Returns, if possible, the path from the first found node whose 
		content matches specified content to root node.
		"""
		if content == self.content:
			#outputDevice.debug( "Content found for <%s>." % ( self,) )
			return [ self ]
		else:
			#outputDevice.debug( "Recursing from <%s>." % (self,)  )
			for child in self.children:
				res = child.searchPathToContent( content )
				if res:
					res.append( self )
					#outputDevice.debug( "Returning <%s>." % (res,) )
					return res
		return None		


	def display( self ) :
		print 'content  = %s' % ( self.content,)
		print 'children = %s' % ( self.children,)
			
					
class NodeExample( Node ):

	def __init__( self, name = None ):
		Node.__init__( self )
		self.content = name
				
		
if __name__ == "__main__": 
       
	import startup  
	#outputDevice = generalUtils.ScreenDisplay() 
	print __doc__
	
	a=NodeExample( 'a' )
	b=NodeExample( 'b' )
	c=NodeExample( 'c' )
	d=NodeExample( 'd' )
	e=NodeExample( 'e' )
	f=NodeExample( 'f' )
	g=NodeExample( 'g' )
	
	#__ a __ b
	#      |_ c __ d __ e
	#                |_ f
	#      |_ g


		
	a.addChild( b )
	a.addChild( c )
	c.addChild( d )
	d.addChild( e )
	d.addChild( f )
	a.addChild( g )

	print a.toString()
	
	u = a.searchContent( 'd' )
	print 'Searching for content d : %s' % ( u, )
	print
	
	path = a.searchPathToContent( 'd' )
	print 'Searching path from content d to root : '
	generalUtils.displayList( path )
	print
	

	l = a.listDepthFirst()
	print 'Listing depth-first tree :'
	generalUtils.displayList( l )
	print '(size of list : %s)' % ( len(l),)
	print

	m = a.listByHeight()	
	print 'Listing height by height tree, starting from root node :'
	generalUtils.displayList( m )
	print '(size of list : %s)' % ( len(m),)
	print
	
