/* 
 * Copyright (C) 2003-2013 Olivier Boudeville
 *
 * This file is part of the Ceylan library.
 *
 * The Ceylan library is free software: you can redistribute it and/or modify
 * it under the terms of either the GNU Lesser General Public License or
 * the GNU General Public License, as they are published by the Free Software
 * Foundation, either version 3 of these Licenses, or (at your option) 
 * any later version.
 *
 * The Ceylan library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License and the GNU General Public License
 * for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License and the GNU General Public License along with the Ceylan library.
 * If not, see <http://www.gnu.org/licenses/>.
 *
 * Author: Olivier Boudeville (olivier (dot) boudeville (at) esperide (dot) com)
 *
 */


#ifndef CEYLAN_TREE_H_
#define CEYLAN_TREE_H_


#include "CeylanVisitable.h"        // for inheritance
#include "CeylanVisitor.h"          // for inheritance
#include "CeylanException.h"        // for Ceylan::Exception
#include "CeylanLogPlug.h"          // for LogPlug
#include "CeylanStringUtils.h"      // for formatStringList



#include <string>
#include <list>                     // for tree sons



namespace Ceylan
{



	/**
	 * Exception to be raised when a Tree operation failed.
	 *
	 * @note Cannot be defined in-place, as it would not be
	 * compiled in the library without a .cc file.
	 *
	 */
	class CEYLAN_DLL TreeException : public Ceylan::Exception
	{
	
		public:
		
			explicit TreeException( const std::string & reason ) ;

			virtual ~TreeException() throw() ;
	
	} ;



	// Forward-declaration.
	template <typename Content>	class Tree ;



	/**
	 * Tree-dedicated visitor.
	 *
	 */
	template <typename Content>
	class /* CEYLAN_DLL */ TreeVisitor : public Ceylan::Visitor
	{
	
		public:
		
			explicit TreeVisitor() ;

			virtual ~TreeVisitor() throw() ;


			/// Action to perform when visiting a tree node.
			virtual void visit( Tree<Content> & tree ) = 0 ;


			/// Action to perform when visiting the content of a tree node.
			virtual void visit( Content & content ) = 0 ;

	} ;



	template <typename Content>
	TreeVisitor<Content>::TreeVisitor()
	{

	}
	

	template <typename Content>
	TreeVisitor<Content>::~TreeVisitor() throw()
	{

	}



	/// The height of a node, measured from its root.
	typedef Ceylan::Uint32 Height ;



	/**
	 * Tree-dedicated visitor, which may use the distance from the current
	 * node to the real root of the tree.
	 *
	 */
	template <typename Content>
	class /* CEYLAN_DLL */ TreeHeightAwareVisitor : public Ceylan::Visitor
	{
	
	
		public:
		
			
			
			/**
			 * Creates a tree visitor that knows the height of the visited
			 * node.
			 *
			 */
			explicit TreeHeightAwareVisitor() ;


			/// Virtual destructor.
			virtual ~TreeHeightAwareVisitor() throw() ;



			/**
			 * Returns current height.
			 *
			 * @note A default do-nothing implementation is provided here,
			 * it is made to be overriden by the actual algorithm, which may
			 * use a single variable, or a stack, etc.
			 *
			 */
			virtual Height getHeight() const ;



			/**
			 * Increments current height.
			 *
			 * @note A default do-nothing implementation is provided here,
			 * it is made to be overriden by the actual algorithm, which may
			 * use a single variable, or a stack, etc.
			 *
			 */
			virtual void incrementHeight() ;
			
			
			
			/**
			 * Decrements current height.
			 *
			 * @note A default do-nothing implementation is provided here,
			 * it is made to be overriden by the actual algorithm, which may
			 * use a single variable, or a stack, etc.
			 *
			 */
			virtual void decrementHeight() ;
			
			
			
			/** 
			 * Action to perform when visiting a tree node.
			 *
			 * @param tree the tree node to visit.
			 *
			 * @note This declaration is commented out, so that multiple 
			 * Content child classes can be visited: otherwise their visit
			 * method would clash with this one, and hide it.
			 *
			 *
			 
			virtual void visit( Tree<Content> & tree ) = 0 ;

			 */



			/**
			 * Action to perform when visiting the content of a tree node.
			 *
			 * @note This declaration is commented out, so that multiple 
			 * Content child classes can be visited: otherwise their visit
			 * method would clash with this one, and hide it.
			 *
			 * 
			 
			virtual void visit( Content & content ) = 0 ;
			
			 */
			 
			 
	} ;
	


	template <typename Content>
	TreeHeightAwareVisitor<Content>::TreeHeightAwareVisitor()
	{

	}



	template <typename Content>
	TreeHeightAwareVisitor<Content>::~TreeHeightAwareVisitor() throw()
	{

	}



	template <typename Content>
	Height TreeHeightAwareVisitor<Content>::getHeight() const
	{
	
		// Override me!
		return 0 ;
	}



	template <typename Content>
	void TreeHeightAwareVisitor<Content>::incrementHeight()
	{
	
		// Override me!

	}



	template <typename Content>
	void TreeHeightAwareVisitor<Content>::decrementHeight()
	{
	
		// Override me!

	}




	/**
	 * Template defining generically trees, parametrized by a
	 * content datatype (typename Content): each tree node will
	 * contain its instance of Content, for example this templated tree
	 * may instanciated with Content == std::string.
	 *
	 * @example For a file hierarchy structure, each tree node would
	 * be a directory, and its content would be a list of its file
	 * entries.
	 *
	 * @note The typename X must be a child class of TextDisplayable,
	 * so that we can call its toString method.
	 *
	 */
	template <typename Content>
	class /* CEYLAN_DLL */ Tree : public Ceylan::Visitable
	{


		public:


			/// Shortcut for the list of sons of a Tree.
			typedef std::list< Tree<Content>* > SubTreeList ;



			// Life-cycle section.


			/**
			 * Constructs a tree made only from a node root, 
			 * with an empty local content.
			 *
			 */
			explicit Tree() ;



			/**
			 * Constructs a tree made only from a node root, 
			 * whose local content is provided.
			 *
			 * @param content the local content of this node.
			 *
			 * @note The tree takes ownership of the content, hence
			 * will deallocate it when itself deallocated.
			 *
			 */
			explicit Tree( Content & content ) ;

			
			
			/**
			 * Virtual destructor, deletes all tree, including the subtrees
			 * and their content.
			 *
			 */
			virtual ~Tree() throw() ;



			
			// Visitor section.
			

			/**
			 * Allows given visitor to visit this object, thanks to a 
			 * callback: 'visitor.visit( *this ) ;'
			 *
			 * Implements the Visitable interface.
			 *
			 * @throw VisitException if the operation failed, including if
			 * the specified visitor is no a tree height-aware visitor.
			 *
			 */
			virtual void accept( Visitor & visitor ) ;




			// Content management section.


			/**
			 * Tells whether this tree node (i.e. the root of this tree) has
			 * a content.
			 *
			 * @return true iff this node owns a content instance.
			 *
			 */
			virtual bool hasContent() const ;
			
			
			 
			/**
			 * Returns the content that this root node of the tree owns.
			 *
			 * @throw TreeException if no content is registered in this
			 * node.
			 *
			 * @see hasContent
			 *
			 */ 
			virtual Content & getContent() ;  
			 
			 
			 
			/**
			 * Returns the content that this root node of the tree owns, as
			 * a const reference
			 *
			 * @throw TreeException if no content is registered in this
			 * node.
			 *
			 * @see hasContent
			 *
			 */ 
			virtual const Content & getContentAsConst() const ;  
			 
			 
			 
			/**
			 * Sets a new content for this root node of the tree.
			 *
			 * @note The node takes ownership of the content, and any 
			 * previously owned content is deallocated first.
			 *
			 * @throw TreeException if the operation failed.
			 *
			 * @see hasContent
			 *
			 */ 
			virtual void setContent( Content & newContent ) ;  
			 
			 
			 
			
			// Tree-based navigation section.
			

			/**
			 * Returns the list of sons (subtrees) of this tree.
			 *
			 */
			virtual const SubTreeList & getSons() const ;



			/**
			 * Returns the father of the specified node, which is searched in
			 * specified tree.
			 *
			 * @param child the node whose father is searched in this tree.
			 *
			 * @return the father tree, if found, otherwise a null pointer.
			 *
			 */
			virtual Tree * getFather( Tree & child ) ;



			/**
			 * Adds specified son to the list of sons of this tree.
			 *
			 * @note A subtree cannot be registered more than once.
			 *
			 * @throw TreeException if the operation failed, included
			 * if the subtree was already registered.
			 *
			 * @note This tree takes ownership of the specified subtree.
			 *
			 */
			virtual void addSon( Tree & subtree ) ;



			/**
			 * Traverses this tree depth-first, and applies specified
			 * processing to each node being selected on this path,
			 * in the path order.
			 *
			 * @param treeVisitor the actual tree visitor that will
			 * visit the tree.
			 *
			 * @param visitContent tells whether the visitor the tree
			 * nodes and their content, if true, or only the tree nodes,
			 * if false.
			 *
			 * @note Content, if visited, will be visited just after its
			 * associated node is visited. If a given node has no content,
			 * then the visitor will only be called for the node, even if
			 * visitContent is true.
			 *
			 * @throw TreeException if the visit failed.
			 *
			 */
			virtual void traverseDepthFirst( TreeVisitor<Content> & treeVisitor,
				bool visitContent = true ) ;



			/**
			 * Traverses this tree breadth-first, and applies specified
			 * processing to each node being selected on this path,
			 * in the path order.
			 *
			 * @param treeVisitor the actual tree visitor that will
			 * visit the tree.
			 *
			 * @param visitContent tells whether the visitor the tree
			 * nodes and their content, if true, or only the tree nodes,
			 * if false.
			 *
			 * @note Content, if visited, will be visited just after its
			 * associated node is visited. If a given node has no content,
			 * then the visitor will only be called for the node, even if
			 * visitContent is true.
			 *
			 * @throw TreeException if the visit failed.
			 *
			 */
			virtual void traverseBreadthFirst( 
				TreeVisitor<Content> & treeVisitor,
				bool visitContent = true ) ;



			/**
			 * Adds in specified list the content of all direct child nodes.
			 *
			 * @param toBeAugmented the list to which contents will be added.
			 *
			 */
			virtual void appendSonsContents( 
				std::list<Content *> & toBeAugmented ) ;
				



			// Content-based navigation section.
			
			
			/**
			 * Returns the tree node that references specified content, if
			 * any.
			 *
			 * @param content the content associated with the searched node.
			 *
			 * @return the (supposed) only one node that is associated with
			 * specified content, or a null pointer if no corresponding node
			 * was found.
			 *
			 */
			virtual Tree * getNodeOf( const Content & content ) ;
			
			
			
			/**
			 * Returns the content associated with the father node of the node
			 * associated with specified content. 
			 *
			 * The ownership of any returned content is kept by the tree.
			 *
			 * @return A pointer to the father content, if any, otherwise a 
			 * null pointer if this node has no father (root node) or if it
			 * has a father but it is not associated with any content.
			 *
			 * @note This is an expensive method, as it may traverse the full
			 * tree: this kind of tree has nodes that are not required to
			 * keep track of their father. Hence needing to use this method
			 * might be the sign of a poor design.
			 *
			 * This search could be provided by a dedicated visitor, but it
			 * would result in a rather complex visitor with no real added
			 * value.
			 *
			 */
			virtual Content * getFatherContent( const Content & content ) ;
				


			/**
			 * Adds in specified list the content of all direct child nodes.
			 *
			 * @param content the content whose sons will be searched for
			 * content to append.
			 *
			 * @param toBeAugmented the list to which contents will be added.
			 *
			 */
			virtual void appendSonsContentsOf( const Content & content,
				std::list<Content *> & toBeAugmented ) ;
				
				

			/**
			 * Returns a user-friendly description of the state of this object.
			 *
			 * @param level the requested verbosity level.
			 *
			 * @note Text output format is determined from overall settings.
			 *
			 * @see TextDisplayable
			 *
			 */
			 virtual const std::string toString( 
			 	Ceylan::VerbosityLevels level = Ceylan::high ) const ;





		protected:



			/**
			 * The content carried by this tree node.
			 *
			 */
			Content * _content ;


/* 
 * Takes care of the awful issue of Windows DLL with templates.
 *
 * @see Ceylan's developer guide and README-build-for-windows.txt 
 * to understand it, and to be aware of the associated risks. 
 * 
 */
#pragma warning( push )
#pragma warning( disable : 4251 )

			/**
			 * The sons, direct subtrees of this tree.
			 *
			 */
			SubTreeList _sons ;

#pragma warning( pop ) 			




		private:



			/**
			 * Copy constructor made private to ensure that it will never be 
			 * called.
			 * The compiler should complain whenever this undefined constructor
			 * is called, implicitly or not.
			 * 
			 * @note Made to avoid unwanted hidden clone of the Singleton.
			 *
			 */			 
			Tree( const Tree & source ) ;
			
			
			/**
			 * Assignment operator made private to ensure that it will never be 
			 * called.
			 * The compiler should complain whenever this undefined operator 
			 * is called, implicitly or not.
			 * 
			 */			 
			Tree & operator = ( const Tree & source ) ;


	} ;




// Public section: implementation.


	template <typename Content>
	Tree<Content>::Tree() :
		_content( 0 ),
		_sons()
	{
	
	}	



	template <typename Content>
	Tree<Content>::Tree( Content & content ) :
		_content( &content ),
		_sons()
	{
	
	}	

			
				
	template <typename Content>
	Tree<Content>::~Tree() throw()
	{

		if ( _content != 0 )
			delete _content ;

		for ( typename SubTreeList::iterator it = _sons.begin(); 
			it != _sons.end(); it++ )
		{
			delete (*it) ;
		}

		// Useless but maybe safer for debugging:
		_sons.clear() ;
	
	}
	


	template <typename Content>
	const std::list< Tree<Content>* > & Tree<Content>::getSons() const
	{
	
		return _sons ;
		
	}
	


	template <typename Content>
	Tree<Content> * Tree<Content>::getFather( Tree<Content> & child ) 
	{
	
		/*
		 * Algorithm: 
		 *  - traverse the tree from the root
		 *  - from a given node, look at the sons
		 *  - if specified child is found, return it
		 *	- else recurse
		 *
		 */
		
		for ( typename SubTreeList::iterator it = _sons.begin();
				it != _sons.end(); it++ )
			if ( *it == & child )
				return this ;
		
		
		// Recurse if next level had not that content:
		for ( typename SubTreeList::iterator it = _sons.begin();
			it != _sons.end(); it++ )
		{		
			Tree * returned = (*it)->getFather( child ) ;
			if ( returned != 0 )
				return returned ;
		}  
		
		return 0 ;
		  
	}
	
	
	
	template <typename Content>
	void Tree<Content>::accept( Visitor & visitor ) 	
	{
		
		//Ceylan::Log::LogPlug::trace( "Tree<Content>::accept" ) ;
		
		TreeHeightAwareVisitor<Content> * actualVisitor = 
			dynamic_cast<TreeHeightAwareVisitor<Content> *>( &visitor ) ;
			
		if ( actualVisitor == 0 )
			throw VisitException( "Tree<Content>::accept failed: "
				"the visitor (" + visitor.toString() + ") is not a "
				"TreeHeightAwareVisitor." ) ;
				
				
		if ( _content != 0 )
			_content->accept( *actualVisitor ) ;
					
		/*
		 * When the height-aware visitor will visit a content that may
		 * enclose other contents (ex: for XML, a XML markup), it will
		 * call its incrementHeight method, which may push an information
		 * that identifies this content, so that the next call to
		 * decrementHeight will pop it and handle the closing (ex: add
		 * the closing XML markup).
		 *
		 * That same information can be pushed on a stack directly when the
		 * content is accepted (a few lines before), if what is pushed 
		 * depends on the content.
		 *
		 * As it can only by popped later at the decrementHeight call, which
		 * does not depend on the content and thus will pop it in all cases,
		 * in the cases where nothing special is to be saved when height is
		 * decremented, a blank element (ex: string) could be pushed on
		 * that stack and ignored later when popped.
		 *
		 * This way each content can be visited only once, and with no special
		 * behaviour towards the visitor, such as returning a special value.
		 *
		 * Note that a stack is useful as soon as we are in such recursive
		 * patterns, as nested calls should not prevent from:
		 *   - closing back what has be opened when going deeper in the tree, 
		 * notably if the closing depends on the opened element 
		 *	 - keeping track of the height, as some algorithm depends not only
		 * on the last level, but also needs to global level they are in
		 *
		 * @see XMLVisitor for such cases.
		 *
		 */
		 
		actualVisitor->incrementHeight() ;

		// Then recurse:
		for ( typename SubTreeList::iterator it = _sons.begin();
				it != _sons.end(); it++ )
			(*it)->accept( *actualVisitor ) ;
		
		actualVisitor->decrementHeight() ;
			
	}
	
	
	
	template <typename Content>
	bool Tree<Content>::hasContent() const
	{
	
		return ( _content != 0 ) ;
	
	}
			
		
			 
	template <typename Content>
	Content & Tree<Content>::getContent() 
	{
	
		if ( _content == 0 )
			throw TreeException( "Tree<Content>::getContent: "
				"no available content to return." ) ;
		
		return *_content ;		
	
	}
			 
	
		
	template <typename Content>
	const Content & Tree<Content>::getContentAsConst() const
	{
	
		if ( _content == 0 )
			throw TreeException( "Tree<Content>::getContentAsConst: "
				"no available content to return." ) ;
		
		return *_content ;		
	
	}
 		
	
			 
	template <typename Content>
	void Tree<Content>::setContent( Content & newContent ) 	
	{
	
		if ( _content != 0 )
			delete _content ;
	
		_content = & newContent ;
		
	}
	
	
	
	template <typename Content>
	void Tree<Content>::addSon( Tree<Content> & subtree ) 	
	{
		
		for ( typename SubTreeList::const_iterator it = _sons.begin();
				it != _sons.end(); it++ )
			if ( *it == &subtree )
				throw Ceylan::TreeException( "Tree<Content>::addSon: "
					"following son was already registered: "
					+ subtree.toString() ) ;

		_sons.push_back( &subtree ) ;

	}



	template <typename Content>
	void Tree<Content>::traverseDepthFirst( TreeVisitor<Content> & treeVisitor,
		bool visitContent ) 
	{
	
		// First recurse:
		for ( typename SubTreeList::iterator it = _sons.begin();
				it != _sons.end(); it++ )
			(*it)->traverseDepthFirst( treeVisitor, visitContent ) ;

		// Then apply:
		treeVisitor.visit( *this ) ;

		if ( visitContent && _content != 0 )
			treeVisitor.visit( *_content ) ;

	}



	template <typename Content>
	void Tree<Content>::traverseBreadthFirst( 
			TreeVisitor<Content> & treeVisitor,	bool visitContent ) 	
	{
	
		// Apply:
		treeVisitor.visit( *this ) ;

		if ( visitContent && _content != 0 )
			treeVisitor.visit( *_content ) ;

		// Then recurse:
		for ( typename SubTreeList::iterator it = _sons.begin();
				it != _sons.end(); it++ )
			(*it)->traverseBreadthFirst( treeVisitor, visitContent ) ;

	}



	template <typename Content>
	void Tree<Content>::appendSonsContents( 
		std::list<Content *> & toBeAugmented )
	{
	
		for ( typename SubTreeList::iterator it = _sons.begin();
			it != _sons.end(); it++ )
		{
		
			Content * retrieved = (*it)->_content ;
			if ( retrieved != 0 )
				toBeAugmented.push_back( retrieved ) ;
		
		}		
		
	}



	template <typename Content>
	Tree<Content> * Tree<Content>::getNodeOf( const Content & content )
	{
	
		// Found at this level?
		if ( this->_content == & content )
			return this ;
		
		// Otherwise recurse to next level:
		for ( typename SubTreeList::iterator it = _sons.begin();
			it != _sons.end(); it++ )
		{		
			Tree<Content> * returned = (*it)->getNodeOf( content ) ;
			if ( returned != 0 )
				return returned ;
		}  
		
		return 0 ;
			
	}
	
	
	
	template <typename Content>
	Content * Tree<Content>::getFatherContent( const Content & content )	
	{
	
		/*
		 * Algorithm: 
		 *  - traverse the tree from the root
		 *  - from a given node, look at the sons
		 *  - if specified content is found, return the current content
		 *	- else recurse
		 *
		 */
		
		for ( typename SubTreeList::iterator it = _sons.begin();
				it != _sons.end(); it++ )
			if ( (*it)->_content == & content )
				return /* might be null */ this->_content ;
		
		
		// Recurse if next level had not that content:
		for ( typename SubTreeList::iterator it = _sons.begin();
				it != _sons.end(); it++ )
		{		
			Content * returned = (*it)->getFatherContent( content ) ;
			if ( returned != 0 )
				return returned ;
		}  
		
		return 0 ;
		  
	}
	
	
	
	template <typename Content>
	void Tree<Content>::appendSonsContentsOf( const Content & content,
		std::list<Content *> & toBeAugmented )
	{
	
		Tree<Content> * father = this->getNodeOf( content ) ;
		
		// Associated node found?
		if ( father == 0 )
			return ;
			
		// Yes, let's return its son contents:
		father->appendSonsContents( toBeAugmented ) ;
			
	}	
	
			
			
	template <typename Content>
	const std::string Tree<Content>::toString( Ceylan::VerbosityLevels level )
		const
	{

		std::string res = "Tree " ;

		if ( _content !=0 )
			res += "whose local content is: '"
				+ _content->toString( level ) + "'" ;
		else
			res += "with no local content" ;

		if ( _sons.empty() )
			return res + ". No registred subtree" ;

		res += ". Following subtrees are registered: " ;

		std::list<std::string> subtrees ;

		for ( typename SubTreeList::const_iterator it =
			_sons.begin(); it != _sons.end(); it++ )
		{
			subtrees.push_back( (*it)->toString( level ) ) ;
		}

		return res + Ceylan::formatStringList( subtrees ) ;

	}


}



#endif // CEYLAN_TREE_H_

