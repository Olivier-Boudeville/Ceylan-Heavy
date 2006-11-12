#ifndef CEYLAN_TREE_H_
#define CEYLAN_TREE_H_


#include "CeylanVisitable.h"        // for Visitable
#include "CeylanException.h"        // for Ceylan::Exception
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
		
			explicit TreeException( const std::string & reason ) throw() ;

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
		
			explicit TreeVisitor() throw() ;

			virtual ~TreeVisitor() throw() ;


			/// Action to perform when visiting a tree node.
			virtual void visit( Tree<Content> & tree ) 
				throw( VisitException ) = 0 ;

			/// Action to perform when visiting the content of a tree node.
			virtual void visit( Content & content ) 
				throw( VisitException ) = 0 ;

	
	} ;


	template <typename Content>
	TreeVisitor<Content>::TreeVisitor() throw()
	{

	}

	template <typename Content>
	TreeVisitor<Content>::~TreeVisitor() throw()
	{

	}


	/**
	 * Template defining generically trees, parametrized by a
	 * content datatype (typename Content) : each tree node will
	 * contain its instance of Content, for example this templated tree
	 * may instanciated with Content == std::string.
	 *
	 * @example For a file hierarchy structure, each tree node would
	 * be a directory, and its contet would be a list of its file
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


			/**
			 * Constructs a tree made only from a node root, 
			 * with an empty local content.
			 *
			 */
			Tree() throw() ;


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
			explicit Tree( Content & content ) throw() ;

			
			/**
			 * Virtual destructor, deletes all tree, including the subtrees
			 * and their content.
			 *
			 */
			virtual ~Tree() throw() ;


			/**
			 * Returns the list of sons (subtrees) of this tree.
			 *
			 */
			virtual const SubTreeList & getSons() const throw() ;


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
			virtual void addSon( Tree & subtree ) throw( TreeException ) ;


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
				bool visitContent = true ) throw( TreeException ) ;


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
				bool visitContent = true ) throw( TreeException ) ;


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
			 	Ceylan::VerbosityLevels level = Ceylan::high ) const throw() ;



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
			Tree( const Tree & source ) throw() ;
			
			
			/**
			 * Assignment operator made private to ensure that it will never be 
			 * called.
			 * The compiler should complain whenever this undefined operator 
			 * is called, implicitly or not.
			 * 
			 */			 
			Tree & operator = ( const Tree & source ) throw() ;


	} ;



// Public section : implementation.


	template <typename Content>
	Tree<Content>::Tree() throw() :
		_content( 0 ),
		_sons()
	{
	
	}	

	template <typename Content>
	Tree<Content>::Tree( Content & content ) throw() :
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

		// Useless but maybe safer for debugging :
		_sons.clear() ;
	
	}


	template <typename Content>
	const std::list< Tree<Content>* > & Tree<Content>::getSons() const throw()
	{
		return _sons ;
	}


	template <typename Content>
	void Tree<Content>::addSon( Tree<Content> & subtree ) 
		throw( Ceylan::TreeException )
	{
		
		for ( typename SubTreeList::const_iterator it = _sons.begin();
				it != _sons.end(); it++ )
			if ( *it == &subtree )
				throw Ceylan::TreeException( "Tree<Content>::addSon : "
					"following son was already registered : "
					+ subtree.toString() ) ;

		_sons.push_back( &subtree ) ;

	}


	template <typename Content>
	void Tree<Content>::traverseDepthFirst( TreeVisitor<Content> & treeVisitor,
		bool visitContent ) throw( TreeException )
	{
	
		// First recurse :
		for ( typename SubTreeList::iterator it = _sons.begin();
				it != _sons.end(); it++ )
			(*it)->traverseDepthFirst( treeVisitor, visitContent ) ;

		// Then apply :
		treeVisitor.visit( *this ) ;

		if ( visitContent && _content != 0 )
			treeVisitor.visit( *_content ) ;

	}


	template <typename Content>
	void Tree<Content>::traverseBreadthFirst( 
			TreeVisitor<Content> & treeVisitor,	bool visitContent ) 
		throw( TreeException )
	{
	
		// Apply :
		treeVisitor.visit( *this ) ;

		if ( visitContent && _content != 0 )
			treeVisitor.visit( *_content ) ;

		// Then recurse :
		for ( typename SubTreeList::iterator it = _sons.begin();
				it != _sons.end(); it++ )
			(*it)->traverseBreadthFirst( treeVisitor, visitContent ) ;

	}


	template <typename Content>
	const std::string Tree<Content>::toString( Ceylan::VerbosityLevels level )
		const throw()
	{

		std::string res = "Tree " ;

		if ( _content !=0 )
			res += "whose local content is : "
				+ _content->toString( level ) ;
		else
			res += "with no local content" ;

		if ( _sons.empty() )
			return res + ". No registred subtree" ;

		res += ". Following subtrees are registered : " ;

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
