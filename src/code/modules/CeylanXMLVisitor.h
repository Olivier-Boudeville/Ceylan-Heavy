#ifndef CEYLAN_XML_VISITOR_H_
#define CEYLAN_XML_VISITOR_H_


#include "CeylanTree.h"             // for Ceylan::Tree
#include "CeylanXML.h"              // for XML general operations
#include "CeylanVisitor.h"          // for inheritance
#include "CeylanXMLElement.h"       // for Markup::MarkupName


#include <string>
#include <stack>   // for stack
#include <list>    // for list



namespace Ceylan
{



	namespace System
	{
	
	
		// A XMLSavingVisitor uses an OutputStream.
		class OutputStream ;
		
		
	}
	
	

	namespace XML
	{
	
	

		// A XML parser manages XML elements.
		class XMLElement ;

	
	
		/**
		 * Exception to be raised when an XML parser operation failed.
		 *
		 */
		class CEYLAN_DLL XMLVisitorException : public Ceylan::XML::XMLException
		{
	
			public:
		
				explicit XMLVisitorException( const std::string & reason )
					throw() ;
					
				virtual ~XMLVisitorException() throw() ;

		} ;



		/// A XML visitor can visit XML markups.
		class XMLMarkup ;
		
		
		/// A XML visitor can visit XML texts.		
		class XMLText ;
		

		/**
		 * Generic XML visitor.
		 *
		 * Its role is to visit a XML tree and process its nodes.
		 *
		 */
		class CEYLAN_DLL XMLVisitor : public TreeHeightAwareVisitor<XMLElement>
		{
		
		
			public:



				/**
				 * Creates an empty XML visitor.
				 *
				 */
				XMLVisitor() throw() ;


				/**
				 * Virtual destructor.
				 *
				 */
				virtual ~XMLVisitor() throw() ;
	
	
				/**
				 * Visits specified XML markup and processes it.
				 *
				 * @param xmlMarkup the XML markup to visit.
				 *
				 * @throw VisitException if this visit failed.
				 *
				 * @note The markup is not 'const' to allow for visitors able
				 * to change the visited elements, ex : a searching visitor
				 * may need to return a list of non-const markups.
				 *
				 */
				virtual void visit( XMLMarkup & xmlMarkup ) 
					throw( VisitException ) = 0 ;
				
	
				/**
				 * Visits specified XML text and processes it.
				 *
				 * @param xmlText the XML text to visit.
				 *
				 * @throw VisitException if this visit failed.
				 *
				 * @note The text is not 'const' to allow for visitors able
				 * to change the visited elements, ex : a searching visitor
				 * may need to return a list of non-const texts.
				 *
				 */
				virtual void visit( XMLText & xmlText ) 
					throw( VisitException ) = 0 ;
				
	
			
				/**
				 * Returns a user-friendly description of the state of this
				 * object.
				 *
				 * @param level the requested verbosity level.
				 *
				 * @note Text output format is determined from overall settings.
				 *
				 * @see TextDisplayable
				 *
				 */
				 virtual const std::string toString( 
			 			Ceylan::VerbosityLevels level = Ceylan::high ) 
					const throw() ;



			protected:

				


			private:


				/**
				 * Copy constructor made private to ensure that it will never
				 * be called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 * 
				 * @note Made to avoid unwanted hidden clone of the Singleton.
				 *
				 */			 
				XMLVisitor( const XMLVisitor & source ) throw() ;
			
			
				/**
				 * Assignment operator made private to ensure that it will 
				 * never be called.
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 * 
				 */			 
				XMLVisitor & operator = ( const XMLVisitor & source ) throw() ;


		} ;
	
	
	
		
	
		/**
		 * XML visitor dedicated to the saving of an XML tree (in memory) to
		 * an output stream, for example a file.
		 *
		 * Its role is to visit a XML tree and saves its nodes in order so
		 * that a well-formed XML document is serialized, included a starting
		 * XML header.
		 *
		 */
		class CEYLAN_DLL XMLSavingVisitor : public XMLVisitor
		{
		
		
			public:



				/**
				 * Creates a XML visitor dedicated to the saving of a XML
				 * tree.
				 *
				 * @param output the output stream that should be used to
				 * save the XML tree. This visitor does not take ownership
				 * of this stream, it will not close nor delete it when itself
				 * destroyed.
				 *
				 */
				XMLSavingVisitor( System::OutputStream & output ) throw() ;


				/**
				 * Virtual destructor.
				 *
				 */
				virtual ~XMLSavingVisitor() throw() ;
	
	
				/// Returns current height.
				virtual Height getHeight() const throw() ;


				/// Increments current height.
				virtual void incrementHeight() throw() ;
			
				/// Decrements current height.
				virtual void decrementHeight() throw() ;
	

				/**
				 * Visits specified XML markup and saves it to the internal
				 * output stream.
				 *
				 * @param xmlMarkup the XML markup to save.
				 *
				 * @throw VisitException if this saving failed.
				 *
				 * @note If the signature was not inherited, the specified 
				 * markup would by 'const'.
				 *
				 */
				virtual void visit( XMLMarkup & xmlMarkup ) 
					throw( VisitException ) ;
				
				
	
				/**
				 * Visits specified XML text and saves it to the internal
				 * output stream.
				 *
				 * @param xmlText the XML text to save.
				 *
				 * @throw VisitException if this saving failed.
				 *
				 * @note If the signature was not inherited, the specified 
				 * text would by 'const'.
				 *
				 */
				virtual void visit( XMLText & xmlText ) 
					throw( VisitException ) ;
				
	
			
				/**
				 * Returns a user-friendly description of the state of this
				 * object.
				 *
				 * @param level the requested verbosity level.
				 *
				 * @note Text output format is determined from overall settings.
				 *
				 * @see TextDisplayable
				 *
				 */
				 virtual const std::string toString( 
			 			Ceylan::VerbosityLevels level = Ceylan::high ) 
					const throw() ;
		
				
				/// The offset for serialized XLM elements.
				static std::string OffsetForMarkup ;
				
						
				typedef std::string ClosingMarkup ;
				
		
		
		
			protected:
			
				
				/// The OutputStream where XML trees will be written.
				System::OutputStream * _output ;
				
				std::stack<ClosingMarkup> _markupsToClose ;
				

			private:


				/**
				 * Copy constructor made private to ensure that it will never
				 * be called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 * 
				 * @note Made to avoid unwanted hidden clone of the Singleton.
				 *
				 */			 
				XMLSavingVisitor( const XMLSavingVisitor & source ) throw() ;
			
			
				/**
				 * Assignment operator made private to ensure that it will 
				 * never be called.
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 * 
				 */			 
				XMLSavingVisitor & operator = ( 
					const XMLSavingVisitor & source ) throw() ;
					
					
		} ;			
				
				
			
		/**
		 * XML visitor dedicated to the searching in an XML tree of specific
		 * elements.
		 *
		 * @note Using a visitor here leads to inefficient search algorithm.
		 *
		 * @note This visitor does not care about height information, hence 
		 * being height-aware is useless here.
		 *
		 */
		class CEYLAN_DLL XMLSearchingVisitor : public XMLVisitor
		{
		
		
			public:



				/**
				 * Creates a XML visitor dedicated to the searching of a XML
				 * tree.
				 *
				 * @param searchedMarkupName the name of the searched markup,
				 * for example 'para'.
				 *
				 */
				XMLSearchingVisitor( MarkupName & searchedMarkupName ) 
					throw() ;


				/**
				 * Virtual destructor.
				 *
				 */
				virtual ~XMLSearchingVisitor() throw() ;
		
				
				/**
				 * Find the XML markups from the visited tree that match the
				 * markup name specified in the constructor of this visitor.
				 *
				 * @note The target tree must have been visited first so that
				 * nodes have been already searched, for example :
				 * <pre>
				 * XMLSearchingVisitor myXMLSearchingVisitor( "ul" ) ;
				 * myXMLTree.accept( myXMLSearchingVisitor ) ; 
				 * // Here the visitor has computed the list, then :
				 * std::list<XMLMarkup *> * myList = &
				 *	 myXMLSearchingVisitor.getMatchingMarkups() ;
				 * ...
				 * // No deleting of myList nor of its elements, as ownership
				 * // kept by the visitor.
				 * </pre>
				 *
				 * @return the internal list of the visitor, which keeps its
				 * ownership. This list references the XML markups, from the 
				 * visited tree, that match the specified markup name.
				 * These markups remain the property of the visited tree,
				 * therefore the elements of the list must not
				 * be deallocated by the caller of getMatchingMarkups.
				 *
				 * @throw VisitException if the operation failed.
				 *
				 * @note getMatchingMarkups should be called only once the 
				 * tree has been visited, otherwise an empty list will be
				 * returned.
				 *
				 */
				std::list<XMLMarkup *> & getMatchingMarkups() 
					throw( VisitException ) ;

				
				/**
				 * Visits specified XML markup and records it if it matches
				 * the search.
				 *
				 * @param xmlMarkup the XML markup to visit.
				 *
				 * @throw VisitException if the visit failed.
				 *
				 */
				virtual void visit( XMLMarkup & xmlMarkup ) 
					throw( VisitException ) ;
				
				
	
				/**
				 * Visits specified XML text.
				 *
				 * @param xmlText the XML text to visit.
				 *
				 * @throw VisitException if this visit failed.
				 *
				 */
				virtual void visit( XMLText & xmlText ) 
					throw( VisitException ) ;
				
	
			
				/**
				 * Returns a user-friendly description of the state of this
				 * object.
				 *
				 * @param level the requested verbosity level.
				 *
				 * @note Text output format is determined from overall settings.
				 *
				 * @see TextDisplayable
				 *
				 */
				 virtual const std::string toString( 
			 			Ceylan::VerbosityLevels level = Ceylan::high ) 
					const throw() ;
						
		
		
		
			protected:
			
			
				/// The markup that is searched for.
				MarkupName _searchedMarkup ;
				
				/// The list that will contain all nodes whose content matches.
				std::list<XMLMarkup *> _matchingNodes ;
				

			private:


				/**
				 * Copy constructor made private to ensure that it will never
				 * be called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 * 
				 * @note Made to avoid unwanted hidden clone of the Singleton.
				 *
				 */			 
				XMLSearchingVisitor( const XMLSearchingVisitor & source )
					throw() ;
			
			
				/**
				 * Assignment operator made private to ensure that it will 
				 * never be called.
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 * 
				 */			 
				XMLSearchingVisitor & operator = ( 
					const XMLSearchingVisitor & source ) throw() ;
					
					
		} ;			
					
		
	}
		
	
}


#endif // CEYLAN_XML_VISITOR_H_
