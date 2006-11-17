#ifndef CEYLAN_XML_VISITOR_H_
#define CEYLAN_XML_VISITOR_H_


#include "CeylanTree.h"             // for Ceylan::Tree
#include "CeylanXML.h"              // for XML general operations
#include "CeylanVisitor.h"          // for inheritance


#include <string>
#include <stack>   // for stack



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
				 */
				virtual void visit( const XMLMarkup & xmlMarkup ) 
					throw( VisitException ) = 0 ;
				
	
				/**
				 * Visits specified XML text and processes it.
				 *
				 * @param xmlText the XML text to visit.
				 *
				 * @throw VisitException if this visit failed.
				 *
				 */
				virtual void visit( const XMLText & xmlText ) 
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
				 */
				virtual void visit( const XMLMarkup & xmlMarkup ) 
					throw( VisitException ) ;
				
				
	
				/**
				 * Visits specified XML text and saves it to the internal
				 * output stream.
				 *
				 * @param xmlText the XML text to save.
				 *
				 * @throw VisitException if this saving failed.
				 *
				 */
				virtual void visit( const XMLText & xmlText ) 
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
					
		
	}
		
	
}


#endif // CEYLAN_XML_VISITOR_H_
