#ifndef CEYLAN_XML_PARSER_H_
#define CEYLAN_XML_PARSER_H_


#include "CeylanTextDisplayable.h"  // for TextDisplayable
#include "CeylanException.h"        // for Ceylan::Exception
#include "CeylanTree.h"             // for Ceylan::Tree


#include <string>


namespace Ceylan
{


	/**
	 * Exception to be raised when an XML operation failed.
	 *
	 */
	class CEYLAN_DLL XMLParserException : public Ceylan::Exception
	{
	
		public:
		
			explicit XMLParserException( const std::string & reason ) throw() ;
			virtual ~XMLParserException() throw() ;

	} ;


	class XMLElement : public TextDisplayable
	{

		public:

			XMLElement() throw() ;
			virtual ~XMLElement() throw() ;

	} ;


	// Forward-declaration.
	template <typename XMLElement> class Tree ;


	/**
	 * XML parser, which can read and write from and to XML file.
	 * From a file, a tree in memory is generated, and reciprocally
	 * a tree can be saved into a XML file.
	 *
	 * This light-weight parser works a little like DOM : it reads
	 * the full XML document before generating its counterpart in
	 * memory.
	 *
	 * No validation nor XML schema managed.
	 *
	 */
	class CEYLAN_DLL XMLParser : public Ceylan::TextDisplayable
	{
		
		public:


			/**
			 * XML parser, which reads a file and generates from it
			 * a tree in memory. This tree can be traversed easily to
			 * extract relevant informations from it.
			 *
			 * @param filename the file whose content is formatted in
			 * XML.
			 *
			 */
			explicit XMLParser( const std::string & filename ) throw() ;


			/**
			 * Virtual destructor.
			 *
			 */
			virtual ~XMLParser() throw() ;
	

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
			 * The filename of the file where the XML document can be serialized
			 * or deserialized.
			 *
			 */
			std::string _filename ;


			/// The tree corresponding to an XML document.
			Ceylan::Tree<Ceylan::XMLElement> * _parsedTree ;



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
			XMLParser( const XMLParser & source ) throw() ;
			
			
			/**
			 * Assignment operator made private to ensure that it will never be 
			 * called.
			 * The compiler should complain whenever this undefined operator 
			 * is called, implicitly or not.
			 * 
			 */			 
			XMLParser & operator = ( const XMLParser & source ) throw() ;


	} ;

}


#endif // CEYLAN_XML_PARSER_H_
