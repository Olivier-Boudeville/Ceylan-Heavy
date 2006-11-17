#ifndef CEYLAN_XML_PARSER_H_
#define CEYLAN_XML_PARSER_H_


#include "CeylanTextDisplayable.h"  // for TextDisplayable
#include "CeylanTree.h"             // for Ceylan::Tree
#include "CeylanXML.h"              // for XML general operations


#include <string>
#include <stack>   // for stack



namespace Ceylan
{


	namespace XML
	{
	
	

		// A XML parser manages XML elements.
		class XMLElement ;

	
	
		/**
		 * Exception to be raised when an XML parser operation failed.
		 *
		 */
		class CEYLAN_DLL XMLParserException : public Ceylan::XML::XMLException
		{
	
			public:
		
				explicit XMLParserException( const std::string & reason )
					throw() ;
					
				virtual ~XMLParserException() throw() ;

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


				typedef Ceylan::Tree<Ceylan::XML::XMLElement> XMLTree ;


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
				 * Returns whether this parser has an available XML tree.
				 *
				 * @return true iff there is an available XML tree.
				 *
				 */
				virtual bool hasXMLTree() const throw() ;
					
					
				/**
				 * Returns the internal XML tree.
				 *
				 * @throw XMLParserException if there is no available tree.
				 *
				 * @see hasXMLTree
				 *
				 * @note The returned reference points to the internal tree,
				 * which is still owned by this parser.
				 *
				 */
				virtual XMLTree & getXMLTree() const 
					throw( XMLParserException ) ;
					
				
				/**
				 * Sets the internal XML tree to the specified one.
				 * If there was already an XML tree available, it is deleted
				 * before the new one is set.
				 *
				 * @param newTree the new XML tree for this parser.
				 *
				 * @note The parser takes ownership of the specified tree.
				 *
				 */
				virtual void setXMLTree( XMLTree & newTree ) throw() ;
	
	
				/**
				 * Saves the internal tree into an XML file, whose filename
				 * was specified in the constructor.
				 *
				 * @note If the file is already existing, its content will be
				 * lost and replaced by this new content.
				 *
				 */
				virtual void saveToFile() const throw( XMLParserException ) ;
	
	
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



				/// Default XML encoding is ISO-8859-15 (Latin-1 with euro).
				static std::string DefaultEncoding ;
				
				

			protected:


				/**
				 * The filename of the file where the XML document can be
				 * serialized or deserialized.
				 *
				 */
				std::string _filename ;


				/// The tree corresponding to an XML document.
				XMLTree * _parsedTree ;
			
			
				/**
				 * The current encoding for the XML document.
				 *
				 * Example : ISO-8859-15.
				 *
				 */
				std::string _encoding ;
				
				


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
				XMLParser( const XMLParser & source ) throw() ;
			
			
				/**
				 * Assignment operator made private to ensure that it will 
				 * never be called.
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 * 
				 */			 
				XMLParser & operator = ( const XMLParser & source ) throw() ;


		} ;
		
	}
	
}


#endif // CEYLAN_XML_PARSER_H_
