#ifndef CEYLAN_XML_ELEMENT_H_
#define CEYLAN_XML_ELEMENT_H_


#include "CeylanTextDisplayable.h"  // for inheritance
#include "CeylanSerializable.h"     // for inheritance
#include "CeylanXML.h"              // for XML general operations


#include <string>
#include <map>



namespace Ceylan
{


	namespace XML
	{
	


		/**
		 * Exception to be raised when an operation on an XML element 
		 * failed.
		 *
		 */
		class CEYLAN_DLL XMLElementException : public XMLException
		{
	
			public:
		
				explicit XMLElementException( const std::string & reason )
					throw() ;
					
				virtual ~XMLElementException() throw() ;

		} ;



		/**
		 * Describes an abstract XML element in a XML document.
		 *
		 * XML elements can be markups, text, binary content, etc.
		 *
		 */
		class CEYLAN_DLL XMLElement : public TextDisplayable, 
			public Serializable
		{


			public:

				
				/**
				 * Creates an empty XML element.
				 *
				 */
				XMLElement() throw() ;
				
				
				/// Virtual destructor.
				virtual ~XMLElement() throw() ;



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
				XMLElement( const XMLElement & source ) throw() ;
			
			
				/**
				 * Assignment operator made private to ensure that it will 
				 * never be called.
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 * 
				 */			 
				XMLElement & operator = ( const XMLElement & source ) throw() ;
				
				
		} ;
		
		
		
		/**
		 * Describes a XML markup.
		 *
		 * A markup has a name and can have attributes, which are pairs of 
		 * name and value.
		 *
		 * @note The fact that it can enclose any number of XML elements is
		 * represented through the XML tree.
		 *
		 */
		class CEYLAN_DLL XMLMarkup : public XMLElement
		{

			public:


				/**
				 * Creates an empty XML markup.
				 *
				 */
				XMLMarkup() throw() ;
				

				/**
				 * Creates a namedXML markup.
				 *
				 */
				explicit XMLMarkup( const std::string & name ) throw() ;
				
				
				
				/// Virtual destructor.
				virtual ~XMLMarkup() throw() ;


				/**
				 * Returns whether this attribute, specified by its name, is
				 * defined in this markup.
				 *
				 * @param name the attribute name.
				 *
				 * @return true iff an attribute with this name already exists
				 * for this markup.
				 *
				 */
				virtual bool hasAttribute( const std::string & name ) 
					const throw() ;
					
					
				/**
				 * Returns the value of the attribute, specified by its name.
				 *
				 * @param name the attribute name.
				 *
				 * @return the value associated with the attribute name, or 
				 * an empty string if the attribute is not defined in this
				 * markup.
				 *
				 */
				virtual std::string getAttribute( const std::string & name ) 
					const throw() ;
					
				
				/**
				 * Adds specified name-value pair as an attribute of this
				 * markup.
				 *
				 * @param name the attribute name.
				 *
				 * @param value the attribute value.
				 *
				 * If an attribute with the same name was already existing,
				 * its value is updated with this newer value.
				 *
				 *
				 */
				virtual void setAttribute( const std::string & name,
					const std::string & value ) throw() ;
					
					
					
				
				// Serializable section.


				/**
				 * Saves the instance state to specified stream.
				 *
				 * @param output the output stream to which the state will be
				 * written.
				 *
				 * @throw SerializationException if the operation failed.
				 *
				 */
				virtual void saveTo( System::OutputStream & output ) 
					throw( SerializationException ) ;
				
			
				/**
				 * Loads a new instance state from specified stream.
				 *
				 * @param input the input stream from which the state will 
				 * be read.
				 *
				 * @throw SerializationException if the operation failed.
				 *
				 */
				virtual void loadFrom( System::InputStream & input ) 
					throw( SerializationException ) ;



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
			
			
				/// Name of the markup.
				std::string _name ;
				
			
				/**
				 * Attributes of a XML markup, sort of tagged values, i.e. 
				 * pair of name-value.
				 *
				 *
				 */
				std::map<std::string,std::string> _attributes ;



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
				XMLMarkup( const XMLMarkup & source ) throw() ;
			
			
				/**
				 * Assignment operator made private to ensure that it will 
				 * never be called.
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 * 
				 */			 
				XMLMarkup & operator = ( const XMLMarkup & source ) throw() ;
				
				
		} ;



		
		/**
		 * Describes a XML text.
		 *
		 * A text is an eligible content for a markup.
		 *
		 */
		class CEYLAN_DLL XMLText : public XMLElement
		{

			public:


				/**
				 * Creates an empty XML text.
				 *
				 */
				XMLText() throw() ;
				

				/**
				 * Creates a XML text from specified string.
				 *
				 */
				explicit XMLText( const std::string & text ) throw() ;
				
				
				
				/// Virtual destructor.
				virtual ~XMLText() throw() ;


				
					
				/**
				 * Returns the text in this element.
				 *
				 */
				virtual std::string getText() const throw() ;
					
				
				/**
				 * Sets the text of this element.
				 *
				 */
				virtual void setText( const std::string & newText ) throw() ;
					
					
					
				
				// Serializable section.


				/**
				 * Saves the instance state to specified stream.
				 *
				 * @param output the output stream to which the state will be
				 * written.
				 *
				 * @throw SerializationException if the operation failed.
				 *
				 */
				virtual void saveTo( System::OutputStream & output ) 
					throw( SerializationException ) ;
				
			
				/**
				 * Loads a new instance state from specified stream.
				 *
				 * @param input the input stream from which the state will 
				 * be read.
				 *
				 * @throw SerializationException if the operation failed.
				 *
				 */
				virtual void loadFrom( System::InputStream & input ) 
					throw( SerializationException ) ;



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
			
			
				/// Text of this element.
				std::string _text ;
				
			

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
				XMLText( const XMLText & source ) throw() ;
			
			
				/**
				 * Assignment operator made private to ensure that it will 
				 * never be called.
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 * 
				 */			 
				XMLText & operator = ( const XMLText & source ) throw() ;
				
				
		} ;
		
			
	}
	
}	


#endif // CEYLAN_XML_ELEMENT_H_

