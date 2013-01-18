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
 * Author: Olivier Boudeville (olivier.boudeville@esperide.com)
 *
 */


#ifndef CEYLAN_XML_ELEMENT_H_
#define CEYLAN_XML_ELEMENT_H_


#include "CeylanVisitable.h" 	    // for inheritance
#include "CeylanSerializable.h"     // for inheritance
#include "CeylanTextDisplayable.h"  // for inheritance
#include "CeylanXML.h"              // for XML general operations


#include <string>




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
		
				explicit XMLElementException( const std::string & reason ) ;
					
				virtual ~XMLElementException() throw() ;

		} ;




		/**
		 * Describes an abstract XML element in a XML document.
		 *
		 * XML elements can be markups, text, binary content, etc.
		 *
		 */
		class CEYLAN_DLL XMLElement: public TextDisplayable, 
			public Serializable, public Visitable
		{


			public:

				
				/**
				 * Creates an empty XML element.
				 *
				 */
				XMLElement() ;
				
				
				/// Virtual destructor.
				virtual ~XMLElement() throw() ;



				/*
				 * The accept method, inherited from Visitable, remains 
				 * pure virtual here.
				 *
				 */
				
				
				
				/**
				 * Saves the instance state to specified stream.
				 *
				 * @param output the output stream to which the state will be
				 * written.
				 *
				 * @throw SerializationException if the operation failed.
				 *
				 */
				virtual void saveTo( System::OutputStream & output ) const = 0 ;
				
				 
				 
				 
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
				XMLElement( const XMLElement & source ) ;
			
			
				/**
				 * Assignment operator made private to ensure that it will 
				 * never be called.
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 * 
				 */			 
				XMLElement & operator = ( const XMLElement & source ) ;
				
				
		} ;
		
		
		
		
		
		/// Various XML elements accept XML visitors.
		class XMLVisitor ;
		
		
		
		
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
		class CEYLAN_DLL XMLMarkup: public XMLElement
		{

			public:

				
				
				/**
				 * Creates an empty XML markup.
				 *
				 */
				XMLMarkup() ;
				

				/**
				 * Creates a named XML markup.
				 *
				 * @param name the markup name, for example 'para' for 
				 * markup <para>.
				 *
				 */
				explicit XMLMarkup( const MarkupName & name ) ;
				
				
				
				/// Virtual destructor.
				virtual ~XMLMarkup() throw() ;



				/**
				 * Returns the markup name.
				 *
				 * @example if '<ul>' is an opening markup, then 'ul' is
				 * the returned name.
				 *
				 */
				virtual MarkupName getMarkupName() const ; 



				/**
				 * Return the markup that closes the opening markup.
				 *
				 * @example if '<ul>' is an opening markup, then '</ul>' is
				 * the closing one.
				 *
				 */
				virtual std::string getClosingMarkup() const ; 
				
				
				
				 
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
				virtual bool hasAttribute( const AttributeName & name ) const ;
			
					
					
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
				virtual AttributeValue getAttribute( 
					const AttributeName & name ) const ;
					
				
				
				/**
				 * Returns the value of the attribute, specified by its name.
				 *
				 * @param name the attribute name.
				 *
				 * @return the value associated with the attribute name.
				 *
				 * @throw XMLElementException if no such attribute could be
				 * found.
				 *
				 */
				virtual AttributeValue getExistingAttribute( 
					const AttributeName & name ) const ;
					
				
				
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
				virtual void setAttribute( const AttributeName & name,
					const AttributeValue & value ) ;
					
					
					
				/**
				 * Returns a reference to the internal attribute map.
				 *
				 * This method may be used to manage the attributes of a 
				 * markup from outside, for example when reading an XML file.
				 *
				 */
				virtual AttributeMap & getAttributes() ;
				 	
					
					
					
				
				// Serializable section.

			
			
				/**
				 * Loads a new instance state from specified stream.
				 *
				 * @param input the input stream from which the state will 
				 * be read.
				 *
				 * @throw SerializationException if the operation failed.
				 *
				 */
				virtual void loadFrom( System::InputStream & input ) ;



				/**
				 * Allows given visitor to visit this object, thanks to a 
				 * callback: 'visitor.visit( *this ) ;'
				 *
				 * Implements the Visitable interface.
				 *
				 * @note The specified visitor must be a XML visitor, 
				 * otherwise a VisitException is thrown.
				 *
				 */
				virtual void accept( Visitor & visitor ) ;

					
					
				/**
				 * Saves the instance state to specified stream.
				 *
				 * @param output the output stream to which the state will be
				 * written.
				 *
				 * @throw SerializationException if the operation failed.
				 *
				 * @note The toString is not used as they do not serve the
				 * same purpose, and differ in terms of encoding.
				 *
				 */
				virtual void saveTo( System::OutputStream & output ) const  ;

					
					
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
			 		Ceylan::VerbosityLevels level = Ceylan::high ) const ;



			protected:
		

				/// Name of the markup.
				MarkupName _name ;


/* 
 * Takes care of the awful issue of Windows DLL with templates.
 *
 * @see Ceylan's developer guide and README-build-for-windows.txt 
 * to understand it, and to be aware of the associated risks. 
 * 
 */
#pragma warning( push )
#pragma warning( disable: 4251 )
			
				/**
				 * Attributes of a XML markup, sort of tagged values, i.e. 
				 * pair of name-value.
				 *
				 */
				AttributeMap _attributes ;

#pragma warning( pop ) 			



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
				XMLMarkup( const XMLMarkup & source ) ;
			
			
				/**
				 * Assignment operator made private to ensure that it will 
				 * never be called.
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 * 
				 */			 
				XMLMarkup & operator = ( const XMLMarkup & source ) ;
				
				
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
				XMLText() ;
				

				/**
				 * Creates a XML text from specified string.
				 *
				 */
				explicit XMLText( const std::string & text ) ;
				
				
				
				/// Virtual destructor.
				virtual ~XMLText() throw() ;


	
				/**
				 * Allows given visitor to visit this object, thanks to a 
				 * callback: 'visitor.visit( *this ) ;'
				 *
				 * Implements the Visitable interface.
				 *
				 * @note The specified visitor must be a XML visitor, 
				 * otherwise a VisitException is thrown.
				 *
				 */
				virtual void accept( Visitor & visitor ) ;

	
					
				/**
				 * Returns the text in this element.
				 *
				 */
				virtual std::string getText() const ;
					
				
				
				/**
				 * Sets the text of this element.
				 *
				 */
				virtual void setText( const std::string & newText ) ;
					
					
					
					
				
				// Serializable section.



				/**
				 * Saves the instance state to specified stream.
				 *
				 * @param output the output stream to which the state will be
				 * written.
				 *
				 * @throw SerializationException if the operation failed.
				 *
				 * @note The toString is not used as they do not serve the
				 * same purpose, and differ in terms of encoding.
				 *
				 */
				virtual void saveTo( System::OutputStream & output ) const  ;
			
			
			
				/**
				 * Loads a new instance state from specified stream.
				 *
				 * @param input the input stream from which the state will 
				 * be read.
				 *
				 * @throw SerializationException if the operation failed.
				 *
				 */
				virtual void loadFrom( System::InputStream & input ) ;



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
			 		Ceylan::VerbosityLevels level = Ceylan::high ) const ;




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
				XMLText( const XMLText & source ) ;
			
			
			
				/**
				 * Assignment operator made private to ensure that it will 
				 * never be called.
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 * 
				 */			 
				XMLText & operator = ( const XMLText & source ) ;
				
				
				
		} ;
		
			
	}
	
}	



#endif // CEYLAN_XML_ELEMENT_H_

