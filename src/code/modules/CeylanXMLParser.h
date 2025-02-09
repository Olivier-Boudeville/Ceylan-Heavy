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


#ifndef CEYLAN_XML_PARSER_H_
#define CEYLAN_XML_PARSER_H_


#include "CeylanTextDisplayable.h"  // for TextDisplayable
#include "CeylanTree.h"             // for Ceylan::Tree
#include "CeylanXML.h"              // for XML general operations
#include "CeylanInputStream.h"      // for InputStream::InputStreamException

#include <string>
#include <stack>




namespace Ceylan
{



	namespace System
	{

		// Some XML operations reads from such streams:
		class InputStream ;

	}




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

				explicit XMLParserException( const std::string & reason ) ;

				virtual ~XMLParserException() throw() ;

		} ;




		// Forward-declaration.
		template <typename XMLElement> class Tree ;




		/**
		 * XML parser, which can read and write from and to XML file.
		 *
		 * From a file, a tree in memory is generated, and reciprocally a tree
		 * can be saved into a XML file.
		 *
		 * This light-weight parser works a little like DOM: it reads the full
		 * XML document before generating its counterpart in memory.
		 *
		 * No validation nor XML schema managed.
		 *
		 */
		class CEYLAN_DLL XMLParser : public Ceylan::TextDisplayable
		{



			public:


				typedef Ceylan::Tree<Ceylan::XML::XMLElement> XMLTree ;



				/**
				 * XML parser, which reads a file and generates from it a tree
				 * in memory. This tree can be traversed easily to extract
				 * relevant informations from it.
				 *
				 * @param filename the file whose content is formatted in
				 * XML. Depending on which methods will be called, the filename
				 * will be used to save an internal tree to the corresponding
				 * file, or to load an internal tree from an already existing
				 * file with that filename.
				 *
				 * Note that therefore no tree is available when this parser is
				 * created.
				 *
				 * @see loadFromFile
				 *
				 */
				explicit XMLParser( const std::string & filename ) ;



				/**
				 * Virtual destructor.
				 *
				 */
				virtual ~XMLParser() throw() ;





				// Internal XML tree management.



				/**
				 * Returns whether this parser has an available XML tree.
				 *
				 * @return true iff there is an available XML tree.
				 *
				 */
				virtual bool hasXMLTree() const ;



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
				virtual XMLTree & getXMLTree() const ;



				/**
				 * Sets the internal XML tree to the specified one.
				 *
				 * If there was already an XML tree available, it is deleted
				 * before the new one is set.
				 *
				 * @param newTree the new XML tree for this parser.
				 *
				 * @note The parser takes ownership of the specified tree.
				 *
				 */
				virtual void setXMLTree( XMLTree & newTree ) ;





				// Serialization section.



				/**
				 * Saves the internal tree into an XML file, whose filename was
				 * specified in the constructor. It becomes the new default
				 * filename.
				 *
				 * @param filename the file where the XML tree should be
				 * saved. If an empty string is specified, then the filename
				 * given to the constructor is used instead.
				 *
				 * @note If the file is already existing, its content will be
				 * lost and replaced by this new content.
				 *
				 */
				virtual void saveToFile( const std::string & filename = "" )
					const ;



				/**
				 * Loads from an XML file, whose filename was specified in the
				 * constructor, a new internal XML tree.
				 *
				 * @note Any already existing tree will be deleted first.
				 *
				 * The parser does its best to be robust and overcome incorrect
				 * XML syntaxes.
				 *
				 */
				virtual void loadFromFile() ;



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





				// Static section.



				/// Default XML encoding is ISO-8859-15 (Latin-1 with euro).
				static std::string DefaultEncoding ;




				/// All sequences that begin with a '<'.
				enum LowerThanSequence
				{


					/// XML declaration, for example: '<?xml version="1.0"?>'.
					Declaration,

					/// XML comment, for example: '<!-- This is a comment -->'.
					Comment,

					/// XML opening Markup, for example: '<para>'
					OpeningMarkup,

					/// XML closing Markup, for example: '</para>'
					ClosingMarkup,

					/// Unexpected XML element:
					UnexpectedElement


				} ;




				/**
				 * Reads a character from input stream, whereas previous read
				 * character is supposed to be '<', and interprets the kind of
				 * sequence it begins.
				 *
				 * @param input the stream to read from.
				 *
				 * @param readChar the variable in which the read variable will
				 * be put once interpreted (one too many read).
				 *
				 * @return the identifier of the interpreted sequence.
				 *
				 * @throw InputStreamException if the operation failed.
				 *
				 */
				static LowerThanSequence InterpretLowerThanSequence(
					System::InputStream & input, Ceylan::Uint8 & readChar ) ;



				/**
				 * Returns a string describing the specified sequence.
				 *
				 * @param the sequence type to describe.
				 *
				 */
				static std::string DescribeLowerThanSequence(
					LowerThanSequence sequence ) ;



				/**
				 * Reads from specified input stream a full XML declaration (ex:
				 * [<?]xml version="1.0"...?>), whereas previous read characters
				 * are supposed to be '<?', interprets the declaration, skips
				 * next whitespace and put the first character read after in
				 * specified char.
				 *
				 * @param input the stream to read from.
				 *
				 * @throw InputStreamException if an I/O operation failed, or
				 * XMLParserException if an incorrect declaration is found.
				 *
				 */
				static void InterpretXMLDeclaration(
					System::InputStream & input ) ;



				/**
				 * Reads from specified string a sequence of attributes (ex:
				 * version="1.0"), and store them in specified map.
				 *
				 * @param toBeParsed the string to read the attributes from.
				 *
				 * @param attributeMap the map in which the read attributes
				 * (name/value pairs) will be stored once interpreted.
				 *
				 * @throw XMLParserException if an incorrect syntax is found.
				 *
				 */
				static void ParseAttributeSequence(
					const std::string & toBeParsed,
					AttributeMap & attributeMap ) ;





			protected:



				/**
				 * Reads from specified input stream until a text, an opening or
				 * a closing markup is found, manages it and continue
				 * recursively until the first open markup is closed: the XML
				 * document is then fully parsed.
				 *
				 * @param input the stream from which XML data is to be read.
				 *
				 * @param markupStack an internal stack used to determine the
				 * relationships between tree nodes.
				 *
				 * @param currentNode the current node being considered. Either
				 * this markup is an opening one, and it will lead to add a son
				 * to current node, or it is a closing one, that will lead the
				 * new current node to be the father of this current node.
				 *
				 * @return true if the document is fully parsed, i.e. if the
				 * root markup hit its closing mark.
				 *
				 */
				virtual void handleNextElement(
					System::InputStream & input,
					std::stack<std::string> & markupStack,
					XMLTree * currentNode,
					Ceylan::Uint8 & remainder ) ;



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
				 * Example: ISO-8859-15.
				 *
				 */
				std::string _encoding ;




			private:



				/**
				 * Copy constructor made private to ensure that it will never be
				 * called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 * @note Made to avoid unwanted hidden clone of the Singleton.
				 *
				 */
				XMLParser( const XMLParser & source ) ;



				/**
				 * Assignment operator made private to ensure that it will never
				 * be called.
				 *
				 * The compiler should complain whenever this undefined operator
				 * is called, implicitly or not.
				 *
				 */
				XMLParser & operator = ( const XMLParser & source ) ;


		} ;


	}


}



#endif // CEYLAN_XML_PARSER_H_
