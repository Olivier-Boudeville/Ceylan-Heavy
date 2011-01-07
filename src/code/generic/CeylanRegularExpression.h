/* 
 * Copyright (C) 2003-2011 Olivier Boudeville
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


#ifndef CEYLAN_REGULAR_EXPRESSION_H_
#define CEYLAN_REGULAR_EXPRESSION_H_


#include "CeylanFeatures.h"          // for FeatureNotAvailableException

#include <string>


namespace Ceylan
{


	/**
 	 * Regular expression support, which is an optional Ceylan feature.
	 *
	 * To analyze a string, construct a RegExp object.
	 *
	 * @example:
	 * <pre>
	 * RegExp re( "www.host.com" ) ;
	 * ...
	 * if ( re.matches( aPattern ) ) ...
	 * </pre>
	 *
	 * @note No RegExp instance will be created if the feature for 
	 * regular expression support is not available, since the RegExp
	 * will fail in this case.
	 *
	 * @see Feature::areRegularExpressionsSupported
	 *
	 * @see Tcl regular expression (man re_syntax) to learn the 
	 * pattern-matching syntax.
	 *
	 * @see http://www.mkssoftware.com/docs/man1/re_syntax.1t.asp
	 *
	 */
	class CEYLAN_DLL RegExp
	{


		public:


			/**
			 * Constructs a regular expression with the string which will be
			 * analyzed.
			 *
			 * @throw FeatureNotAvailableException if the regular expression
			 * support feature is not available.
			 *
			 */
			explicit RegExp( const std::string & toAnalyze ) ;


			/// Basic virtual destructor.
			virtual ~RegExp() throw() ;



			/**
			 * Tells whether the string matches the expression pattern
			 * <b>pattern</b>.
			 *
			 * @throw FeatureNotAvailableException if the regular expression
			 * support feature is not available.
			 *
			 */
			virtual bool matches( const std::string & pattern ) const ;



			/**
			 * Tells whether the string is a valid XML name.
			 *
			 * @throw FeatureNotAvailableException if the regular expression
			 * support feature is not available.
			 *
			 * @todo move to XML module.
			 *
			 */
			virtual bool isXMLName() const ;



		private:


			/// The string that will be analyzed.
			std::string _toAnalyze ;
			

	} ;


}



#endif // CEYLAN_REGULAR_EXPRESSION_H_

