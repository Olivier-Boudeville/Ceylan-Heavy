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
	 * @example :
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
	class RegExp
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
			explicit RegExp( const std::string & toAnalyze )
				throw( Features::FeatureNotAvailableException ) ;


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
			virtual bool matches( const std::string & pattern ) const 
				throw( Features::FeatureNotAvailableException ) ;


			/**
			 * Tells whether the string is a valid XML name.
			 *
			 * @throw FeatureNotAvailableException if the regular expression
			 * support feature is not available.
			 *
			 * @todo move to XML module.
			 *
			 */
			virtual bool isXMLName() const 
				throw( Features::FeatureNotAvailableException ) ;



		private:


			/// The string that will be analyzed.
			std::string _toAnalyze ;
			

	} ;


}


#endif // CEYLAN_REGULAR_EXPRESSION_H_
