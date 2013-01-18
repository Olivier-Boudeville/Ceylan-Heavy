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


#ifndef CEYLAN_LOCALE_H_
#define CEYLAN_LOCALE_H_


#include "CeylanTypes.h"       // for Ceylan::Uint8
#include "CeylanException.h"   // for inheritance

#include <string>
#include <list>


namespace Ceylan
{

	
	/// Exception raised in case of localization issues.
	class CEYLAN_DLL LocalizationException: public Ceylan::Exception
	{

		public:

			explicit LocalizationException( const std::string & reason ) ;
			virtual ~LocalizationException() throw() ;

	} ;


	// Numerical code corresponding to a specific localization setting.
	typedef Ceylan::Uint16 LocaleCode ;
	
	
	
    /**
     * Describes the localization settings, notably for a given application.
	 *
     */
    class CEYLAN_DLL LocalizationSettings : public TextDisplayable
    {
	
		
        public:


            /**
             * Constructs an instance to localization settings.
			 *
             */
            explicit LocalizationSettings() ;


            /// Virtual destructor.
            virtual ~LocalizationSettings() throw() ;



			// Locale support section.
			

			/**
			 * Adds a new supported locale.
			 *
			 * @throw LocalizationException if the locale is not a 
			 * known one.
			 *
			 */
			virtual void addSupportedLocale( LocaleCode code ) ;
			
			
			
			/**
			 * Adds a new supported locale.
			 *
			 * @throw LocalizationException if the locale is not a 
			 * known one.
			 *
			 */
			virtual void addSupportedLocale( const std::string & localeName ) ;
			
			
			
			/**
			 * Returns true iff the locale corresponding to specified code is
			 * supported.
			 *
			 */
			virtual bool isSupported( LocaleCode code ) ;
			
			
			
			/**
			 * Returns true iff the specified locale is supported.
			 *
			 */
			virtual bool isSupported( const std::string & localeName ) ;
			
			 
			 
			/**
			 * Returns the list of the codes of all supported locales.
			 *
			 */
			virtual const std::list<LocaleCode> & getSupportedLocales() const ;
			
			
			
			 
			 
			// Current locale section.
			
			
			/// Returns true iff a current locale is set.
			virtual bool hasCurrentLocale() const ;
			
			
			/**
			 * Returns the code of the current locale in use.
			 *
			 * @throw LocalizationException if none is set.
			 *
			 */
			virtual LocaleCode getCurrentLocaleCode() const	;
			
			
			
			/**
			 * Returns the name of the current locale in use.
			 *
			 * @throw LocalizationException if none is set.
			 *
			 */
			virtual const std::string & getCurrentLocaleName() const ;
			
			
			
			/**
			 * Sets the current locale from its code.
			 *
			 * @throw LocalizationException if the operation failed.
			 *
			 */
			virtual void setCurrentLocale( LocaleCode code ) ;
			
			
			/**
			 * Sets the current locale from its name.
			 *
			 * @throw LocalizationException if the operation failed.
			 *
			 */
			virtual void setCurrentLocale( const std::string & localeName ) ;
			
			
			
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
            virtual const std::string toString( VerbosityLevels level = high )
				const ;



			/**
			 * Returns true iff the specified locale name is a known locale.
			 *
			 */
			static bool IsAKnownLocale( const std::string & localeName ) ;
				
				
			/**
			 * Returns the locale code corresponding to the specified locale
			 * name.
			 *
			 * @throw LocalizationException if the locale name does not 
			 * correspond to any known locale.
			 *
			 */
			static LocaleCode GetLocaleCodeFromName( 
					const std::string & localeName ) ;	
			
			
			/**
			 * Returns the locale name corresponding to the specified locale
			 * code.
			 *
			 * @throw LocalizationException if the locale code does not 
			 * correspond to any known locale.
			 *
			 */
			static const std::string & GetLocaleNameFromCode( 
				LocaleCode code ) ;	
			
			
			
			// Known locales listed here:
				
			static const LocaleCode EnglishLocale ;
			static const LocaleCode FrenchLocale ;
			static const LocaleCode GermanLocale ;




        protected:

/* 
 * Takes care of the awful issue of Windows DLL with templates.
 *
 * @see Ceylan's developer guide and README-build-for-windows.txt 
 * to understand it, and to be aware of the associated risks. 
 * 
 */
#pragma warning( push )
#pragma warning( disable : 4251 )

			/// The list of supported locales.
			std::list<LocaleCode> _supportedLocales ;

#pragma warning( pop ) 			

			/// The current locale in use.
			LocaleCode _currentLocale ;
			



		private:
		
		
			/**
			 * Copy constructor made private to ensure that it will be never
			 * called.
			 * The compiler should complain whenever this undefined operator 
			 * is called, implicitly or not.
			 * 
			 */
			LocalizationSettings( const LocalizationSettings & source ) ;
			
			
			
			/**
			 * Assignment operator made private to ensure that it will be never
			 * called.
			 * The compiler should complain whenever this undefined operator is
			 * called, implicitly or not.
			 * 
			 */			 
			LocalizationSettings & operator = ( 
				const LocalizationSettings & source ) ;


    } ;

}



#endif // CEYLAN_LOCALE_H_

