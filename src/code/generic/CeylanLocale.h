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

			explicit LocalizationException( const std::string & reason ) 
				throw() ;
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
            explicit LocalizationSettings() 
				throw( LocalizationException ) ;


            /// Virtual destructor.
            virtual ~LocalizationSettings() throw() ;


			/**
			 * Adds a new supported locale.
			 *
			 * @throw LocalizationException if the locale is not a 
			 * known one.
			 *
			 */
			virtual void addSupportedLocale( const std::string & localeName )
				throw( LocalizationException ) ;
			
			
			/**
			 * Returns true iff the locale corresponding to specified code is
			 * supported.
			 *
			 */
			virtual bool isSupported( LocaleCode code ) throw() ;
			
			
			/**
			 * Returns true iff the specified locale is supported.
			 *
			 */
			virtual bool isSupported( const std::string & localeName ) throw() ;
			
			 
			
			/**
			 * Returns the list of the codes of all supported locales.
			 *
			 */
			virtual const std::list<LocaleCode> & getSupportedLocales() 
				const throw() ;
			
			
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
				const throw() ;



			/**
			 * Returns true iff the specified locale name is a known locale.
			 *
			 */
			static bool IsAKnownLocale( const std::string & localeName ) 
				throw() ;
				
				
			/**
			 * Returns the locale code corresponding to the specified locale
			 * name.
			 *
			 * @throw LocalizationException if the locale name does not 
			 * correspond to any known locale.
			 *
			 */
			static LocaleCode GetLocaleCodeFromName( 
					const std::string & localeName ) 
				throw( LocalizationException ) ;	
			
			
			/**
			 * Returns the locale name corresponding to the specified locale
			 * code.
			 *
			 * @throw LocalizationException if the locale code does not 
			 * correspond to any known locale.
			 *
			 */
			static const std::string & GetLocaleNameFromCode( LocaleCode code ) 
				throw( LocalizationException ) ;	
			
			
			
			// Known locales listed here:
				
			static const LocaleCode EnglishLocale ;
			static const LocaleCode FrenchLocale ;
			static const LocaleCode GermanLocale ;



        protected:


			std::list<LocaleCode> _supportedLocales ;



		private:
		
		
			/**
			 * Copy constructor made private to ensure that it will be never
			 * called.
			 * The compiler should complain whenever this undefined operator 
			 * is called, implicitly or not.
			 * 
			 */
			LocalizationSettings( const LocalizationSettings & source ) 
				throw() ;
			
			
			/**
			 * Assignment operator made private to ensure that it will be never
			 * called.
			 * The compiler should complain whenever this undefined operator is
			 * called, implicitly or not.
			 * 
			 */			 
			LocalizationSettings & operator = ( 
				const LocalizationSettings & source ) throw() ;


    } ;

}



#endif // CEYLAN_LOCALE_H_

