#include "CeylanLocale.h"

#include "CeylanStringUtils.h"   // for formatStringList
#include "CeylanOperators.h"     // for toString


using std::string ;
using std::list ;

#include <algorithm>

using namespace Ceylan ;


LocalizationException::LocalizationException( const string & reason ) throw() :
	Ceylan::Exception( reason )
{

}


LocalizationException::~LocalizationException()	throw()
{

}



/*
 * Known locales listed here:
 * (IsAKnownLocale, GetLocaleCodeFromName and GetLocaleNameFromCode must be
 * updated if that list is updated)
 *
 */

const Ceylan::LocaleCode LocalizationSettings::EnglishLocale = 1 ;
const Ceylan::LocaleCode LocalizationSettings::FrenchLocale  = 2 ;
const Ceylan::LocaleCode LocalizationSettings::GermanLocale  = 3 ;

const std::string EnglishLocaleName = "english" ;
const std::string FrenchLocaleName  = "french" ;
const std::string GermanLocaleName  = "german" ;



LocalizationSettings::LocalizationSettings() throw( LocalizationException ):
	_currentLocale( 0 )
{

}



LocalizationSettings::~LocalizationSettings() throw()
{

}



void LocalizationSettings::addSupportedLocale( LocaleCode code )
	throw( LocalizationException )
{
	
		
	// Adds only if not already there:		
	if ( ! isSupported( code ) )
		_supportedLocales.push_back( code ) ;
		
}


void LocalizationSettings::addSupportedLocale( const string & localeName )
	throw( LocalizationException )
{

	if ( ! IsAKnownLocale( localeName ) )
		throw LocalizationException( 
			"LocalizationSettings::addSupportedLocale failed: locale '"
			+ localeName + "' not known." ) ;
		
	addSupportedLocale( GetLocaleCodeFromName( localeName ) ) ;
	
}



bool LocalizationSettings::isSupported( LocaleCode code ) throw()
{
	
	return ( std::find( _supportedLocales.begin(), _supportedLocales.end(), 
		code ) != _supportedLocales.end() ) ;

}



bool LocalizationSettings::isSupported( const std::string & localeName ) throw()
{

	if ( ! IsAKnownLocale( localeName ) )
		return false ;
	
	// No exception should be raised:	
	return isSupported( GetLocaleCodeFromName(localeName) ) ;
	
}


				
const std::list<LocaleCode> & LocalizationSettings::getSupportedLocales() 
	const throw()
{

	return _supportedLocales ;
	
}




bool LocalizationSettings::hasCurrentLocale() const throw()
{

	return ( _currentLocale != 0 ) ;
	
}



LocaleCode LocalizationSettings::getCurrentLocaleCode() const 
	throw( LocalizationException )
{

	if ( _currentLocale == 0 )
		throw LocalizationException( 
			"LocalizationSettings::getCurrentLocaleCode failed: "
			"no current locale set." ) ;
			
	return _currentLocale ;	
}

				
				
const std::string & LocalizationSettings::getCurrentLocaleName() const 
	throw( LocalizationException )				
{

	return GetLocaleNameFromCode( getCurrentLocaleCode() ) ;
	
}

				
								
void LocalizationSettings::setCurrentLocale( LocaleCode code ) 
	throw( LocalizationException )
{

	if ( ! isSupported( code ) )
		throw LocalizationException( 
			"LocalizationSettings::setCurrentLocale failed for locale code "
				+ Ceylan::toString( code ) + ": locale not supported." ) ;
				
	_currentLocale = code ;
	
}
														


void LocalizationSettings::setCurrentLocale( const std::string & localeName ) 
	throw( LocalizationException )
{

	setCurrentLocale( GetLocaleCodeFromName( localeName ) ) ;
	
}
														


const std::string LocalizationSettings::toString( VerbosityLevels level ) 
	const throw()
{

	if ( _supportedLocales.empty() )
		return "No locale supported" ;
	
	string currentMessage ;
	
	if ( hasCurrentLocale() )
		currentMessage = "Current locale is " + getCurrentLocaleName() ;
	else
		currentMessage = "No current locale set" ;
			
	list<string> res ;
	
	for ( list<LocaleCode>::const_iterator it = _supportedLocales.begin();
			it != _supportedLocales.end(); it++ )
		res.push_back( GetLocaleNameFromCode( *it ) ) ;
		
	return currentMessage + ". Supported locales are: " 
		+ Ceylan::formatStringList( res ) ;
	
}



bool LocalizationSettings::IsAKnownLocale( const string & localeName ) throw()
{

	// Clumsy and stupid, as C++ is: (could also use GetLocaleCodeFromName)
	
	if ( localeName == "english" || localeName == "french" 
			|| localeName == "german" )
		return true ;
	
	return false ;
		
}



LocaleCode LocalizationSettings::GetLocaleCodeFromName( 
	const string & localeName ) throw( LocalizationException )
{

	if ( localeName == EnglishLocaleName )
		return EnglishLocale ;
		
	if ( localeName == FrenchLocaleName )
		return FrenchLocale ;
		
	if ( localeName == GermanLocaleName )
		return GermanLocale ;
		
	throw LocalizationException( 
		"LocalizationSettings::GetLocaleCodeFromName failed: unknown locale: "
		+ localeName ) ;
		
}	
				
				
				
const std::string & LocalizationSettings::GetLocaleNameFromCode( 
	LocaleCode code ) throw( LocalizationException )				
{

	switch( code )
	{
	
		case EnglishLocale:
			return EnglishLocaleName ;
			break ;
	
		case FrenchLocale:
			return FrenchLocaleName ;
			break ;
	
		case GermanLocale:
			return GermanLocaleName ;		
			break ;
		
		default:
			throw LocalizationException( 
				"LocalizationSettings::GetLocaleNameFromCode failed: "
				"unknown locale code " + Ceylan::toString( code ) ) ;
			break ;
				
	}
	
}

