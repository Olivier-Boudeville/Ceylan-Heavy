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



LocalizationSettings::LocalizationSettings() throw( LocalizationException )
{

}



LocalizationSettings::~LocalizationSettings() throw()
{

}



void LocalizationSettings::addSupportedLocale( const string & localeName )
	throw( LocalizationException )
{

	if ( ! IsAKnownLocale( localeName ) )
		throw LocalizationException( 
			"LocalizationSettings::addSupportedLocale failed: locale '"
			+ localeName + "' not known." ) ;
	
	LocaleCode code = GetLocaleCodeFromName( localeName ) ;
		
		
	// Adds only if not already there:		
	if ( ! isSupported( code ) )
		_supportedLocales.push_back( code ) ;
		
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



const list<LocaleCode> & LocalizationSettings::getSupportedLocales() 
	const throw()
{

	return _supportedLocales ;
	
}



const string LocalizationSettings::toString( VerbosityLevels level ) 
	const throw()
{

	if ( _supportedLocales.empty() )
		return "No locale supported" ;
		
	list<string> res ;
	
	for ( list<LocaleCode>::const_iterator it = _supportedLocales.begin();
			it != _supportedLocales.end(); it++ )
		res.push_back( GetLocaleNameFromCode( *it ) ) ;
		
	return "Supported locales are: " + Ceylan::formatStringList( res ) ;
	
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
				
				
				
const string & LocalizationSettings::GetLocaleNameFromCode( LocaleCode code ) 
	throw( LocalizationException )				
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

