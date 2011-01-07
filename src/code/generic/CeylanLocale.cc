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


#include "CeylanLocale.h"

#include "CeylanStringUtils.h"   // for formatStringList
#include "CeylanOperators.h"     // for toString


using std::string ;
using std::list ;

#include <algorithm>

using namespace Ceylan ;



LocalizationException::LocalizationException( const string & reason ) :
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



LocalizationSettings::LocalizationSettings() :
	_currentLocale( 0 )
{

}



LocalizationSettings::~LocalizationSettings() throw()
{

}



void LocalizationSettings::addSupportedLocale( LocaleCode code )
{
	
	// Adds only if not already there:		
	if ( ! isSupported( code ) )
		_supportedLocales.push_back( code ) ;
		
}



void LocalizationSettings::addSupportedLocale( const string & localeName )
{

	if ( ! IsAKnownLocale( localeName ) )
		throw LocalizationException( 
			"LocalizationSettings::addSupportedLocale failed: locale '"
			+ localeName + "' not known." ) ;
		
	addSupportedLocale( GetLocaleCodeFromName( localeName ) ) ;
	
}



bool LocalizationSettings::isSupported( LocaleCode code )
{
	
	return ( std::find( _supportedLocales.begin(), _supportedLocales.end(), 
		code ) != _supportedLocales.end() ) ;

}



bool LocalizationSettings::isSupported( const std::string & localeName )
{

	if ( ! IsAKnownLocale( localeName ) )
		return false ;
	
	// No exception should be raised:	
	return isSupported( GetLocaleCodeFromName(localeName) ) ;
	
}


				
const std::list<LocaleCode> & LocalizationSettings::getSupportedLocales() const
{

	return _supportedLocales ;
	
}



bool LocalizationSettings::hasCurrentLocale() const
{

	return ( _currentLocale != 0 ) ;
	
}



LocaleCode LocalizationSettings::getCurrentLocaleCode() const 
{

	if ( _currentLocale == 0 )
		throw LocalizationException( 
			"LocalizationSettings::getCurrentLocaleCode failed: "
			"no current locale set." ) ;
			
	return _currentLocale ;	
}

				
				
const std::string & LocalizationSettings::getCurrentLocaleName() const
{

	return GetLocaleNameFromCode( getCurrentLocaleCode() ) ;
	
}

				
								
void LocalizationSettings::setCurrentLocale( LocaleCode code ) 
{

	if ( ! isSupported( code ) )
		throw LocalizationException( 
			"LocalizationSettings::setCurrentLocale failed for locale code "
				+ Ceylan::toString( code ) + ": locale not supported." ) ;
				
	_currentLocale = code ;
	
}
														


void LocalizationSettings::setCurrentLocale( const std::string & localeName )	
{

	setCurrentLocale( GetLocaleCodeFromName( localeName ) ) ;
	
}
														


const std::string LocalizationSettings::toString( VerbosityLevels level ) const
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



bool LocalizationSettings::IsAKnownLocale( const string & localeName )
{

	// Clumsy and stupid, as C++ is: (could also use GetLocaleCodeFromName)
	
	if ( localeName == "english" || localeName == "french" 
			|| localeName == "german" )
		return true ;
	
	return false ;
		
}



LocaleCode LocalizationSettings::GetLocaleCodeFromName( 
	const string & localeName ) 
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
	LocaleCode code ) 				
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

