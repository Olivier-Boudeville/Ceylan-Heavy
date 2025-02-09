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


#include "CeylanUnicode.h"


#include "CeylanUtils.h"      // for Ceylan::swapBytes
#include "CeylanOperators.h"  // for Ceylan::toString


using std::string ;

using namespace Ceylan ;


/*
 * The first Unicode support was heavily inspired from SDL_ttf-2.0.7
 * (SDL_ttf.c).
 *
 */



UnicodeString::UnicodeString() :
	_buffer( 0 )
{

}



UnicodeString::~UnicodeString() throw()
{

	if ( _buffer != 0 )
		delete [] _buffer ;

}



Ceylan::StringSize UnicodeString::size() const
{

	/*
	 * Empty strings can be coded two different ways: no buffer or a buffer with
	 * only \0 in it.
	 *
	 */

	if ( _buffer == 0 )
		return 0 ;

	Ceylan::StringSize res = 0 ;

	Unicode * p = _buffer ;

	while ( *( p++ ) )
		res++ ;

	return res ;

}



UnicodeString & UnicodeString::copy( bool swap ) const
{

	Ceylan::StringSize sourceSize = size() ;

	UnicodeString & res = * new UnicodeString() ;

	if ( sourceSize == 0 )
		return res ;

	res.setCapacity( sourceSize ) ;

	Unicode * from = _buffer ;
	Unicode * to   = res._buffer ;

	if ( swap )
	{
		while ( *from != 0 )
		{
			*to = Ceylan::swapBytes( *from ) ;
			from++ ;
			to++ ;
		}
	}
	else
	{
		while ( *from != 0  )
		{
			*to = *from ;
			from++ ;
			to++ ;
		}
	}

	*to = '\0' ;

	return res ;

}



void UnicodeString::setFromLatin1( const std::string & latin1String )
{

	setCapacity( latin1String.size() ) ;

	Ceylan::StringSize i = 0 ;
	for ( string::const_iterator it = latin1String.begin();
		it != latin1String.end(); it++ )
	{
		_buffer[i] = static_cast<unsigned char>( *it ) ;
		i++ ;
	}
	_buffer[i] = 0 ;

}



void UnicodeString::setFromUTF8( const std::string & utf8String,
	Ceylan::StringSize characterCount )
{

	/*
	 * Decodes UTF-8 characters, encoded into 1, 2, 3 or 4 bytes in a row.
	 * Total character count must be specified.
	 *
	 */

	setCapacity( characterCount ) ;

	Unicode uniChar ;
	Ceylan::StringSize index = 0 ;


	for ( string::const_iterator it = utf8String.begin();
		it != utf8String.end(); it++ )
	{
		uniChar = static_cast<unsigned char>( *it ) ;

		if ( uniChar >= 0xF0 )
		{
			uniChar  = static_cast<Unicode>( (*it) & 0x07 ) << 18 ;
			it++ ;
			uniChar |= static_cast<Unicode>( (*it) & 0x3F ) << 12 ;
			it++ ;
			uniChar |= static_cast<Unicode>( (*it) & 0x3F ) <<  6 ;
			it++ ;
			uniChar |= static_cast<Unicode>( (*it) & 0x3F ) ;

		}
		else
		{
			if ( uniChar >= 0xE0 )
			{
				uniChar  = static_cast<Unicode>( (*it) & 0x0F ) << 12 ;
				it++ ;
				uniChar |= static_cast<Unicode>( (*it) & 0x3F ) <<  6 ;
				it++ ;
				uniChar |= static_cast<Unicode>( (*it) & 0x3F ) ;
			}
			else
			{
				if ( uniChar >= 0xC0 )
				{
					uniChar  = static_cast<Unicode>( (*it) & 0x1F ) << 6 ;
					uniChar |= static_cast<Unicode>( (*it) & 0x3F ) ;
				}
				// else: one byte character in [0;0xC0[ , already managed.
			}
		}

		_buffer[index] = uniChar ;
		index++ ;
	}

	_buffer[index] = 0 ;

}



const string UnicodeString::toString( Ceylan::VerbosityLevels level ) const
{

	return "Unicode string made of " + Ceylan::toString(
		static_cast<Ceylan::Uint32>( size() ) )
		+ " characters" ;

}



Unicode UnicodeString::ConvertFromLatin1( Ceylan::Latin1Char latin1Char )
{

	return static_cast<Unicode>( latin1Char ) ;

}



void UnicodeString::setCapacity( Ceylan::StringSize newSize )
{

	if ( newSize == size() )
		return ;

	if ( _buffer != 0 )
		delete [] _buffer ;

	_buffer = new Unicode[ newSize ] ;

}
