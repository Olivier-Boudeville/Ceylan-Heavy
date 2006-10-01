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
 
 
UnicodeString::UnicodeString() throw() :
	_buffer( 0 )
{

}


UnicodeString::~UnicodeString() throw()
{
	if ( _buffer != 0 )
		delete [] _buffer ;
}


Ceylan::StringSize UnicodeString::size() const throw()
{

	/*
	 * Empty strings can be coded two different ways : no buffer or a buffer 
	 * with only \0 in it.
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


UnicodeString & UnicodeString::copy( bool swap ) const throw()
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


void UnicodeString::setFromLatin1( const std::string & latin1String ) throw()
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
	Ceylan::StringSize characterCount ) throw()
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
			uniChar  =  static_cast<Unicode>( (*it) & 0x07 ) << 18 ;
			it++ ;
			uniChar |=  static_cast<Unicode>( (*it) & 0x3F ) << 12 ;
			it++ ;
			uniChar |=  static_cast<Unicode>( (*it) & 0x3F ) <<  6 ;
			it++ ;
			uniChar |=  static_cast<Unicode>( (*it) & 0x3F ) ;		
		
		}
		else
		{
			if ( uniChar >= 0xE0 ) 
			{
				uniChar  =  static_cast<Unicode>( (*it) & 0x0F ) << 12 ;
				it++ ;
				uniChar |=  static_cast<Unicode>( (*it) & 0x3F ) <<  6 ;
				it++ ;
				uniChar |=  static_cast<Unicode>( (*it) & 0x3F ) ;
			} 
			else
			{
				if ( uniChar >= 0xC0 ) 
				{
					uniChar  =  static_cast<Unicode>( (*it) & 0x1F ) << 6 ;
					uniChar |=  static_cast<Unicode>( (*it) & 0x3F ) ;
				}
				// else : one byte character in [0;0xC0[ , already managed.
			}
		}

		_buffer[index] = uniChar ;
		index++ ;
	}	
	
	_buffer[index] = 0 ;
		
}


const string UnicodeString::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{
	return "Unicode string made of " + Ceylan::toString( 
		static_cast<Ceylan::Uint32>( size() ) ) 
		+ " characters" ;
}


Unicode UnicodeString::ConvertFromLatin1( Ceylan::Latin1Char latin1Char )
	throw()
{
	return static_cast<Unicode>( latin1Char ) ;
}


void UnicodeString::setCapacity( Ceylan::StringSize newSize ) throw()
{

	if ( newSize == size() )
		return ;
	
	if ( _buffer != 0 )
		delete [] _buffer ;
	
	_buffer = new Unicode[ newSize ] ;
			
}

