#include "CeylanException.h"

using std::string ;

using namespace Ceylan ;



Exception::Exception( const string & reason ) throw() :
	_reason( reason )
{
    
}


Exception::~Exception() throw()
{

}


const string Exception::toString( VerbosityLevels level ) const throw()
{
    return ( _reason ) ;
}


const char * Exception::what() const throw()
{
    return toString( high ).c_str() ;
}


std::ostream & operator << ( std::ostream & os, const Exception & e ) throw()
{
    return os << e.toString( high ) ;
}
