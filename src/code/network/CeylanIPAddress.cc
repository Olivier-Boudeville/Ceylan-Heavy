#include "CeylanIPAddress.h"

using std::string ;


using namespace Ceylan::Network ;




IPAddress::IPAddress() throw() : Ceylan::TextDisplayable() 
{

}


IPAddress::~IPAddress() throw()
{

}


const string IPAddress::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{
	return "Abstract IP address" ;
}


bool IPAddress::IsValid( const std::string IPString ) throw() 
{

	// Abstract addresses cannot be valid :
	return false ;
	
}	
