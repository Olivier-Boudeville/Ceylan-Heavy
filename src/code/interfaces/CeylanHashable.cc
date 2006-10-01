#include "CeylanHashable.h"


using std::string ;

#include "CeylanOperators.h"  // for toString



using namespace Ceylan ;



Hashable::Hashable() throw() 
{
}


Hashable::~Hashable() throw() 
{
}


const string Hashable::toString( VerbosityLevels level ) const throw()
{

	if ( level == Ceylan::low )
		return "Hashable object" ; 
	
	if ( level == Ceylan::medium )
		return "Hashable object whose weak hash code is " 
			+ Ceylan::toString( getWeakHashCode() ) ; 
	
	return "Hashable object whose strong hash code is " 
			+ Ceylan::toString( getStrongHashCode() ) ; 
}	
		

WeakHashCode Hashable::GetWeakHashCode( const std::string & stringToHash ) throw()
{

	WeakHashCode hash = 0 ;
 
	Ceylan::Uint32 charCount = 0 ;
	
	while ( stringToHash[charCount] )
	{

		hash <<= 1 ;

		hash = hash ^ stringToHash[charCount] ;
		charCount++ ;
		
	}

	return hash ;
}


StrongHashCode Hashable::GetStrongHashCode( const std::string & stringToHash ) throw()
{

	StrongHashCode hash = 0 ;
 
	Ceylan::Uint32 charCount = 0 ;
	
	while ( stringToHash[charCount] )
	{

		hash <<= 1 ;

		hash = hash ^ stringToHash[charCount] ;
		charCount++ ;
		
	}

	return hash ;
}

