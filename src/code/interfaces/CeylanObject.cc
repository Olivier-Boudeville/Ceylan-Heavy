#include "CeylanObject.h"

#include "CeylanNetwork.h"             // for getMostPreciseLocalHostName
#include "CeylanProcess.h"             // for getHostingPID
#include "CeylanTextIdentifier.h"
#include "CeylanOperators.h"
#include "CeylanLogLight.h"            // for CEYLAN_LOG
#include "CeylanLogPlug.h"             // for LogPlug
#include "CeylanObjectIdentifier.h"    // for constructor


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"              // for CEYLAN_DEBUG
#endif // CEYLAN_USES_CONFIG_H


#include <typeinfo>   // for typeid
#include <cctype>     // for isdigit


using std::string ;

using namespace Ceylan ;
using namespace Ceylan::Log ;




Object::Object( bool trackInstance, bool dropIdentifierOnExit ) 
		throw( LogException ) : 
	IdentifierOwner(),
	Loggable( "Unknown object" ),
	_trackInstance( trackInstance ) 
{

	CEYLAN_LOG( "Object entering constructor" ) ; 
	
	if ( _trackInstance )
	{
	
		CEYLAN_LOG( "Object sending its first message "
			"just after allocation." ) ;
			
		send( "Being allocated now." ) ;
		
		/*
		 * Used to force rebuilding of its identifier so that it 
		 * is not mangled :
		 *
		 */
		if ( dropIdentifierOnExit )
			dropIdentifier() ;
			
	}	
	
}


Object::~Object() throw()
{

	if ( _trackInstance )
	{
	
		CEYLAN_LOG( "Object sending its last message "
			"just before deallocation." ) ;	
				
		send( "Being deallocated now." ) ;
		
	}
		
}


const std::string Object::getClassName() const throw()
{

    string className = typeid( * this ).name() ;
	
	/*
	 * On g++, this name is prefixed with the length in characters of the name
	 * (Log would be 3Log), so we remove this numerical prefix.
	 *
	 */
	 
	Ceylan::Uint16 i = 0 ; 
	 
	while ( ::isdigit( className[ i ] ) ) 
		i++ ;
	
#if CEYLAN_DEBUG

	string result = className.substr( i ) ;	 
	CEYLAN_LOG( "Object::getClassName is : " + result ) ;
	return result ;
	
#else // CEYLAN_DEBUG
		
	return className.substr( i ) ;	
	 
#endif // CEYLAN_DEBUG
	
}


bool Object::isOfSameType( const Object & other ) const throw()
{
    return ( getClassName() == other.getClassName() ) ;
}


void Object::logState( Ceylan::VerbosityLevels level ) throw()
{
	send( toString( level ) ) ;
}


void Object::send( const string & message, LevelOfDetail levelOfDetail ) 
	throw( LogException )
{

	CEYLAN_LOG( "Object::send : will send message " + message ) ;
	
	if ( _id == 0 )
	{
		forgeIdentifier() ;
		CEYLAN_LOG( "Object::send : channel name set to " 
			+ _id->toString() ) ;
		setChannelName( _id->toString() ) ;
	}	
	
	CEYLAN_LOG( "Object::send : effective sending of message " 
		+ message ) ;
	
	Loggable::send( message, levelOfDetail ) ;	
	
}


const string Object::toString( Ceylan::VerbosityLevels level ) 
	const throw() 
{

	string result ;
	
	if ( _trackInstance )
		result = "This Ceylan object instance's "
			"life cycle is monitored. " ;
	else
		result = "No monitoring for this Ceylan object "
			"instance's life cycle. " ;
	
	result += Loggable::toString( level ) + ". " 
		+ IdentifierOwner::toString( level ) ; 

	return result ;
	
}
	
	
void Object::forgeIdentifier() throw()
{
	
	CEYLAN_LOG( "Object::forgeIdentifier : new identifier required." ) ;
	
	if ( _id != 0 )
		dropIdentifier() ;
		
	CEYLAN_LOG( "Object::forgeIdentifier : "
		"creating new object identifier." ) ;
	
	/*
	 * This was the place where using this instead of *this caused 
	 * the object constructor to be mistakenly called 
	 * (this pointer being converted to bool, and Object constructor
	 * had not the explicit keyword).
	 *
	 */
	 _id = new ObjectIdentifier( * this ) ;
		
	CEYLAN_LOG( "Object::forgeIdentifier : new ID is " 
		+ _id->toString() ) ;

}	


void Object::dropIdentifier() throw()
{

	CEYLAN_LOG( "Object dropIdentifier : "
		"getting rid of mangled identifier." ) ;
		
	delete _id ;
	_id = 0 ;	
		
}
