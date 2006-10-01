#include "CeylanIdentifierOwner.h"

#include "CeylanIdentifier.h"        // for CeylanIdentifier

#include "CeylanUtils.h"             // for emergencyShutdown


#if CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"            // for CEYLAN_DEBUG
#endif // CEYLAN_USES_CONFIG_H


using std::string ;

using namespace Ceylan ;


IdentifierNotAvailableException::IdentifierNotAvailableException(
    	const string & message ) throw() :
	Ceylan::Exception( message )
{

}


IdentifierNotAvailableException::~IdentifierNotAvailableException() throw()
{

}



IdentifierOwner::IdentifierOwner() throw() :
	_id( 0 )
{

}


IdentifierOwner::~IdentifierOwner() throw()
{

    if ( hasIdentifier() )
        deleteIdentifier() ;

}


Identifier & IdentifierOwner::getIdentifier() 
	const throw( IdentifierNotAvailableException )
{

    if ( _id != 0 )
    {
        return * _id ;
    }
    else
    {
        throw IdentifierNotAvailableException(
            "No available identifier to return for "
			" IdentifierOwner::getIdentifier" ) ;
    }

}


void IdentifierOwner::setIdentifier( Identifier & id )
	throw( IdentifierNotAvailableException )
{

    if ( _id != 0 )
    {
        throw IdentifierNotAvailableException( 
			"IdentifierOwner::setIdentifier unable to assign "
            "new identifier cause a previous one is still available." ) ;
    }
    else
    {
        _id = & id ;
    }

}


bool IdentifierOwner::hasIdentifier() const throw()
{
    return ( _id != 0 ) ;
}


void IdentifierOwner::deleteIdentifier() throw()
{


#if CEYLAN_DEBUG

    if ( _id != 0 )
    {
        delete _id ;
		_id = 0 ;
    }
    else
    {
        Ceylan::emergencyShutdown( 
			"IdentifierOwner::deleteIdentifier : trying to "
			"delete a non-existent identifier." ) ;
    }

#else // CEYLAN_DEBUG

    delete _id ;
	_id = 0 ;

#endif // CEYLAN_DEBUG

}


const string IdentifierOwner::toString( VerbosityLevels level ) const throw()
{

	if ( _id != 0 )
	{
		return "This identifier owner has a registered identifier : "
			+ _id->toString( level ) ;
	}	
	else
	{		
		return "This identifier owner has no registered identifier" ;
	}	

}
  

