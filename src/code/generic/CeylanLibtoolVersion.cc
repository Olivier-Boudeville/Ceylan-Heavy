#include "CeylanLibtoolVersion.h"

#include "CeylanOperators.h"  // for toNumericalString

#include <list>


using std::string ;

using namespace Ceylan ;



LibtoolVersion::LibtoolVersion( VersionNumber current, 
		VersionNumber revision, VersionNumber age ) throw( VersionException ):
	Version( current, revision, age )
{

	checkConsistency() ;
	
}



LibtoolVersion::LibtoolVersion( const std::string & versionText ) 
		throw( VersionException ):
	Version( versionText )
{

	checkConsistency() ;
			
}

 
 
LibtoolVersion::~LibtoolVersion() throw()
{

}



Version::VersionNumber LibtoolVersion::getCurrentInterfaceNumber() const throw()
{

	return _major ;
	
}



void LibtoolVersion::setCurrentInterfaceNumber( VersionNumber newCurrent )
	throw()
{

	_major = newCurrent ;
	
}



Version::VersionNumber LibtoolVersion::getRevisionInterfaceNumber() 
	const throw()
{

	return _minor ;
	
}



void LibtoolVersion::setRevisionInterfaceNumber( VersionNumber newRevision )
	throw()
{

	_minor = newRevision ;
	
}



Version::VersionNumber LibtoolVersion::getInterfaceAge() const throw()
{

	return _release ;
	
}



void LibtoolVersion::setInterfaceAge( VersionNumber newAge ) throw()
{

	_release = newAge ;
	
}



bool LibtoolVersion::isCompatibleWith( const Version & expectedVersion ) const
	throw( VersionException )
{
  	
 	if ( ! canBeComparedWith( expectedVersion ) )
		throw VersionException( "LibtoolVersion::isCompatibleWith: "
			+ toString( Ceylan::high ) + " and "
			+ expectedVersion.toString( Ceylan::high ) 
			+ " cannot be compared." ) ;
		
	// The cast has been checked by previous call:	
 	const LibtoolVersion * originalVersion = 
		dynamic_cast<const LibtoolVersion *>( & expectedVersion ) ;
	
	VersionNumber originalInterfaceNumber 
		= originalVersion->getCurrentInterfaceNumber() ;
		
	
	/* 
	 * The user program is expecting to link with original version, let's 
	 * check whether this version is the same, or if it is more recent yet
	 * still compatible.
	 * If older, no hope of compatibility.
	 *
	 */
	 
	// Same is ok. 
	if ( getCurrentInterfaceNumber() == originalInterfaceNumber )
		return true ;
	
	// Older is ko.	
	if ( getCurrentInterfaceNumber() < originalInterfaceNumber )
		return false ;
		
	/*
	 * Here this library version is more recent than the user-expected one,
	 * age will tell if compatibility is ensured:
	 *
	 */	
	return ( originalInterfaceNumber >=
		( getCurrentInterfaceNumber() - getInterfaceAge() ) ) ; 
		
}



const string LibtoolVersion::toString( VerbosityLevels level ) const throw()
{


	string rawVersion = Version::toString( low ) ;
			
	if ( level == Ceylan::low )
    	return rawVersion ;
		
	if ( level == Ceylan::medium )
    	return "Libtool version " + rawVersion ;
				
	return "Libtool version: current interface number = " 
		+ Ceylan::toNumericalString( _major ) 
		+ ", current revision number = "
		+ Ceylan::toNumericalString( _minor ) + ", interface age = "
		+ Ceylan::toNumericalString( _release ) ;
		
}



bool LibtoolVersion::isUsualVersionSchemeCompliant() const throw()
{

	return false ;
	
}



bool LibtoolVersion::canBeComparedWith( const Version & version ) const throw()
{	
	
	// Only Libtool versions can be compared with Libtool versions:
	
	return ( dynamic_cast<const LibtoolVersion *>( & version ) != 0 ) ;
	
}



void LibtoolVersion::checkConsistency() const throw( VersionException )
{

	/*
	 * More restrictions apply to Libtool versions, age must be less than or
	 * equal to current:
	 *
	 */
	
	if ( _release > _major )
		throw VersionException( "Incorrect Libtool version: interface age ("
			+ Ceylan::toString( _release ) 
			+ ") must be less than or equal to current ("
			+ Ceylan::toString( _major ) + ")." ) ;

}

