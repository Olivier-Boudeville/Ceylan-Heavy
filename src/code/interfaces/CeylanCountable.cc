#include "CeylanCountable.h"


#include "CeylanUtils.h"     // for emergencyShutdown


#include "CeylanOperators.h"
#include "CeylanLogLight.h"




using std::string ;

using namespace Ceylan ;


Countable::InstanceCount Countable::ReferenceCount = 0 ;
Countable::InstanceCount Countable::MaximumReferenceCount = 0 ;

const string Countable::LogPrefix = "[Instance count]" ;


Countable::Countable( bool verbose ) throw() : 
	_verbose( verbose ) 
{

	ReferenceCount++ ;
	
	if ( ReferenceCount > MaximumReferenceCount )
		MaximumReferenceCount = ReferenceCount ;
		
	if ( _verbose )
	{
		CEYLAN_LOG( LogPrefix + " one more : " + Ceylan::toString( ReferenceCount )
			+ "/" + Ceylan::toString( MaximumReferenceCount ) ) ;
	}
			
}


Countable::~Countable() throw()
{

	if ( ReferenceCount == 0 )
		Ceylan::emergencyShutdown( "Countable destructor : negative reference count detected." ) ;
		
	ReferenceCount-- ;
	
	if ( _verbose )
	{
		CEYLAN_LOG( LogPrefix + " one less : " + Ceylan::toString( ReferenceCount )
			+ "/" + Ceylan::toString( MaximumReferenceCount ) ) ;
	}
	
}


const string Countable::ToString( Ceylan::VerbosityLevels level ) throw()
{
	return "Current Countable count is " + Ceylan::toString( ReferenceCount )
		+ ", maximum was " + Ceylan::toString( MaximumReferenceCount ) ;
}


Countable::InstanceCount Countable::GetInstanceCount() throw()
{
	return ReferenceCount ;
}


Countable::InstanceCount Countable::GetMaximumInstanceCount() throw()
{
	return MaximumReferenceCount ;
}

