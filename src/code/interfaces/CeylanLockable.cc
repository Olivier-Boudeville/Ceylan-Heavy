#include "CeylanLockable.h"

#include "CeylanUtils.h"     // for Ceylan::ExitDebugFailure
#include "CeylanLogPlug.h"   // for LogPlug




using std::string ;

using namespace Ceylan ;
using namespace Ceylan::Log ;


Lockable::LockException::LockException( const string & reason ) 
		throw() :
	Ceylan::Exception( reason ) 
{

}


Lockable::LockException::~LockException() throw()
{

}



// Public section.


Lockable::Lockable() throw() : 
	_locked( false )  
{

}


Lockable::~Lockable() throw() 
{

	try 
	{
	
		if ( mustBeLocked() && _locked )
		{
	
			LogPlug::warning( "Ceylan Lockable destructor : "
				"Lockable instance was still holding a lock at "
				"deallocation, this lock has been released.") ;
			
			unlock() ;
		}
		
	} 
	catch( const Lockable::LockException & e )
	{

		LogPlug::error( "Ceylan Lockable destructor : "
			"unable to unlock Lockable whereas "
			"it seemed to be locked : " + e.toString() ) ;	
	}

}



bool Lockable::mustBeLocked() const throw() 
{
	return true ;
}


void Lockable::lock() throw( LockException )
{

	if ( mustBeLocked() )
	{
	
		if ( ! _locked )
		{
			_locked = true ;
			postLock() ;
		}	
		else
		{
			throw LockException( 
				"Attempt to lock an already locked Lockable." ) ;
		}
		
	}
	
	// else : does not need to be locked, do nothing.
	
}



void Lockable::unlock() throw( LockException )
{

	if ( mustBeLocked() )
	{

		if ( _locked )
		{
			preUnlock() ;
			_locked = false ;
		}	
		else
		{
			throw LockException( "Attempt to unlock a non locked Lockable." ) ;
		}
		
	}	
	
	// else : does not need to be locked (hence unlocked), do nothing.
	
}


const string Lockable::toString( Ceylan::VerbosityLevels level ) const throw()
{
	if ( _locked )
		return "Lockable currently locked" ;
	else
		return "Lockable currently not locked" ;		
}



// Protected section.


void Lockable::postLock() throw( LockException ) 
{

	// Override me.
	
}


void Lockable::preUnlock() throw( LockException ) 
{

	// Override me.

}

