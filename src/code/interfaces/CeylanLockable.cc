/* 
 * Copyright (C) 2003-2009 Olivier Boudeville
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
 * Author: Olivier Boudeville (olivier.boudeville@esperide.com)
 *
 */


#include "CeylanLockable.h"

#include "CeylanUtils.h"     // for Ceylan::ExitDebugFailure
#include "CeylanLogPlug.h"   // for LogPlug




using std::string ;

using namespace Ceylan ;
using namespace Ceylan::Log ;




Lockable::LockException::LockException( const string & reason ) :
	Ceylan::Exception( reason ) 
{

}



Lockable::LockException::~LockException() throw()
{

}




// Public section.


Lockable::Lockable() : 
	_locked( false )  
{

}



Lockable::~Lockable() throw()
{

	try 
	{
	
		if ( mustBeLocked() && _locked )
		{
	
			LogPlug::warning( "Ceylan Lockable destructor: "
				"Lockable instance was still holding a lock at "
				"deallocation, this lock has been released.") ;
			
			unlock() ;
		}
		
	} 
	catch( const Lockable::LockException & e )
	{

		LogPlug::error( "Ceylan Lockable destructor: "
			"unable to unlock Lockable whereas "
			"it seemed to be locked: " + e.toString() ) ;	
	}

}



bool Lockable::mustBeLocked() const 
{

	return true ;
	
}



void Lockable::lock()
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
	
	// else: does not need to be locked, do nothing.
	
}



void Lockable::unlock()
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
	
	// else: does not need to be locked (hence unlocked), do nothing.
	
}



const string Lockable::toString( Ceylan::VerbosityLevels level ) const
{

	if ( _locked )
		return "Lockable currently locked" ;
	else
		return "Lockable currently not locked" ;	
			
}




// Protected section.


void Lockable::postLock() 
{

	// Override me.
	
}



void Lockable::preUnlock() 
{

	// Override me.

}


