/*
 * Copyright (C) 2003-2011 Olivier Boudeville
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


#include "Ceylan.h"


#include <exception>
#include <string>


using namespace Ceylan ;
using namespace Ceylan::Log ;



class LockableExample : public Lockable
{

	public:


		LockableExample() throw() :
			Lockable(),
			_postLockCalled( false ),
			_preUnlockCalled( false )
		{

		}


		virtual ~LockableExample() throw()
		{

		}


		virtual bool mustLock() const throw()
		{
			return true ;
		}



		bool getPostConditionCalled() const throw()
		{
			return _postLockCalled ;
		}


		bool getPreConditionCalled() const throw()
		{
			return _preUnlockCalled ;
		}



	protected:



		virtual void postLock() throw()
		{
			_postLockCalled = true ;
		}


		virtual void preUnlock() throw()
		{
			_preUnlockCalled = true ;
		}



	private:

		bool _postLockCalled ;
		bool _preUnlockCalled ;


} ;




/**
 * Test for Lockable implementation.
 *
 * @see Lockable
 *
 */
int main( int argc, char * argv[] )
{

	LogHolder myLog( argc, argv ) ;

	try
	{


		LogPlug::info( "Testing Lockable implementation." ) ;


		LockableExample myExample ;

		LogPlug::info( "Before lock : " + myExample.toString()
			+ ", locking now test Lockable." ) ;

		myExample.lock() ;

		LogPlug::info( "After lock, before unlocking: "
			+ myExample.toString() ) ;

		LogPlug::info( "Unlocking test Lockable." ) ;
		myExample.unlock() ;

		bool caught = false ;

		LogPlug::info( "After unlock: " + myExample.toString() ) ;


		// Should raise a LockException.
		try
		{
			LogPlug::info( "Unlocking test Lockable "
				"(whereas not locked)." ) ;
			myExample.unlock() ;
		}
		catch( const Lockable::LockException & e )
		{
			LogPlug::info( "Alright, LockException caught ("
				+ e.toString() + ")." ) ;
			caught = true ;
		}


		if ( ! caught )
			throw TestException( "Unlocking a non-locked Lockable "
				"should raise a LockException." ) ;


		caught = false ;
		myExample.lock() ;

		// Should raise a LockException:
		try
		{
			LogPlug::info( "Locking test Lockable "
				"(whereas already locked)." ) ;
			myExample.lock() ;
		}
		catch( const Lockable::LockException & e )
		{
			LogPlug::info( "Alright, LockException caught ("
				 + e.toString() + ")." ) ;
			caught = true ;
		}


		if ( ! caught )
			throw TestException( "Locking an already locked Lockable "
				"should raise a LockException." ) ;


		LogPlug::info( "Checking correctness of postLock/preUnlock calls." ) ;

		LockableExample myOtherExample ;


		// Each time, test first whether postLock was called, then preUnlock.

		if ( myOtherExample.getPostConditionCalled() )
			throw TestException(
				"Lockable starts with wrong post-condition." ) ;

		if ( myOtherExample.getPreConditionCalled() )
			throw TestException(
				"Lockable starts with wrong pre-condition." ) ;

		myOtherExample.lock() ;

		if ( ! myOtherExample.getPostConditionCalled() )
			throw TestException(
				"Lockable after lock is in wrong post-condition." ) ;

		if ( myOtherExample.getPreConditionCalled() )
			throw TestException(
				"Lockable after lock is in wrong pre-condition." ) ;

		myOtherExample.unlock() ;


		if ( ! myOtherExample.getPostConditionCalled() )
			throw TestException(
				"Lockable after unlock is in wrong post-condition." ) ;

		if ( ! myOtherExample.getPreConditionCalled() )
			throw TestException(
				"Lockable after unlock is in wrong pre-condition." ) ;

		LogPlug::info( "A warning should have been triggered since "
			"a Lockable will still be locked when deallocated." ) ;

		/*
		 * Both myExample and myOtherExample are deallocated here. 'myExample'
		 * should trigger a warning since it is still locked.
		 *
		 */

	}

	catch ( const Ceylan::Exception & e )
	{
		LogPlug::error( "Ceylan exception caught: "
			 + e.toString( Ceylan::high ) ) ;
		return Ceylan::ExitFailure ;

	}

	catch ( const std::exception & e )
	{
		LogPlug::error( "Standard exception caught: "
			 + std::string( e.what() ) ) ;
		return Ceylan::ExitFailure ;

	}

	catch ( ... )
	{
		LogPlug::error( "Unknown exception caught" ) ;
		return Ceylan::ExitFailure ;

	}

	Ceylan::shutdown() ;

	return Ceylan::ExitSuccess ;

}
