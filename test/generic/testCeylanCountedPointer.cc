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


/*
 * One way of inspecting closely this test is:
 *  - in 'generic/CeylanCountedPointer.h', to set CEYLAN_COUNTED_POINTER_DEBUG
 * to 1
 *  - to recompile the Ceylan library and this test
 *  - to run it as:
 * './testCeylanCountedPointer.exe --consolePlug 1>test.txt 2>&1'
 *
 * One can then check the changes in reference counting, and the fact that*
 * only one TestCountedPointer is created indeed.
 *
 * Hint: first '[CountedPointer] after referent construction, refcount = 1'
 * corresponds to the creation of the static referent for arrays.
 *
 */
 
 

/**
 * Test class for counter pointer.
 *
 * Displays many informations about its life cycle in its own log channel.
 *
 */
class TestCountedPointer : public Ceylan::Object
{

	public:
	
	
		TestCountedPointer()
		{
			send( "Created a TestCountedPointer." ) ;
		}
	
	
		~TestCountedPointer() throw()
		{
			send( "Destroying a TestCountedPointer." ) ;
		}
		
		
		TestCountedPointer & operator = ( const TestCountedPointer & source )
		{
		
			send( "Assigning a TestCountedPointer." ) ;
			return * this ; 
						
		}	
		
		
	private:
	
		TestCountedPointer( const TestCountedPointer & source ) ;
			
		
} ;




/// Pass by value.
Ceylan::ReferenceCount aFunction( CountedPointer<TestCountedPointer> counted )
{

	LogPlug::info( "aFunction: argument reference count is: " 
		+ counted.toString() + "." ) ;
	
	return counted.getReferenceCount() ;
	
}


#include <iostream>




/**
 * Test of counted pointer facility.
 *
 * @see CeylanCountedPointer.h (CountedPointer)
 *	
 */
int main( int argc, char * argv[] )
{

	LogHolder logger( argc, argv ) ;


    try
    {


		LogPlug::info( "Testing counted pointer implementation." ) ;

		LogPlug::info( "Normally, exactly one TestCountedPointer "
			"instance should be created." ) ;

		CountedPointer<TestCountedPointer> myCounted =
			new TestCountedPointer() ;
			
		LogPlug::info( "Displaying first counted pointer: " 
			+ myCounted.toString() ) ;

		
		if ( myCounted.getReferenceCount() != 1 )
			throw Ceylan::TestException( 
				"#1: expected reference count equal to one." ) ;
		else
			LogPlug::info( 
				"Having reference count equal to one, as expected." ) ;
		
		
		// This block will force myOtherCounted deallocation:
		{
		
			LogPlug::info( 
				"Creating a copy of the original counted pointer." ) ; 
				
			CountedPointer<TestCountedPointer> myOtherCounted = myCounted ;
		
			LogPlug::info( 
				"Displaying first counted pointer after assignment: " 
				+ myCounted.toString() ) ;
				
			if ( myCounted.getReferenceCount() != 2 )
				throw Ceylan::TestException( 
					"#2: expected reference count equal to two, got instead " 
					+ Ceylan::toString( myCounted.getReferenceCount() ) 
					+ "." ) ;
			else
				LogPlug::info( "First counted pointer having reference count "
					"equal to two, as expected." ) ;

			LogPlug::info( "Displaying second counted pointer: " 
				+ myOtherCounted.toString() ) ;
				
			if ( myOtherCounted.getReferenceCount() != 2 )
				throw Ceylan::TestException( 
					"#3: expected reference count equal to two, got instead " 
					+ Ceylan::toString( myOtherCounted.getReferenceCount() ) 
					+ "." ) ;
			else
				LogPlug::info( "Second counted pointer having reference count "
					"equal to two, as expected." ) ;

			Ceylan::ReferenceCount result = aFunction( myOtherCounted ) ;
			
			if ( result != 3 )
				throw Ceylan::TestException( 
					"#4: expected reference count equal to three, got instead "
					+ Ceylan::toString( result ) + "." ) ;
			else
				LogPlug::info( "After function call, reference count "
					"equal to three, as expected." ) ;
									
			LogPlug::info( "Second counted pointer going out of scope." ) ;
			
		}
		
		LogPlug::info( "Second counted pointer gone out of scope." ) ;
		
		Ceylan::ReferenceCount result = aFunction( myCounted ) ;
		
		if ( result != 2 )
			throw Ceylan::TestException( 
				"#5: expected reference count equal to two, got instead " 
				+ Ceylan::toString( result ) + " )." ) ;
		else
			LogPlug::info( "After second function call, reference count "
				"equal to two, as expected." ) ;

		if ( myCounted.getReferenceCount() != 1 )
			throw Ceylan::TestException( 
				"#6: expected reference count equal to one, got instead " 
				+ Ceylan::toString( myCounted.getReferenceCount() ) + "." ) ;
		else
			LogPlug::info( "New reference count equal to one, as expected." ) ;

		
		LogPlug::info( "Displaying final counted pointer: " 
			+ myCounted.toString() ) ;
		
        LogPlug::info( "End of counted pointer test." ) ;


	}
	
	
	/*
	 * Here the TestCountedPointer instance is expected to be deallocated.
	 *
	 * Can be checked by setting CEYLAN_COUNTED_POINTER_DEBUG during the
	 * build, one should then see in the console after the test something like:
	 
[CountedPointer] after referent construction for resource pointer 0, 
	refcount = 1
[CountedPointer] after referent construction for resource pointer 0x9ad2f60,
	refcount = 0
[CountedPointer] after setReferent, refcount = 1 for 0x9ad2f60
[CountedPointer] after setReferent, refcount = 2 for 0x9ad2f60
[CountedPointer] after setReferent, refcount = 3 for 0x9ad2f60
[CountedPointer] after release, refcount = 2 for 0x9ad2f60
[CountedPointer] after release, refcount = 1 for 0x9ad2f60
[CountedPointer] after setReferent, refcount = 2 for 0x9ad2f60
[CountedPointer] after release, refcount = 1 for 0x9ad2f60
[CountedPointer] after release, refcount is null, deallocating resource
	0x9ad2f60
[CountedPointer] referent destructor deallocating its resource 0x9ad2f60

	 */
	
	
    catch ( const Ceylan::Exception & e )
    {
        std::cerr << "Ceylan exception caught: "
        	<< e.toString( Ceylan::high ) << std::endl ;
		return Ceylan::ExitFailure ;

    }

    catch ( const std::exception & e )
    {
        std::cerr << "Standard exception caught: " 
			 << e.what() << std::endl ;
		return Ceylan::ExitFailure ;

    }

    catch ( ... )
    {
        std::cerr << "Unknown exception caught" << std::endl ;
		return Ceylan::ExitFailure ;

    }

    return Ceylan::ExitSuccess ;

}
