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



/**
 * Test class for holder template.
 *
 * Displays many informations about its life cycle in its own log channel.
 *
 */
class TestHolder: public Ceylan::Object
{


	public:
	
	
	
		TestHolder() throw()
		{
		
			send( "Created a TestHolder." ) ;
			Count++ ;
			
		}
	
		
		
		~TestHolder() throw()
		{
		
			send( "Destroying a TestHolder." ) ;
			Count-- ;
			
		}
		


		int sayHello( Ceylan::Uint8 aNumber )
		{
		
			send( "Hello !" ) ;
			return aNumber ;
			
		}

		
		static Ceylan::Uint8 GetCount() throw()
		{
		
			return Count ;
			
		}	
		
		
		
	private:
	
	
		static Ceylan::Uint8 Count ;
		
		
		TestHolder( const TestHolder & source ) throw() ;
			
		TestHolder & operator = ( const TestHolder & source ) throw() ;
		
} ;



Ceylan::Uint8 TestHolder::Count = 0 ;


#include <iostream>



/**
 * Test of holder facility.
 *
 * @see CeylanHolder.h (Holder)
 *	
 */
int main( int argc, char * argv[] )
{

	LogHolder logger( argc, argv ) ;


    try
    {


		LogPlug::info( "Testing Holder template." ) ;

		LogPlug::info( "Before Holder creation, holder count is "
			+ Ceylan::toNumericalString( TestHolder::GetCount() ) ) ;
		
		if ( TestHolder::GetCount() != 0 )
			throw Ceylan::TestException( "Wrong initial test holder count." ) ;
			
		Holder<TestHolder> myHolder( * new TestHolder() ) ;

		LogPlug::info( "After first Holder creation, holder count is "
			+ Ceylan::toNumericalString( TestHolder::GetCount() ) ) ;

		if ( TestHolder::GetCount() != 1 )
			throw Ceylan::TestException( 
				"Wrong test holder count after first Holder creation." ) ;
		
		if ( myHolder->sayHello( 17 ) != 17 )
			throw Ceylan::TestException( 
				"A call to held object returned a wrong result." ) ;
		
		
		// This block will force myOtherHolder deallocation.
		{
		
			Holder<TestHolder> myOtherHolder( * new TestHolder() ) ;

			if ( TestHolder::GetCount() != 2 )
				throw Ceylan::TestException( 
					"Wrong test holder count after second Holder creation." ) ;
		
			LogPlug::info( "Forcing deallocation of second Holder" ) ;
													
		}

		if ( TestHolder::GetCount() != 1 )
			throw Ceylan::TestException( 
				"Wrong test holder count after first Holder deletion." ) ;

		
        LogPlug::info( "End of Holder pointer test." ) ;


	}
	
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
