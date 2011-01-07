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
using namespace Ceylan ;
using namespace Ceylan::Network ;
using namespace Ceylan::Log ;
using namespace Ceylan::System ;


#include <iostream>
using namespace std ;


#include <string>
using std::string ;



class TestThread : public Ceylan::System::Thread
{

	public:
	
	
		TestThread( const std::string & name, 
			Synchronized<Ceylan::UnsignedLongInteger> & sharedCounter,
			Ceylan::Uint32 & privateCounter,
			bool interactiveMode ) 
				throw( Features::FeatureNotAvailableException ) :
			Thread( name ),
			_sharedCounter( & sharedCounter ),
			_privateCounter( & privateCounter ),
			_interactiveMode( interactiveMode )
		{
		
		
		}
		
		
		void start() throw()
		{
		
			Maths::Random::WhiteNoiseGenerator randGen( 0, 100 ) ;
			
			LogPlug::info( "TestThread::run begun for thread '" + getName() 
				+ "' : " + toString() ) ;
			
			while ( ! stopDemanded() )
			{
			
				// In interactive mode ? Run like hell ! Otherwise :

				if ( ! _interactiveMode )
				{
				
					// In [0,100[ :
					Maths::Random::RandomValue rand = randGen.getNewValue() ;

					LogPlug::info( "TestThread::run : '" + getName() 
						+ "' sleeping for " + Ceylan::toString( rand / 100.0f )
						+ " second." ) ;
				
					// In [0,1[ second :
					Thread::Sleep( 0 /* seconds */, 
						rand * 10000 /* microseconds */ ) ;
				
				}
				
				
				try
				{
					(*_sharedCounter)++ ;
					(*_privateCounter)++ ;
				}
				catch( const Ceylan::Lockable::LockException & e )
				{
					LogPlug::error( "TestThread::start failed : "
						+ e.toString() + ", stopping thread now." ) ;
					askToStop() ;
				}
				
				
				if ( *_sharedCounter % 100000 == 0 )
				{
					cout << getName() << " [" << *_privateCounter << " / " 
						<< _sharedCounter->getValue() << "]" << endl ;
				}
									
			}
								
			LogPlug::info( "TestThread::run ended for thread '" + getName() 
				+ "' : " + toString() ) ;
				
		}
		
		
		private:
		
			/// A counter shared by all threads.
			Ceylan::System::Synchronized<Ceylan::UnsignedLongInteger> * _sharedCounter ;
			
			/// The counter specific for this thread.
			Ceylan::Uint32 * _privateCounter ;
			
			bool _interactiveMode ;
			
} ;



/**
 * Test of Ceylan threads.
 *
 * @see Ceylan::System::Thread.
 *
 */
int main( int argc, char * argv[] )
{


	LogHolder logger( argc, argv ) ;


    try
    {


        LogPlug::info( "Testing Ceylan's thread support." ) ;


		if ( ! Features::isMultithreadingSupported() )
		{
			LogPlug::warning( "Thread support not available, "
				"nothing tested." ) ;
			return Ceylan::ExitSuccess ;
		
		}
		
		std::string executableName ;
		std::list<std::string> options ;
		
		bool interactiveMode = false ;
		
		Ceylan::parseCommandLineOptions( executableName, options, argc, argv ) ;
	
		std::string token ;
		bool tokenEaten ;
		
		
		while ( ! options.empty() )
		{
		
			token = options.front() ;
			options.pop_front() ;

			tokenEaten = false ;
						
			if ( token == "--batch" )
			{
				LogPlug::info( "Running in batch mode." ) ;
				interactiveMode = false ;
				tokenEaten = true ;
			} else
			if ( token == "--interactive" )
			{
				LogPlug::info( "Running in interactive mode." ) ;
				interactiveMode = true ;
				tokenEaten = true ;
			} else		
			if ( LogHolder::IsAKnownPlugOption( token ) )
			{
				// Ignores log-related (argument-less) options.
				tokenEaten = true ;
			}
			
			if ( ! tokenEaten )
			{
				LogPlug::error( "Unexpected command line argument : "
					+ token ) ;
			}
		
		}
	
		
		// Block made to force deallocation of automatic threads :
		{
		
		
        	LogPlug::info( "Creating a synchronized shared counter." ) ;
			
			Synchronized<Ceylan::UnsignedLongInteger> sharedCounter( 0 ) ;
			
			
        	LogPlug::info( "Creating five test threads with local counter." ) ;
		
			Ceylan::Uint32 t1Counter = 0 ;
			TestThread t1( "#1", sharedCounter, t1Counter, interactiveMode ) ;

			Ceylan::Uint32 t2Counter = 0  ;
			TestThread t2( "#2", sharedCounter, t2Counter, interactiveMode ) ;

			Ceylan::Uint32 t3Counter = 0  ;
			TestThread t3( "#3", sharedCounter, t3Counter, interactiveMode ) ;

			Ceylan::Uint32 t4Counter = 0  ;
			TestThread t4( "#4", sharedCounter, t4Counter, interactiveMode ) ;

			Ceylan::Uint32 t5Counter = 0  ;
			TestThread t5( "#5", sharedCounter, t5Counter, interactiveMode ) ;


        	LogPlug::info( "Running the five test threads." ) ;
			
			t1.run() ;
			t2.run() ;
			t3.run() ;
			t4.run() ;
			t5.run() ;
			
			Ceylan::Uint16 waitingTimeInSeconds ;
			
			if ( interactiveMode )
				waitingTimeInSeconds = 20 ;
			else
				waitingTimeInSeconds = 2 ;
				
					
        	LogPlug::info( "Waiting for " 
				+ Ceylan::toString( waitingTimeInSeconds )
				+ " seconds to let the threads work." ) ;
			
			Thread::Sleep( waitingTimeInSeconds /* seconds */ ) ;


        	LogPlug::info( "Requesting the threads to stop." ) ;
			
			t1.askToStop() ;
			t2.askToStop() ;
			t3.askToStop() ;
			t4.askToStop() ;
			t5.askToStop() ;

        	LogPlug::info( "Waiting the threads to stop." ) ;
			
			t1.waitUntilOver() ;
			t2.waitUntilOver() ;
			t3.waitUntilOver() ;
			t4.waitUntilOver() ;
			t5.waitUntilOver() ;
			
			LogPlug::debug( "Counter for t1 is " 
				+ Ceylan::toString( t1Counter ) ) ;
				
			LogPlug::debug( "Counter for t2 is " 
				+ Ceylan::toString( t2Counter ) ) ;
				
			LogPlug::debug( "Counter for t3 is " 
				+ Ceylan::toString( t3Counter ) ) ;
				
			LogPlug::debug( "Counter for t4 is " 
				+ Ceylan::toString( t4Counter ) ) ;
				
			LogPlug::debug( "Counter for t5 is " 
				+ Ceylan::toString( t5Counter ) ) ;
			
			LogPlug::debug( "Shared counter is " 
				+ Ceylan::toString( sharedCounter.getValue() ) ) ;
			
			Ceylan::UnsignedLongInteger	localTotal = t1Counter + t2Counter 
				+ t3Counter + t4Counter + t5Counter ;

			if ( localTotal != sharedCounter.getValue() )
				throw TestException( "Total of local counters (" 
					+ Ceylan::toString( localTotal ) 
					+ ") is not equal to shared counter ("	
					+ Ceylan::toString( sharedCounter.getValue() )
					+ ")." ) ;
			
			LogPlug::info( "Shared and local match, both are equal to "
				+ Ceylan::toString( localTotal ) + "." ) ;
						 
		}
		
		
        LogPlug::info( "End of thread test." ) ;


	}
	
    catch ( const Ceylan::Exception & e )
    {
        std::cerr << "Ceylan exception caught : "
        	<< e.toString( Ceylan::high ) << std::endl ;
		return Ceylan::ExitFailure ;

    }

    catch ( const std::exception & e )
    {
        std::cerr << "Standard exception caught : " 
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
