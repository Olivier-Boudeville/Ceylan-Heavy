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


#include "Ceylan.h"

#include <exception>
#include <string>



using namespace Ceylan ;
using namespace Ceylan::Log ;


/**
 * Test class for Smart Resource manager.
 *
 */
class TestSmartResource : public Ceylan::SmartResource
{

	public:
	
	
		TestSmartResource( const std::string & name ) throw() :
			_name( name )
		{
			LogPlug::info( "TestSmartResource instance created for '" 
				+ _name + "'." ) ;

		}
	
	
		virtual ~TestSmartResource()  throw()
		{
			LogPlug::info( "TestSmartResource '" + _name 
				+ "' deallocated." ) ;
		}
		
		
		virtual Ceylan::System::Size getSizeInMemory() const throw()
		{
			return 10 ;
		}
		
		
		Clonable & clone() const throw()
		{
		
			LogPlug::info( "TestSmartResource::clone : clone of '"
				+ _name + "' created." ) ;
			
			// Not a real clone to help distinguishing :	
			return * new TestSmartResource( "Clone of " + _name ) ;
			
		}
		
		
		virtual const std::string toString( VerbosityLevels level 
			= Ceylan::high ) const throw()
		{
			return "TestSmartResource '" + _name 
				+ "'. " + Measurable::toString( level ) ;
		}	
		
		
		
	private:
	
		std::string _name ;	
		
} ;



/**
 * Test of smart Resource manager.
 *
 * @see SmartResourceManager
 *	
 */
int main( int argc, char * argv[] )
{

	LogHolder logger( argc, argv ) ;


	/** 
	 * To ensure there is no memory leak, one can execute :
	 * './testSmartResourceManager' : file testSmartResourceManager.log 
	 * is created, and :
	 * 'grep 'created for'  ./testSmartResourceManager.log | wc -l' 
	 * should return the same number as :
	 * 'grep 'deallocated\.'  ./testSmartResourceManager.log | wc -l'
	 *
	 *
	 */
	 
    try
    {

		LogPlug::info( "Testing Smart Resource manager implementation" ) ;


		// Indented block to force premature manager deallocation.
		{
		
			LogPlug::info( "Creating a first default Smart Resource manager, "
				"hence with the 'NeverDrop' policy "
				"(called here 'non-dropping manager')." ) ;
				
			SmartResourceManager<Ceylan::Latin1Char> nonDroppingManager ;
			
			LogPlug::info( "New first manager state is : " 
				+ nonDroppingManager.toString( Ceylan::high ) ) ;
				
			LogPlug::info( "Creating a second Smart Resource manager, "
				"with the 'DropLessRequestedFirst' policy "
				"(called here 'dropping manager')." ) ;
				
			
			/**
			 * Quota of at most 50 bytes, i.e. 5 TestSmartResource instances.
			 *
			 */
			SmartResourceManager<Ceylan::Latin1Char> dropperManager( 
				/* quota of at most 50 bytes */ 50,
				/* cache policy */ 
				SmartResourceManager<Ceylan::Latin1Char>::DropLessRequestedFirst
			) ;
				
			LogPlug::info( "Second manager : " 
				+ dropperManager.toString( Ceylan::high ) ) ;
			
				
			LogPlug::info( "Creating a set of resources, each named after "
				"a letter and whose key will be that letter." ) ;
				
			LogPlug::info( "Testing first 'get' and "
				"'takeOwnershipOf' methods." ) ;
			
			/// Will be property of first (non-dropping) manager :
			TestSmartResource * a = new TestSmartResource( "a" ) ;
			TestSmartResource * b = new TestSmartResource( "b" ) ;
			TestSmartResource * c = new TestSmartResource( "c" ) ;
			
			/// Will be property of second (dropping) manager :
			TestSmartResource * d = new TestSmartResource( "d" ) ;
			TestSmartResource * e = new TestSmartResource( "e" ) ;
			TestSmartResource * f = new TestSmartResource( "f" ) ;
			TestSmartResource * g = new TestSmartResource( "g" ) ;
			TestSmartResource * h = new TestSmartResource( "h" ) ;
			TestSmartResource * i = new TestSmartResource( "i" ) ;
			TestSmartResource * j = new TestSmartResource( "j" ) ;
		
			LogPlug::info( "Causing cache misses "
				"(not put in cache, hence cannot be found)" ) ;
			nonDroppingManager.get( 'a' ) ;
			nonDroppingManager.get( 'a' ) ;

			LogPlug::info( "First manager : " 
				+ nonDroppingManager.toString( Ceylan::high ) ) ;

			dropperManager.get( 'b' ) ;
			dropperManager.get( 'c' ) ;
			dropperManager.get( 'd' ) ;

			LogPlug::info( "Second manager : " 
				+ dropperManager.toString( Ceylan::high ) ) ;

			LogPlug::info( "Causing cache hits for first manager "
				"thanks to 'takeOwnershipOf' :" ) ;
			nonDroppingManager.takeOwnershipOf( 'a', *a ) ;
			nonDroppingManager.takeOwnershipOf( 'b', *b ) ;
			nonDroppingManager.takeOwnershipOf( 'c', *c ) ;
		
		
			if ( nonDroppingManager.get( 'a' ) != a )
				throw TestException( "Non dropping manager failed to return "
					"correct pointer for cached resource." ) ;
			else
				LogPlug::info( 
					"Correct resource returned, new first manager state is : "
					+ nonDroppingManager.toString( Ceylan::high ) ) ;
		
		
			/*
			 * Check a special case of drop, when two resources are 
			 * submitted with the same key :
			 *
			 */
			bool raised = false ;
			
			try
			{
				TestSmartResource * cannotReplace = 
					new TestSmartResource( "cannotReplace" ) ;
				nonDroppingManager.takeOwnershipOf( 'a', *cannotReplace ) ;
			}
			catch( const ResourceManagerException & )
			{
				raised = true ;
				LogPlug::info( "First manager detected appropriately "
					"that an already associated key could not be "
					"associated again with the 'NeverDrop' policy." ) ;
			}
			
			if ( ! raised )
				throw TestException( "Non dropping manager failed to raise "
					"an exception on attempt to reaffect "
					"a key despite 'NeverDrop' policy." ) ;
			
			
			LogPlug::info( "Causing cache hits for second manager :" ) ;
			dropperManager.takeOwnershipOf( 'd', *d ) ;
			dropperManager.takeOwnershipOf( 'e', *e ) ;
			dropperManager.takeOwnershipOf( 'f', *f ) ;
		
			if ( dropperManager.get( 'e' ) != e )
				throw TestException( "Dropping manager failed to return "
					"correct pointer for cached resource." ) ;
			else
				LogPlug::info( "Correct resource returned, "
					"new second manager state is : "
					+ dropperManager.toString( Ceylan::high ) ) ;
			
			LogPlug::info( "Causing resource drops for second "
				"(dropping) manager by taking ownership of resources "
				"whose total size is too high :" ) ;
			
			dropperManager.takeOwnershipOf( 'g', *g ) ;
			LogPlug::info( "Second manager for the fourth resource, g : " 
				+ dropperManager.toString( Ceylan::high ) ) ;

			dropperManager.takeOwnershipOf( 'h', *h ) ;
			LogPlug::info( "Second manager for the fifth resource, h, "
				"which is the last that should fit in cache : " 
				+ dropperManager.toString( Ceylan::high ) ) ;
			
			LogPlug::info( "First drop should occur here normally." ) ;
			
			dropperManager.takeOwnershipOf( 'i', *i ) ;
			LogPlug::info( "Second manager for i, "
				"must have replaced an already cached resource : "
				+ dropperManager.toString( Ceylan::high ) ) ;
			
			dropperManager.takeOwnershipOf( 'j', *j ) ;			
			LogPlug::info( "Second manager for j, "
				"another cached resource should have bitten the dust : " 
				+ dropperManager.toString( Ceylan::high ) ) ;
		

			LogPlug::info( "Testing then 'getClone' and "
				"'takeOwnershipOf' methods." ) ;
			
			TestSmartResource * k = new TestSmartResource( "k" ) ;
			TestSmartResource * l = new TestSmartResource( "l" ) ;
			
			LogPlug::info( "First manager before new test : " 
				+ nonDroppingManager.toString( Ceylan::high ) ) ;
			LogPlug::info( "Second manager before new test : " 
				+ dropperManager.toString( Ceylan::high ) ) ;

			LogPlug::info( "Two smart resources are submitted "
				"to the two managers, without letting them take "
				"the ownership (just scan the resources). "
				"The added clone should result in a drop for "
				"second manager." ) ;
				
			nonDroppingManager.scanForAddition( 'k', *k ) ;
			dropperManager.scanForAddition( 'l', *l ) ;
			
			LogPlug::info( "After scan and clone, first manager is : "  
				+ nonDroppingManager.toString( Ceylan::high ) ) ;
			LogPlug::info( "After scan and clone, second manager : " 
				+ dropperManager.toString( Ceylan::high ) ) ;


			// First manager :
			
			LogPlug::info( "Testing now 'getClone' with first manager, "
				"both for a resource submitted with 'takeOwnershipOf' "
				"and with 'scanForAddition'." ) ;		
			
			SmartResource * firstClone = nonDroppingManager.getClone( 'a' ) ;
			
			if ( firstClone == 0 )
				throw TestException( "Non-dropping manager returned "
					"null pointer after clone request" ) ;			
			
			if ( firstClone == a )
				throw TestException( "Non-dropping manager returned "
					"submitted resource pointer instead of a clone, "
					"despite the use of the 'getClone' method." ) ;
			else
				LogPlug::info( "Correct clone returned, "
					"new first manager state is : "
					+ nonDroppingManager.toString( Ceylan::high ) ) ;
					
			LogPlug::info( "Clone is : " + firstClone->toString() ) ;
			
			LogPlug::info( "Deleting this clone should not interfer "
				"with this manager." ) ;
			delete firstClone ;
			
			LogPlug::info( "Result on first manager : " 
				+ nonDroppingManager.toString( Ceylan::high ) ) ;
			
			LogPlug::info( "Same exercice with a resource "
				"submitted through 'scanForAddition'." ) ;
			firstClone = nonDroppingManager.getClone( 'k' ) ;
			
			if ( firstClone == 0 )
				throw TestException( "Non-dropping manager returned "
					"null pointer after clone request" ) ; 			
			
			if ( firstClone == k )
				throw TestException( "Non-dropping manager returned "
					"submitted resource pointer instead of a clone, "
					"despite the use of the 'getClone' method." ) ;
			else
				LogPlug::info( "Correct clone returned, "
					"new second manager state is : "
					+ nonDroppingManager.toString( Ceylan::high ) ) ;
			
			LogPlug::info( "Clone is : " + firstClone->toString() ) ;
					
			LogPlug::info( "This clone is owned by the user, "
				"she can delete it whenever wanted." ) ;
				
			delete firstClone ;
					


			// Second manager :

			
			LogPlug::info( "Testing 'getClone' with second manager, "
				"both for a resource submitted with 'takeOwnershipOf' "
				"and with 'scanForAddition'." ) ;		

			LogPlug::info( "Second manager before new test : " 
				+ dropperManager.toString( Ceylan::high ) ) ;
			
			// 'j' should not have already been desallocated :
			SmartResource * secondClone = dropperManager.getClone( 'j' ) ;
			
			if ( secondClone == 0 )
				throw TestException( "Dropping manager returned null pointer "
					"after clone request" ) ;			
			
			if ( secondClone == j )
				throw TestException( "Dropping manager returned submitted "
					" resource pointer instead of a clone, "
					"despite the use of the 'getClone' method." ) ;
			else
				LogPlug::info( "Correct clone returned, "
					"new first manager state is : "
					+ dropperManager.toString( Ceylan::high ) ) ;
				LogPlug::info( "Clone is : " + secondClone->toString() ) ;
			
			LogPlug::info( "Deleting this clone should not interfer "
				"with this manager." ) ;
			delete secondClone ;
			
			LogPlug::info( "Result on second manager : " 
				+ dropperManager.toString( Ceylan::high ) ) ;
			
			LogPlug::info( "Same exercice with a resource submitted "
				"through 'scanForAddition'." ) ;
			secondClone = dropperManager.getClone( 'l' ) ;
			
			if ( secondClone == 0 )
				throw TestException( "Dropping manager returned null pointer "
					"after clone request" ) ;			
			
			if ( secondClone == l )
				throw TestException( "Dropping manager returned submitted "
					" resource pointer instead of a clone, "
					"despite the use of the 'getClone' method." ) ;
			else
				LogPlug::info( "Correct clone returned, "
					"new second manager state is : "
					+ dropperManager.toString( Ceylan::high ) ) ;
				LogPlug::info( "Clone is : " + secondClone->toString() ) ;
					
			LogPlug::info( "This clone is owned by the user, "
				"she can delete it whenever wanted." ) ;
			delete secondClone ;

			LogPlug::info( "Deleting user-owned resources." ) ;					
			delete k ;
			delete l ;

			LogPlug::info( "Deallocating the two managers now "
				"(they get out of scope)." ) ;
								
		}
					
		LogPlug::info( "Here all the cached resources "
			"should have been deallocated by their owner." ) ;			
		
		
        LogPlug::info( "End of Smart Resource manager test." ) ;

		

	}
	
    catch ( const Ceylan::Exception & exception )
    {
        std::cerr << "Ceylan exception caught : "
        	<< exception.toString( Ceylan::high ) << std::endl ;
		return Ceylan::ExitFailure ;

    }

    catch ( const std::exception & exception )
    {
        std::cerr << "Standard exception caught : " 
			 << exception.what() << std::endl ;
		return Ceylan::ExitFailure ;

    }

    catch ( ... )
    {
        std::cerr << "Unknown exception caught" << std::endl ;
		return Ceylan::ExitFailure ;

    }

    return Ceylan::ExitSuccess ;

}
