#include "Ceylan.h"

using namespace Ceylan ;
using namespace Ceylan::Log ;


#include <iostream>    // for cerr, endl
#include <exception>

#include <string>
using std::string ;




/**
 * Test of Locatable implementation.
 *
 * @see Locatable, Locatable2D.
 *
 */
int main( int argc, char * argv[] )
{

	LogHolder logger( argc, argv ) ;
	
    try
    {
		
		LogPlug::info( "Starting testing Locatable2D." ) ;
		
		Locatable2D root ;
		
		LogPlug::info( "Create root absolute Locatable2D: " 
			+ root.toString() ) ;

		
		/*
		 * Will be the transformation matrix from the first child to the 
		 * root, obtained by rotating the two axis of 90 degrees clockwise
		 * and then translating the origin to (5,10):
		 *
		 */
		 
		/*
		 * We want to define a first local referential whose axis are 
		 * obtained from the ones of the root thanks to a counterclockwise
		 * rotation of 90 degrees, and whose origin is located at (5,10).
		 *
		 * The corresponding transformation matrix is: 
		 * P(from Rfirst to Rroot ) = 
		 * [ 0  ;  -1 ;  5 ]
		 * [ 1  ;  0  ; 10 ]
		 * [ 0  ;  0  ;  1 ]
		 *
		 * For example: 
		 *
		 *   * in referential Rfirst, a point A is:
		 * Afirst = (1,0) = [ 1 ; 0 ; 1 ]
		 * and in referential Rroot is: 
		 * Aroot = (5,11) = [ 5 ; 11 ; 1 ] = P(from Rfirst to Rroot ) * Afirst
		 *
		 *   * in referential Rfirst, a point B is:
		 * Bfirst = (0,1) = [ 0 ; 1 ; 1 ]
		 * and in referential Rroot is: 
		 * Broot = (4,10) = [ 4 ; 10 ; 1 ] = P(from Rfirst to Rroot ) * Afirst
		 *
		 * @note As the Locatable will take ownership of the specified
		 * matrix (hence will deallocate it when appropriate), it must not 
		 * be an automatic variable.
		 *
		 */
		Maths::Linear::Matrix & transform = 
			* new Maths::Linear::HomogeneousMatrix3 (
				/* AngleInDegrees */ 90, Maths::Linear::Vector2( 5, 10 ) ) ;
			
		Locatable2D first( /* fatherLocatable */ root,
			/* localReferential */ transform ) ;
			
		LogPlug::info( "Create first child: " + first.toString() ) ;
		
		Locatable2D second( root ) ;
		LogPlug::info( "Create second child: " + second.toString() ) ;
		
		Locatable2D third( first ) ;
		
		/*
		 * Relations between created referentials:
		 * 
		 * root
		 * |-- first
		 * |   `-- third
		 * `-- second
		 *
		 */
		 
		 
		// Result not stored, it is just used to force cached computation:
		//third.getGlobalReferential() ;
		
		LogPlug::info( "Create third Locatable2D, a child of "
			"previously defined first child: " + third.toString() ) ;
		
		LogPlug::info( "Root displays now as: " + root.toString() ) ;
		LogPlug::info( "Its first child displays now as: " 
			+ first.toString() ) ;
	
		LogPlug::info( "End of Locatable2D test." ) ;

		// The 'transform' matrix will be deallocated by the 'first' Locatable.
		
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

