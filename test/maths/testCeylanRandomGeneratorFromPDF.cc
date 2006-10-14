#include "Ceylan.h"
using namespace Ceylan::Maths::Random ;
using namespace Ceylan::Log ;


#include <iostream>    // for cerr, endl
#include <exception>

#include <string>
using std::string ;




/**
 * Test of PDF-based random generator.
 *
 * @see Maths
 *
 */
int main( int argc, char * argv[] )
{


	LogHolder logger( argc, argv ) ;
	

    try
    {


        LogPlug::info( "Testing PDF-based random generator's "
			"implementation." ) ;

		Seed seed = 145 ;
		
		Sample mean = 77 ;
		Deviation sigma = 4 ;
		
		LogPlug::info( "Creating a probability density function (PDF) "
			"corresponding to a normal distribution whose "
			"sample mean is " + Ceylan::toString( mean )
			+ " and whose standard deviation is " 
			+ Ceylan::toString( sigma ) + "." ) ;
			
		NormalProbabilityFunction myGaussian( mean, sigma ) ; 	
		
		
		RandomValue sampleStart = 50 ;
		RandomValue sampleStop  = 120 ;
		 		
		LogPlug::info( "Creating a random generator whose PDF is : " 
			+ myGaussian.toString() + " Samples will range from "
			+ Ceylan::toString( sampleStart ) + " (included) to " 
			+ Ceylan::toString( sampleStop )  + " (excluded)." ) ;
		
		RandomGeneratorFromPDF myGaussianGenerator( myGaussian, 
			sampleStart, sampleStop, seed ) ;
		
		LogPlug::info( "Random generator created." ) ;

		/*
		 * A Gaussian generator embeds a WhiteNoiseGenerator, which is the
		 * second object to have a dedicated channel in logs :
		 *
		 */
		myGaussianGenerator.send( myGaussianGenerator.toString() ) ;
		
		LogPlug::info( "Displaying normal law. " 
			+ myGaussianGenerator.displayProbabilities() ) ;
			
		
		Ceylan::Uint16 sample_count = 500 ;
		
		LogPlug::info( "Generating a series of " 
			+ Ceylan::toString( sample_count ) + " random samples." ) ;
		
		RandomValue newValue ;
		
		
		/*
		 * Constructs a table recording how many samples are drawn for 
		 * each possible value.
		 *
		 */
		
		Ceylan::Uint32 * distributionTable = 
			new Ceylan::Uint32[ sampleStop - sampleStart ] ;
		
		for ( Ceylan::Uint32 i = 0 ; i < sampleStop - sampleStart; i++ )
			distributionTable[ i ] = 0 ;
			
		LogPlug::info( "Throwing dice (one out of ten displayed)..." ) ;
		
		for ( Ceylan::Uint32 draw_count = 0; draw_count < sample_count;
			draw_count++ )
		{
		
			newValue = myGaussianGenerator.getNewValue() ;
			
			// Avoid too many useless logs :
			if ( draw_count % 10 == 0 )
				LogPlug::info( "Drawing value " 
					+ Ceylan::toString( newValue ) + "." ) ;
			
			distributionTable[ newValue - sampleStart ] += 1 ;
			
		}
		
		
		LogPlug::info( "Displaying final distribution table : " ) ;
		
		for ( Ceylan::Uint32 i = 0 ; i < sampleStop - sampleStart; i++)
			LogPlug::info( Ceylan::toString( i + sampleStart ) + " occured " 
				+ Ceylan::toString( distributionTable[ i ] ) + " time(s)." ) ;
		

		LogPlug::info( "End of PDF-based random generator's test." ) ;
		
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
