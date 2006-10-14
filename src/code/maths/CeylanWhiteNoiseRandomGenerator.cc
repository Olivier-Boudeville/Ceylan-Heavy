#include "CeylanWhiteNoiseRandomGenerator.h"

#include "CeylanOperators.h"
#include "CeylanLogPlug.h"
#include "CeylanSystem.h"         // for getPreciseTime


#include <list>

#include <cstdlib>                // for rand, srand



using std::string ;


using namespace Ceylan ;
using namespace Ceylan::System ;    // for time primitives

using namespace Ceylan::Maths ;
using namespace Ceylan::Maths::Random ;

using namespace Ceylan::Log ;


WhiteNoiseGenerator::WhiteNoiseGenerator( Sample lowerLimit, 
	Sample upperLimit ) 
		throw( MathsException ) :
	RandomGenerator( lowerLimit, upperLimit ) 
{

	LogPlug::trace( "WhiteNoiseGenerator constructor called." ) ;

	generateSeedFromCurrentTime() ;	
	preCompute() ;
	
	LogPlug::trace( "WhiteNoiseGenerator constructor ended." ) ;

}


WhiteNoiseGenerator::WhiteNoiseGenerator( Sample lowerLimit, 
	Sample upperLimit, Seed aSeed ) 
		throw( MathsException ) :
	RandomGenerator( lowerLimit, upperLimit, aSeed ) 
{

	preCompute() ;
	
}




WhiteNoiseGenerator::~WhiteNoiseGenerator() throw()
{

}


void WhiteNoiseGenerator::generateSeedFromCurrentTime() throw( MathsException )
{

	try
	{	
	
		Second sec ;
		Microsecond microsec ;
		
		getPreciseTime( sec, microsec ) ;	
		_seed = static_cast<Seed>( sec + microsec ) ;
		
	}
	catch( const SystemException & e )
	{
		throw MathsException( 
			"WhiteNoiseGenerator::generateSeedFromCurrentTime : "
			"unable to forge random seed from current time : " 
			+ e.toString() ) ;
	}
	
}	 	


RandomValue WhiteNoiseGenerator::getNewValue() throw()
{

	// rand returns int.
	
	return static_cast<RandomValue>( _lowerLimit +
		static_cast<double>( ::rand() ) * 
			( _upperLimit - _lowerLimit )/ RAND_MAX ) ; 
}


void WhiteNoiseGenerator::reset( Seed newSeed ) throw() 
{
	_seed = newSeed ;
	::srand( _seed ) ;
}


const string WhiteNoiseGenerator::toString( VerbosityLevels level ) 
	const throw()
{
	return "White noise generator. " 
		+ RandomGenerator::toString( level ) ;
}


void WhiteNoiseGenerator::preCompute() throw( MathsException ) 
{
	// Initialize new sequence with seed.
	::srand( _seed ) ;
}

