#include "CeylanRandomGenerator.h"

#include "CeylanObject.h"            // for Ceylan::Object
#include "CeylanOperators.h"
#include "CeylanLogPlug.h"           // for LogPlug

#include <limits.h>                  // for INT_MIN, INT_MAX



using std::string ;


using namespace Ceylan ;
using namespace Ceylan::Log ;
using namespace Ceylan::Maths ;
using namespace Ceylan::Maths::Random ;


/// Default seed for random generators.
const Seed RandomGenerator::DefaultSeed = 1 ;


RandomGenerator::RandomGenerator( Sample lowerLimit, Sample upperLimit, 
	Seed aSeed )
		throw( MathsException ) :
	_lowerLimit( lowerLimit ), 
	_upperLimit( upperLimit ), 
	_seed( aSeed ) 
{

	LogPlug::trace( "Maths::RandomGenerator constructor called." ) ;

	if ( _lowerLimit >= _upperLimit )
		throw MathsException( "RandomGenerator : lower limit (" 
			+ Ceylan::toString( _lowerLimit ) 
			+ " is not strictly inferior to upper limit ("
			+ Ceylan::toString( _upperLimit ) + ")." ) ; 
		
	/*
	 * Should not call "preCompute() ;" since it would force to 
	 * have it non-(pure virtual) and the RandomGenerator's child 
	 * classes would admittedly call preCompute from their 
	 * constructor automatically, but they would call
	 * RandomGenerator::preCompute, not their own overriden version.
	 *
	 */
}


RandomGenerator::~RandomGenerator() throw()  
{

}


const string RandomGenerator::toString( VerbosityLevels level ) const throw()
{
	return "Random generator output ranging from "
		+ Ceylan::toString( _lowerLimit ) + " to " 
		+ Ceylan::toString( _upperLimit ) 
		+ ", with initial seed being " + Ceylan::toString( _seed ) ;
}


RandomGenerator::RandomGenerator( const RandomGenerator & original ) throw() :
	Object(),
	_lowerLimit( original._lowerLimit ), _upperLimit( original._upperLimit ), 
	_seed( original._seed )  
{

}

