#include "CeylanNormalProbability.h"

#include "CeylanOperators.h"


using std::string ;

using namespace Ceylan ;
using namespace Ceylan::Maths ;
using namespace Ceylan::Maths::Random ;



NormalProbabilityFunction::NormalProbabilityFunction( Sample mean, 
	Deviation sigma ) throw( MathsException ) :
	ProbabilityFunction(),
	_mean( mean ),
	_sigma( sigma )
{

	if ( Maths::IsNull( _sigma ) )
		throw MathsException( "NormalProbabilityFunction constructor : "
			"standard deviation (sigma) must not be null or almost." ) ;

}


NormalProbabilityFunction::~NormalProbabilityFunction() throw() 
{

}


Probability NormalProbabilityFunction::operator() ( Sample aSample ) 
	const throw()
{

	/*
	 * Gaussian law : 
	 * P(x) = exp( -1/2*((x-mean)/sigma)²) / (sigma * sqrt(2.Pi))
	 *
	 */
	
	return static_cast<Probability>( 
		Exp( -0.5 * Pow( ( aSample - _mean ) / _sigma, 2 ) 
			/ ( _sigma * Sqrt( 2 * Pi ) ) ) ) ;
	
}


const string NormalProbabilityFunction::toString( 
	VerbosityLevels level ) const throw()
{
	return "Normal probability function whose mean is " 
		+ Ceylan::toString( _mean ) 
		+ " and whose standard deviation is " 
		+ Ceylan::toString( _sigma ) ;
}

