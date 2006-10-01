#include "CeylanRandomGeneratorFromPDF.h"


#include "CeylanOperators.h"
#include "CeylanWhiteNoiseRandomGenerator.h" // for WhiteNoiseGenerator
#include "CeylanUtils.h"                     // for emergencyShutdown
#include "CeylanStringUtils.h"               // for formatStringList
#include "CeylanLogPlug.h"                   // for LogPlug

#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"                    // for CEYLAN_DEBUG
#endif // CEYLAN_USES_CONFIG_H


#include <cstdlib>                           // for RAND_MAX
#include <list>



using std::string ;
using std::list ;

using namespace Ceylan ;
using namespace Ceylan::Log ;

using namespace Ceylan::Maths ;
using namespace Ceylan::Maths::Random ;



RandomGeneratorFromPDF::RandomGeneratorFromPDF( 
	const ProbabilityFunction & pdf, 
	Sample lowerLimit, Sample upperLimit, Seed aSeed ) 
		throw( MathsException ) :
	RandomGenerator( lowerLimit, upperLimit, aSeed ), 
	_pdf( & pdf ), 
	_whiteNoiseGenerator( 0 ),
	_probabilitiesTable( 0 ) 
{
	preCompute() ;
}


RandomGeneratorFromPDF::~RandomGeneratorFromPDF() throw()
{

	// Not owning _pdf, not destroying it.
	
	if ( _whiteNoiseGenerator )
		delete _whiteNoiseGenerator ;
	
	if ( _probabilitiesTable )
		delete _probabilitiesTable ;
		
}


RandomValue RandomGeneratorFromPDF::getNewValue() throw()
{


#if CEYLAN_DEBUG

	if ( _whiteNoiseGenerator == 0 )
		Ceylan::emergencyShutdown( "RandomGeneratorFromPDF::getNewValue : "
			"no white noise generator available." ) ;
			
#endif // CEYLAN_DEBUG
	
	Sample uniformRandomValue = _whiteNoiseGenerator->getNewValue() ;
	
	/*
	LogPlug::debug( "RandomGeneratorFromPDF::getNewValue : "
		"white noise generator drew " 
		+ Ceylan::toString( uniformRandomValue ) ) ;
	*/
	
	unsigned int count = 0 ;
	
	/*
	LogPlug::debug( "Comparing " + Ceylan::toString( uniformRandomValue )
		+ " with " + Ceylan::toString( _sampleRangesTable[ count ] ) + " : " ) ;
	*/
		
	while ( uniformRandomValue > _sampleRangesTable[ count ] )
		count++ ;

	return count + _lowerLimit ;

}


void RandomGeneratorFromPDF::reset( Seed newSeed ) throw() 
{

	if ( _whiteNoiseGenerator != 0 )
		_whiteNoiseGenerator->reset( newSeed ) ;
	else
		LogPlug::warning( "RandomGeneratorFromPDF::reset : "
			"no white noise generator available, none reset." ) ;
			
}


const string RandomGeneratorFromPDF::displayProbabilities() 
	const throw( MathsException )
{


	if ( _probabilitiesTable == 0 )
		throw MathsException( 
			"RandomGeneratorFromPDF::displayProbabilities : "
			"no probability table available." ) ;
			
	list<string> l ;	
	
	Probability sum = 0 ;
	Probability currentProbability ;
	

	// Should never be reached :
	Probability minProbability = 1000 ;
	Sample sampleMin = 0 ;
	
	Probability maxProbability = 0 ;
	Sample sampleMax = 0 ;
	
	string result = "Probability table is : " ;
	 
	for ( Sample sample = _lowerLimit; sample < _upperLimit; sample++ )
	{
	
		currentProbability =  _probabilitiesTable[ sample - _lowerLimit ] ;
		
		l.push_back( "Probability to realize sample " 
			+ Ceylan::toString( sample )
			+ " : " + Ceylan::toString( 100 * currentProbability )
			+ " %." ) ;
			
		sum += _probabilitiesTable[ sample - _lowerLimit ] ;
		
		if ( currentProbability > maxProbability )
		{
			maxProbability = currentProbability ;
			sampleMax = sample ;
		}	
		
		if ( currentProbability < minProbability )
		{
			minProbability = currentProbability ;
			sampleMin = sample ;
		}	
		
		
	}
		
	LogPlug::debug( 
		"Displaying corresponding look-up sample range table :" ) ;
		
	for ( Sample sample = _lowerLimit; sample < _upperLimit; sample++ )
	{
		LogPlug::debug( "Sample " + Ceylan::toString( sample ) 
			+ " stops at " + _sampleRangesTable[ sample - _lowerLimit ] ) ;
	}
	
	return result + Ceylan::formatStringList( l ) 
		+ "Probabilities sum up to "
		+ Ceylan::toString( 100 * sum ) 
		+ "%. Maximum probability ("
		+ Ceylan::toString( 100 * maxProbability ) 
		+ "%) reached for sample "
		+ Ceylan::toString( sampleMax ) 
		+ ", minimum probability ("
		+ Ceylan::toString( 100 * minProbability ) 
		+ "%) reached for sample "
		+ Ceylan::toString( sampleMin ) + "." ;  

}

						
const string RandomGeneratorFromPDF::toString( VerbosityLevels level ) 
	const throw()
{
	
	string base = "Probability Density Function-based random generator, " ;
	
	if ( _pdf != 0 )
	{
		return base + "whose PDF is " + _pdf->toString() + ". " 
			+ RandomGenerator::toString( level) ;
	}
	else
	{
		return base + "with no PDF registered. "
			+ RandomGenerator::toString( level ) ;	
	}
	
	// Useless but avoids warnings :
	return base ;
		
}


void RandomGeneratorFromPDF::preCompute() throw( MathsException ) 
{
	
	//LogPlug::trace( "RandomGeneratorFromPDF::preCompute called." ) ;
	
	/*
	 * Creates internally-used white noise generator, with 
	 * maximum sample span :
	 *
	 */
	_whiteNoiseGenerator = new WhiteNoiseGenerator( 0, RAND_MAX ) ;
	
	if	( _whiteNoiseGenerator == 0 )
		throw MathsException( "RandomGeneratorFromPDF::preCompute : "
			"allocation of internal white noise generator failed." ) ;
		
	if ( _pdf == 0 )
		throw MathsException( "RandomGeneratorFromPDF::preCompute : "
			"no probability density function available" ) ;
			

	/*
	 * We have to populate the uniform (white noise) sample space 
	 * with areas whose lengths are proportionnal to the probability 
	 * of their corresponding sample.
	 *
	 */	 
	_probabilitiesTable = new Probability[ _upperLimit - _lowerLimit ] ;
	
	if ( _probabilitiesTable == 0 )
		throw MathsException( "RandomGeneratorFromPDF::preCompute : "
			"not enough memory to allocate probability table." ) ;
	
	Probability probaTemp ;
	Probability toNormalize = 0 ;
			
	for ( Sample sample = _lowerLimit; sample < _upperLimit; sample++ )
	{
		probaTemp = (*_pdf)( sample ) ;
		_probabilitiesTable[ sample - _lowerLimit ] = probaTemp ;
		toNormalize += probaTemp ;
	}	

	/*
	 * Normalize the probability table, so that the sum of probabilities 
	 * equals 1 :
	 *
	 */
	
	for ( Sample sample = _lowerLimit; sample < _upperLimit; sample ++ )
		_probabilitiesTable[ sample - _lowerLimit ] /= toNormalize ;
	
	
	// Now records which area each sample needs in uniform space :
	
	_sampleRangesTable = new Sample[ _upperLimit - _lowerLimit ] ;
	
	if ( _sampleRangesTable == 0 )
		throw MathsException( "RandomGeneratorFromPDF::preCompute : "
			"not enough memory to allocate probability table." ) ;

	Sample sampleIndex = 0 ;
	
	for ( Sample sample = _lowerLimit; sample < _upperLimit; sample++ )
	{	
		sampleIndex += static_cast<Sample>( 
			_probabilitiesTable[ sample - _lowerLimit ] * RAND_MAX ) ;
			
		_sampleRangesTable[ sample - _lowerLimit ] = sampleIndex ;
	}	
	
	/*
	delete _probabilitiesTable ;
	_probabilitiesTable = 0 ;
	*/
		
}

