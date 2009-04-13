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
	
	LogPlug::trace( "Maths::RandomGeneratorFromPDF constructor called." ) ;

	preCompute() ;

}


RandomGeneratorFromPDF::~RandomGeneratorFromPDF() throw()
{

	// Not owning _pdf, not destroying it.
	
	if ( _whiteNoiseGenerator != 0 )
		delete _whiteNoiseGenerator ;
	
	if ( _probabilitiesTable != 0 )
		delete [] _probabilitiesTable ;
	
	if ( _sampleRangesTable != 0 )
		delete [] _sampleRangesTable ;

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
	
	Ceylan::Uint32 count = 0 ;
	
	/*
	LogPlug::debug( "Comparing " + Ceylan::toString( uniformRandomValue )
		+ " with " + Ceylan::toString( _sampleRangesTable[ count ] ) + " : " ) ;
	*/
		
	while ( uniformRandomValue > _sampleRangesTable[ count ] )
		count++ ;

	RandomValue res = count + _lowerLimit ;

#if CEYLAN_DEBUG

	if ( static_cast<Sample>( res ) >= _upperLimit )
	{

		LogPlug::error( "RandomGeneratorFromPDF::getNewValue : value "
			+ Ceylan::toString( res ) 
			+ " was drawn, whereas upper excluded limit is " 
			+ Ceylan::toString( _upperLimit ) ) ;

		try
		{
			displayProbabilities() ;
		}
		catch ( const Ceylan::Exception & e )
		{
			LogPlug::error( "RandomGeneratorFromPDF::getNewValue : "
				"additional error : " + e.toString() ) ;
		}
		

		Ceylan::emergencyShutdown( "RandomGeneratorFromPDF::getNewValue : "
			"incorrect random value returned : " 
			+ Ceylan::toString( res ) + "." ) ;

	}
#endif // CEYLAN_DEBUG

	return res ;

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
	
		currentProbability = _probabilitiesTable[ sample - _lowerLimit ] ;
		
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
	
	LogPlug::debug( "Upper bound to White noise random values is " 
		+ Ceylan::toString( RAND_MAX ) ) ;

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
	
	LogPlug::trace( "RandomGeneratorFromPDF::preCompute called." ) ;
	
	/*
	 * Creates internally-used white noise generator, with 
	 * maximum sample span :
	 *
	 */
	_whiteNoiseGenerator = new WhiteNoiseGenerator( 0, RAND_MAX ) ;
	
	if ( _whiteNoiseGenerator == 0 )
		throw MathsException( "RandomGeneratorFromPDF::preCompute : "
			"allocation of internal white noise generator failed." ) ;
		
	if ( _pdf == 0 )
		throw MathsException( "RandomGeneratorFromPDF::preCompute : "
			"no probability density function available" ) ;
			
	LogPlug::trace( "Creating probability table." ) ;

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
	 * Ensures that rounding does not cause latest sample range to be
	 * lower than the maximum value that the white noise generator can
	 * draw (RAND_MAX) :
	 *
	 */
	_sampleRangesTable[ _upperLimit - _lowerLimit - 1 ] = RAND_MAX ;

	/*
	delete _probabilitiesTable ;
	_probabilitiesTable = 0 ;
	*/
	
	LogPlug::trace( "RandomGeneratorFromPDF::preCompute ended." ) ;

}

