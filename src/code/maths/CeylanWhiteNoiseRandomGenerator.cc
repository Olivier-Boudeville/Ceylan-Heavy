/* 
 * Copyright (C) 2003-2013 Olivier Boudeville
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


#include "CeylanWhiteNoiseRandomGenerator.h"

#include "CeylanOperators.h"
#include "CeylanLogPlug.h"
#include "CeylanSystem.h"         // for getPreciseTime


#include <list>

#include <cstdlib>                // for rand, srand



using std::string ;


using namespace Ceylan ;
using namespace Ceylan::System ;  // for time primitives

using namespace Ceylan::Maths ;
using namespace Ceylan::Maths::Random ;

using namespace Ceylan::Log ;



#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"         // for CEYLAN_DEBUG_RANDOM and al
#endif // CEYLAN_USES_CONFIG_H




WhiteNoiseGenerator::WhiteNoiseGenerator( Sample lowerLimit, 
		Sample upperLimit ) :
	RandomGenerator( lowerLimit, upperLimit ) 
{

#if CEYLAN_DEBUG_RANDOM
	LogPlug::trace( "Maths::WhiteNoiseGenerator constructor called." ) ;
#endif // CEYLAN_DEBUG_RANDOM

	generateSeedFromCurrentTime() ;	
	preCompute() ;
	
#if CEYLAN_DEBUG_RANDOM
	LogPlug::trace( "Maths::WhiteNoiseGenerator constructor ended." ) ;
#endif // CEYLAN_DEBUG_RANDOM

}



WhiteNoiseGenerator::WhiteNoiseGenerator( Sample lowerLimit, 
		Sample upperLimit, Seed aSeed )	:
	RandomGenerator( lowerLimit, upperLimit, aSeed ) 
{

	preCompute() ;
	
}




WhiteNoiseGenerator::~WhiteNoiseGenerator() throw()
{

#if CEYLAN_DEBUG_RANDOM
	LogPlug::trace( "Maths::WhiteNoiseGenerator destructor called." ) ;
#endif // CEYLAN_DEBUG_RANDOM

}



void WhiteNoiseGenerator::generateSeedFromCurrentTime() 
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
			"WhiteNoiseGenerator::generateSeedFromCurrentTime: "
			"unable to forge random seed from current time: " 
			+ e.toString() ) ;
			
	}
	
}	 	



RandomValue WhiteNoiseGenerator::getNewValue()
{

	// rand returns int.
	
	return static_cast<RandomValue>( _lowerLimit +
		static_cast<double>( ::rand() ) * 
			( _upperLimit - _lowerLimit )/ RAND_MAX ) ; 
			
}



void WhiteNoiseGenerator::reset( Seed newSeed ) 
{

	_seed = newSeed ;
	::srand( _seed ) ;
	
}



const string WhiteNoiseGenerator::toString( VerbosityLevels level ) const
{

	return "White noise generator. " + RandomGenerator::toString( level ) ;
		
}



void WhiteNoiseGenerator::preCompute()  
{

	// Initialize new sequence with seed.
	::srand( _seed ) ;
	
}

