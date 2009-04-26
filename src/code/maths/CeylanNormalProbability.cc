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


#include "CeylanNormalProbability.h"

#include "CeylanOperators.h"


using std::string ;

using namespace Ceylan ;
using namespace Ceylan::Maths ;
using namespace Ceylan::Maths::Random ;




NormalProbabilityFunction::NormalProbabilityFunction( Sample mean, 
		Deviation sigma ) :
	ProbabilityFunction(),
	_mean( mean ),
	_sigma( sigma )
{

	if ( Maths::IsNull( _sigma ) )
		throw MathsException( "NormalProbabilityFunction constructor: "
			"standard deviation (sigma) must not be null or almost." ) ;

}



NormalProbabilityFunction::~NormalProbabilityFunction() throw() 
{

}



Probability NormalProbabilityFunction::operator() ( Sample aSample ) const
{

	/*
	 * Gaussian law: 
	 * P(x) = exp( -1/2*((x-mean)/sigma)²) / (sigma * sqrt(2.Pi))
	 *
	 */
	
	return static_cast<Probability>( 
		Exp( -0.5 * Pow( ( aSample - _mean ) / _sigma, 2 ) 
			/ ( _sigma * Sqrt( 2 * Pi ) ) ) ) ;
	
}



const string NormalProbabilityFunction::toString( 
	VerbosityLevels level ) const
{

	return "Normal probability function whose mean is " 
		+ Ceylan::toString( _mean ) 
		+ " and whose standard deviation is " 
		+ Ceylan::toString( _sigma ) ;
		
}

