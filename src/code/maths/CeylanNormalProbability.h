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
 * Author: Olivier Boudeville (olivier (dot) boudeville (at) esperide (dot) com)
 *
 */


#ifndef CEYLAN_NORMAL_PROBABILITY_H_
#define CEYLAN_NORMAL_PROBABILITY_H_


#include "CeylanProbability.h"  // for inheritance
#include "CeylanMathsBasic.h"   // for MathsException

#include <string>



namespace Ceylan
{


	namespace Maths
	{
	
	
		namespace Random
		{
		
		
		
			/**
			 * Probability function used for gaussian (normal) distributions.
			 *
			 */
			class CEYLAN_DLL NormalProbabilityFunction : 
				public ProbabilityFunction
			{
		
		
				public:
			
			
					/**
					 * Creates the distribution.
					 *
					 * @param mean the mean value
					 *
					 * @param sigma the standard deviation, must not be 
					 * null or almost null.
					 *
					 * @throw MathsException if sigma is zero.
					 *
					 */
					NormalProbabilityFunction( Sample mean, Deviation sigma ) ;
				
				
				
					/// Basic virtual destructor.
					virtual ~NormalProbabilityFunction() throw() ;
				
				
				
					/**
					 * The callable method, returns the probability 
					 * that this sample is realized.
					 *
					 * For normal law, it is:
					 * 
					 * P(x) = exp( -1/2*((x-mean)/sigma)²) 
					 *            / (sigma * sqrt(2.Pi))
					 *
					 */
					virtual Probability operator() ( Sample aSample ) const ;
		
					
					
					/**
					 * Returns a user-friendly description of the state 
					 * of this object.
					 *
					 * @param level the requested verbosity level.
					 *
					 * @note Text output format is determined from 
					 * overall settings.
					 *
					 * @see TextDisplayable
					 *
					 */
            		virtual const std::string toString( 
						VerbosityLevels level = high ) const ;

		
		
				protected:
			
			
			
					/// The mean value for the normal law.
					Sample _mean ;
				
				
					/// The standard deviation for the normal law.
					Deviation _sigma ;
					
					
							
 			} ;
				
											
		}
		
	}

}



#endif // CEYLAN_NORMAL_PROBABILITY_H_

