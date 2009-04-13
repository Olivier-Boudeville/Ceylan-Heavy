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


#ifndef CEYLAN_PROBABILITY_H_
#define CEYLAN_PROBABILITY_H_


#include "CeylanMathsBasic.h" // for Math::Functor
#include "CeylanTypes.h"      // for Ceylan::Sint32, etc.


#include <string>



namespace Ceylan
{

	namespace Maths
	{
	
	
		namespace Random
		{
		
		
		
			/// Samples for distributions.
			typedef Ceylan::Sint32 Sample ;
		
		
			/// Used for standard deviation.
			typedef Ceylan::Float32 Deviation ;
		
		
			/// Probability, should always be between 0 and 1, included.
			typedef Ceylan::Float32 Probability ;		
				
					
		
			/**
			 * A probability function (also called the probability density
			 * function, or density function) of a continuous distribution
			 * is defined as the derivative of the (cumulative) 
			 * distribution function.
			 *
			 */
			class CEYLAN_DLL ProbabilityFunction : public Ceylan::Functor
			{
		
				public:
			
			
					/// Basic constructor.
					ProbabilityFunction() throw() ;
				
					/// Basic virtual destructor;
					virtual ~ProbabilityFunction() throw() ;
				
				
					/**
					 * The callable method, returns the probability that 
					 * this sample is realized.
					 *
					 */
					virtual Probability operator() ( Sample aSample ) 
						const throw() = 0 ;


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
						VerbosityLevels level = high ) const throw() = 0 ;
						
						
									
 			} ;			
							
		}
		
	}

}


#endif // CEYLAN_PROBABILITY_H_
