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


#ifndef CEYLAN_RANDOM_GENERATOR_FROM_PDF_H_
#define CEYLAN_RANDOM_GENERATOR_FROM_PDF_H_


#include "CeylanRandomGenerator.h"  // for inheritance


#include <string>



namespace Ceylan
{


	namespace Maths
	{
	
	
		namespace Random
		{
		
		
			
			// Needed internally.
			class WhiteNoiseGenerator ;
			
			
			
			/**
			 * Random generator created from a user-defined probability
			 * density function (PDF).
			 *
			 * Output random value will obey the specified law of 
			 * probability.
			 *
			 * @note For a continuous distribution, this probability
			 * function is defined as the derivative of the (cumulative)
			 * distribution function. 
			 *
			 */
			class CEYLAN_DLL RandomGeneratorFromPDF : public RandomGenerator 
			{
			
			
				public:
				
				
					/**
					 * Creates a random generator which will produce
					 * values between specified <b>lowerLimit</b>, 
					 * included, and specified <b>upperLimit</b>, excluded,
					 * with respect to the specified probability density
					 * function (PDF), described thanks to a
					 * ProbabilityFunction instance.
					 * 
					 * The generator will be initialized by specified seed,
					 * if provided, otherwise will default to DefaultSeed.
					 * 
					 * @note The seed does <b>not</b> need to be in range
					 * [lowerLimit;upperLimit[
					 *
					 * @note The random generator will use, but not own,
					 * the specified ProbabilityFunction object.
					 *
					 * @throw MathsException if lowerLimit is not strictly
					 * inferior to upperLimit.
					 *
					 */					 
					RandomGeneratorFromPDF( 
							const ProbabilityFunction & pdf, 
							Sample lowerLimit, Sample upperLimit, 
							Seed aSeed = DefaultSeed ) ;
					
					
					
            		/// Basic virtual destructor.
            		virtual ~RandomGeneratorFromPDF() throw() ;
					
					
					
					/// Returns the next random value.
					virtual RandomValue getNewValue() ;
					
					
					
					/// Resets the random generator with specified seed.
					virtual void reset( Seed neeSeed ) ;
						
						
										
					/**
					 * Returns a user-friendly description of the 
					 * internal probability table.
					 *
					 * @throw MathsException if no probability table 
					 * is available.
					 *
					 */
					 virtual const std::string displayProbabilities() const  ;
					 
					 
					 
					/**
					 * Returns a user-friendly description of the 
					 * state of this object.
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
					
				
				
					/**
					 * Does the computation necessary to determine next
					 * new value.  
					 *
					 */
					virtual void preCompute()  ;	
					
					
					
					/// Internal link to probability function.
					ProbabilityFunction const * _pdf ;
					
					
					
					/// Internal white noise random generator.
					WhiteNoiseGenerator * _whiteNoiseGenerator ;
					
					
					
					/**
					 * Where probabilities calculated thanks to the PDF
					 * are stored.
					 *
					 */
					Probability * _probabilitiesTable ;
				
				
				
					/**
					 * Tells how the uniform random space is split into 
					 * sample areas.
					 *
					 */
					Sample * _sampleRangesTable ;
					
					
			} ;
			
		
		}
		
	}

}



#endif // CEYLAN_RANDOM_GENERATOR_FROM_PDF_H_

