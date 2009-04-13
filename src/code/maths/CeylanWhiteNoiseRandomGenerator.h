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


#ifndef CEYLAN_WHITE_NOISE_GENERATOR_H_
#define CEYLAN_WHITE_NOISE_GENERATOR_H_


#include "CeylanRandomGenerator.h"  // for RandomGenerator


#include <string>



namespace Ceylan
{

	namespace Maths
	{
	
	
		namespace Random
		{
		
		
			
			
			/**
			 * White noise generator : the probabilistic law of the 
			 * output random values is uniform : each value should have
			 * the same probability of showing up, in specified range. 
			 *
			 * Seed value for randon generators can be specified or forged
			 * automatically from current time.
			 *
			 */
			class CEYLAN_DLL WhiteNoiseGenerator : public RandomGenerator 
			{
			
			
				public:

				
					/**
					 * Creates a white noise generator which will produce
					 * values between specified <b>lowerLimit</b>, 
					 * included, and specified <b>upperLimit</b>, excluded.
					 * 
					 * The generator will be initialized by a seed will 
					 * computed from current time, so that two successive
					 * executions should use, generally, different seeds.
					 * 
					 * @param the lower limit of the value of generated 
					 * samples. This value is included (i.e. can be drawn).
					 *
					 * @param the upper limit of the value of generated 
					 * samples. This value is excluded (i.e. cannot be drawn).
					 * 
					 * @throw MathsException if lowerLimit is not
					 * strictly inferior to upperLimit.
					 *
					 */					 
					WhiteNoiseGenerator( Sample lowerLimit, 
						Sample upperLimit ) throw( MathsException ) ;

				
					/**
					 * Creates a white noise generator which will produce
					 * values between specified <b>lowerLimit</b>, 
					 * included, and specified <b>upperLimit</b>, excluded.
					 * 
					 * The generator will be initialized by specified 
					 * seed, which may be CeylanRandomGenerator::DefaultSeed. 
					 *
					 * @param the lower limit of the value of generated 
					 * samples. This value is included (i.e. can be drawn).
					 *
					 * @param the upper limit of the value of generated 
					 * samples. This value is excluded (i.e. cannot be drawn).
					 *
					 * @param aSeed the specified seed
					 *
					 * @note Using always the same seed will lead to 
					 * always the same random numbers.
					 * 
					 * @note The seed does <b>not</b> need to be in range
					 * [lowerLimit;upperLimit[
					 *
					 * @throw MathsException if lowerLimit is not 
					 * strictly inferior to upperLimit.
					 *
					 */					 
					WhiteNoiseGenerator( Sample lowerLimit, 
							Sample upperLimit, Seed aSeed ) 
						throw( MathsException ) ;
					
					
					
					
            		/// Basic virtual destructor.
            		virtual ~WhiteNoiseGenerator() throw() ;
					
					
					/**
					 * Generates a new seed for this generator, using 
					 * current time so that two successive launches 
					 * have random seeds, hence actual random series 
					 * of values.
					 *
					 * @throw MathsException if the system time could
					 * not be determined.
					 *
					 */
					virtual void generateSeedFromCurrentTime() 
						throw( MathsException ) ;
					
					
					/// Returns the next random value.
					virtual RandomValue getNewValue() throw() ;
					
					
					/// Resets the random generator with specified seed.
					virtual void reset( Seed neeSeed ) throw() ;
										
					
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
						VerbosityLevels level = high ) const throw() ;
				
				
				
												
				protected:
									
									
					/**
					 * Does the computation necessary to determine next
					 * new value.
					 *
					 */
					virtual void preCompute() throw( MathsException ) ;		
				
				
				
				private:
				
				
					/**
					 * Copy constructor made private to ensure that it
					 * will be never called.
					 *
					 * The compiler should complain whenever this 
					 * undefined constructor is called, implicitly or not.
					 * 
					 */			 
					WhiteNoiseGenerator( 
						const WhiteNoiseGenerator & source ) throw() ;
			
			
					/**
					 * Assignment operator made private to ensure that
					 * it will be never called.
					 *
					 * The compiler should complain whenever this 
					 * undefined operator is called, implicitly or not.
					 * 
					 *
					 */			 
					WhiteNoiseGenerator & operator = ( 
						const WhiteNoiseGenerator & source ) throw() ;
					
				
			} ;
		
		}
		
	}

}


#endif // CEYLAN_WHITE_NOISE_GENERATOR_H_
