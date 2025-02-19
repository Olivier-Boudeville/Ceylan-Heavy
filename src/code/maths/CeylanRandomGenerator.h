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


#ifndef CEYLAN_RANDOM_GENERATOR_H_
#define CEYLAN_RANDOM_GENERATOR_H_


#include "CeylanObject.h"          // for inheritance
#include "CeylanProbability.h"     // for Sample
#include "CeylanMathsBasic.h"      // for MathsException


#include <string>




namespace Ceylan
{


	namespace Maths
	{
	
	
		namespace Random
		{
		
		
		
			/// Seeds are initial values used for random generators.
			typedef unsigned int Seed ;
			
			
			/// The type of the results of a random generator.
			typedef unsigned int RandomValue ;
			
				
				
			/**
			 * Abstract mother class of all kinds of random generators.
			 *
			 * Each of these generators should implement at least the
			 * getNewValue method.
			 *
			 */
			class CEYLAN_DLL RandomGenerator : public Ceylan::Object
			{
			
			
				public:
				

				
					/**
					 * Creates a random generator which will produce values
					 * between specified <b>lowerLimit</b>, included, 
					 * and specified <b>upperLimit</b>, excluded.
					 * 
					 * The generator will be initialized by specified 
					 * seed, if provided, otherwise will default to 
					 * DefaultSeed.
					 * 
					 * @note The seed does <b>not</b> need to be in range
					 * [lowerLimit;upperLimit[
					 *
					 * @throw MathsException if lowerLimit is not strictly
					 * inferior to upperLimit.
					 *
					 */					 
					RandomGenerator( Sample lowerLimit, Sample upperLimit, 
						Seed aSeed = DefaultSeed ) ;
					
					
            		/// Basic virtual destructor.
            		virtual ~RandomGenerator() throw() ;
					
					
					
					/// Returns the next random value.
					virtual RandomValue getNewValue() = 0 ;
					
					
					
					/// Resets the random generator with specified seed.
					virtual void reset( Seed newSeed ) = 0 ;
					
									
										
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
							


					
					// Static section.
					
					
					/// Default seed to be used.
					static const Seed DefaultSeed ;



				
				protected:
					
					
				
					/**
					 * Does the computation necessary to determine next 
					 * new value.  
					 *
					 */
					virtual void preCompute()  = 0 ;		
					
					
					
					/**
					 * The lower limit to output random values
					 * (this value is the first that <b>can</b> be drawn).
					 *
					 */
					Sample _lowerLimit ;
					
					
					
					/**
					 * The upper limit to output random values 
					 * (this value is the first that is superior to 
					 * _lowerLimit and that <b>cannot</b> be drawn).
					 *
					 */
					Sample _upperLimit ;



					/**
					 * The seed which was used to initialize the 
					 * generator.
					 *
					 */
					Seed _seed ;
										
					
					
					
				private:
				
				
				
					/**
					 * Copy constructor  specifically defined.
					 * 
					 */			 
					RandomGenerator( const RandomGenerator & source ) ;
			
			
			
					/**
					 * Assignment operator made private to ensure that
					 * it will be never called.
					 *
					 * The compiler should complain whenever this 
					 * undefined operator is called, implicitly or not.
					 * 
					 *
					 */			 
					RandomGenerator & operator = ( 
						const RandomGenerator & source ) ;
							
			
			} ;
			
								
		}
		
	}

}



#endif // CEYLAN_RANDOM_GENERATOR_H_

