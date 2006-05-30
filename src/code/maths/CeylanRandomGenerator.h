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
			class RandomGenerator : public Ceylan::Object
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
					explicit RandomGenerator( Sample lowerLimit, 
						Sample upperLimit, Seed aSeed = DefaultSeed ) 
							throw( MathsException ) ;
					
					
            		/// Basic virtual destructor.
            		virtual ~RandomGenerator() throw() ;
					
					
					/// Returns the next random value.
					virtual RandomValue getNewValue() throw() = 0 ;
					
					
					/// Resets the random generator with specified seed.
					virtual void reset( Seed newSeed ) throw() = 0 ;
					
										
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
						VerbosityLevels level = high ) const throw() ;
							

					
					// Static section.
					
					
					/// Default seed to be used.
					static const Seed DefaultSeed ;


				
				protected:
					
				
					/**
					 * Does the computation necessary to determine next 
					 * new value.  
					 *
					 */
					virtual void preCompute() throw( MathsException ) = 0 ;		
					
					
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
					 * Copy constructor made private to ensure that it
					 * will be never called.
					 *
					 * The compiler should complain whenever this 
					 * undefined constructor is called, implicitly or not.
					 * 
					 */			 
					RandomGenerator( const RandomGenerator & source ) throw() ;
			
			
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
						const RandomGenerator & source ) throw() ;
					
								
			
			} ;
								
		}
		
	}

}


#endif // CEYLAN_RANDOM_GENERATOR_H_
