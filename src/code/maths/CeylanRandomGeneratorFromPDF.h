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
			class RandomGeneratorFromPDF : public RandomGenerator 
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
					explicit RandomGeneratorFromPDF( 
							const ProbabilityFunction & pdf, 
							Sample lowerLimit, Sample upperLimit, 
							Seed aSeed = DefaultSeed ) 
						throw( MathsException ) ;
					
					
            		/// Basic virtual destructor.
            		virtual ~RandomGeneratorFromPDF() throw() ;
					
					
					/// Returns the next random value.
					virtual RandomValue getNewValue() throw() ;
					
					
					/// Resets the random generator with specified seed.
					virtual void reset( Seed neeSeed ) throw() ;
						
										
					/**
					 * Returns a user-friendly description of the 
					 * internal probability table.
					 *
					 * @throw MathsException if no probability table 
					 * is available.
					 *
					 */
					 virtual const std::string displayProbabilities() 
					 	const throw( MathsException ) ;
					 
					 
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
