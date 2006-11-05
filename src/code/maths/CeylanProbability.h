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
