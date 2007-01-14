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
					NormalProbabilityFunction( Sample mean, Deviation sigma ) 
						throw( MathsException ) ;
				
				
					/// Basic virtual destructor.
					virtual ~NormalProbabilityFunction() throw() ;
				
				
					/**
					 * The callable method, returns the probability 
					 * that this sample is realized.
					 *
					 * For normal law, it is :
					 * 
					 * P(x) = exp( -1/2*((x-mean)/sigma)²) 
					 *            / (sigma * sqrt(2.Pi))
					 *
					 */
					virtual Probability operator() ( Sample aSample ) 
						const throw() ;
		
					
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
