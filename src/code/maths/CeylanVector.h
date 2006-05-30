#ifndef CEYLAN_VECTOR_H_
#define CEYLAN_VECTOR_H_


#include "CeylanTextDisplayable.h"     // for TextDisplayable
#include "CeylanLinear.h"              // for Real, LinearException


#include <string>


namespace Ceylan
{


	namespace Maths
	{


		namespace Linear
		{

			

			/**
			 * Abstract class for vectors, whose coordinates are floating-point values (Real).
			 *
			 * Indices ranges for 0 to Dimensions-1			 
			 *
			 * @note Beware of counter-intuitive operator priorities.
			 *
			 * @note Some methods could be gathered in this abstract class, instead of child classes
			 * (ex : setAllElementsTo could use the Dimensions attribute so that this method could
			 * be generically defined). The problem is however that Dimensions should be static,
			 * but it would not be overloaded by child classes. Even from a Vector3, lines such as
			 * 'for ( MatrixIndex i = 0 ; i < Dimensions ; i++ )'  would use Vector::Dimensions,
			 * and not Vector3::Dimensions. A solution could be to define a getDimensions method.
			 * As it would have to be overloaded, inlining is not an option, and the overhead
			 * would remain significant. Better define these methods in each child class, even if 
			 * their text is exactly duplicated.
			 *
			 */
			class Vector : public TextDisplayable
			{
		

				public:
				
				
					/**
					 * Constructs a new vector. 
					 *
					 */
					 Vector() throw() ;
				
				
					/// Basic virtual destructor.
					virtual ~Vector() throw() ;
					
					
					/// Sets this vector to the null vector (all coordinates zeroed).
					virtual void nullify() throw() ;
					
					
					/// Sets this vector so that all its elements equal commonValue.
					virtual void setAllElementsTo( Real commonValue ) throw() = 0 ;
					
						
					/**
					 * Normalizes this vector, so that its norm equals one.
					 *
					 * @throw LinearException if the null vector is passed, since it is the only one
					 * that cannot be normalized.
					 *
					 */
					virtual void normalize() throw( LinearException ) = 0 ;


					/**
					 * Returns this vector's magnitude.
					 *
					 * @see operator ~
					 *
					 */
					virtual Real magnitude() const throw() = 0 ;

					
					/**
					 * Returns a user-friendly description of the state of this object.
					 *
					 * @param level the requested verbosity level.
					 *
					 * @note Text output format is determined from overall settings.
					 *
					 * @see TextDisplayable
					 *
					 */
            		virtual const std::string toString( VerbosityLevels level = high ) 
						const throw() ;
					 
					 
					/**
					 * Tells what is the dimension for this vector.
					 *
					 */
					static const MatrixIndex Dimensions = 0 ;
								

			} ;

		}
		
	}
	
}


#endif // CEYLAN_VECTOR_H_
