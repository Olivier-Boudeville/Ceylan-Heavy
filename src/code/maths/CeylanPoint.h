#ifndef CEYLAN_POINT_H_
#define CEYLAN_POINT_H_


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
			 * Abstract point, whose coordinates are floating-point values
			 * (Real).
			 *
			 * Points and vectors are basically the same beasts, except 
			 * they can mean different things to the developer. 
			 * The Vector class is more versatile.
			 *
			 * @note Beware of counter-intuitive operator priorities.
			 *
			 * @note Some methods could be gathered in this abstract class,
			 * instead of child classes (ex : setAllElementsTo could use 
			 * the Dimensions attribute so that this method could be 
			 * generically defined). The problem is however that Dimensions
			 * should be static, but it would not be overloaded by child
			 * classes. Even from a Matrix3, lines such as
			 * 'for ( MatrixIndex i = 0 ; i < Dimensions ; i++ )' would use
			 * Matrix::Dimensions, and not Matrix3::Dimensions. 
			 * A solution could be to define a getDimensions method.
			 * As it would have to be overloaded, inlining is not an option, 
			 * and the overhead would remain significant. 
			 * Better define these methods in each child class, even if 
			 * their text is exactly duplicated.
			 *
			 */
			class CEYLAN_DLL Point : public TextDisplayable
			{


				public:
				
				
					/**
					 * Constructs a new point.
					 *
					 */
					explicit Point() throw() ;
				
				
					/// Basic virtual destructor.
					virtual ~Point() throw() ;
					

					/**
					 * Sets this Point to the null Point (all coordinates
					 * zeroed).
					 *
					 */
					virtual void nullify() throw() ;
					
					
					/**
					 * Sets this point so that all its elements equal
					 * commonValue.
					 *
					 */
					virtual void setAllElementsTo( Real commonValue ) 
						throw() = 0 ;
					
					
					/**
					 * Returns indexed element. 
					 * Index ranges from 0 to Dimensions-1.
					 *
					 * @note An emergency shutdown is triggered if index 
					 * is out of bounds.
					 *
					 */
					virtual Real getElementAt( MatrixIndex index ) 
						const throw() = 0 ;
					
					
					/**
					 * Sets indexed element to specified value. 
					 * Index ranges from 0 to Dimensions-1.
					 *
					 * @note An emergency shutdown is triggered if index 
					 * is out of bounds.
					 *
					 */
					virtual void setElementAt( MatrixIndex index, 
						Real newValue ) throw() = 0 ;
					
					 
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
				
					 
					/**
					 * Tells what is the dimension for this point.
					 *
					 */
					static const MatrixIndex Dimensions = 0 ;
					

			} ;

		}
		
	}
	
}


#endif // CEYLAN_POINT_H_
