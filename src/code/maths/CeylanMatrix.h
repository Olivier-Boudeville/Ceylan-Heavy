#ifndef CEYLAN_MATRIX_H_
#define CEYLAN_MATRIX_H_


#include "CeylanLinear.h"              // for Real, LinearException
#include "CeylanTypes.h"               // for Uint8

#include "CeylanTextDisplayable.h"     // for inheritance

#include <string>



namespace Ceylan
{


	namespace Maths
	{


		namespace Linear
		{

					
						
			/**
			 * Abstract class for square matrices, whose coordinates are
			 * floating-point values (Real).
			 *
			 * Indices ranges for 0 to Dimensions-1, the first index is 
			 * the abscissa (left to right index) and the second one the
			 * ordinate (top to bottom index).
			 *
			 * @example, m[1][0] refers to the matrix element located 
			 * in the second column (1) at its top (0).
			 *
			 * @note Beware of counter-intuitive operator priorities.
			 *
			 * @note Some methods could be gathered in this abstract
			 * class, instead of child classes (ex : setAllElementsTo
			 * could use the Dimensions attribute so that this method
			 * could be generically defined). 
			 * The problem is however that Dimensions should be static,
			 * but it would not be overloaded by child classes. 
			 * Even from a Matrix3, lines such as
			 * 'for ( MatrixIndex i = 0 ; i < Dimensions ; i++ )'
			 * would use Matrix::Dimensions, and not Matrix3::Dimensions.
			 * A solution could be to define a getDimensions method.
			 * As it would have to be overloaded, inlining is not an
			 * option, and the overhead would remain significant. 
			 * Better define these methods in each child class, even if 
			 * their text is exactly duplicated.
			 * 
			 */
			class Matrix : public TextDisplayable
			{


				public:
				
				
					/**
					 * Constructs a new matrix.
					 *
					 */
					Matrix() throw() ;
				
				
					/// Basic virtual destructor.
					virtual ~Matrix() throw() ;
					
					
					/**
					 * Sets this matrix to the null matrix 
					 *(all elements zeroed).
					 *
					 */
					virtual void nullify() throw() ;
					
					
					/**
					 * Sets this matrix so that all its elements equal
					 * commonValue.
					 *
					 */
					virtual void setAllElementsTo( Real commonValue ) 
						throw() = 0 ;

					
					/**
					 * Returns indexed element. 
					 * Index ranges from 0 to Dimensions-1 for both
					 * dimensions.
					 *
					 * @note An emergency shutdown is triggered if index
					 * is out of bounds.
					 *
					 */
					virtual Real getElementAt( MatrixIndex abscissa, 
						MatrixIndex ordinate ) const throw() = 0 ;
					
					
					/**
					 * Sets indexed element to specified value. 
					 * Index ranges from 0 to Dimensions-1 for both
					 * dimensions.
					 *
					 * @note An emergency shutdown is triggered if index
					 * is out of bounds.
					 *
					 */
					virtual void setElementAt( MatrixIndex abscissa, 
						MatrixIndex ordinate, Real newValue ) throw() = 0 ;
					
					
					/// Sets this matrix to the identity matrix.
					virtual void setToIdentity() throw() ;
					
					
					/**
					 * Sets this matrix to the a diagonal matrix 
					 * whose diagonal term is diagonalTerm.
					 *
					 */
					virtual void setToDiagonal( Real diagonalTerm ) 
						throw() = 0 ;
					
					
					/// Tranposes this matrix.
					virtual void transpose() throw() = 0 ;
					
					
					/// Computes this matrix's trace.
					virtual Real trace() const throw() = 0 ;
					
					
					/// Computes this matrix's determinant.
					virtual Real determinant() const throw() = 0 ;
									
							
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
					 
				 
					 /**
					  * Tells what is the dimension for this Matrix.
					  *
					  * @note To be redefined by each Matrix child class.
					  *
					  */
					 static const MatrixIndex Dimensions = 0 ;
					
				

			} ;
			
		}
		
	}
	
}


#endif // CEYLAN_MATRIX_H_
