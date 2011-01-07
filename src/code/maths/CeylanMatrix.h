/* 
 * Copyright (C) 2003-2011 Olivier Boudeville
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
			 * class, instead of child classes (ex: setAllElementsTo
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
			class CEYLAN_DLL Matrix : public TextDisplayable
			{


				public:
				
				
				
					/**
					 * Constructs a new matrix.
					 *
					 */
					Matrix() ;
				
				
				
					/// Basic virtual destructor.
					virtual ~Matrix() throw() ;
					
					
					
					/**
					 * Sets this matrix to the null matrix 
					 *(all elements zeroed).
					 *
					 */
					virtual void nullify() ;
					
					
					
					/**
					 * Sets this matrix so that all its elements equal
					 * commonValue.
					 *
					 */
					virtual void setAllElementsTo( Real commonValue ) = 0 ;


					
					/**
					 * Returns indexed element. 
					 * Index ranges from 0 to Dimensions-1 for both
					 * dimensions.
					 *
					 * @throw MathsException if index is out of bounds and if
					 * in debug mode.
					 *
					 */
					virtual Real getElementAt( MatrixIndex abscissa, 
						MatrixIndex ordinate ) const = 0 ;
					
					
					
					/**
					 * Sets indexed element to specified value. 
					 * Index ranges from 0 to Dimensions-1 for both
					 * dimensions.
					 *
					 * @throw MathsException if index is out of bounds and if
					 * in debug mode.
					 *
					 */
					virtual void setElementAt( MatrixIndex abscissa, 
						MatrixIndex ordinate, Real newValue ) = 0 ;
					
					
					
					/// Sets this matrix to the identity matrix.
					virtual void setToIdentity() ;
					
					
					
					/**
					 * Sets this matrix to the a diagonal matrix 
					 * whose diagonal term is diagonalTerm.
					 *
					 */
					virtual void setToDiagonal( Real diagonalTerm )	= 0 ;
					
					
					
					/// Tranposes this matrix.
					virtual void transpose() = 0 ;
					
					
					
					/// Computes this matrix's trace.
					virtual Real trace() const = 0 ;
					
					
					
					/// Computes this matrix's determinant.
					virtual Real determinant() const = 0 ;
							
									
							
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
