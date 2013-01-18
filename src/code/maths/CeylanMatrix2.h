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
 * Author: Olivier Boudeville (olivier.boudeville@esperide.com)
 *
 */


#ifndef CEYLAN_MATRIX_2_H_
#define CEYLAN_MATRIX_2_H_


#include "CeylanVector2.h"             // for Vector2, used in 'CreateFrom'
#include "CeylanMatrix.h"              // for inheritance
#include "CeylanLinear.h"              // for Real, LinearException

#include <string>




namespace Ceylan
{


	namespace Maths
	{


		namespace Linear
		{


			
			// Applied on matrices.		
			class Bipoint ;


			// Applied on matrices.		
			class Vector2 ;
		
			
			
			/**
			 * Two-dimensional square matrix (2x2), whose coordinates are
			 * floating-point values (Real).
			 *
			 * Indices ranges for 0 to 1, the first index is the abscissa 
			 * (left to right index) and the second one the ordinate 
			 * (top to bottom index).
			 *
			 * @example, m[1][0] refers to the matrix element located in 
			 * the second column (1) at its top (0).
			 *
			 * @note Beware of counter-intuitive operator priorities.
			 *
			 * @see CeylanEndomorphism.h for most common endomorphisms.
			 *
			 */
			class CEYLAN_DLL Matrix2: public Matrix
			{


				// Friends section.


				/// Must access to each other's coordinates.				
				friend class Bipoint ;


				/// Must access to each other's coordinates.					
				friend class Vector2 ;
				
				

				/**  
				 * Tells whether the two matrices have the same
				 * elements.
				 *
				 * @note Exact comparison of floating point values is 
				 * meaningless on most cases, hence strict equality is
				 * replaced here by relative equality.
				 *
				 * @see Maths::AreRelativelyEqual.
				 *
				 */
				CEYLAN_DLL friend bool operator == ( const Matrix2 & m1, 
					const Matrix2 & m2 ) ;
					
					
								
				/**  
				 * Tells whether the two matrices are different.
				 *
				 * @note Exact comparison of floating point values is 
				 * meaningless on most cases, hence strict equality is
				 * replaced here by relative equality.
				 *
				 * @see Maths::AreRelativelyEqual.
				 *
				 */
				CEYLAN_DLL friend bool operator != ( const Matrix2 & m1, 
					const Matrix2 & m2 ) ;



				/**  
				 * Adds two matrices: result = m1 + m2.
				 *
				 */
				CEYLAN_DLL friend Matrix2 operator + ( const Matrix2 & m1, 
					const Matrix2 & m2 ) ;
				
				
				
				/**  
				 * Substracts two matrices: result = m1 - m2.	
				 *
				 */				
				CEYLAN_DLL friend Matrix2 operator - ( const Matrix2 & m1, 
					const Matrix2 & m2 ) ;
				
				
				
				/**  
				 * Multiplies two matrices: result = m1 * m2.	
				 *
				 */								
				CEYLAN_DLL friend Matrix2 operator * ( const Matrix2 & m1, 
					const Matrix2 & m2 ) ;
				
				
				
				/**  
				 * Multiplies a matrix by a scalar: result = lambda * m.
				 *
				 */							
				CEYLAN_DLL friend Matrix2 operator * ( Real lambda, 
					const Matrix2 & m ) ;
				
				
				
				/**
				 * Returns the inverse (reciproqual) of specified matrix,
				 * if possible.
				 *
				 * @throw LinearException if the matrix is singuler.
				 *
				 */
				CEYLAN_DLL friend Matrix2 operator ! ( const Matrix2 & m ) ;
				
				
				
				/**  
				 * Returns the transposed matrix corresponding to 
				 * this matrix.
				 *
				 */
				CEYLAN_DLL friend Matrix2 operator ~ ( const Matrix2 & m ) ;



				/** 	
				 * Multiplies a matrix by a vector: result = m * v
				 *
				 */
				CEYLAN_DLL friend Vector2 operator * ( const Matrix2 & m , 
					const Vector2 & v ) ;




				public:
				
				
				
					/**
					 * Constructs a new 2x2 matrix with specified elements.
					 *
					 * @note If no argument is specified, constructs the 
					 * null matrix.
					 *
					 */
					explicit Matrix2( Real x0 = 0, Real x1 = 0, 
						Real y0 = 0, Real y1 = 0 ) ;
				
				
				
					/** 
					 * Copy constructor.
					 *
					 * @note Cannot be 'explicit', as it would prevent
					 * the multiply operator Matrix2 m = m1 * m2.
					 * to return a Matrix2 on the stack.
					 *
					 */
					Matrix2( const Matrix2 & source ) ;
					
					
					
					/// Basic virtual destructor.
					virtual ~Matrix2() throw() ;
					
					
					
					/**
					 * Reassigns all matrix elements.
					 *
					 */
					virtual void setTo( Real x0 = 0, Real x1 = 0, 
						Real y0 = 0, Real y1 = 0 ) ;
					
					
					
					/**
					 * Reassigns specified column of this matrix.
					 *
					 * @note Column numbers range from 0 to 2 (included).
					 *
					 */
					virtual void setColumn( MatrixIndex columnNumber, 
						const Vector2 & newColumn )	;


				
					/** 
					 * Sets this matrix so that all its elements equal
					 * commonValue.
					 *
					 */
					virtual void setAllElementsTo( Real commonValue ) ;
					
					
					
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
						MatrixIndex ordinate ) const ;
					
					
					
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
						MatrixIndex ordinate, Real newValue ) ;
					
					
					
					/// Sets this matrix to the identity matrix.
					virtual void setToIdentity() ;
					
					
					
					/**
					 * Sets this matrix to the a diagonal matrix whose 
					 * diagonal term is diagonalTerm.
					 *
					 */
					virtual void setToDiagonal( Real diagonalTerm ) ;
					
					
					
					/// Tranposes this matrix.
					virtual void transpose() ;
					
					
					
					/// Computes this matrix's trace.
					virtual Real trace() const ;
					
					
					
					/// Computes this matrix's determinant.
					virtual Real determinant() const ;					
					
								
												
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
			
					
					
					 /// Returns the cofactor matrix. 
					static Matrix2 Cofactor( const Matrix2 & m ) ;
					 
					 
					 
					 /// Returns the adjoint matrix. 
					static Matrix2 Adjoint( const Matrix2 & m ) ;
					 
					 
					 
					/**
					 * Returns the matrix corresponding to the specified
					 * rotation.
					 *
					 * @param angle in degrees
					 *
					 */
					static Matrix2 CreateFromRotation( AngleInDegrees angle ) ; 
					 
					 
					 
					/**
					 * Returns the matrix corresponding to specified
					 * endomorphism.
					 *
					 * @param endomorphism can be a functor 
					 * (ex: Endomorphism2DFunctor) or a simple function:
					 * Vector2 -> Vector.
					 *
					 * @note C++ does not allow virtual template method.
					 *
					 */
					template<typename T>
					static Matrix2 CreateFrom( T endomorphism )
					{
					
						Matrix2 result ;
	
						Vector2 v( 1, 0 ) ;
						v = endomorphism( v ) ;
						result.setColumn( 0, v ) ;
	
						v.setTo( 0 ,1 ) ;
						v = endomorphism( v ) ;
						result.setColumn( 1, v ) ;
	
						return result ;
						
					}
					
					 
					/**
					 * Tells what is the dimension for this Matrix.
					 *
					 */
					static const MatrixIndex Dimensions = 2 ;
					 
				
					
				protected:
				
				
					/// The nine coordinates of this matrix.
					Real _mat[ Dimensions ][ Dimensions ] ;



			} ;



			/*
			 * g++ seems to require this outside declaration, only for this
			 * particular operator though!
			 *
			 */
			CEYLAN_DLL Matrix2 operator ! ( const Matrix2 & m ) ;
			
			
		}
		
	}
	
}

					
#endif // CEYLAN_MATRIX_2_H_

