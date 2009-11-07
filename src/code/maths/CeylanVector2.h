/* 
 * Copyright (C) 2003-2009 Olivier Boudeville
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


#ifndef CEYLAN_VECTOR_2_H_
#define CEYLAN_VECTOR_2_H_


#include "CeylanVector.h"			// for Vector
#include "CeylanLinear.h"			// for Real, LinearException

#include <string>




namespace Ceylan
{


	namespace Maths
	{


		namespace Linear
		{


			
			// Substracts them to have vectors.			
			class Bipoint ;


			// Transforms vectors.
			class Matrix2 ;


			// Special cases of 3D matrices.
			class HomogeneousMatrix3 ;



			/**
			 * Three-dimensional vector, whose coordinates are 
			 * floating-point values (Real).
			 *
			 * @note Beware of counter-intuitive operator priorities.
			 *
			 */
			class CEYLAN_DLL Vector2 : public Vector
			{


				// Friends section.


				/// Must access to each other's coordinates.				
				friend class Bipoint ;


				/// Must access to each other's coordinates.					
				friend class Matrix2 ;
				
				
				
				/** 
				 * Tells whether the two vectors have the same elements.
				 *
				 */
				CEYLAN_DLL friend bool operator == ( const Vector2 & v1, 
					const Vector2 & v2 ) ;
						
						
						
				/** 
				 * Tells whether the two vectors have exactly the 
				 * same elements.
				 *
				 */
				CEYLAN_DLL friend bool operator != ( const Vector2 & v1, 
					const Vector2 & v2 ) ;



				/** 
				 * Translates a 2D point of specified 2D vector: 
				 * result = t + v.
				 *
				 */
				CEYLAN_DLL friend Bipoint operator + ( const Bipoint & t, 
					const Vector2 & v ) ;
				
				
				
				/** 
				 * Translates a 2D point of specified opposite 2D vector:
				 * result = t - v.
				 *
				 */
				CEYLAN_DLL friend Bipoint operator - ( const Bipoint & t, 
					const Vector2 & v ) ;
	
	
				
				/** 
				 * Transforms a 2D point into a 2D vector, which will
				 * have the same coordinates.
				 *
				 */
				CEYLAN_DLL friend Vector2 vectorize( const Bipoint & t ) ;
				
				
				
				/** 
				 * Constructs a 2D vector from two 2D points: 
				 * result = t1 - t2.
				 *
				 */
				CEYLAN_DLL friend Vector2 vectorize( const Bipoint & t1, 
					const Bipoint & t2 ) ;



				/** 
				 * Adds two vectors: result = v1 + v2.
				 *
				 */
				CEYLAN_DLL friend Vector2 operator + ( const Vector2 & v1, 
					const Vector2 & v2 ) ;



				/** 
				 * Substracts two vectors: result = v1 - v2.
				 *
				 */			
				CEYLAN_DLL friend Vector2 operator - ( const Vector2 & v1, 
					const Vector2 & v2 ) ;
	
				
				
				/** 
				 * Multiplies v by coefficient lambda: 
				 * result = lamba.v.
				 *
				 */
				CEYLAN_DLL friend Vector2 operator * ( Real lambda, 
					const Vector2 & v ) ;
				
				
				
				/** 
				 * Applies vector v to matrix m: 
				 * result = m.v
				 *
				 */
				CEYLAN_DLL friend Vector2 operator * ( const Matrix2 & m, 
					const Vector2 & v ) ;
	
	
	
				/** 
				 * Applies vector v to homogeneous matrix hm: 
				 * result = hm.v
				 *
				 */
				CEYLAN_DLL friend Vector2 operator * ( 
					const HomogeneousMatrix3 & m, const Vector2 & v ) ;



				/** 
				 * Computes the dot product of v1 and v2: 
				 * result = v1 (dotproduct) v2.
				 *
				 */
				CEYLAN_DLL friend Real operator | ( const Vector2 & v1, 
					const Vector2 & v2 ) ;



				// Cross product is a non-sense in 2D.
				
				
				/// Returns the magnitude of specified vector.
				CEYLAN_DLL friend Real operator ~ ( const Vector2 & v ) ;
				
				
				

				public:
				
				
				
					/**
					 * Constructs a new vector with specified coordinates.
					 *
					 * @note If no argument is specified, constructs 
					 * the null vector.
					 *
					 */
					explicit Vector2( Real x0 = 0, Real x1 = 0 ) ;
				
				
				
					/**
					 * Constructs a new vector from specified array of 
					 * two floating-point coordinates.
					 *
					 */
					explicit Vector2( const Real (& array)[2] ) ;
				
				
				
					/// Basic virtual destructor.
					virtual ~Vector2() throw() ;
					
					
					
					/// Reassigns this vector's coordinates.
					virtual void setTo( Real x0, Real x1 ) ;



					/**
					 * Sets this vector to the null vector 
					 * (all coordinates zeroed).
					 *
					 */
					virtual void nullify() ;
	
	
					
					/**
					 * Sets this vector so that all its elements equal
					 * commonValue.
					 *
					 */
					virtual void setAllElementsTo( Real commonValue ) ;



					/// Returns the abscissa of this vector.
					virtual Real getX() const ;
					
					
					
					/// Returns the ordinate of this vector.
					virtual Real getY() const ;



					/**
					 * Returns indexed element. 
					 * Index ranges from 0 to Dimensions-1.
					 *
					 * @throw MathsException if index is out of bounds and if
					 * in debug mode.
					 *
					 */
					virtual Real getElementAt( MatrixIndex index ) const ;
					
					
					
					/**
					 * Sets indexed element to specified value. 
					 * Index ranges from 0 to Dimensions-1.
					 *
					 * @throw MathsException if index is out of bounds and if
					 * in debug mode.
					 *
					 */
					virtual void setElementAt( MatrixIndex index, 
						Real newValue ) ;
					
					
					
					/**
					 * Normalizes this vector, so that its norm equals one.
					 *
					 * @throw LinearException if the null vector is 
					 * passed, since it is the only one that cannot be
					 * normalized.
					 *
					 */
					virtual void normalize() ;



					/**
					 * Returns this vector's magnitude.
					 *
					 * @see operator ~
					 *
					 */
					virtual Real magnitude() const ;


					
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
					 * Tells what is the dimension for this point.
					 *
					 */
					static const MatrixIndex Dimensions = 2 ;
		
		
					 
				protected:
				
				
					/// The three coordinates of this vector.
					Real _vec[ Dimensions ] ;



			} ;
			

		}
		
	}
	
}



#endif // CEYLAN_VECTOR_2_H_

