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
 * Author: Olivier Boudeville (olivier (dot) boudeville (at) esperide (dot) com)
 *
 */


#ifndef CEYLAN_VECTOR_3_H_
#define CEYLAN_VECTOR_3_H_

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
			class Tripoint ;


			// Transform vectors.
			class Matrix3 ;



			/**
			 * Three-dimensional vector, whose coordinates are 
			 * floating-point values (Real).
			 *
			 * @note Beware of counter-intuitive operator priorities.
			 *
			 */
			class CEYLAN_DLL Vector3 : public Vector
			{


				// Friends section.


				/// Must access to each other's coordinates.				
				friend class Tripoint ;


				/// Must access to each other's coordinates.
				friend class Matrix3 ;
				
				
				
				/** 
				 * Tells whether the two vectors have the same elements.
				 *
				 */
				CEYLAN_DLL friend bool operator == ( const Vector3 & v1, 
					const Vector3 & v2 ) ;
						
						
						
				/** 
				 * Tells whether the two vectors have the same elements.
				 *
				 */
				CEYLAN_DLL friend bool operator != ( const Vector3 & v1, 
					const Vector3 & v2 ) ;



				/** Translates a 3D point of specified 3D vector: 
				 * result = t + v.
				 *
				 */
				CEYLAN_DLL friend Tripoint operator + ( const Tripoint & t, 
					const Vector3 & v ) ;
				
				
				
				/** 
				 * Translates a 3D point of specified opposite 3D vector:
				 * result = t - v.
				 *
				 */
				CEYLAN_DLL friend Tripoint operator - ( const Tripoint & t, 
					const Vector3 & v ) ;
	
				
				
				/** 
				 * Transforms a 3D point into a 3D vector, which will
				 * have the same coordinates.
				 *
				 */
				CEYLAN_DLL friend Vector3 vectorize( const Tripoint & t ) ;
				
				
				
				/** 
				 * Constructs a 3D vector from two 3D points: 
				 * result = t1 - t2.
				 *
				 */
				CEYLAN_DLL friend Vector3 vectorize( const Tripoint & t1, 
					const Tripoint & t2 ) ;



				/** 
				 * Adds two vectors: result = v1 + v2.
				 *
				 */
				CEYLAN_DLL friend Vector3 operator + ( const Vector3 & v1, 
					const Vector3 & v2 ) ;
					
					

				/** 
				 * Substracts two vectors: result = v1 - v2.	
				 *
				 */			
				CEYLAN_DLL friend Vector3 operator - ( const Vector3 & v1, 
					const Vector3 & v2 ) ;
	
				
				
				/** 
				 * Multiplies v by coefficient lambda: result = lamba.v.
				 *
				 */
				CEYLAN_DLL friend Vector3 operator * ( Real lambda, 
					const Vector3 & v )  ;
				
				
				
				/** 
				 * Applies vector v to matrix m: result = m.v
				 *
				 */
				CEYLAN_DLL friend Vector3 operator * ( const Matrix3 & m, 
					const Vector3 & v ) ;
	
				
				
				/** 
				 * Computes the dot product of v1 and v2: 
				 * result = v1 (dotproduct) v2.
				 *
				 */
				CEYLAN_DLL friend Real operator | ( const Vector3 & v1, 
					const Vector3 & v2 )  ;


				/** 
				 * Returns the cross product of v1 and v2: 
				 * result = v1 (crossproduct) v2.
				 *
				 */
				CEYLAN_DLL friend Vector3 operator ^ ( const Vector3 & v1, 
					const Vector3 & v2 ) ;
				
				
				
				/** 
				 * Returns the magnitude of specified vector.
				 *
				 */
				CEYLAN_DLL friend Real operator ~ ( const Vector3 & v ) ;
				
				
				

				public:
				
				
					/**
					 * Constructs a new vector with specified coordinates.
					 *
					 * @note If no argument is specified, constructs 
					 * the null vector.
					 *
					 */
					explicit Vector3( Real x0 = 0, Real x1 = 0, Real x2 = 0 ) ;
				
				
				
					/**
					 * Constructs a new vector from specified array of 
					 * three floating-point coordinates.
					 *
					 */
					explicit Vector3( const Real (& array)[3] ) ;
				
				
				
					/// Basic virtual destructor.
					virtual ~Vector3() throw() ;
					
					
					
					/// Reassigns this vector's coordinates.
					virtual void setTo( Real x0, Real x1, Real x2 ) ;



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
					 
					 
					 
					/**
					 * Tells what is the dimension for this vector.
					 *
					 */
					static const MatrixIndex Dimensions = 3 ;



					 
				protected:
				
				
					/// The coordinates of this vector.
					Real _vec[ Dimensions ] ;



			} ;

			
			
			/*
			 * g++ seems to require this outside declaration, only for this
			 * particular operator though!
			 *
			 */
			CEYLAN_DLL Real operator | ( const Vector3 & v1, 
				const Vector3 & v2 )  ;
					
				
					
			/*
			 * g++ seems to require this outside declaration, only for this
			 * particular operator though!
			 *
			 */
			CEYLAN_DLL Vector3 operator ^ ( const Vector3 & v1, 
				const Vector3 & v2 ) ;
					
					
		}
		
	}
	
}



#endif // CEYLAN_VECTOR_3_H_

