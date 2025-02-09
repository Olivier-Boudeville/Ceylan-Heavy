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


#ifndef CEYLAN_BIPOINT_H_
#define CEYLAN_BIPOINT_H_


#include "CeylanPoint.h"		// for Point
#include "CeylanLinear.h"		// for Real, LinearException

#include <string>



namespace Ceylan
{

	namespace Maths
	{


		namespace Linear
		{

			
			
			// Substraction of two Bipoint instances.
			class Vector2 ;


			// Matrix to use with Bipoint instances.			
			class Matrix2 ;



			/**
			 * Two-dimensional point, whose coordinates are floating-point
			 * values (Real).
			 *
			 * Points and vectors are basically the same beasts, except 
			 * they can mean different things to the developer. 
			 * The Vector class is more versatile.
			 *
			 * @note Beware of counter-intuitive operator priorities.
			 *
			 */
			class CEYLAN_DLL Bipoint : public Point
			{


				// Friends section.
				

				/// Must access to each other's coordinates.				
				friend class Vector2 ;


				/// Must access to each other's coordinates.					
				friend class Matrix2 ;
				
				
				
				/**
				 * Tells whether the two Bipoints have the same
				 * elements.
				 *
				 * @note Exact comparison of floating point values is 
				 * meaningless on most cases, hence strict equality is
				 * replaced here by relative equality.
				 *
				 * @see Maths::AreRelativelyEqual.
				 *
				 */
				CEYLAN_DLL friend bool operator == ( const Bipoint & t1, 
					const Bipoint & t2 ) ;
				
				
				
				/** 
				 * Tells whether the two Bipoints are different.
				 *
				 * @note Exact comparison of floating point values is 
				 * meaningless on most cases, hence strict equality is
				 * replaced here by relative equality.
				 *
				 * @see Maths::AreRelativelyEqual.
				 *
				 */
				CEYLAN_DLL friend bool operator != ( const Bipoint & t1, 
					const Bipoint & t2 ) ;
				
				
				
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
				 * result = t2 - t1.
				 *
				 */
				CEYLAN_DLL friend Vector2 vectorize( const Bipoint & t1, 
					const Bipoint & t2 ) ;

				
				

				public:
				
				
				
					/**
					 * Constructs a new vector with specified coordinates.
					 *
					 * @note If no argument is specified, constructs the 
					 * null vector.
					 *
					 */
					explicit Bipoint( Real x0 = 0, Real x1 = 0 ) ;
				
				
				
					/// Basic virtual destructor.
					virtual ~Bipoint() throw() ;
					
					
					
					/// Reassigns this Bipoint's coordinates.
					virtual void setTo( Real x0, Real x1 ) ;



					/**
					 * Sets this point so that all its elements equal
					 * commonValue.
					 *
					 */
					virtual void setAllElementsTo( Real commonValue ) ;



					/// Returns the abscissa of this point.
					virtual Real getX() const ;
					
					
					/// Returns the ordinate of this point.
					virtual Real getY() const ;
					
					
					
					/**
					 * Returns indexed element. 
					 * Index ranges from 0 to Dimensions-1.
					 *
					 * @throw MathsException if index is out of bounds, and 
					 * if in debug mode.
					 *
					 */
					virtual Real getElementAt( MatrixIndex index ) const ;
					
					
					
					/**
					 * Sets indexed element to specified value. 
					 * Index ranges from 0 to Dimensions-1.
					 *
					 * @throw MathsException if index is out of bounds, and 
					 * if in debug mode.
					 *
					 */
					virtual void setElementAt( MatrixIndex index, 
						Real newValue ) ;
					
					
					
					/**
					 * Returns a user-friendly description of the state of
					 * this object.
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
					 * Returns the distance between the two bipoints.
					 *
					 */
					static Real Distance( const Bipoint & first, 
						const Bipoint & second ) ;
					


					/**
					 * Returns the square of the distance between the two
					 * bipoints.
					 *
					 * @note Allows to avoid to compute a square root, 
					 * which may be long.
					 *
					 */
					static Real DistancePow2( const Bipoint & first, 
						const Bipoint & second ) ;
					
					
					
					/**
					 * Tells what is the dimension for this point.
					 *
					 */
					static const MatrixIndex Dimensions = 2 ;
					 
					 
					 
				protected:
				
				
					/// The three coordinates of this Bipoint.
					Real _coordinates[ Dimensions ] ;



			} ;

		}
		
	}
	
}



#endif // CEYLAN_BIPOINT_H_

