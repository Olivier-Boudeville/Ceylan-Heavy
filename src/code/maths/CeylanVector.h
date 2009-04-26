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
			 * Abstract class for vectors, whose coordinates are 
			 * floating-point values (Real).
			 *
			 * Indices ranges for 0 to Dimensions-1			 
			 *
			 * @note Beware of counter-intuitive operator priorities.
			 *
			 * @note Some methods could be gathered in this abstract class,
			 * instead of child classes (ex: setAllElementsTo could use
			 * the Dimensions attribute so that this method could be 
			 * generically defined). The problem is however that Dimensions
			 * should be static, but it would not be overloaded by child
			 * classes. Even from a Vector3, lines such as
			 * 'for ( MatrixIndex i = 0 ; i < Dimensions ; i++ )' would use
			 * Vector::Dimensions, and not Vector3::Dimensions. 
			 * A solution could be to define a getDimensions method.
			 * As it would have to be overloaded, inlining is not an option, a
			 * nd the overhead would remain significant. Better define 
			 * these methods in each child class, even if their text is 
			 * exactly duplicated.
			 *
			 */
			class CEYLAN_DLL Vector: public TextDisplayable
			{
		

				public:
				
				
				
					/**
					 * Constructs a new vector. 
					 *
					 */
					Vector() ;
				
				
					/// Basic virtual destructor.
					virtual ~Vector() throw() ;
					
					
					
					/**
					 * Sets this vector to the null vector (all coordinates
					 * zeroed).
					 *
					 */
					virtual void nullify() ;
					
					
					
					/**
					 * Sets this vector so that all its elements equal
					 * commonValue.
					 *
					 */
					virtual void setAllElementsTo( Real commonValue ) = 0 ;
					
						
						
					/**
					 * Normalizes this vector, so that its norm equals one.
					 *
					 * @throw LinearException if the null vector is passed,
					 * since it is the only one that cannot be normalized.
					 *
					 */
					virtual void normalize() = 0 ;



					/**
					 * Returns this vector's magnitude.
					 *
					 * @see operator ~
					 *
					 */
					virtual Real magnitude() const = 0 ;


					
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
					static const MatrixIndex Dimensions = 0 ;
				
								

			} ;
			

		}
		
	}
	
}



#endif // CEYLAN_VECTOR_H_

