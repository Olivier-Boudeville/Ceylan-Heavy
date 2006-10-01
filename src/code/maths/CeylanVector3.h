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
				friend bool operator == ( const Vector3 & v1, 
					const Vector3 & v2 ) throw() ;
						
						
				/** 
				 * Tells whether the two vectors have the same elements.
				 *
				 */
				friend bool operator != ( const Vector3 & v1, 
					const Vector3 & v2 ) throw() ;


				/** Translates a 3D point of specified 3D vector : 
				 * result = t + v.
				 *
				 */
				friend Tripoint operator + ( const Tripoint & t, 
					const Vector3 & v ) throw() ;
				
				
				/** 
				 * Translates a 3D point of specified opposite 3D vector :
				 * result = t - v.
				 *
				 */
				friend Tripoint operator - ( const Tripoint & t, 
					const Vector3 & v ) throw();
	
				
				/** 
				 * Transforms a 3D point into a 3D vector, which will
				 * have the same coordinates.
				 *
				 */
				friend Vector3 vectorize( const Tripoint & t ) throw() ;
				
				
				/** 
				 * Constructs a 3D vector from two 3D points : 
				 * result = t1 - t2.
				 *
				 */
				friend Vector3 vectorize( const Tripoint & t1, 
					const Tripoint & t2 ) throw() ;


				/** 
				 * Adds two vectors : result = v1 + v2.
				 *
				 */
				friend Vector3 operator + ( const Vector3 & v1, 
					const Vector3 & v2 ) throw() ;
					

				/** 
				 * Substracts two vectors : result = v1 - v2.	
				 *
				 */			
				friend Vector3 operator - ( const Vector3 & v1, 
					const Vector3 & v2 ) throw() ;
	
				
				/** 
				 * Multiplies v by coefficient lambda : result = lamba.v.
				 *
				 */
				friend Vector3 operator * ( Real lambda, 
					const Vector3 & v ) throw()  ;
				
				
				/** 
				 * Applies vector v to matrix m : result = m.v
				 *
				 */
				friend Vector3 operator * ( const Matrix3 & m, 
					const Vector3 & v ) throw() ;
	
				
				/** 
				 * Computes the dot product of v1 and v2 : 
				 * result = v1 (dotproduct) v2.
				 *
				 */
				friend Real operator | ( const Vector3 & v1, 
					const Vector3 & v2 ) throw()  ;


				/** 
				 * Returns the cross product of v1 and v2 : 
				 * result = v1 (crossproduct) v2.
				 *
				 */
				friend Vector3 operator ^ ( const Vector3 & v1, 
					const Vector3 & v2 ) throw() ;
				
				
				/** 
				 * Returns the magnitude of specified vector.
				 *
				 */
				friend Real operator ~ ( const Vector3 & v ) throw() ;
				
				
				

				public:
				
				
					/**
					 * Constructs a new vector with specified coordinates.
					 *
					 * @note If no argument is specified, constructs 
					 * the null vector.
					 *
					 */
					explicit Vector3( Real x0 = 0, Real x1 = 0, 
						Real x2 = 0 ) throw() ;
				
				
					/// Basic virtual destructor.
					virtual ~Vector3() throw() ;
					
					
					/// Reassigns this vector's coordinates.
					virtual void setTo( Real x0, Real x1, 
						Real x2 ) throw() ;


					/** 
					 * Sets this vector to the null vector 
					 * (all coordinates zeroed).
					 *
					 */
					virtual void nullify() throw() ;
					
					
					/** 
					 * Sets this vector so that all its elements equal
					 * commonValue.
					 *
					 */
					virtual void setAllElementsTo( Real commonValue ) throw() ;


					/**
					 * Returns indexed element. 
					 * Index ranges from 0 to Dimensions-1.
					 *
					 * @note An emergency shutdown is triggered if 
					 * index is out of bounds.
					 *
					 */
					virtual Real getElementAt( MatrixIndex index ) 
						const throw() ;
					
					
					/**
					 * Sets indexed element to specified value. 
					 * Index ranges from 0 to Dimensions-1.
					 *
					 * @note An emergency shutdown is triggered if 
					 * index is out of bounds.
					 *
					 */
					virtual void setElementAt( MatrixIndex index, 
						Real newValue ) throw() ;
				
					
					/**
					 * Normalizes this vector, so that its norm equals one.
					 *
					 * @throw LinearException if the null vector is
					 * passed, since it is the only one that cannot be
					 * normalized.
					 *
					 */
					virtual void normalize() throw( LinearException ) ;


					/**
					 * Returns this vector's magnitude.
					 *
					 * @see operator ~
					 *
					 */
					virtual Real magnitude() const throw() ;

					
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
					 * Tells what is the dimension for this vector.
					 *
					 */
					static const MatrixIndex Dimensions = 3 ;


					 
				protected:
				
				
					/// The coordinates of this vector.
					Real _vec[ Dimensions ] ;



			} ;

		}
		
	}
	
}


#endif // CEYLAN_VECTOR_3_H_
