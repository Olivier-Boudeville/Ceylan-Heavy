#ifndef CEYLAN_TRIPOINT_H_
#define CEYLAN_TRIPOINT_H_


#include "CeylanPoint.h"		// for Point
#include "CeylanLinear.h"		// for Real, LinearException

#include <string>



namespace Ceylan
{


	namespace Maths
	{


		namespace Linear
		{

			
			// Result of Tripoint substraction.
			class Vector3 ;

			// Used by Tripoint.			
			class Matrix3 ;


			/**
			 * Three-dimensional point, whose coordinates are 
			 * floating-point values (Real).
			 *
			 * Points and vectors are basically the same beasts, except 
			 * they can mean different things to the developer. 
			 * The Vector class is more versatile.
			 *
			 * @note Beware of counter-intuitive operator priorities.
			 *
			 */
			class CEYLAN_DLL Tripoint : public Point
			{


				// Friends section.


				/// Must access to each other's coordinates.				
				friend class Vector3 ;

				/// Must access to each other's coordinates.								
				friend class Matrix3 ;
				
				
				/** 
				 * Tells whether the two tripoints have the same
				 * elements.
				 *
				 * @note Exact comparison of floating point values is 
				 * meaningless on most cases, hence strict equality is
				 * replaced here by relative equality.
				 *
				 * @see Maths::AreRelativelyEqual.
				 */
				CEYLAN_DLL friend bool operator == ( const Tripoint & t1, 
					const Tripoint & t2 ) throw() ;
				
				
				/** 
				 * Tells whether the two tripoints have the same elements.
				 *
				 */
				CEYLAN_DLL friend bool operator != ( const Tripoint & t1, 
					const Tripoint & t2 ) throw() ;
				
				/** 
				 * Translates a 3D point of specified 3D vector : 
				 * result = t + v.
				 *
				 */
				CEYLAN_DLL friend Tripoint operator + ( const Tripoint & t, 
					const Vector3 & v ) throw() ;
					
				
				/** 
				 * Translates a 3D point of specified opposite 3D vector :
				 * result = t - v.
				 *
				 */
				CEYLAN_DLL friend Tripoint operator - ( const Tripoint & t, 
					const Vector3 & v ) throw();
				
				
				/** 
				 * Transforms a 3D point into a 3D vector, which will 
				 * have exactly the same coordinates.
				 *
				 */
				CEYLAN_DLL friend Vector3 vectorize( const Tripoint & t ) throw() ;
				
				
				/** 
				 * Constructs a 3D vector from two 3D points : 
				 * result = t2 - t1.
				 *
				 */
				CEYLAN_DLL friend Vector3 vectorize( const Tripoint & t1, 
					const Tripoint & t2 ) throw() ;



				public:
				
				
					/**
					 * Constructs a new vector with specified coordinates.
					 *
					 * @note If no argument is specified, constructs the
					 * null vector.
					 *
					 */
					explicit Tripoint( Real x0 = 0, Real x1 = 0, 
						Real x2 = 0 ) throw() ;
				
				
					/// Basic virtual destructor.
					virtual ~Tripoint() throw() ;
					
					
					/// Reassigns this tripoint's coordinates.
					virtual void setTo( Real x0, Real x1, Real x2 ) throw() ;
				
					
					/**
					 * Sets this point so that all its elements equal
					 * commonValue.
					 *
					 */
					virtual void setAllElementsTo( Real commonValue ) throw() ;
			
					
					/**
					 * Returns indexed element. 
					 * Index ranges from 0 to Dimensions-1.
					 *
					 * @note An emergency shutdown is triggered if index 
					 * is out of bounds.
					 *
					 */
					virtual Real getElementAt( MatrixIndex index ) 
						const throw() ;
					
					
					/**
					 * Sets indexed element to specified value. 
					 * Index ranges from 0 to Dimensions-1.
					 *
					 * @note An emergency shutdown is triggered if index
					 * is out of bounds.
					 *
					 */
					virtual void setElementAt( MatrixIndex index, 
						Real newValue ) throw() ;
					
					
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
					static const MatrixIndex Dimensions = 3 ;
					
					
					 
				protected:
				
				
					/// The three coordinates of this tripoint.
					Real _coordinates[ Dimensions ] ;



			} ;

		}
		
	}
	
}


#endif // CEYLAN_TRIPOINT_H_
