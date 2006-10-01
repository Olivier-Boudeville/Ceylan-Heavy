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
				friend bool operator == ( const Bipoint & t1, 
					const Bipoint & t2 ) throw() ;
				
				
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
				friend bool operator != ( const Bipoint & t1, 
					const Bipoint & t2 ) throw() ;
				
				
				/**
				 * Translates a 2D point of specified 2D vector : 
				 * result = t + v.
				 *
				 */
				friend Bipoint operator + ( const Bipoint & t, 
					const Vector2 & v ) throw() ;
				
				
				/**
				 * Translates a 2D point of specified opposite 2D vector :
				 * result = t - v.
				 *
				 */
				friend Bipoint operator - ( const Bipoint & t, 
					const Vector2 & v ) throw();
				
				/**
				 * Transforms a 2D point into a 2D vector, which will 
				 * have the same coordinates.
				 *
				 */
				friend Vector2 vectorize( const Bipoint & t ) throw() ;
				
				
				/**
				 * Constructs a 2D vector from two 2D points : 
				 * result = t2 - t1.
				 *
				 */
				friend Vector2 vectorize( const Bipoint & t1, 
					const Bipoint & t2 ) throw() ;

				

				public:
				
				
					/**
					 * Constructs a new vector with specified coordinates.
					 *
					 * @note If no argument is specified, constructs the 
					 * null vector.
					 *
					 */
					explicit Bipoint( Real x0 = 0, Real x1 = 0 ) throw() ;
				
				
					/// Basic virtual destructor.
					virtual ~Bipoint() throw() ;
					
					
					/// Reassigns this Bipoint's coordinates.
					virtual void setTo( Real x0, Real x1 ) throw() ;



					/**
					 * Sets this point so that all its elements equal
					 * commonValue.
					 *
					 */
					virtual void setAllElementsTo( Real commonValue ) throw() ;


					/// Returns the abscissa of this point.
					virtual Real getX() const throw() ;
					
					/// Returns the ordinate of this point.
					virtual Real getY() const throw() ;
					
					
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
						VerbosityLevels level = high ) const throw() ;
					 

					/**
					 * Returns the distance between the two bipoints.
					 *
					 */
					static Real Distance( const Bipoint & first, 
						const Bipoint & second ) throw() ;
					

					/**
					 * Returns the square of the distance between the two
					 * bipoints.
					 *
					 * @note Allows to avoid to compute a square root, 
					 * which may be long.
					 *
					 */
					static Real DistancePow2( const Bipoint & first, 
						const Bipoint & second ) throw() ;
					
					
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
