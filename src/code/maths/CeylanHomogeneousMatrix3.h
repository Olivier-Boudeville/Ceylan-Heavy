#ifndef CEYLAN_HOMOGENEOUS_MATRIX_3_H_
#define CEYLAN_HOMOGENEOUS_MATRIX_3_H_


#include "CeylanMatrix3.h"             // for inheritance
#include "CeylanLinear.h"              // for Real, LinearException


#include <string>


namespace Ceylan
{


	namespace Maths
	{


		namespace Linear
		{


			// Used by 2D homogeneous matrices.
			class Vector2 ;

			// Homogeneous ones are special cases.
			class Matrix2 ;
			
			// Used by 3D homogeneous matrices.
			class Tripoint ;

			// Used by 3D homogeneous matrices.
			class Vector3 ;
			
		
			
			/**
			 * Homogeneous square matrix (3x3, dedicated to 2D computations),
			 * whose coordinates are floating-point values (Real).
			 *
			 * Indices ranges for 0 to 2, the first index is the abscissa 
			 * (left to right index) and the second one the ordinate 
			 * (top to bottom index).
			 *
			 * @example m[1][0] refers to the matrix element located in 
			 * the second column (1) at its top (0).
			 *
			 * @note Homogeneous matrices can be interpreted as block 
			 * matrices. 
			 *
			 * For 2D needs, an HomogeneousMatrix3 [ r, t; 0, 1 ] could
			 * be used, where r is a 2x2 rotation matrix, and t a 2D
			 * translation vector.
			 *
			 * @see Ceylan users guide for more details.
			 *
			 * @note Beware of counter-intuitive operator priorities.
			 *
			 * @note The operations on homogeneous matrices take into 
			 * account the fact that their bottom row is filled with zeroes
			 * but the last element, which is equal to one : they optimize
			 * computations using that hypothesis, that should be always
			 * enforced. Otherwise computations would be erroneous.
			 *
			 * @note All matrix operations could be redefined so that for
			 * homogeneous matrices there would be no need to store their
			 * bottom row, since it is always made of zeroes and of one one.
			 *
			 * This would however prevent from considering that an 
			 * homogeneous matrix is a (particular case of) matrix 
			 * (all of the matrix elements would be inherited), or force to
			 * add more complexity to the inheritance tree.
			 *
			 */
			class CEYLAN_DLL HomogeneousMatrix3 : public Matrix3
			{

				
				/**
				 * Multiplies efficiently two homogeneous matrices :
				 * HomogeneousMatrix3 result = m1 * m2.
				 *
				 */
				CEYLAN_DLL friend HomogeneousMatrix3 operator * ( 
					const HomogeneousMatrix3 & hm1, 
					const HomogeneousMatrix3 & hm2 ) throw() ;
				
				
				/**
				 * Multiplies an homogeneous 3x3 matrix by a 2D vector,
				 * returning another 2D vector : 
				 * Vector2 result = hm3 * v2.
				 *
				 * @note This obviously has a meaning only for homogeneous
				 * matrices.
				 *
				 */
				CEYLAN_DLL friend Vector2 operator * ( 
					const HomogeneousMatrix3 & hm, const Vector2 & v ) throw() ;
				

				public:
				
				
					/**
					 * Constructs an homogeneous 3x3 matrix from <b>r</b>, 
					 * a 2x2 rotation matrix, and from <b>t</b>, 
					 * a 2D translation vector. 
					 *
					 * @note r may not be a strict rotation matrix, it may 
					 * be a stretching (determinant non-equal to 1 for 
					 * example) matrix.
					 *
					 */
					explicit HomogeneousMatrix3( 
						const Maths::Linear::Matrix2 & r, 
						const Maths::Linear::Vector2 & v ) throw() ;
					
					
					/**
					 * Constructs an homogeneous 3x3 matrix whose 2x2
					 * matrix corresponds to a rotation of <b>angle</b> in
					 * trigonometrical sens (direction) with <b>t</b> as a 
					 * 2D translation vector. 
					 *
					 * @param angle in degrees.
					 *
					 */
					HomogeneousMatrix3( AngleInDegrees angle, 
						const Maths::Linear::Vector2 & v ) throw() ;
					
				
					/**
					 * Constructs a new homogenous 3x3 matrix with 
					 * specified elements.
					 *
					 * This matrix can be seen as the following block matrix :
					 * [ r, t; 0, 1 ], where r = [ r0, r1; r2, r3 ] is a 2x2
					 * matrix and t = [ t0; t1 ] a 2D vector.
					 *
					 * @note If no argument is specified, constructs the 
					 * null matrix.
					 *
					 */
					explicit HomogeneousMatrix3( 
							 Real r0 = 0, Real r1 = 0, Real r2 = 0, 
							 Real r3 = 0, Real t0 = 0, Real t1 = 0 ) 
						throw() ;
				
				
					/** 
					 * Copy constructor.
					 *
					 * @note Cannot be 'explicit', as it would prevent
					 * the multiply operator HomogeneousMatrix3 m = m1 * m2.
					 * to return a HomogeneousMatrix3 on the stack.
					 *
					 * No need to define it since the basic compiler-generated
					 * code is correct.
					 *
					 */
					/*
					HomogeneousMatrix3( 
						const HomogeneousMatrix3 & source )	throw() ;
					*/
					
					/// Basic virtual destructor.
					virtual ~HomogeneousMatrix3() throw() ;
	
	
										
					/**
					 * Returns a user-friendly description of the state 
					 * of this object.
					 *
					 * @param level the requested verbosity level.
					 *
					 * @note Text output format is determined from
					 *  overall settings.
					 *
					 * @see TextDisplayable
					 *
					 */
             		virtual const std::string toString( 
						VerbosityLevels level = high ) const throw() ;
									
					 
					 /**
					  * Tells what is the dimension for this Matrix.
					  *
					  */
					 static const MatrixIndex Dimensions = 3 ;
					 
					 
					/**
					 * Creates the camera matrix in a 2D world, whose focal
					 * length and center of interest are specified.
					 *
					 *
					 */ 
					/*static Matrix3 CreateCamera( Length focalLength, 
						const Bipoint & centerOfInterest ) ;
					 */
					 


				protected:
				
				
				
					/**
					 * Sets the bottom row of the homogenous matrix 
					 * to all zero but a final one.
					 *
					 */
					virtual void setBottomRow() throw() ;
					
					
					/**
					 * Sets the embedded translation vector of this 
					 * homogeneous matrix.
					 *
					 * Assigns specified Vector2 to the top of last column.
					 *
					 */
					 virtual void setTranslationVector( const Vector2 & v )
					 	throw() ;
					 
					 
					/**
					 * Sets the embedded rotation matrix of this homogeneous
					 * matrix.
					 *
					 * Assigns specified Matrix2 on top-left position.
					 *
					 */
					 virtual void setRotationMatrix( const Matrix2 & m ) 
					 	throw() ;
					 					 
					
					/**
					 * Sets this homogeneous matrix in its canonical form, 
					 * where bottom-right element is 1.
					 * 
					 * @note Does not change anything in the meaning of 
					 * this matrix, all coordinates are divided by this 
					 * bottom-right element if not null or almost, it
					 * is just a matter of numerical convention.
					 *
					 * @throw LinearException if bottom-right element is zero.
					 *
					 */
					virtual void setInCanonicalForm() 
						throw( LinearException ) ;  
						

			} ;
			
		}
		
	}
	
}


#endif // CEYLAN_HOMOGENEOUS_MATRIX_3_H_
