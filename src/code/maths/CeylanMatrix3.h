#ifndef CEYLAN_MATRIX_3_H_
#define CEYLAN_MATRIX_3_H_


#include "CeylanVector3.h"             // for Vector3, used in 'CreateFrom'
#include "CeylanMatrix.h"              // for Matrix
#include "CeylanLinear.h"              // for Real, LinearException

#include <string>



namespace Ceylan
{


	namespace Maths
	{


		namespace Linear
		{

			
			// Applied to matrices.
			class Tripoint ;

			// Applied to matrices.
			class Vector3 ;
		
			
			/**
			 * Three-dimensional square matrix (3x3), whose coordinates
			 * are floating-point values (Real).
			 *
			 * Indices ranges for 0 to 2, the first index is the abscissa
			 * (left to right index) and the second one the ordinate 
			 * (top to bottom index).
			 *
			 * @example, m[1][0] refers to the matrix element located 
			 * in the second column (1) at its top (0).
			 *
			 * @note Beware of counter-intuitive operator priorities.
			 *
			 */
			class CEYLAN_DLL Matrix3 : public Matrix
			{


				// Friends section.


				/// Must access to each other's coordinates.				
				friend class Tripoint ;

				/// Must access to each other's coordinates.
				friend class Vector3 ;
				

				/** 
				 * Tells whether the two matrices have the 
				 * same elements.
				 *
				 * @note Exact comparison of floating point values is 
				 * meaningless on most cases, hence strict equality is
				 * replaced here by relative equality.
				 *
				 * @see Maths::AreRelativelyEqual.
				 *
				 */
				CEYLAN_DLL friend bool operator == ( const Matrix3 & m1, 
					const Matrix3 & m2 ) throw() ;
								
								
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
				CEYLAN_DLL friend bool operator != ( const Matrix3 & m1, 
					const Matrix3 & m2 ) throw() ;


				/**  
				 * Adds two matrices : result = m1 + m2.
				 *
				 */
				CEYLAN_DLL friend Matrix3 operator + ( const Matrix3 & m1, 
					const Matrix3 & m2 ) throw() ;
				
				
				/**  
				 * Substracts two matrices : result = m1 - m2.
				 *
				 */					
				CEYLAN_DLL friend Matrix3 operator - ( const Matrix3 & m1, 
					const Matrix3 & m2 ) throw() ;
				
				
				/**  
				 * Multiplies two matrices : result = m1 * m2.
				 *
				 */									
				CEYLAN_DLL friend Matrix3 operator * ( const Matrix3 & m1, 
					const Matrix3 & m2 ) throw() ;
				
				
				/**  
				 * Multiplies a matrix by a scalar : result = lambda * m.
				 *
				 */							
				CEYLAN_DLL friend Matrix3 operator * ( Real lambda, 
					const Matrix3 & m ) throw() ;
				
				
				/**
				 * Returns the inverse (reciproqual) of specified 
				 * matrix, if possible.
				 *
				 * @throw LinearException if the matrix is singuler.
				 *
				 */
				CEYLAN_DLL friend Matrix3 operator ! ( const Matrix3 & m ) 
					throw( LinearException ) ;
				
				
				/**  
				 * Returns the transposed matrix corresponding 
				 * to this matrix.
				 *
				 */
				CEYLAN_DLL friend Matrix3 operator ~ ( const Matrix3 & m )
					throw() ;


				/** 	
				 * Multiplies a matrix by a vector : result = m * v
				 *
				 */
				CEYLAN_DLL friend Vector3 operator * ( const Matrix3 & m , 
					const Vector3 & v ) throw() ;



				public:
				
				
					/**
					 * Constructs a new 3x3 matrix with specified elements.
					 *
					 * @note If no argument is specified, constructs 
					 * the null matrix.
					 *
					 */
					explicit Matrix3( 
							 Real x0 = 0, Real x1 = 0, Real x2 = 0,
							 Real y0 = 0, Real y1 = 0, Real y2 = 0,
							 Real z0 = 0, Real z1 = 0, Real z2 = 0
						) throw() ;
				
				
					/** 
					 * Copy constructor.
					 *
					 * @note Cannot be 'explicit', as it would prevent
					 * the multiply operator Matrix3 m = m1 * m2.
					 * to return a Matrix3 on the stack.
					 *
					 */
					Matrix3( const Matrix3 & source ) throw() ;
					
					
					/// Basic virtual destructor.
					virtual ~Matrix3() throw() ;
					
					
					/**
					 * Reassigns all matrix elements.
					 *
					 */
					virtual void setTo( 
							 Real x0 = 0, Real x1 = 0, Real x2 = 0,
							 Real y0 = 0, Real y1 = 0, Real y2 = 0,
							 Real z0 = 0, Real z1 = 0, Real z2 = 0
						) throw() ;
					
					
					/**
					 * Reassigns specified column of this matrix.
					 *
					 * @note Column numbers range from 0 to 2 (included).
					 *
					 *
					 */
					virtual void setColumn( MatrixIndex columnNumber, 
						const Vector3 & newColumn )	throw() ;

					
					/**
					 * Sets this matrix so that all its elements equal
					 * commonValue.
					 *
					 */
					virtual void setAllElementsTo( Real commonValue ) throw() ;
					

					/**
					 * Returns indexed element. 
					 * Index ranges from 0 to Dimensions-1 for both 
					 * dimensions.
					 *
					 * @note An emergency shutdown is triggered if 
					 * index is out of bounds.
					 *
					 */
					virtual Real getElementAt( MatrixIndex abscissa, 
						MatrixIndex ordinate ) const throw() ;
					
					
					/**
					 * Sets indexed element to specified value. 
					 * Index ranges from 0 to Dimensions-1 for both 
					 * dimensions.
					 *
					 * @note An emergency shutdown is triggered if 
					 * index is out of bounds.
					 *
					 */
					virtual void setElementAt( MatrixIndex abscissa, 
						MatrixIndex ordinate, Real newValue ) throw() ;

					
					/// Sets this matrix to the identity matrix.
					virtual void setToIdentity() throw() ;
					
					
					/**
					 * Sets this matrix to the a diagonal matrix 
					 * whose diagonal term is diagonalTerm.
					 *
					 */
					virtual void setToDiagonal( Real diagonalTerm ) throw() ;
					
					
					/// Tranposes this matrix.
					virtual void transpose() throw() ;
					
					/// Computes this matrix's trace.
					virtual Real trace() const throw() ;
					
					/// Computes this matrix's determinant.
					virtual Real determinant() const throw() ;					
												
					
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
				 
					
					
					// Static section.
					
					
					 /// Returns the cofactor matrix. 
					static Matrix3 Cofactor( const Matrix3 & m ) throw() ;
					 
					 
					 /// Returns the adjoint matrix. 
					static Matrix3 Adjoint( const Matrix3 & m ) throw() ;
					 

					/**
					 * Returns the matrix corresponding to specified
					 * endomorphism.
					 *
					 * @param endomorphism can be a functor 
					 * (ex : Endomorphism3DFunctor) or a simple function :
					 * Vector3 -> Vector3.
					 *
					 * @note C++ does not allow virtual template method.
					 *
					 */
					template<typename T>
					static Matrix3 CreateFrom( T endomorphism ) throw()
					{
						Matrix3 result ;
	
						Vector3 v( 1, 0, 0 ) ;
						v = endomorphism( v ) ;
						result.setColumn( 0, v ) ;
	
						v.setTo( 0 ,1 ,0 ) ;
						v = endomorphism( v ) ;
						result.setColumn( 1, v ) ;
	
						v.setTo( 0, 0, 1 ) ;
						v = endomorphism( v ) ;
						result.setColumn( 2, v ) ;
	
						return result ;
					}
					
					 
					 /**
					  * Tells what is the dimension for this Matrix.
					  *
					  */
					 static const MatrixIndex Dimensions = 3 ;
					 
					 					
					
				protected:
				
				
					/// The nine coordinates of this matrix.
					Real _mat[Dimensions][Dimensions] ;



			} ;
			
		}
		
	}
	
}


#endif // CEYLAN_MATRIX_3_H_
