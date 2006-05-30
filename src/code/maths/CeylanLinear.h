#ifndef CEYLAN_LINEAR_H_
#define CEYLAN_LINEAR_H_


#include "CeylanTypes.h"        // for Uint8
#include "CeylanMathsBasic.h"   // for MathsException, Real

#include <string>



namespace Ceylan
{


	namespace Maths
	{


		/// Gathers all linear-based computations, main geometric.
		namespace Linear
		{


			/// Exception for Linear-related issues.
			class LinearException : public MathsException
			{
		
				public:
			
					explicit LinearException( const std::string & message )
						throw() ;
					virtual ~LinearException() throw() ;
		
			} ;



			/**
			 * Matrix indices are used for all relevant classes, including
			 * vectors and points, since all of them are matrices.
			 *
			 */
			typedef Ceylan::Uint8 MatrixIndex ;


			// Section dedicated to 2D.
			
			// Forward definition.
			class Vector2 ;
			
			/// Defines endomorphism in 3D space.
			typedef Vector2 (*Endomorphism2D) ( Vector2 arg ) ;
			
			

			// Section dedicated to 3D.
			
			// Forward definition.
			class Vector3 ;
			
			/// Defines endomorphism in 3D space.
			typedef Vector3 (*Endomorphism3D) ( Vector3 arg ) ;			
		
		
		}
		
	}
	
}


#endif // CEYLAN_LINEAR_H_
