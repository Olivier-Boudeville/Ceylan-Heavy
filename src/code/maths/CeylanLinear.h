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
			class CEYLAN_DLL LinearException : public MathsException
			{
		
				public:
			
					explicit LinearException( const std::string & message ) ;
					
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

