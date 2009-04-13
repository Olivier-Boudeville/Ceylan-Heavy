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


#ifndef CEYLAN_ENDOMORPHISM_H_
#define CEYLAN_ENDOMORPHISM_H_


#include "CeylanLinear.h"              // for Real, LinearException
#include "CeylanMathsBasic.h"          // for Functor
#include "CeylanVector3.h"             // for Vector3

#include <string>



namespace Ceylan
{


	namespace Maths
	{


		namespace Linear
		{



			/*
			 * Here are declared various 2D and 3D endomorphisms which 
			 * are all functors, including rotations and line projections.
			 *
			 */


			// Used by 2D endomorphisms.
			class Vector2 ;
			

			/**
			 * Functor encapsulating a 2D endomorphism.
			 * Functors are especially useful when the endomorphism can be
			 * parametrized. 
			 *
			 * For example, a rotation needs an angle and an axis to be
			 * specified, which a simple pointer to function would not allow
			 * nicely, since its only argument should be the vector to
			 * transform, not the rotation settings.
			 * 
			 * @note These endomorphisms are in the Linear namespace
			 * since they are mostly used with linear matrix-based algrebra,
			 * but it does not imply that all the endomorphisms must be 
			 * linear too.
			 *
			 */
			class CEYLAN_DLL Endomorphism2DFunctor : public Functor
			{


				public:
				
				
					/**
					 * Constructs a new functor encapsulating a 2D endomorphism.
					 *
					 */
					Endomorphism2DFunctor() throw() ;
				
				
					/// Basic virtual destructor.
					virtual ~Endomorphism2DFunctor() throw() ;
					
					
					/**
					 * The callable method, returning the endomorphism's 
					 * value corresponding to v.
					 *
					 */
					virtual Vector2 operator() ( const Vector2 & v ) 
						throw() = 0 ;
												
								
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
					 

			} ;
			
			

			/**
			 * Functor of 2D rotation of specified angle.
			 *
			 */
			class CEYLAN_DLL Rotation2DFunctor : public Endomorphism2DFunctor
			{
			
				public:
				
				
					/**
					 * Creates the endomorphism corresponding to the 2D 
					 * rotation of specified angle.
				 	 *
					 * @param axis The axis of this rotation.
					 *
					 * @param angle The axis, in degrees, of this rotation.
					 *
				 	 * @note axis does not need to be normalized, the
					 * constructor will do it.
				 	 *
				 	 */
					explicit Rotation2DFunctor( AngleInDegrees angle ) throw() ;
			
			
					/// Virtual destructor.
					virtual ~Rotation2DFunctor() throw() ;
					
			
					/**
					 * The callable method, returning the projection of v 
					 * on the axis.
					 *
					 */
					virtual Vector2 operator() ( const Vector2 & v ) throw()  ;
			
			
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
						
						
				protected:
				
									
					/// The rotation angle, in radian.
					AngleInRadians _angle ;	
						
			} ;			
			
						
			
			/**
			 * Functor encapsulating a 3D endomorphism.
			 * Functors are especially useful when the endomorphism can 
			 * be parametrized. 
			 *
			 * For example, a rotation needs an angle and an axis to be
			 * specified, which a simple pointer to function would not 
			 * allow nicely, since its only argument should be the
			 * vector to transform, not the rotation settings.
			 * 
			 * @note These endomorphisms are in the Linear namespace 
			 * since they are mostly used
			 * with linear matrix-based algrebra, but it does not imply
			 * that the endomorphisms must be linear too.
			 *
			 */
			class CEYLAN_DLL Endomorphism3DFunctor : public Functor
			{


				public:
				
				
					/**
					 * Constructs a new functor encapsulating a 3D endomorphism.
					 *
					 */
					Endomorphism3DFunctor() throw() ;
				
				
					/// Basic virtual destructor.
					virtual ~Endomorphism3DFunctor() throw() ;
					
					
					/**
					 * The callable method, returning the endomorphism's
					 * value corresponding to v.
					 *
					 */
					virtual Vector3 operator() ( 
						const Vector3 & v ) throw() = 0 ;
												
								
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
					 

			} ;
			
			
			
			/**
			 * Functor of a line projection on specified axis.
			 *
			 */
			class CEYLAN_DLL LineProjection3DFunctor : 
				public Endomorphism3DFunctor
			{
			
				public:
				
				
					/**
					 * Creates the endomorphism corresponding to the line
					 * projection of specified axis.
					 *
				 	 * @note axis does not need to be normalized, the
					 * constructor will do it.
				 	 *
				 	 */
					explicit LineProjection3DFunctor( 
						const Vector3 & axis ) throw() ;
			
			
					/// Virtual destructor.
					~LineProjection3DFunctor() throw() ;
					
			
					/**
					 * The callable method, returning the projection of 
					 * v on the axis.
					 *
					 */
					virtual Vector3 operator() ( const Vector3 & v ) throw()  ;
			
			
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
						
						
				protected:
				
					/// The normalized axis.
					Vector3 _axis ;		
						
			} ;
			
			
			
			/**
			 * Functor for 3D rotation relative to specified axis, of 
			 * specified angle.
			 *
			 */
			class CEYLAN_DLL Rotation3DFunctor : public Endomorphism3DFunctor
			{
			
				public:
				
				
					/**
					 * Creates the endomorphism corresponding to the
					 * 3D rotation of specified axis and angle.
					 *
					 * @param axis The axis of this rotation.
					 *
					 * @param angle The axis, in degrees, of this rotation.
					 *
				 	 * @note axis does not need to be normalized, the
					 * constructor will do it.
				 	 *
				 	 */
					explicit Rotation3DFunctor( const Vector3 & axis, 
						AngleInDegrees angle ) throw() ;
			
			
					/// Virtual destructor.
					virtual ~Rotation3DFunctor() throw() ;
					
			
					/**
					 * The callable method, returning the projection of 
					 * v on the axis.
					 *
					 */
					virtual Vector3 operator() ( const Vector3 & v ) throw()  ;
			
			
					/**
					 * Returns a user-friendly description of the state
					 * of this object.
					 *
					 * @param level the requested verbosity level.
					 *
					 * @note Text output format is determined from overall
					 * settings.
					 *
					 * @see TextDisplayable
					 *
					 */
             		virtual const std::string toString( 
						VerbosityLevels level = high ) const throw() ;
						
						
						
				protected:
				
					/// The normalized axis.
					Vector3 _axis ;	
					
					/// The rotation angle, in radians.
					AngleInRadians _angle ;	
					
						
			} ;
			
		}
		
	}
	
}


#endif // CEYLAN_ENDOMORPHISM_H_
