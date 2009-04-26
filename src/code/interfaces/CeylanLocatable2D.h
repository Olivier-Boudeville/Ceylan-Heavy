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


#ifndef CEYLAN_LOCATABLE_2D_H_
#define CEYLAN_LOCATABLE_2D_H_


#include "CeylanBipoint.h"              // for Bipoint
#include "CeylanHomogeneousMatrix3.h"   // for HomogeneousMatrix3
#include "CeylanLocatable.h"            // for inheritance

#include <string>



namespace Ceylan
{
	
		
	
	/**
	 * Objects that should be locatable in a 2D-space should inherit 
	 * from this class.
	 * 
	 * @note A Locatable2D embeds a 3x3 homogeneous matrix, allowing it to
	 * define a referential that locates this object relatively to its 
	 * parent referential. 
	 *
	 * A referential is defined by both a point in space and a set of 
	 * angles or vectors that indicates which orientation it has.
	 *
	 */
	class CEYLAN_DLL Locatable2D : public Locatable
	{
	

		public:

			
			/**
			 * Constructs a new Locatable2D, defined relatively to its
			 * specified <b>fatherLocatable</b>.
			 *
			 * This local referential describes on creation the same
			 * referential as the father one, since the transformation
			 * between this referential and its fathers is the identity.
			 *
			 * @note The father Locatable2D is not owned by this object, 
			 * which has only a reference onto it. 
			 * It cannot be specified as 'const' since the non-const
			 * getGlobalReferential method should be callable on it.
			 *
			 */
			explicit Locatable2D( Locatable2D & fatherLocatable ) ;
			
			
			
			/**
			 * Basic constructor, no local nor father referential registered.
			 *
			 */
			Locatable2D() ;



			/**
			 * Constructs a new Locatable2D, defined relatively to its 
			 * specified father Locatable, starting with specified local
			 * referential.
			 *
			 * @param fatherLocatable the referential this referential is 
			 * relative to.
			 *
			 * @param localReferential the transformation from this local
			 * referential to its father referential.
			 *
			 * @note The father Locatable is not owned by this object, 
			 * which has only a reference onto it. Reciprocally, this 
			 * Locatable will register itself to its father, so that
			 * it can be notified of its father's change.
			 *
			 * @note This is not meant to be a copy constructor. Father
			 * locatable cannot be 'const' since registering to it is a
			 * non-const operation.
			 *
			 * @note The Locatable will take ownership of the specified
			 * referential.
			 *
			 */
			Locatable2D( Locatable2D & fatherLocatable, 
				Maths::Linear::Matrix & localReferential ) ;
			
			
			
			/**
			 * Basic constructor, no father referential registered, this
			 * referential is therefore considered as an absolute on. 
			 *
			 * Its local referential, which is in this particular case a 
			 * global one too, is specified.
			 * 
			 * @note The Locatable will take ownership of the specified
			 * referential.
			 *
			 */
			explicit Locatable2D( Maths::Linear::Matrix & localReferential ) ;

			
			
			/// Virtual destructor.
			virtual ~Locatable2D() throw() ;			
			
			
			
			/**
			 * Returns this Locatable's referential, expressed in father's
			 * space.
			 *
			 * @note The C++ feature of returning a Matrix3 instead of its
			 * mother class Matrix could be used if gcc did not issue
			 * 'conflicting return type specified'.
			 * 
			 */
			virtual Maths::Linear::Matrix & getLocalReferential() const ;
			
			

			/**
			 * Blanks local referential, so that its matrix is the identity
			 * matrix.
			 *
			 * @note If no referential was existing, a new one is created.
			 *
			 */	
			virtual void blankLocalReferential() ;



			/**
			 * Returns the center of this Locatable, expressed in father 
			 * space. 
			 *
			 * Coordinates are modified according to the homogeneous factor:
			 * they are normalized.
			 *
			 * @throw LocatableException if no relevant local referential
			 * was available, or if the homogeneous factor is zero.
			 *
			 */
			virtual Ceylan::Maths::Linear::Bipoint getCenter() const ;
			 
			
			 
			/**
			 * Sets the center of the local referential. 
			 * As it is relative to father locatable, it must be defined in
			 * father space.
			 *
			 * @param newCenter the new center, in father space.
			 *
			 * @throw LocatableException if no local referential was 
			 * available.
			 *
			 */
			virtual void setCenter( 
				const Ceylan::Maths::Linear::Bipoint & newCenter ) ;
			
			
			
			/**
			 * Sets the center of the local referential. 
			 * As it is relative to father locatable, it must be defined in
			 * father space.
			 *
			 * @param newX the new center abscissa, in father space.
			 *
			 * @param newX the new center ordinate, in father space.
			 *			 
			 * @throw LocatableException if no local referential was available.
			 *
			 */
			virtual void setCenter( 
					Ceylan::Maths::Real newX = 0, 
					Ceylan::Maths::Real newY = 0 ) ;
			
			
			
            /**
             * Returns a user-friendly description of the state of this 
			 * object.
             *
			 * @param level the requested verbosity level.
			 *
			 * @note Text output format is determined from overall settings.
			 *
			 * @see TextDisplayable
             *
             */
             virtual const std::string toString( VerbosityLevels level = high )
			 	const ;
				
				
		
		protected:
		
		
		
			/**
			 * Updates this Locatable's state (actually its precooked global 
			 * referential) from specified matrix, which is the up-to-date
			 * version of this Locatable's father referential.
			 *
			 * @note Basically, it consists on applying the father
			 * transformation to the local one to have a fully world-to-local
			 * precomputed transformation. 
			 *
			 * @note This Locatable must already have a local referential.
			 *
			 */ 
			virtual void updateFromFather( 
				const Maths::Linear::Matrix & upToDateFatherReferential ) ;
			
			
			
			/**
			 * Helper non-virtual inline method to easily retrieve the 
			 * internal matrix with correct (already casted) type 
			 * (ex: HomogeneousMatrix3, not Matrix).
			 *
			 */	
			inline Ceylan::Maths::Linear::HomogeneousMatrix3 & 
				getLocalMatrix() const 
			{
			
				if ( _localReferential == 0 )
					throw LocatableException( "Locatable2D::getLocalMatrix: "
						"no local matrix available." ) ;
						
				Ceylan::Maths::Linear::HomogeneousMatrix3 * temp = 
					dynamic_cast<Ceylan::Maths::Linear::HomogeneousMatrix3*>(
						_localReferential ) ;
				
				if ( temp == 0 )
					throw LocatableException( "Locatable2D::getLocalMatrix: "
						"was not an HomogeneousMatrix3 instance." ) ;
				
				return * temp ;
					
			}
			
							
	} ;


}



#endif // CEYLAN_LOCATABLE_2D_H_

