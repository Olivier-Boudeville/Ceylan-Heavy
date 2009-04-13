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


#ifndef CEYLAN_LIBTOOL_VERSION_H_
#define CEYLAN_LIBTOOL_VERSION_H_


#include "CeylanVersion.h"        // for inheritance



namespace Ceylan
{



    /**
     * Describes a Libtool interface version, with respect to the form :
	 * (current version).(revision version).(age)
	 *
	 * @see http://www.gnu.org/software/libtool/manual.html#Libtool-versioning
	 *
     */
    class CEYLAN_DLL LibtoolVersion : public Version
    {
	
	

        public:


            /**
             * Constructs a new Libtool version identifier.
             *
             * @param current the current version interface number.
			 *
             * @param revision the revision version interface number.
			 *
             * @param age the age version interface number.
			 *
			 * @throw VersionException if the string could not be transformed
			 * into a valid Libtool version.
			 *
             */
            explicit LibtoolVersion( VersionNumber current = 0, 
					VersionNumber revision = 0, VersionNumber age= 0 )
				throw( Ceylan::VersionException ) ;


            /**
             * Constructs a new Libtool version identifier from a string of the
			 * form 'x.y.z' where x, y and z are textual representations of
			 * positive or null integers.
             *
             * @param versionText a string describing the version, 
			 * ex : "2.15.1"
			 *
			 * @throw VersionException if the string could not be transformed
			 * into a valid Libtool version.
			 *
             */
            explicit LibtoolVersion( const std::string & versionText ) 
				throw( Ceylan::VersionException ) ;



            /// Basic virtual destructor.
            virtual ~LibtoolVersion() throw() ;



			/**
			 * Returns the current interface version of this version identifier.
			 *
			 */
			virtual VersionNumber getCurrentInterfaceNumber() const throw() ;
			
			
			/**
			 * Sets the current interface version of this version identifier.
			 *
			 * @note Consistency of the Libtool version is not checked.
			 *
			 */
			virtual void setCurrentInterfaceNumber( VersionNumber newCurrent )
				throw() ;
			
			
			
			/// Returns the revision number of this version identifier.
			virtual VersionNumber getRevisionInterfaceNumber() const throw() ;
			
			
			/**
			 * Sets the revision interface number of this version identifier.
			 *
			 * @note Consistency of the Libtool version is not checked.
			 *
			 */
			virtual void setRevisionInterfaceNumber( VersionNumber newRevision )
				throw() ;
			
			
			
			/**
			 * Returns the interface age of this version
			 * identifier.
			 *
			 */
			virtual VersionNumber getInterfaceAge() const throw() ;
			
			
			/**
			 * Sets the release number (or patch number) of this version
			 * identifier.
			 *
			 * @note Consistency of the Libtool version is not checked.
			 *
			 */
			virtual void setInterfaceAge( VersionNumber newAge ) throw() ;
			
			
			/**
			 * Tells whether this version, taken as an actual library version,
			 * is compatible according to Libtool rules with the specified
			 * one, taken as a requested version.
			 *
			 * The interface age is used to know whether API compatibility is
			 * claimed or not.
			 *
			 * Basically, this version is supposed to correspond with the one 
			 * of a library, the parameter version being the one the user 
			 * program was written with. This method tells whether the user
			 * program can work with this corresponding library.
			 *
			 * @param expectedVersion, according to Libtool rules the
			 * Libtool-style library version expected by the user program. 
			 * This must be a LibtoolVersion instance indeed, otherwise an
			 * exception is raised.
			 *
			 * @return true iff the program can be safely linked with this
			 * library version.
			 *
			 * @throw VersionException if the comparison of the two versions 
			 * cannot be done, for example in the case where they belong to
			 * incompatible version schemes : this would be a meaningless
			 * operation.
			 *
			 */
			virtual bool isCompatibleWith( const Version & expectedVersion )
				const throw( VersionException ) ;
			
			
			/**
			 * Tells whether this version scheme is compatible with the one of 
			 * the supplied version instance.
			 *
			 * @return true iff comparing these two versions would have a
			 * meaning.
			 *
			 * @note This method is public so that the operators can use it.
			 *
			 */
			virtual bool canBeComparedWith( const Version & version )
				const throw() ;
				
				
            /**
             * Returns a user-friendly description of the state of this object.
             *
			 * @param level the requested verbosity level.
			 *
			 * @note Text output format is determined from overall settings.
			 *
			 * @see TextDisplayable
             *
             */
            virtual const std::string toString( VerbosityLevels level = high )
				const throw() ;



        protected:


			/**
			 * Tells that a LibtoolVersion, for compatibility matters, should
			 * not be mixed with basic versions, since their meaning differ.
			 *
			 */ 
			virtual bool isUsualVersionSchemeCompliant() const throw() ;

			
			/**
			 * Raises an exception iff the Libtool version is not consitent.
			 *
			 */
			virtual void checkConsistency() const throw( VersionException ) ;
			
			 
			
		private:
		
		
			/**
			 * Copy constructor made private to ensure that it will be never
			 * called.
			 * The compiler should complain whenever this undefined operator 
			 * is called, implicitly or not.
			 * 
			 */
			LibtoolVersion( const LibtoolVersion & source ) throw() ;
			
			
			/**
			 * Assignment operator made private to ensure that it will be never
			 * called.
			 * The compiler should complain whenever this undefined operator is
			 * called, implicitly or not.
			 * 
			 */			 
			LibtoolVersion & operator = ( const LibtoolVersion & source )
				throw() ;


    } ;

}



#endif // CEYLAN_LIBTOOL_VERSION_H_
