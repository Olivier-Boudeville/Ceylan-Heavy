#ifndef CEYLAN_VERSION_H_
#define CEYLAN_VERSION_H_


#include "CeylanTextDisplayable.h"  // for inheritance
#include "CeylanTypes.h"            // for Uint8
#include "CeylanException.h"        // for Exception


#include <string>


namespace Ceylan
{



	class VersionException : public Ceylan::Exception
	{
	
		public:
		
			VersionException( const std::string & message ) throw() ;
			virtual ~VersionException() throw() ;
	
	} ;
	


    /**
     * Describes a version, with respect to the form :
	 * (major version).(minor version).(release or patch version)
	 *
	 * @see CeylanUtils.h for an actual use of this version facility, including 
	 * automated checking on loading.
	 *
     */
    class Version : public TextDisplayable
    {
	
		
        public:

		
			/**
			 * Describes a version number, a part of the full version
			 * identifier.
			 *
			 */
			typedef Ceylan::Uint8 VersionNumber ;


            /**
             * Constructs a new version identifier.
             *
             * @param major the major version number.
			 *
             * @param minor the minor version number.
			 *
             * @param release the release version number, or the patch number.
			 *
             */
            explicit Version( VersionNumber major = 0, VersionNumber minor = 0, 
				VersionNumber release = 0 ) throw() ;


            /**
             * Constructs a new version identifier from a string of the form
			 * 'x.y.z' where x, y and z are textual representations of positive
			 * or null integers.
             *
             * @param versionText a string describing the version, 
			 * ex : "1.15.216"
			 *
			 * @throw VersionException if the string could not be transformed
			 * into a valid version.
			 *
             */
            explicit Version( const std::string & versionText ) 
				throw( VersionException ) ;



            /// Basic virtual destructor.
            virtual ~Version() throw() ;



			/// Returns the major number of this version identifier.
			virtual VersionNumber getMajorNumber() const throw() ;
			
			/// Sets the major number of this version identifier.
			virtual void setMajorNumber( VersionNumber newNumber ) throw() ;
			
			
			
			/// Returns the minor number of this version identifier.
			virtual VersionNumber getMinorNumber() const throw() ;
			
			/// Sets the minor number of this version identifier.
			virtual void setMinorNumber( VersionNumber newNumber ) throw() ;
			
			
			
			/**
			 * Returns the release number (or patch number) of this version
			 * identifier.
			 *
			 */
			virtual VersionNumber getReleaseNumber() const throw() ;
			
			/**
			 * Sets the release number (or patch number) of this version
			 * identifier.
			 *
			 */
			virtual void setReleaseNumber( VersionNumber newNumber ) throw() ;
			
			
			/**
			 * Tells whether this version, taken as an actual library version,
			 * is compatible with the specified one, taken as a requested
			 * version.
			 *
			 * Basically, for these all-purpose versions, the user-supplied
			 * version must exactly match this version to be deemed compatible
			 * with.
			 *
			 * @param version the library version expected by the user 
			 * program, which corresponds to the headers it has been compiled
			 * with.
			 * 
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
			 * Tells whether a Version instance (be it a child class or not),
			 * can be considered as a all-purpose usual version, or if it obeys
			 * to specific rules that prevent from comparing it according to
			 * the common rules.
			 *
			 * For example, Libtool versions should not be mixed with basic 
			 * versions, since their meaning differ.
			 *
			 */ 
			virtual bool isUsualVersionSchemeCompliant() const throw() ;
			
			
				 


			/// The major version number, i.e. the first version coordinate.
			VersionNumber _major ;

			/// The minor version number, i.e. the second version coordinate.
			VersionNumber _minor ;

			/**
			 * The release version number (or patch number), i.e. the third
			 * version coordinate.
			 *
			 */
			VersionNumber _release ;



		private:
		
		
			/*
			 * Copy constructor made private to ensure that it will be never
			 * called.
			 * The compiler should complain whenever this undefined operator 
			 * is called, implicitly or not.
			 * 
			 */
			Version( const Version & source ) throw() ;
			
			
			/**
			 * Assignment operator made private to ensure that it will be never
			 * called.
			 * The compiler should complain whenever this undefined operator is
			 * called, implicitly or not.
			 * 
			 *
			 */			 
			Version & operator = ( const Version & source ) throw() ;


    } ;

}



/**
 * Returns true iff first version is strictly smaller than the second.
 *
 * @note The 'throw' statement is commented since with such operators it cannot
 * be specifically set.
 *
 * @throw VersionException if the comparison would be meaningless.
 *
 */
bool ::operator < ( const Ceylan::Version & vfirst, 
	const Ceylan::Version & vsecond ) /* throw( VersionException ) */ ;


/**
 * Returns true iff the two versions are strictly equal.
 *
 * @note The 'throw' statement is commented since with such operators it cannot
 * be specifically set.
 *
 * @throw VersionException if the comparison would be meaningless.
 *
 */
bool ::operator == ( const Ceylan::Version & vFirst, 
	const Ceylan::Version & vSecond ) /* throw( VersionException ) */ ;



#endif // CEYLAN_VERSION_H_
