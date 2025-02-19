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


#ifndef CEYLAN_MODULE_H_
#define CEYLAN_MODULE_H_


#include "CeylanObject.h"           // for inheritance
#include "CeylanVersion.h"          // for Ceylan::Version
#include "CeylanException.h"        // for Exception


#include <string>



namespace Ceylan
{



	/// Raised whenever a module-related operation failed.
	class CEYLAN_DLL ModuleException: public Ceylan::Exception
	{
	
		public:
		
			ModuleException( const std::string & message ) ;
			virtual ~ModuleException() throw() ;
	
	} ;




    /**
	 * This module class provides basic services so that modules can
	 * be handled in an automatic, safe and easy way.
	 *
	 * This class paves the way for dynamic plugging of various run-time
	 * discovered implementations (a.k.a as plug-in). 
	 *
	 */
    class CEYLAN_DLL Module: public Ceylan::Object
    {



        public:



            /**
			 * Constructor of a new Ceylan Module.
			 *
			 * @param name the full name of the module, example: 
			 * 'OSDL::Video'.
			 *
			 * @param description an informative description of what 
			 * this module provides, example: 'provides a full 2D API to 
			 * handle simple vector graphic objects'.
			 *
			 * @param homepageURL the URL of this module's homepage 
			 * (if any), example: 'http://ceylan.sourceforge.net'.
			 *
			 * @param author the author of the module, example: 
			 * 'Olivier Boudeville'.
			 *
			 * @param authorMail the e-mail address of the author, example:
             * 'olivier (dot) boudeville (at) online (dot) fr'. 
			 *
			 * @param version the full version of this module. The version
			 * object is copied, the module does not take ownership of it.
			 *
			 * @param licence the licence under which this module is 
			 * released, example: 'LGPL'.
             *
			 * A module is identified by its name and full version:
			 * 'OSDL::Video.2.5.42' behaves as a primary key.
             *
             */
            Module( 
				const std::string & name, 
				const std::string & description, 
				const std::string & homepageURL, 
				const std::string & author, 
				const std::string & authorMail, 
				const Ceylan::Version & version,
				const std::string & licence ) ;



			/**
			 * Constructor of a blank module.
			 * Useful whenever the module informations cannot be obtained at
			 * the object creation, ex: Ceylan::Plugin.
			 *
			 */
			Module() ;
			
			
			
            /// Basic virtual destructor.
            virtual ~Module() throw() ;



            /**
			 * Returns the name of this module.
			 *
			 * @throw ModuleException if the operation failed.
			 *
			 */
            virtual std::string getName() const ;



            /**
			 * Sets the name of this module.
			 *
			 * @throw ModuleException if the operation failed.
			 *
			 */
            virtual void setName( const std::string & name ) ;



            /// Returns this module's description.
            virtual std::string getDescription() const ;


            /// Sets this module's description.
            virtual void setDescription( const std::string & description ) ;



            /// Returns this module's home page URL.
            virtual std::string getHomePage() const ;


            /// Sets this module's home page URL.
            virtual void setHomePage( const std::string & homePage ) ;



            /// Returns this module's author.
            virtual std::string getAuthor() const ;


            /// Sets this module's author.
            virtual void setAuthor( const std::string & author ) ;



            /// Returns this module's author e-mail address.
            virtual std::string getAuthorMail() const ;


            /// Sets this module's author e-mail address.
            virtual void setAuthorMail( const std::string & authorMail ) ;



            /// Returns this module's full version.
            virtual const Ceylan::Version & getVersion() const ;


            /// Sets this module's version, which is copied internally.
            virtual void setVersion( const Ceylan::Version & version ) ;



            /// Returns this module's release licence.
            virtual std::string getLicence() const ;


            /// Sets this module's release licence.
            virtual void setLicence( const std::string & licence ) ;



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
			virtual const std::string toString( 
				VerbosityLevels level = high ) const ;




		private:
		

 
			/// Name of the module.
            std::string _name ;
			
			
			/// Human-friendly description of the module.
            std::string _description ;
			
			
			/// URL of the module's documentation, if any.
            std::string _homepageURL ;


			/// Full Name of the proud author.
            std::string _author ;
			
			
			/// Mail address of the proud author.
            std::string _authorMail ;


			/// Full version number.
            Ceylan::Version _version ;
			
			
			/**
			 * Name of this module's licence, URL to its full text and
			 * description welcome.
			 *
			 */
            std::string _licence ;


		
			/**
			 * Copy constructor made private to ensure that it will be 
			 * never called.
			 *
			 * The compiler should complain whenever this undefined 
			 * constructor is called, implicitly or not.
			 * 
			 */			 
			explicit Module( const Module & source ) ;
			
			
			/**
			 * Assignment operator made private to ensure that it will 
			 * be never called.
			 *
			 * The compiler should complain whenever this undefined 
			 * operator is called, implicitly or not.
			 * 
			 */			 
			Module & operator = ( const Module & source ) ;
			

    } ;


}


#endif // CEYLAN_MODULE_H_

