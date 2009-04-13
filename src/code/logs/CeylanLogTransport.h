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


#ifndef CEYLAN_LOG_TRANSPORT_H_
#define CEYLAN_LOG_TRANSPORT_H_


#include "CeylanLog.h"                // for LevelOfDetail, etc.
#include "CeylanTextDisplayable.h"    // for inheritance


namespace Ceylan
{

	namespace Log
	{
	
	
		// They are transported by LogTransport.
		class LogMessage ;
	
	
		/**
		 * This abstract class has for mission to take a Log message from
		 * a LogSource and to bring it to the relevant LogListener.
		 *
		 * @note the inheritance from TextDisplayable is virtual since, 
		 * for local log messaging, a LogTransport could be a LogListener
		 * too, whereas only one instance of the TextDisplayable mother
		 * class would be required.
		 *
		 * @note the transport takes ownership of log message objects,
		 * it has therefore to make sure that they are eventually deallocated.
		 *
		 * @see LogSource, LogListener
		 *
		 */
		class CEYLAN_DLL LogTransport : public virtual TextDisplayable
		{
		
			public:
			
			
				/**
				 * Constructs a blank LogTransport.
				 *
				 * @see setTransport
				 * 
				 */
				LogTransport() throw() ;
			
				
				/// Basic virtual destructor.
				virtual ~LogTransport() throw() ;
						
						
		        /**
		         * Propagates <b>message</b> to the relevant LogListener.
		         *
				 * @param message the message which is to be propagated
				 *
				 * @note Most implementations, more elaborate that local
				 * raw log transfers, should end up deallocating the 
				 * specified message.
		         *
		         */		
				virtual void propagate( LogMessage & message ) 
					throw( LogException ) = 0 ; 
				
										
	            /**
	             * Returns a user-friendly description of the state of
				 * this object.
	             *
				 * @param level the requested verbosity level.
				 *
				 * @note Text output format is determined from overall settings.
				 *
				 * @see TextDisplayable
				 *
	             */
            	virtual const std::string toString( 
					Ceylan::VerbosityLevels level = Ceylan::high )
						const throw() ;

					
			private:
			
			
				/**
				 * Copy constructor made private to ensure that it will 
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 * 
				 */			 
				LogTransport( const LogTransport & source ) throw() ;
			
			
				/**
				 * Assignment operator made private to ensure that it 
				 * will be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * operator is called, implicitly or not.
				 * 
				 */			 
				LogTransport & operator = ( const LogTransport & source )
					throw() ;
	
		} ;
	
	
	}


}


#endif // CEYLAN_LOG_TRANSPORT_H_
