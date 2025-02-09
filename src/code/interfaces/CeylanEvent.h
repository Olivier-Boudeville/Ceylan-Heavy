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


#ifndef CEYLAN_EVENT_H_
#define CEYLAN_EVENT_H_


#include "CeylanException.h"        // for Ceylan::Exception
#include "CeylanTextDisplayable.h"  // for inheritance

#include <string>



namespace Ceylan
{


	/**
	 * Exception to be raised when the event system encounters an abnormal
	 * situation.
	 *
	 */
	class CEYLAN_DLL EventException: public Ceylan::Exception
	{

		public:
		
			explicit EventException( const std::string & reason ) ;
			
			virtual ~EventException() throw() ;
			
	} ;

	
	
	// They send events.
	class EventSource ;
	
	
	
    /**
     * Encapsulates the concept of high-level events.
     *
	 * @note In an event-driven system such as a graphical user interface (GUI),
	 * basic (raw) events might be too numerous to model them with this class,
	 * which is dedicated to higher-level, often user-centric, events.
	 * For example, instead of many 'mouse moved' basic events, an Event could
	 * be 'user pressed the quit button' or even 'user wants to quit'.
	 *
     */
    class CEYLAN_DLL Event : public TextDisplayable
    {


        public:
					
								
			/**
			 * All events should know where they come from.
			 *
			 * @note Events have only <b>breferences</b> towards their source,
			 * of course they do not own it.
			 *
			 */
			explicit Event( EventSource & source ) ;
			
						
						
			/// Basic virtual destructor.
			virtual ~Event() throw() ;


							
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
				Ceylan::VerbosityLevels level = Ceylan::high ) const ;
			
					
		
		protected:
		
		
			/**
			 * The source of this event.
			 *
			 */
			EventSource * _source ;
			
			
			
		private:
		
		
			/**
			 * Copy constructor made private to ensure that it will never be
			 * called.
			 * The compiler should complain whenever this undefined
			 * constructor is called, implicitly or not.
			 * 
			 *
			 */			 
			Event( const Event & source ) ;
			
			
			/**
			 * Assignment operator made private to ensure that it will
			 * never be called.
			 *
			 * The compiler should complain whenever this undefined operator
			 * is called, implicitly or not.
			 * 
			 */			 
			Event & operator = ( const Event & source ) ;
			
			 			
    } ;	

}



#endif // CEYLAN_EVENT_H_

