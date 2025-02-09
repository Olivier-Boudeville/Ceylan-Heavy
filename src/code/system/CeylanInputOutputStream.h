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


#ifndef CEYLAN_INPUT_OUTPUT_STREAM_H_
#define CEYLAN_INPUT_OUTPUT_STREAM_H_


#include "CeylanInputStream.h"      // for inheritance
#include "CeylanOutputStream.h"     // for inheritance





namespace Ceylan 
{


	namespace System 
	{
	
	
	
		/**
 		 * Abstract input/output stream class used in IPC.
 		 *
 		 * @see Socket, Pipe, File, AnonymousInputOutputStream.
 		 *
		 *
		 * @note The virtual inheritance has been set for classes such as 
		 * System::File which are both input and output streams: had the 
		 * Stream class a data member, it would not be duplicated in a 
		 * System::File instance (it would be allocated only once).
		 *
 		 */
		class CEYLAN_DLL InputOutputStream : 
			public InputStream, public OutputStream
		{


			public:
	
	
		
				/// Basic constructor for InputStream, created not selected.
				explicit InputOutputStream( bool blocking = true ) ;
		
		
				/// Basic virtual destructor.
				virtual ~InputOutputStream() throw() ;
		
			
			
            	/**
            	 * Returns an user-friendly description of the state of
				 * this object.
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
					Ceylan::VerbosityLevels level = Ceylan::high ) const ;

		
	
			private:



				/**
				 * Copy constructor made private to ensure that it will 
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 * 
				 */			 
				InputOutputStream( const InputOutputStream & source ) ;
			
			
			
				/**
				 * Assignment operator made private to ensure that it will 
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 * 
				 */			 
				InputOutputStream & operator = ( 
					const InputOutputStream & source ) ;
	
	
	
		} ;
		
					
	}
	
}



#endif // CEYLAN_INPUT_OUTPUT_STREAM_H_

