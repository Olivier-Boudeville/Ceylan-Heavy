/* 
 * Copyright (C) 2003-2011 Olivier Boudeville
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


#ifndef CEYLAN_STREAM_H_
#define CEYLAN_STREAM_H_


#include "CeylanTextDisplayable.h"     // for inheritance
#include "CeylanSystem.h"              // for IOException


#include <string>




namespace Ceylan 
{


	namespace System 
	{
	
	
	
		/**
		 * A stream's unique ID.
		 *
		 * @note A value of -1 denotes an invalid stream ID.
		 *
		 */
		typedef int StreamID ;
		
		
		
		/**
 		 * Abstract stream class used in IPC.
 		 *
 		 * @see InputStream, Stream.
 		 *
 		 */
		class CEYLAN_DLL Stream : public Ceylan::TextDisplayable
		{


			public:
			
			
			
				/// Exception thrown when a stream operation failed.
				class StreamException : public Ceylan::System::IOException
				{
					public: 
					
						explicit StreamException( 
								const std::string & reason ) : 
							IOException( reason )
						{
						
						}
						
				} ;



				/// Exception thrown when a stream operation failed.
				class CloseException : public StreamException
				{
					public: 
					
						explicit CloseException( 
								const std::string & reason ) : 
							StreamException( reason )
						{
						
						}
						
				} ;
				
				
				
				/**
				 * Exception thrown when a stream is set to non-blocking
				 * whereas it failed or is not supported at all.
				 *
				 */
				class NonBlockingNotSupportedException: 
					public StreamException
				{ 
					public: 
					
						explicit NonBlockingNotSupportedException( 
								const std::string & reason ) : 
							StreamException( reason )
						{
						
						}
								
				} ;
				
				
				
				
				/**
				 * Basic constructor for stream instances.
				 *
				 * @param blocking tells whether this stream should be
				 * created in blocking mode (the default) or in non-blocking
				 * mode (if supported).
				 *
				 */
				explicit Stream( bool blocking = true ) ;
	
	
	
				/// Basic virtual destructor.
				virtual ~Stream() throw() ;
	
	

				/**
				 * Tells whether this stream is in blocking mode (if true),
				 * or in non-blocking mode (if false).
				 *
				 */
				bool isBlocking() const ;
	
	
	
				/**
				 * Closes the stream.
				 *
				 * @return true iff an operation had to be performed.
				 *
				 * @throw CloseException if the close operation failed.
				 *
				 */
				virtual bool close() = 0 ;
				
				
				
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
					Ceylan::VerbosityLevels level = Ceylan::high ) const = 0 ;
			
			
			
				/**
				 * Closes and zeroes the specified file descriptor.
				 * It is passed by address so that this function can set it
				 * to zero on successful close.
				 *
				 * @return true iff an operation had to be performed.
				 *
				 * @throw CloseException if the close operation failed.
				 *
				 */
				static bool Close( FileDescriptor & fd ) ;





			protected:
		
		
		
				/**
				 * Sets the blocking mode of this stream.
				 *
				 * @param newStatus if true, sets the stream in blocking mode,
				 * if false set to non-blocking mode. If the stream is 
				 * already in the target state, nothing is done.
				 *
				 * @throw NonBlockingNotSupportedException if the operation
				 * failed or is not supported.
				 *
				 * @note This default implementation always raises its 
				 * exception, streams that supports non-blocking access have to
				 * override it.
				 *
				 */
				virtual void setBlocking( bool newStatus ) ;


				/// Stores whether the stream is in blocking mode.
				bool _isBlocking ;
				
				
				

			private:
			
			
			
				/**
				 * Copy constructor made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 * 
				 */			 
				Stream( const Stream & source ) ;
			
			
				/**
				 * Assignment operator made private to ensure that it 
				 * will be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 * 
				 */			 
				Stream & operator = ( const Stream & source ) ;
					
					
		} ;	
		
	
	}
	
}



#endif // CEYLAN_STREAM_H_

