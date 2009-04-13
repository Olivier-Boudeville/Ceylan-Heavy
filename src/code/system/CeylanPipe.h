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


#ifndef CEYLAN_PIPE_H_
#define CEYLAN_PIPE_H_


#include "CeylanInputOutputStream.h"  // for inheritance
#include "CeylanFeatures.h"           // for FeatureNotAvailableException



namespace Ceylan
{


	namespace System
	{
	
	

		/**
		 * Pipe class for Inter-Process Communication (IPC).
		 *
		 * @note The file descriptor feature must be available to have 
		 * usable pipes.
		 *
		 * @see following feature symbols to spot the actual support 
		 * beforehand : Features::areFileDescriptorsSupported
		 *
		 */
		class CEYLAN_DLL Pipe: public InputOutputStream 
		{

	
			public:
		

				/// Mother class for all file-related exceptions.
				class PipeException: public SystemException
				{ 
					public: 
					
						explicit PipeException( const std::string & reason )
							throw() ;
						
						virtual ~PipeException() throw() ; 
				} ;


				class CouldNotCreate: public PipeException
				{ 
					public: 
					
						explicit CouldNotCreate( 
								const std::string & reason ) throw() ; 
				} ;


				class ReadFailed: public InputStream::ReadFailedException
				{ 
					public: 
					
						explicit ReadFailed( 
								const std::string & reason ) throw() ; 
				} ;



				class WriteFailed: public OutputStream::WriteFailedException
				{ 
					public: 
					
						explicit WriteFailed( 
								const std::string & reason ) throw() ; 
				} ;
		
		
		
		
				/**
				 * Constructs a new pipe with its own IO channels.
				 *
				 * @throw CouldNotCreate on failure, including
				 * if the file descriptor feature is not available.
				 *
				 */
				Pipe() throw( CouldNotCreate ) ;
		
		
				/**
				 * Copy constructor.
				 *
				 * @note File descriptors are duplicated.
				 *
				 * @throw PipeException on failure, including if the file
				 * descriptor feature is not available.
				 *
				 */
				explicit Pipe( const Pipe & other ) throw( PipeException ) ;


				/// Virtual destructor.
				virtual ~Pipe() throw() ;
		
		
		
				/**
				 * Reads up to <b>maxLength</b> bytes from pipe to 
				 * <b>buffer</b>.
				 *
				 * @param buffer the buffer where to store read bytes. 
				 * Its size must be at least maxLength bytes.
				 *
				 * @param maxLength the maximum number of bytes that should 
				 * be read.
				 *
				 * @return The number of bytes actually read, which should
				 * be maxLength or lower.
				 *
				 * @throw ReadFailed if a read error occurred.
				 *
				 */
		 		virtual Size read( char * buffer, Size maxLength ) 
					throw( InputStream::ReadFailedException ) ;


				/**
				 * Writes message to this pipe.
				 *
				 * @param message the message to write to this pipe.
				 *
				 * @return The number of bytes actually written, which 
				 * should be equal to the size of the string or lower.
				 *
				 * @throw WriteFailed if a write error occurred.
				 *
				 */
				virtual Size write( const std::string & message ) 
					throw( OutputStream::WriteFailedException ) ;
	
		
				/**
				 * Writes up to maxLength bytes from the specified buffer
				 * to this pipe.
				 *
				 * @param buffer the buffer where to find bytes that must
				 * be written to this pipe.
				 * Its size must be at least maxLength bytes.
				 *
				 * @return The number of bytes actually written, which 
				 * should be equal to maxLength.
				 *
				 * @throw WriteFailed if a write error occurred.
				 *
				 */
				virtual Size write( const char * buffer, Size maxLength ) 
					throw( OutputStream::WriteFailedException ) ;
		
		
				/**
				 * Tells whether there is data available on input.
				 *
				 */
				virtual bool hasAvailableData() const throw() ;
		
		
				/// Clears up the input data stream
				virtual void clearInput() throw() ;
		
				
				/**
				 * Closes the pipe.
				 *
				 * @return true iff an operation had to be performed.
				 *
				 * @throw CloseException if the close operation failed.
				 *
				 */
				virtual bool close() throw( Stream::CloseException ) ;
		
		
				virtual StreamID getInputStreamID() const throw() ;
		
				virtual StreamID getOutputStreamID() const throw() ;




		protected:


				/// Returns the input stream file descriptor.
				FileDescriptor getReadFileDescriptor() const throw() ;
		
				/// Returns the output stream file descriptor.
				FileDescriptor getWriteFileDescriptor() const throw() ;
	
	
	
		private:



				/**
				 * Copy constructor is not private here, and is explicity 
				 * defined.
				 *
				Pipe( const Pipe & source ) throw() ;
				 */			 
			
			
				/**
				 * Assignment operator made private to ensure that it will 
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 * 
				 */			 
				Pipe & operator = ( const Pipe & source ) throw() ;
	
	
				/**
				 * The descriptors for both ends of the pipe.
				 *
				 */
				mutable FileDescriptor _fd[ 2 ] ;
		
		} ;	

	}

}


#endif // CEYLAN_PIPE_H_
