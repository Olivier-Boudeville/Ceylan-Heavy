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


#ifndef CEYLAN_SERIALIZABLE_H_
#define CEYLAN_SERIALIZABLE_H_


#include "CeylanException.h" 

#include <string>


namespace Ceylan
{


	namespace System
	{
	
		
		/// A Serializable instance can be loaded from an InputStream.
		class InputStream ;
		
		/// A Serializable instance can be saved to an OutputStream.
		class OutputStream ;
		
	
	}
	


	/// This exception is raised when a serialization-related operation failed.
	class CEYLAN_DLL SerializationException : public Ceylan::Exception
	{
			
		public :
				
			explicit SerializationException( 
				const std::string & reason ) ;
				
			virtual ~SerializationException() throw() ;
				
	} ;
	



    /**
     * Interface that every object which can be serialized should implement. 
     *
	 * Serializing objects is converting them to a stream of bytes. 
	 * It is useful to send them accross the network, or to store them in files
	 * for example.
	 *
     */
    class CEYLAN_DLL Serializable
    {

        public:


			/// Do-nothing constructor.
			Serializable()
			{
			
			}
			
			
			/// Do-nothing virtual destructor.
			virtual ~Serializable() throw()
			{
			
			}
			
			
			
			/**
			 * Saves the instance state to specified stream.
			 *
			 * @param output the output stream to which the state will be
			 * written.
			 *
			 * @throw SerializationException if the operation failed.
			 *
			 */
			virtual void saveTo( System::OutputStream & output ) const  = 0 ;
			
				
			
			/**
			 * Loads a new instance state from specified stream.
			 *
			 * @param input the input stream from which the state will be read.
			 *
			 * @throw SerializationException if the operation failed.
			 *
			 */
			virtual void loadFrom( System::InputStream & input ) = 0 ;
				
			
			
			/*
			 * A recommended factory:
			 *
			 
			static Serializable & CreateFrom( System::InputStream & input ) ;
			
			 *
			 */
			 
			  

		private:
		
		
		
			/**
			 * Copy constructor made private to ensure that it will be 
			 * never called.
			 *
			 * The compiler should complain whenever this undefined 
			 * constructor is called, implicitly or not.
			 * 
			 *
			 */			 
			Serializable( const Serializable & source ) ;
			
			
			/**
			 * Assignment operator made private to ensure that it will be
			 * never called.
			 *
			 * The compiler should complain whenever this undefined operator
			 * is called, implicitly or not.
			 * 
			 *
			 */			 
			Serializable & operator = ( const Serializable & source ) ;
		
			 
    } ;

}



#endif // CEYLAN_SERIALIZABLE_H_

