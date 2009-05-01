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


#ifndef CEYLAN_PROCESS_H_
#define CEYLAN_PROCESS_H_


#include "CeylanRunnable.h"    // for inheritances
#include "CeylanSystem.h"      // for SystemException
#include "CeylanTypes.h"       // for Ceylan::Uint32
#include "CeylanFeatures.h"    // for FeatureNotAvailableException



#include <string>
#include <list>




namespace Ceylan 
{


	namespace System
	{
	
	
		// For process input redirection.
		class InputStream ;
		
		
		// For process output redirection.
		class OutputStream ;



		/// Exception class for process concerns.
		class CEYLAN_DLL ProcessException: public RunnableException
		{
		
			public:
			
				explicit ProcessException( const std::string message ) ;
				virtual ~ProcessException() throw() ; 			
							
		} ;
		
		
		
		/// Describes a Process Identifier (PID).
		typedef Ceylan::Uint32 Pid ;
		
		
		
		
		/**
		 * Child process creation and management class.
		 *
		 * @example:
		 * <pre>
		 * Process & p =  * new myProcess() ;
		 * ...
		 * p.run() ;
		 * ...
		 * waitChildProcess( p ) ;
		 * ...
		 * </pre>
		 *
		 * @note Depending on the platform support, some primitives may not
		 * be available, which results in ProcessException being raised
		 * whenever called. The reason for that is either the underlying
		 * platform is unable to provide these features, or the Ceylan
		 * porting effort did not manage them for the moment.
		 *
		 * @see the CEYLAN_USES_ADVANCED_PROCESS_MANAGEMENT feature symbol
		 * to spot actual support beforehand.
		 *
		 */
		class CEYLAN_DLL Process: public Runnable
		{


			public:
	
	
	
				/// Describes how a child process terminated.
				enum ExitReason
				{
				
					/// Child exited normally:
					ExitedNormally,
	
					/// Child exited after having received a signal:
					Signaled,
	
					/// Child exited after having been stopped:
					Stopped,
	
					/// Child PID was abnormal:
					BadChildPid
					
				} ;
	
	
	
				/// Creates an anonymous Process object.
				Process() ;



				/// Creates a named Process object.
				explicit Process( const std::string & name ) ;
	
	
	
				/// Basic virtual destructor.
				virtual ~Process() throw() ;
	
	
	
				/**
				 * To be called in order to launch the process after its
				 * creation.
				 *
				 * @throw RunnableException if the process creation failed.
				 *
				 */
				virtual void run() ;
	
	
	
				/// Start point of the process: defines what it actually does.
				virtual void start() = 0 ;
	
	
	
				/**
				 * Kills the process.
				 *
				 * @throw ProcessException if the process could not be 
				 * killed.
				 *
				 */
				virtual void kill() ;
	
	
	
				/**
				 * Tells whether the child process is running.
				 *
				 * @throw ProcessException if the process state could not
				 * be known.
				 *
				 */
				bool isRunning() const ;
	
	
	
				/// Returns the error number.
				inline ErrorCode getError() const ;
	
	
	
				/**
				 * Returns the process identifier, the PID, of corresponding
				 * process.
				 *
				 */
				inline Pid getPID() const ;
	
	
	
            	/**
            	 * Returns a user-friendly description of the state of 
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



	
				// Static section.
				
	
				/**
				 * Returns the identifier (PID) of the hosting process.
				 *
				 * @throw ProcessException if the operation failed or is
				 * not supported.
				 *
				 * On OS-less machines (ex: the Nintendo DS), there is only
				 * one process, the current one, and its PID is set to 0.
				 *
				 */
				static Pid GetHostingPID() ;
	
	
	
				/**
				 * Returns the parent process identifier of the hosting 
				 * process, the PPID.
				 *
				 * @throw ProcessException if the operation failed or is
				 * not supported.
				 *
				 */
				static Pid GetParentID() ;
	
	
	
				/**
				 * Waits for the child process to terminate.
				 *
				 * @param childProcess the child process reference.
				 *
				 * @param executionInfo the return status if exited normally,
				 * or the signal number if signaled.
				 *
				 * @return the exit reason.
				 *
				 * @see ExitReason
				 *
				 * @throw ProcessException if the operation failed or is
				 * not supported.
				 *
				 */
				static ExitReason WaitChildProcess( 
					const Process & childProcess,
					ErrorCode * executionInfo = 0 ) ;
	
	
	
				/**
				 * Returns the user name of the process owner.
				 *
				 * @throw ProcessException on failure. 
				 *
				 */
				static std::string GetOwner() ;
	
	
	
				/**
				 * Replaces the current process by the one obtained by 
				 * executing <b>filename</b>.
				 *
				 * @param filename executable file name.
				 *
				 * @param arguments the arguments vector, without
				 * <b>filename</b> at the beginning.
				 *
				 * @param stdoutFilename file name for the stdout to be
				 * redirected to, if not empty.
				 *
				 * @param stderrFilename file name for the stderr to be
				 * redirected to, if not empty.
				 *
				 * @param stdinFilename file name for the stdin to be 
				 * redirected from, if not empty.
				 *
				 */
				static void RunExecutable(
					const std::string & filename,
					const std::list<std::string> & arguments,
					const std::string & stdoutFilename = 0,
					const std::string & stderrFilename = 0,
					const std::string & stdinFilename  = 0 ) ;



				/**
				 * Redirects the stdout to the file <b>filename</b>.
				 *
				 * @throw ProcessException if the redirection failed, 
				 * for example if the file descriptor feature is not 
				 * available.
				 *
				 */				 
				static bool RedirectStdout( const std::string & filename ) ;



				/**
				 * Redirects the stdout to the output stream <b>os</b>.
				 *
				 * @see OutputStream, Pipe, File, Socket.
				 *
				 * @throw ProcessException if the redirection failed, 
				 * for example if the file descriptor feature is not 
				 * available.
				 *
				 */
				static bool RedirectStdout( OutputStream & os ) ;



				/**
				 * Redirects the stderr to the file <b>filename</b>.
				 *
				 * @throw ProcessException if the redirection failed, 
				 * for example if the file descriptor feature is not 
				 * available.
				 *
				 */
				static bool RedirectStderr( const std::string & filename ) ;
	
	
	
				/**
				 * Redirects the stderr to the output stream <b>os</b>.
				 *
				 * @see OutputStream, Pipe, File, Socket.
				 *
				 * @throw ProcessException if the redirection failed, 
				 * for example if the file descriptor feature is not 
				 * available.
				 *
				 */
				static bool RedirectStderr( OutputStream & os ) ;



				/**
				 * Redirects the stdin from the file <b>filename</b>.
				 *
				 * @throw ProcessException if the redirection failed, 
				 * for example if the file descriptor feature is not 
				 * available.
				 *
				 */				 
				static bool RedirectStdin( const std::string & filename ) ;
	
	
	
				/**
				 * Redirects the stdin from the input stream <b>is</b>.
				 *
				 * @see InputStream, Pipe, File, Socket.
				 *
				 * @throw ProcessException if the redirection failed, 
				 * for example if the file descriptor feature is not 
				 * available.
				 *
				 */
				static bool RedirectStdin( InputStream & is ) ;



				/**
				 * Returns the number of seconds consumed by the process.
				 *
				 * @throw ProcessException if this value could not be
				 * determined.
				 *
				 */
				static Ceylan::Uint32 GetTime() ;
	
	
	
				/**
				 * Saves the command line of the corresponding process,
				 * line which is to be used in the restart mehod.
				 *
				 * @see restart
				 *
				 */
				static void SaveState( int argc, char ** argv ) ;
	
	
	
				/**
				 * Restarts the corresponding processs with command line options
				 * as saved previously by calling saveState.
				 *
				 * @throw ProcessException if the operation failed.
				 *
				 * @see saveState
				 *
				 */
				static void Restart() ;
	
	
	
			protected:
	
	
				/**
				 * Called whenever new process creation fails.
				 *
				 * @throw ProcessException if needed.
				 *
				 */
				virtual void processCreationFailed() ;
	
	
	
			private:



				/**
				 * Duplicates a file descriptor.
				 *
				 * @throw FeatureNotAvailableException if the file descriptor
				 * feature is not available.
				 *
				 */
				static bool DuplicateStream( FileDescriptor FDOld,
						FileDescriptor FDNew ) ;



				/**
				 * Copy constructor made private to ensure that it will never
				 * be called.
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 * 
				 */			 
				Process( const Process & source ) ;
			
			
				/**
				 * Assignment operator made private to ensure that it will be
				 * never called.
				 * The compiler should complain whenever this undefined
				 * operator is called, implicitly or not.
				 *
				 */			 
				Process & operator = ( const Process & source ) ;

	
	
				/// The PID of this process.
				Pid _id ;
	
	
				/// The error code returned by this process, if any.
				ErrorCode _error ;
	
				
				
				
				// Static section.
				
				
				/// The owner of this process.
				static std::string _Owner ;
				
				
				/// Tells whether the process state has already been saved.
				static bool _Saved ;
				
				
				/// The path where the executable of this process should be run.
				static std::string _Path ;
				
				
				/// The name of the executable corresponding to this process.
				static std::string _Executable ;
	
/* 
 * Takes care of the awful issue of Windows DLL with templates.
 *
 * @see Ceylan's developer guide and README-build-for-windows.txt 
 * to understand it, and to be aware of the associated risks. 
 * 
 */
#pragma warning( push )
#pragma warning( disable: 4251 )

				/// The argument list given to this process.
				static std::list<std::string> _ArgumentList ;

#pragma warning( pop ) 	
					

		} ;



		ErrorCode Process::getError() const
		{
		
			return _error ;
			
		}



		Pid Process::getPID() const
		{
		
			return _id ;
			
		}


	}
	
}



#endif  // CEYLAN_PROCESS_H_

