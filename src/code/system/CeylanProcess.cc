#include "CeylanProcess.h"

#include "CeylanLogPlug.h"
#include "CeylanSystem.h"
#include "CeylanStandardFile.h"      // for StandardFile
#include "CeylanDirectory.h"
#include "CeylanInputStream.h"
#include "CeylanOutputStream.h"
#include "CeylanOperators.h"
#include "CeylanTypes.h"             // for Ceylan::Uint16


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"            // for configure-time settings
#endif // CEYLAN_USES_CONFIG_H


extern "C"
{

#ifdef CEYLAN_USES_UNISTD_H
#include <unistd.h>                  // for pid_t, sysconf, fork 
#endif // CEYLAN_USES_UNISTD_H

#ifdef CEYLAN_USES_SYS_WAIT_H
#include <sys/wait.h>                // for waitpid
#endif // CEYLAN_USES_SYS_WAIT_H

#ifdef CEYLAN_USES_PWD_H
#include <pwd.h>                     // for getpwuid
#endif // CEYLAN_USES_PWD_H

#ifdef CEYLAN_USES_SYS_TIMES_H
#include <sys/times.h>               // for times
#endif // CEYLAN_USES_SYS_TIMES_H 

#ifdef CEYLAN_USES_SYS_TYPES_H
#include <sys/types.h>               // for fork
#endif // CEYLAN_USES_SYS_TYPES_H

#ifdef CEYLAN_USES_SIGNAL_H
#include <signal.h>                  // for kill
#endif // CEYLAN_USES_SIGNAL_H

#ifdef CEYLAN_USES_PROCESS_H
#include <process.h>                 // for _getpid
#endif // CEYLAN_USES_PROCESS_H
	
}


#include <csignal>
#include <cerrno>


using std::string ;
using std::list ;

using namespace Ceylan ;
using namespace Ceylan::System ;
using namespace Ceylan::Log ;


string Process::_Owner ;
string Process::_Path ;
string Process::_Executable ;


list<string> Process::_ArgumentList ;

bool Process::_Saved = false ;


/* 
 * Fork is the function that implements subprocesses on Unix. 
 * It does not exist on MS-Windows, and has to be replaced by a series of
 * different API calls, such as spawn or CreateProcess. 
 *
 * @see http://msdn.microsoft.com/library/en-us/dnucmg/html/UCMGch09.asp
 *
 */



ProcessException::ProcessException( const string message ) throw():
	RunnableException( message )
{

}


ProcessException::~ProcessException() throw()
{

}




Process::Process() throw():
	Runnable(),
	_id     ( 0 ),
	_error  ( 0 )
{

}


Process::Process( const string & name ) throw():
	Runnable( name ),
	_id     ( 0 ),
	_error  ( 0 )
{

}


Process::~Process() throw()
{

}


void Process::run() throw( RunnableException )
{

#if CEYLAN_USES_ADVANCED_PROCESS_MANAGEMENT

	// pid_t can be -1, whereas _id is unsigned:
	pid_t res = ::fork() ;

	if ( res == -1 )
	{
		_error = errno ;
		processCreationFailed() ;
		return ;
	}

	_id = static_cast<Pid>( res ) ;
	
	_error = 0 ;

	if ( _id == 0 )
	{
		// We are the forked child. getpid is expected to be non-negative:
		_id = ::getpid() ;
		start() ;
	}

	// We are the forked father.

#else // CEYLAN_USES_ADVANCED_PROCESS_MANAGEMENT

	throw ProcessException( "Process::run: "
		"advanced process management feature not available." ) ;
		 	
#endif // CEYLAN_USES_ADVANCED_PROCESS_MANAGEMENT
	
}


void Process::kill() throw( ProcessException )
{


#if CEYLAN_USES_ADVANCED_PROCESS_MANAGEMENT

	if ( _id <= 0 )
		return ;

	int ret = ::kill( _id, SIGTERM ) ;

	if ( ret != 0 )
	{
		_error = errno ;
		throw ProcessException( "Process::kill: could not kill process "
			+ Ceylan::toString( _id ) + ": "
			+ System::explainError( _error ) ) ;
	}
	else
	{
		ErrorCode executionInfo ;
		WaitChildProcess( * this, & executionInfo ) ;
	}
	
#else // CEYLAN_USES_ADVANCED_PROCESS_MANAGEMENT

	throw ProcessException( "Process::kill: "
		"advanced process management feature not available." ) ;
	
#endif // CEYLAN_USES_ADVANCED_PROCESS_MANAGEMENT

}


bool Process::isRunning() const throw( ProcessException )
{

#if CEYLAN_USES_ADVANCED_PROCESS_MANAGEMENT

	pid_t p = 0 ;

	if ( _id > 0 )
	{
		bool stayInLoop = true ;
		while( stayInLoop )
		{
			p = ::waitpid( _id, 0, WNOHANG ) ;
			stayInLoop = ( p < 0 && errno == EINTR ) ;
		}
	}

	return p == 0 ;
	
#else // CEYLAN_USES_ADVANCED_PROCESS_MANAGEMENT

	throw ProcessException( "Process::isRunning: "
		"advanced process management feature not available." ) ;
		
#endif // CEYLAN_USES_ADVANCED_PROCESS_MANAGEMENT

}


const string Process::toString( Ceylan::VerbosityLevels level )
	const throw()
{

	return "Process whose PID is " + Ceylan::toString( _id ) ;
	
}
	

// Static section.


Pid Process::GetHostingPID() throw( ProcessException )
{

#ifdef CEYLAN_USES_PROCESS_H

	return ::_getpid() ;

#else // CEYLAN_USES_PROCESS_H

#ifdef CEYLAN_USES_GETPID

	return ::getpid() ;

#else // CEYLAN_USES_GETPID

	throw ProcessException( "Process::GetHostingPID: "
		"not supported on this platform" ) ;

#endif // CEYLAN_USES_GETPID

#endif // CEYLAN_USES_PROCESS_H

}


Pid Process::GetParentID() throw( ProcessException )
{

#ifdef CEYLAN_USES_GETPID

	return ::getpid() ;

#else // CEYLAN_USES_GETPID

	throw ProcessException( "Process::GetParentID: "
		"not supported on this platform" ) ;

#endif // CEYLAN_USES_GETPID
}


Process::ExitReason Process::WaitChildProcess( const Process & childProcess,
	ErrorCode * executionInfo ) throw( ProcessException )
{

#ifdef CEYLAN_USES_WAITPID

	int status = 0 ;
	ExitReason ret = BadChildPid ;

	Pid p = ::waitpid( childProcess.getPID(), & status, 0 ) ;

	// Tracks down how the child process finished:

	if ( p == childProcess.getPID() )
	{

		/*
		 * These macros (WIFEXITED, WEXITSTATUS, etc.) include
		 * old-style C casts that cannot be avoided.
		 *
		 */
		if ( WIFEXITED( status ) )
		{
			if ( executionInfo != 0 )
				* executionInfo = WEXITSTATUS( status ) ;
			ret = ExitedNormally ;
		}
		else if ( WIFSIGNALED( status ) )
		{
			if ( executionInfo != 0 )
				* executionInfo = WTERMSIG( status ) ;
			ret = Signaled ;
		}
		else if ( WIFSTOPPED( status ) )
		{
			if ( executionInfo != 0 )
				* executionInfo = WSTOPSIG( status ) ;
			ret = Stopped ;
		}

	}

	return ret ;


#else // CEYLAN_USES_WAITPID

	throw ProcessException( "Process::WaitChildProcess: "
		"not supported on this platform" ) ;

#endif // CEYLAN_USES_WAITPID

}


string Process::GetOwner() throw( ProcessException )
{

#if CEYLAN_USES_ADVANCED_PROCESS_MANAGEMENT

	if ( _Owner.empty() )
	{

		struct passwd * p ;
		p = ::getpwuid( ::geteuid() ) ;
		
		if ( p != 0 )
			_Owner = p->pw_name ;
		else
			throw ProcessException( "Process::GetOwner: "
				"unable to determine process owner." ) ;
	}

	return _Owner ;

#else // CEYLAN_USES_ADVANCED_PROCESS_MANAGEMENT

	throw ProcessException( "Process::GetOwner: "
		"advanced process management feature not available" ) ;
	
#endif // CEYLAN_USES_ADVANCED_PROCESS_MANAGEMENT

}


void Process::RunExecutable(
	const string & filename,
	const list<string> & argv,
	const string & stdoutFilename,
	const string & stderrFilename,
	const string & stdinFilename ) throw( ProcessException )
{


#if CEYLAN_USES_ADVANCED_PROCESS_MANAGEMENT


	// Constructs the argument list:

	char ** cargv = new char * [ argv.size() + 2 ] ;

	cargv[0] = new char[ filename.size() + 1 ] ;
	filename.copy( cargv[0], filename.size() ) ;
	cargv[0][ filename.size() ] = 0 ;

	int currentArg = 1 ;

	for( list<string>::const_iterator it = argv.begin(); 
		it != argv.end(); it++ )
	{
		cargv[currentArg] = new char[ (*it).size() + 1 ] ;
		(*it).copy( cargv[currentArg], (*it).size() ) ;
		cargv[currentArg][ (*it).size() ] = 0 ;
		currentArg++ ;
	}

	cargv[currentArg] = 0 ;

	// Redirects I/O if necessary:

	if ( ! stdoutFilename.empty() 
			&& ! RedirectStdout( stdoutFilename ) )
		throw ProcessException( 
			"Process::runExecutable: could not redirect stdout into "
			+ stdoutFilename ) ;

	if ( ! stderrFilename.empty() 
			&& ! RedirectStderr( stderrFilename ) )
		throw ProcessException( 
			"Process::runExecutable: could not redirect stderr into "
			+ stderrFilename ) ;

	if ( ! stdinFilename.empty() 
			&& ! RedirectStdin( stdinFilename ) )
		throw ProcessException( 
			"Process::runExecutable: could not redirect stdin into "
			+ stdinFilename ) ;

	// Executes it:

	::execvp( cargv[0], cargv ) ;

	throw ProcessException( 
		"Process::runExecutable: could not execute code from "
		+ filename + ": "
		+ System::explainError( errno ) + " " + cargv[0] ) ;

	for ( Ceylan::Uint16 i = 0; cargv[i] != 0 ; i++ )
		delete [] cargv[i] ;

	delete [] cargv ;

#else // CEYLAN_USES_ADVANCED_PROCESS_MANAGEMENT

	throw ProcessException( "Process::RunExecutable: "
		"advanced process management feature not available" ) ;

#endif // CEYLAN_USES_ADVANCED_PROCESS_MANAGEMENT


}


bool Process::RedirectStdout( const string & filename ) 
	throw( ProcessException )
{

#if CEYLAN_ARCH_NINTENDO_DS
	
	throw ProcessException( "Process::RedirectStdout failed: "
		"not available on the Nintendo DS." ) ;

#else // CEYLAN_ARCH_NINTENDO_DS

#if CEYLAN_USES_FILE_DESCRIPTORS

	bool ret = false ;

	try
	{
	
		StandardFile & f = StandardFile::Create( filename ) ;
		
		ret = DuplicateStream( f.getFileDescriptor(), STDOUT_FILENO ) ;
		f.close() ;
		
		delete & f ;
		
	}
	catch( const SystemException & e )
	{
		LogPlug::error( 
			"Ceylan::System::Process: could not redirect stdout to "
			+ filename + ": " + e.toString() ) ;
	}

	return ret ;
	
#else // CEYLAN_USES_FILE_DESCRIPTORS

	throw ProcessException( "Process::RedirectStdout: "
		"file descriptor feature not available" ) ;
		
#endif // CEYLAN_USES_FILE_DESCRIPTORS

#endif // CEYLAN_ARCH_NINTENDO_DS
	
}


bool Process::RedirectStdout( OutputStream & os ) throw( ProcessException )
{

#if CEYLAN_ARCH_NINTENDO_DS
	
	throw ProcessException( "Process::RedirectStdout failed: "
		"not available on the Nintendo DS." ) ;

#else // CEYLAN_ARCH_NINTENDO_DS

#if CEYLAN_USES_FILE_DESCRIPTORS

	return DuplicateStream( os.getOutputStreamID(), STDOUT_FILENO ) ;
	
#else // CEYLAN_USES_FILE_DESCRIPTORS

	throw ProcessException( "Process::RedirectStdout: "
		"file descriptor feature not available" ) ;
		
#endif // CEYLAN_USES_FILE_DESCRIPTORS

#endif // CEYLAN_ARCH_NINTENDO_DS

}


bool Process::RedirectStderr( const string & filename ) 
	throw( ProcessException )
{

#if CEYLAN_ARCH_NINTENDO_DS
	
	throw ProcessException( "Process::RedirectStderr failed: "
		"not available on the Nintendo DS." ) ;

#else // CEYLAN_ARCH_NINTENDO_DS

#if CEYLAN_USES_FILE_DESCRIPTORS

	bool ret = false ;

	try
	{

		StandardFile & f = StandardFile::Create( filename ) ;
		
		ret = DuplicateStream( f.getFileDescriptor(), STDERR_FILENO ) ;
		f.close() ;
		
		delete & f ;

	}
	catch( const SystemException & e )
	{
		LogPlug::error( 
			"Ceylan::System::Process: could not redirect stderr to "
			+ filename + ": " + e.toString() ) ;
	}

	return ret ;
	
#else // CEYLAN_USES_FILE_DESCRIPTORS

	throw ProcessException( "Process::RedirectStderr: "
		"file descriptor feature not available" ) ;
		
#endif // CEYLAN_USES_FILE_DESCRIPTORS

#endif // CEYLAN_ARCH_NINTENDO_DS

}


bool Process::RedirectStderr( OutputStream & os ) throw( ProcessException )
{

#if CEYLAN_ARCH_NINTENDO_DS
	
	throw ProcessException( "Process::RedirectStderr failed: "
		"not available on the Nintendo DS." ) ;

#else // CEYLAN_ARCH_NINTENDO_DS

#if CEYLAN_USES_FILE_DESCRIPTORS

	return DuplicateStream( os.getOutputStreamID(), STDERR_FILENO ) ;
	
#else // CEYLAN_USES_FILE_DESCRIPTORS

	throw ProcessException( "Process::RedirectStderr: "
		"file descriptor feature not available" ) ;
		
#endif // CEYLAN_USES_FILE_DESCRIPTORS

#endif // CEYLAN_ARCH_NINTENDO_DS

}


bool Process::RedirectStdin( const string & filename ) throw( ProcessException )
{

#if CEYLAN_ARCH_NINTENDO_DS
	
	throw ProcessException( "Process::RedirectStdin failed: "
		"not available on the Nintendo DS." ) ;

#else // CEYLAN_ARCH_NINTENDO_DS

#if CEYLAN_USES_FILE_DESCRIPTORS


	bool ret = false ;

	try
	{

		StandardFile & f = StandardFile::Create( filename ) ;
		
		ret = DuplicateStream( f.getFileDescriptor(), STDIN_FILENO ) ;
		f.close() ;
		
		delete & f ;

	}
	catch( const SystemException & e )
	{
		LogPlug::error( 
			"Ceylan::System::Process: could not redirect stdin to "
			+ filename + ": " + e.toString() ) ;
	}

	return ret ;
	
#else // CEYLAN_USES_FILE_DESCRIPTORS

	throw ProcessException( "Process::RedirectStdin: "
		"file descriptor feature not available" ) ;
		
#endif // CEYLAN_USES_FILE_DESCRIPTORS

#endif // CEYLAN_ARCH_NINTENDO_DS

}


bool Process::RedirectStdin( InputStream & is ) throw( ProcessException )
{

#if CEYLAN_ARCH_NINTENDO_DS
	
	throw ProcessException( "Process::RedirectStdin failed: "
		"not available on the Nintendo DS." ) ;

#else // CEYLAN_ARCH_NINTENDO_DS

#if CEYLAN_USES_FILE_DESCRIPTORS

	return DuplicateStream( is.getInputStreamID(), STDIN_FILENO ) ;
	
#else // CEYLAN_USES_FILE_DESCRIPTORS

	throw ProcessException( "Process::RedirectStdin: "
		"file descriptor feature not available" ) ;
		
#endif // CEYLAN_USES_FILE_DESCRIPTORS

#endif // CEYLAN_ARCH_NINTENDO_DS

}




void Process::processCreationFailed() throw( ProcessException )
{

	throw ProcessException( "Ceylan::Process:: process creation failed: "
		+ System::explainError( _error ) ) ;
		
}


Ceylan::Uint32 Process::GetTime() throw( ProcessException )
{

#if CEYLAN_ARCH_NINTENDO_DS
	
	throw ProcessException( "Process::RedirectStdout failed: "
		"not available on the Nintendo DS." ) ;

#else // CEYLAN_ARCH_NINTENDO_DS

#if CEYLAN_USES_ADVANCED_PROCESS_MANAGEMENT

	static long clockticks = 0 ;

	if ( clockticks == 0 )
		clockticks = ::sysconf( _SC_CLK_TCK ) ;

	if ( clockticks < 0 )
		throw ProcessException( 
			"Process::GetTime: unable to determine system clock ticks: "
			+ System::explainError( errno ) ) ;

	if ( clockticks == 0 )
		throw ProcessException( "Process::GetTime: "
			"clock ticks equal to zero according to the system" ) ;

	struct tms t ;
	
	if ( ::times( & t ) == static_cast<clock_t>( -1 ) )
		throw ProcessException( "Process::GetTime: unable to determine "
			"time spent in the process: " 
			+ System::explainError( errno ) ) ;
	
	return static_cast<Ceylan::Uint32>( 
		static_cast<SignedLongInteger>( t.tms_utime ) / clockticks ) ;
		
#else // CEYLAN_USES_ADVANCED_PROCESS_MANAGEMENT

	throw ProcessException( "Process::GetTime: "
		"advanced process management feature not available" ) ;
		
#endif // CEYLAN_USES_ADVANCED_PROCESS_MANAGEMENT

#endif // CEYLAN_ARCH_NINTENDO_DS
		
}


void Process::SaveState( int argc, char ** argv ) throw()
{

	_Path = Directory::GetCurrentWorkingDirectoryPath() ;
	_Executable  = argv[ 0 ] ;

	for ( Ceylan::Uint16 i = 1; i < argc; i++ )
		_ArgumentList.push_back( argv[ i ] ) ;
		
	_Saved = true ;	
		
}


void Process::Restart() throw( ProcessException )
{

	if ( ! _Saved )
		throw ProcessException( "Ceylan::System::Process::restart: "
			"process command line not available." ) ;

	try
	{
		Directory::ChangeWorkingDirectory( _Path ) ;
	}
	catch ( const DirectoryException & e )
	{
		throw ProcessException( "Ceylan::System::Process::restart: "
			"could not change directory to initial path " + _Path ) ;
	}

	RunExecutable( _Executable, _ArgumentList ) ;

	throw ProcessException( "Ceylan::System::Process::restart: "
		"could not run executable " + _Executable ) ;

}



bool Process::DuplicateStream( FileDescriptor FDOld, 
	FileDescriptor FDNew ) throw( Features::FeatureNotAvailableException )
{

#if CEYLAN_ARCH_NINTENDO_DS

	throw Features::FeatureNotAvailableException( 
		"Process::DuplicateStream failed: not available on the Nintendo DS." ) ;

#else // CEYLAN_ARCH_NINTENDO_DS

#if CEYLAN_USES_FILE_DESCRIPTORS
	return ::dup2( FDOld, FDNew ) == FDNew ;
#endif // CEYLAN_USES_FILE_DESCRIPTORS	

	throw Features::FeatureNotAvailableException( "Process::DuplicateStream: "
		"file descriptor feature not available" ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS
		
}



