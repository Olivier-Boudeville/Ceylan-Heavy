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


#include "Ceylan.h"

#include <iostream>

#include <string>
using std::string ;

#include <list>
using std::list ;

#include <exception>


using namespace Ceylan ;
using namespace Ceylan::Log ;
using namespace Ceylan::System ;



void displayDirectory( const string & directory, 
	const list<string> & subDirectories, const list<string> & files, 
	const list<string> & otherEntries ) throw()
{

		LogPlug::info( "Listing the content of directory '" 
			+ directory + "':" ) ;
		
		
		if ( subDirectories.empty() )
		{
		
			LogPlug::info( "  + no subdirectory found" ) ;
			
		}	
		else
		{
		
			LogPlug::info( "  + subdirectories:" ) ;
			
			for ( list<string>::const_iterator it = subDirectories.begin();
					it != subDirectories.end(); it++ )
				LogPlug::info( "     * " + (*it) ) ;
		}	
			  
		
		if ( files.empty() )
		{
		
			LogPlug::info( "  + no file found" ) ;
			
		}	
		else
		{
		
			LogPlug::info( "  + files:" ) ;
			
			for ( list<string>::const_iterator it = files.begin();
					it != files.end(); it++ )
				LogPlug::info( "     * " + (*it) ) ;
		}	
			
			  
		if ( otherEntries.empty() )
		{
		
			LogPlug::info( "  + no other entry found" ) ;
			
		}	
		else
		{
		
			LogPlug::info( "  + other entries:" ) ;
			
			for ( list<string>::const_iterator it = otherEntries.begin();
					it != otherEntries.end(); it++ )
				LogPlug::info( "     * " + (*it) ) ;
		}	

}

	


/**
 * Test for the libfat support offered by the Ceylan library on Nintendo DS.
 *
 * Test coverage is far less complete than for usual computer platforms though.
 *
 */
int main( int argc, char * argv[] )
{

	 

	LogHolder myLog( argc, argv ) ;

	
    try
    {
	
		System::InitializeInterrupts( true ) ;
		
		LogPlug::info( "Test of Ceylan support for libfat filesystems" ) ;
		LogPlug::info( "Opening current directory." ) ;

		Directory & d = Directory::Open() ; 

		LogPlug::info( "Current directory opened." ) ;

		list<string> subDirectories, files, otherEntries ;
		
		LogPlug::info( "Listing the directory content." ) ;
		d.getSortedEntries( subDirectories, files, otherEntries ) ;		
		displayDirectory( Directory::GetCurrentWorkingDirectoryPath(), 
			subDirectories, files, otherEntries ) ;
		
		string filename = "CeylanTest.txt" ;
		
		LogPlug::info( "Creating now a new file, '" + filename + "'." ) ;
		
		LogPlug::warning( "On some linker interfaces (ex: the SuperCard GUI), " 
			"the created file ('" + filename 
			+ "') may not be displayed, even though it exists on the card" ) ;
			
		File & createdFile = File::Create( filename ) ;

		
		std::string message = "Exercise caution in your daily affairs. Try to have as good a life as you can under the circumstances. Afternoon very favorable for romance. Try a single person for a change. Thanks fortune !" ;

		Ceylan::StringSize targetSize = message.size() ;
		
		LogPlug::info( "Writing a string in it, whose length is "
			+ Ceylan::toString( targetSize ) + " bytes." ) ;
		
		createdFile.write( message ) ;
		
		LogPlug::info( "Deleting the file object, close implied." ) ;
		delete & createdFile ;
		

		LogPlug::info( "Listing the directory content again." ) ;
		
		subDirectories.clear() ;
		files.clear() ;
		otherEntries.clear() ;
		
		d.getSortedEntries( subDirectories, files, otherEntries ) ;		
		displayDirectory( Directory::GetCurrentWorkingDirectoryPath(), 
			subDirectories, files, otherEntries ) ;

		delete & d ;
		
		LogPlug::info( "Opening now this newly created file, '" 
			+ filename + "'." ) ;
			
		File & openedFile = File::Open( filename ) ;
		
		Ceylan::Uint32 readSize = openedFile.size() ;
		LogPlug::info( "This file has for size " 
			+ Ceylan::toString( readSize ) + " bytes." ) ;
		
		if ( readSize != targetSize )
			throw TestException( 
				"Created file does not have the expected size." ) ; 
		
		LogPlug::info( "Read size is the expected one." ) ;
		
		char* textBuffer = new char[ readSize + 1 ] ;
		textBuffer[readSize] = 0 ;
		
		openedFile.readExactLength( textBuffer, readSize ) ;
		
		string readMessage( textBuffer ) ;
		
		LogPlug::info( "Listing its content: '" + readMessage + "'." ) ;
		
		if ( readMessage != message )
			throw TestException( 
				"Created file does not have the expected content." ) ; 

		LogPlug::info( "Read content is the expected one." ) ;
		
		delete [] textBuffer ;
			
		delete & openedFile ;
		
			
		LogPlug::info( 
			"Press any key to end the program and look at the logs" ) ;
			
		// LogHolder out of scope: log browser triggered.
    }
   
    catch ( const Ceylan::Exception & e )
    {
	
        LogPlug::error( "Ceylan exception caught: " 
			+ e.toString( Ceylan::high ) ) ;
			
		return Ceylan::ExitFailure ;

    }

    catch ( const std::exception & e )
    {
	
        LogPlug::error( string( "Standard exception caught: " ) + e.what() ) ;
		return Ceylan::ExitFailure ;

    }

    catch ( ... )
    {
	
        LogPlug::error( "Unknown exception caught" ) ;
		return Ceylan::ExitFailure ;

    }

	LogPlug::info( "Exit on success (no error)" ) ;
	
    return Ceylan::ExitSuccess ;

}
