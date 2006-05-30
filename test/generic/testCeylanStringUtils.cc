#include "Ceylan.h"

#include <string>
using std::string ;

#include <list>
using std::list ;

#include <exception>



using namespace Ceylan ;
using namespace Ceylan::Log ;


/**
 * Test of Ceylan utilities.
 *
 * @see CeylanStringUtils.h.
 *
 */
int main( int argc, char * argv[] )
{

	LogHolder logger( argc, argv ) ;

    try
    {

		LogPlug::info( "Testing Ceylan string utilities." ) ;

		const string toBeConverted = "Here is a lighter than symbol (<), "
			"a greater than (>) and an ampersand (&)." ;
		
		LogPlug::info( "Testing HTML encoding, "
			"which should be hereby visible : " 
			+ Ceylan::encodeToHTML( toBeConverted ) ) ;
			
		const string toBePhonetized = 
			"The lazy cat quick jumps .. over the 4 brown OSDL errr ! Hum." ;
				
		LogPlug::info( "The sentence '" + toBePhonetized 
			+ "' is phonetically converted to : '"
			+ encodeToPhonetic( toBePhonetized ) + "'." ) ;
			
					
		/* 
		 * Bad idea : we want to be able to send HTML code in log messages,
		 * so automatic is mangling has been disabled.
		 *
		 * Hence the following string would mess up the HTML tags :
		 *
		 * LogPlug::info( "This is an implicit test of Ceylan::encodeToHTML : "
		 *  + toBeConverted
		 *  + " since the log system should automatically correct them." ) ;	
		 *
		 */

		const string toDemangle = "N3One3Two11ExampleFourE" ; 
		
		LogPlug::info( "Testing C++ symbol demangling of g++ 3.x encoding : "
			" the decoding of "	+  toDemangle + " is "  
			+ Ceylan::demangleSymbol( toDemangle ) + "." ) ;
		 
		 
		string tester = "I like the little red rooster. "
			"I always thought she was not a careless stupid idiot. "
			"She proved she was not. The grand'ma should be happy "
			"to be able to rely on the little red rooster, after all." ;
		
		string testerCopy = tester ;
		
		// Different sizes is the interesting case :
		
		const string tobeReplaced = "the little red rooster" ;
		const string replacement  = "the Wolf" ;
			
		
		LogPlug::info( "Replacing, in [" + tester + "], [" 
			+ tobeReplaced + "] by [" + replacement + "]." ) ;
			
		Ceylan::StringSize count = Ceylan::substituteInString( tester,
			tobeReplaced, replacement )  ;
		 	
		LogPlug::info( Ceylan::toString( count )
			+ " substitutions made, result is : " + tester ) ;
		
		if ( count != 2 )
			throw TestException( "Test for Ceylan::substituteInString failed, " 
				+ Ceylan::toString( count ) 
				+ " occurences changed instead of 2, in phase one." ) ;
		
		LogPlug::info( "Doing the reverse should result in "
			"the same first string (involution)." ) ;		
			
		count = Ceylan::substituteInString( tester, replacement, 
			tobeReplaced )  ;
		
		LogPlug::info( Ceylan::toString( count )
			+ " substitutions made, result is : " + tester ) ;
		
		if ( count != 2 )
			throw TestException( "Test for Ceylan::substituteInString failed, " 
				+ Ceylan::toString( count ) 
				+ " occurences changed instead of 2, in phase two." ) ;
		
		if ( testerCopy != tester )
			throw TestException( "Test for Ceylan::substituteInString failed, " 
				"involution was expected." ) ;
		
		LogPlug::info( "Testing reverse function : string [" + tobeReplaced	
			+  "] would become [" + Ceylan::reverse( tobeReplaced ) + "]." ) ;
			
		LogPlug::info( "Testing formatting of list of strings." ) ;
		
		list<string> l ;
		
		l.push_back( "I found my thrill - on Blueberry Hill" ) ;
		l.push_back( "On Blueberry Hill - when I found you" ) ;
		l.push_back( "The moon stood still - on Blueberry Hill" ) ;
		l.push_back( "And lingered until - my dreams came true" ) ;
		
		TextDisplayable::TextOutputFormat previous 
			= TextDisplayable::GetOutputFormat() ;
		
		TextDisplayable::SetOutputFormat( TextDisplayable::html ) ;
		LogPlug::info( "HTML formatting gives : " 
			+ Ceylan::formatStringList( l ) ) ;
		
		TextDisplayable::SetOutputFormat( TextDisplayable::rawText ) ;
		LogPlug::info( "Raw formatting gives : "  
			+ Ceylan::formatStringList( l ) ) ;
		
		// Restore :
		TextDisplayable::SetOutputFormat( previous ) ;
		
		string aPath = "/home/user/Projects/OSDL-loanized" ;
		list<string> split = Ceylan::split( aPath, '/' ) ;
		LogPlug::info( "Splitting path '" + aPath 
			+ "' with separator '/' gives : "
			+ Ceylan::formatStringList( split ) ) ;
			
		LogPlug::info( "Now, reverse operation." ) ;
			
		string joiner = ", " ;
		LogPlug::info( "Joining former list with '" + joiner + "' gives : '"
			+ Ceylan::join( split, joiner ) + "'." ) ;
		
		
		LogPlug::info( "(beware, HTML output eats spaces when more "
			"than two on a row)" ) ;
		
		string testWordSplit = "a b" ;		
		split = Ceylan::splitIntoWords( testWordSplit ) ;
		LogPlug::info( "Splitting '" + testWordSplit 
			+ "' (one space) into words leads to : "
			+ Ceylan::formatStringList( split, /* surround by ticks */ true ) 
			+ " instead of a basic space-splitting algorithm "
			"which would result in : " 
			+ Ceylan::formatStringList( Ceylan::split( testWordSplit, ' ' ), 
				/* surround by ticks */ true ) ) ;
			
		testWordSplit = "a  b" ;
		split = Ceylan::splitIntoWords( testWordSplit ) ;
		LogPlug::info( "Splitting '" + testWordSplit 
			+ "' (two spaces) into words leads to : "
			+ Ceylan::formatStringList( split, /* surround by ticks */ true )
			+ " instead of a basic space-splitting algorithm "
			"which would result in : " 
			+ Ceylan::formatStringList( Ceylan::split( testWordSplit, ' ' ), 
				/* surround by ticks */ true ) ) ;

		testWordSplit = "a   b" ;
		split = Ceylan::splitIntoWords( testWordSplit ) ;
		LogPlug::info( "Splitting '" + testWordSplit 
			+ "' (three spaces) into words leads to : "
			+ Ceylan::formatStringList( split, /* surround by ticks */ true )
			+ " instead of a basic space-splitting algorithm "
			"which would result in : " 
			+ Ceylan::formatStringList( Ceylan::split( testWordSplit, ' ' ), 
				/* surround by ticks */ true ) ) ;

		testWordSplit = "a    b" ;
		split = Ceylan::splitIntoWords( testWordSplit ) ;
		LogPlug::info( "Splitting '" + testWordSplit 
			+ "' (four spaces) into words leads to : "
			+ Ceylan::formatStringList( split, /* surround by ticks */ true )
			+ " instead of a basic space-splitting algorithm "
			"which would result in : " 
			+ Ceylan::formatStringList( Ceylan::split( testWordSplit, ' ' ), 
				/* surround by ticks */ true ) ) ;
		
		
		const string fullText = "This is a full text, which is to be split "
			"into paragraphs.\nThis is the second paragraph, it is quite "
			"short.\n\nThe latter paragraph is empty. ADDING 'JUST KIDDING' "
			"DOESN'T MAKE IT OKAY TO INSULT THE PRINCIPAL. " 
			"Bart Simpson on chalkboard in episode 2F09. "
			"This is a long one indeed." ;
		
		LogPlug::info( "Splitting following text into paragraphs : '" 
			+ fullText + "'." ) ;
		
		list<string> paragraphs = Ceylan::splitIntoParagraphs( fullText ) ;
		for ( list<string>::const_iterator it = paragraphs.begin();
				it != paragraphs.end(); it++ )
			LogPlug::info( "New paragraph : '" + (*it) + "'." ) ;
			
        LogPlug::info( "End of Ceylan utilities test." ) ;

 
    }
   
    catch ( const Ceylan::Exception & e )
    {
        std::cerr << "Ceylan exception caught : "
        	<< e.toString( Ceylan::high ) << std::endl ;
		return Ceylan::ExitFailure ;

    }

    catch ( const std::exception & e )
    {
        std::cerr << "Standard exception caught : " 
			 << e.what() << std::endl ;
		return Ceylan::ExitFailure ;

    }

    catch ( ... )
    {
        std::cerr << "Unknown exception caught" << std::endl ;
		return Ceylan::ExitFailure ;

    }

    return Ceylan::ExitSuccess ;

}
