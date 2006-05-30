#include "Ceylan.h"
using namespace Ceylan ;
using namespace Ceylan::Log ;


#include <exception>

#include <iostream>
using std::cout ;
using std::cerr ;
using std::endl ;

#include <string>
using std::string ;





class ExampleOfObject : public Object
{

    public:

		ExampleOfObject() throw( LogException )
		{
		
		}
		
        virtual void aVirtualMethod()
        {
		
		}

} ;



/**
 * Test of LogAggregatorRaw implementation for the Log system, not 
 * in immediate mode.
 *
 * Therefore messages will be stored and, if a smart aggregator 
 * is requested, they will be checked for mangled names.
 *
 * @see LogAggregatorRaw
 *
 */
int main( int argc, char * argv[] )
{


    try
    {

		// To avoid writing logs alongside the test executable :
		std::string speakerName ;
		Ceylan::System::Directory::StripFilename( argv[0], 
			/* base path */ 0, & speakerName ) ;
				
   		LogPlugClassical::StartService( speakerName, 
			/* immediate */ false, /* smart */ true ) ;
		
		LogPlug::info( "Starting testing LogAggregatorRaw implementation "
			"in non-immediate mode." ) ;
		
		LogPlug::info( "Dumping some informations about the log system : " 
			+ LogPlugClassical::ToString() ) ;
		
		LogPlug::info( "Creating a Ceylan::Object "
			"whose class name will be mangled, "
			"in order to check whether the smart demangling works.") ;
			
		ExampleOfObject * myExample = new ExampleOfObject() ;
		
		myExample->send( "Ceylan rocks !" ) ;
		myExample->send( "OSDL rocks !" ) ;
		
		delete myExample ;
		
		
		LogPlug::info( "End of test for LogAggregatorRaw "
			"implementation in non-immediate mode." ) ;
		
		/*
		 * No LogMessage to delete, Aggregators take care of it 
		 * (ownership taken).
		 *
		 * Aggregation triggered by LogPlugClassical, at the end.
		 *
		 */
		
	}
	catch ( const Ceylan::Exception & e )
    {
        cerr << "Ceylan exception caught : "
        	 << e.toString( Ceylan::high ) << endl ;
        return Ceylan::ExitFailure ;

    }

    catch ( const std::exception & e )
    {
        cerr << "Standard exception caught : " << e.what() << endl ;
        return Ceylan::ExitFailure ;

    }

    catch ( ... )
    {
        cerr << "Unknown exception caught" << endl ;
        return Ceylan::ExitFailure ;

    }
	
	
	// Triggers the aggregation :
   	LogPlugClassical::StopService() ;

    return Ceylan::ExitSuccess ;

}
