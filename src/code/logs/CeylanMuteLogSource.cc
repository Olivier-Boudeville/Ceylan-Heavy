#include "CeylanMuteLogSource.h"



using std::string ;

using namespace Ceylan::Log ;



MuteLogSource::MuteLogSource() throw(): 
	LogSource( "(muted)" )
{

}


MuteLogSource::~MuteLogSource() throw() 
{
	
}



void MuteLogSource::send( const string & message, 
	LevelOfDetail levelOfDetail ) throw( LogException ) 
{
	
	// Nothing must be done here.
	
}



const string MuteLogSource::toString( Ceylan::VerbosityLevels level ) 
	const throw() 
{

	return "Mute Log source" ;
	 
}	

