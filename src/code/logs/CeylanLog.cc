#include "CeylanLog.h"

using std::string ;


using namespace Ceylan::Log ;



/**
 * Let's define in a centralized way informations shared at the 
 * framework level.
 *
 *
 */
 
const LevelOfDetail Ceylan::Log::DefaultLevelOfDetailForMessage  =  5 ;
const LevelOfDetail Ceylan::Log::MaximumLevelOfDetailForMessage  =  0 ;

const LevelOfDetail Ceylan::Log::DefaultLevelOfDetailForListener = 10 ;
const LevelOfDetail Ceylan::Log::MaximumLevelOfDetailForListener =  0 ;





LogException::LogException( const std::string & reason ) throw() :
 	Ceylan::Exception( reason ) 
{

}

	
LogException::~LogException() throw()
{

}

