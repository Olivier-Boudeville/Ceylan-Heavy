#include "CeylanLoggable.h"

#include "CeylanLogPlug.h"  // for GetTransport
#include "CeylanLog.h"      // for ProtocolSeparator

// for getEmbeddedURI, getProtocolName,ProtocolSeparator  :
#include "CeylanUniformResourceIdentifier.h" 



using std::string ;

using namespace Ceylan::Log ;


const string Loggable::ProtocolName = "loggable" ;


Loggable::Loggable( const string & name ) throw() : 
	LogSource( LogPlug::GetTransport() )
{
	setChannelName( name ) ;
}


Loggable::~Loggable() throw()
{
}


void Loggable::setChannelName( const std::string & channelName ) throw()
{
	LogSource::setChannelName( ProtocolName + Ceylan::URI::ProtocolSeparator 
		+ channelName ) ;
}


bool Loggable::IsALoggableChannelName( const string & channelName ) throw()
{

	return ( Ceylan::URI::getProtocolName( channelName,
		Ceylan::URI::ProtocolSeparator ) == ProtocolName ) ;
		
}


const string Loggable::GetEmbeddedChannelName( const string & fullChannelName )
	throw()
{	

	// Removes protocol separator (typically : '//')
	return Ceylan::URI::getEmbeddedURI( fullChannelName, 
		Ceylan::URI::ProtocolSeparator ) ;
			
}
