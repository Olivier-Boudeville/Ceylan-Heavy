#include "CeylanLogChannel.h"

#include "CeylanLogMessage.h"
#include "CeylanLogLight.h"      // for CEYLAN_LOG


#include "CeylanOperators.h"
#include "CeylanStringUtils.h"   // for formatStringList

#if CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"        // for CEYLAN_DEBUG
#endif // CEYLAN_USES_CONFIG_H


using std::string ;
using std::list ;


using namespace Ceylan::Log ;


LogChannel::LogChannel( const string & name ) throw() :
	_name( name )
{

}


LogChannel::~LogChannel() throw() 
{

	for (  list<LogMessage *>::iterator it = _messages.begin(); 
		it != _messages.end(); it++ )
	{
		delete (*it) ;
	}
	
		
}


void LogChannel::addMessage( LogMessage & message, bool check ) 
	throw( LogException )
{

	if ( check ) 
	{
		if ( message.getChannelName() != _name )
			throw LogException( 
				"LogChannel::addMessage : trying to add to LogChannel "
				+ _name 
				+ " a log message whose registered LogChannel is "
				+ message.getChannelName() + " (not " + _name + ")." ) ;	
	}
	
	_messages.push_back( & message ) ;

}


const string LogChannel::getName() const throw()
{
	return _name ;
}


unsigned int LogChannel::getMessageCount() const throw()
{

	/*
	 * If there were null pointers in this list (which should not be),
	 * count would be inaccurate.	
	 *
	 */
	return static_cast<unsigned int>( _messages.size() ) ;
	
}


const string LogChannel::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

	string result = "LogChannel " + _name ;
	
	if ( _messages.empty() )
		return result + " contains no message" ;
	
	if ( _messages.size() == 1 )
	{
		result += " contains only one message : " 
			+ _messages.back()->toString( level ) ;
		return result ;
	}	
	else
	{
		result += " contains " + Ceylan::toString( 
			static_cast<Ceylan::Uint32>( _messages.size() ) ) 
			+ " messages : " ;
	}
	
	list<string> res ;
		
	for ( list<LogMessage *>::const_iterator it = _messages.begin(); 
		it != _messages.end(); it++ )
	{
	
#if CEYLAN_DEBUG

		if ( (*it) == 0 )
		{
			CEYLAN_LOG( "Error, LogChannel::toString : "
				"null pointer in message list, skipping." ) ;
			break ;
		}	
		
#endif // CEYLAN_DEBUG

		res.push_back( (*it)->toString( level ) ) ;
		
	}	

	return result + formatStringList( res ) ;
	
}	
