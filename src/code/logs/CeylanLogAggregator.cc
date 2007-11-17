#include "CeylanLogAggregator.h"

#include "CeylanLog.h"              // for ProtocolSeparator
#include "CeylanLogLight.h"         // for CEYLAN_LOG
#include "CeylanObjectIdentifier.h" // for generateFromChannelName
#include "CeylanObjectChannel.h"    // for ObjectChannel
#include "CeylanLoggable.h"         // for Loggable, getEmbeddedChannelName
#include "CeylanStringUtils.h"      // for demangle

#include "CeylanOperators.h"
#include "CeylanLogPlug.h"
#include "CeylanLogChannel.h"
#include "CeylanLogMessage.h"


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"           // for CEYLAN_DEBUG
#endif // CEYLAN_USES_CONFIG_H




using std::string ;
using std::list ;

using namespace Ceylan::Log ;



LogAggregator::LogAggregatorException::LogAggregatorException( 
		const string & message ) throw(): 
	LogException( message )
{

}



LogAggregator::LogAggregatorException::~LogAggregatorException() throw()	
{

}
	
	
	
	
const LevelOfDetail LogAggregator::DefaultGlobalLevelOfDetail 
	= Ceylan::Log::DefaultLevelOfDetailForListener ;
	
	
							
LogAggregator::LogAggregator( bool beSmart, 
		bool useGlobalLevelOfDetail ) throw(): 
	_beSmart( beSmart ),
	_useGlobalLevelOfDetail( useGlobalLevelOfDetail ),
	_globalLevelOfDetail( DefaultGlobalLevelOfDetail )

{

	CEYLAN_LOG( "LogAggregator constructor." ) ;
	
}



LogAggregator::~LogAggregator() throw() 
{

	CEYLAN_LOG( "LogAggregator destructor called." ) ;

	// Deallocating gathered channels, which will deallocate their messages:
	
	// Curiously enough, a const_iterator can be used too !
	for ( list<LogChannel *>::iterator it = _channelList.begin(); 
		it != _channelList.begin(); it++ )
	{
		if ( (*it) != 0 )
			delete (*it) ;
	}		

}



LogChannel & LogAggregator::createBasicChannel( 
	const string & channelName ) throw( LogException )
{
	
#if CEYLAN_DEBUG

	if ( hasChannel( channelName ) )
		throw LogException( "LogAggregator::createBasicChannel: "
			"attempt to create an already-existing channel." ) ;	
	
	CEYLAN_LOG( "LogAggregator: creating basic channel " + channelName ) ;
				
#endif // CEYLAN_DEBUG
	
	LogChannel * newChannel = new LogChannel( channelName ) ;
	_channelList.push_back( newChannel ) ;
	
	return * newChannel ;
	
}



ObjectChannel & LogAggregator::createObjectChannel( 
	LogMessage & message ) throw( LogException )
{

	string realChannelName = Loggable::GetEmbeddedChannelName(
		message.getChannelName() ) ;
	
#if CEYLAN_DEBUG

	if ( hasChannel( realChannelName ) )
		throw LogException( "LogAggregator::createObjectChannel: "
			"attempt to create an already-existing channel, " 
			+ realChannelName + "." ) ;	
	
	CEYLAN_LOG( "LogAggregator: creating object channel for " 
		+ realChannelName ) ;
				
#endif // CEYLAN_DEBUG
	
	ObjectChannel * newChannel = new ObjectChannel( realChannelName ) ;
	_channelList.push_back( newChannel ) ;
	
	CEYLAN_LOG( "New object channel registered." ) ;
	
	return * newChannel ;
	
}



bool LogAggregator::hasChannel( const std::string & channelName ) const 
	throw( LogException )
{

	return ( findChannel( channelName ) != 0 ) ;

}




LogChannel * LogAggregator::findChannel( const string & channelName ) 
	const throw( LogException )
{


	CEYLAN_LOG( "LogAggregator::findChannel: looking for channel " 
		+ channelName ) ;
	
	/*
	 * Two cases: we have got to search a LogChannel or an 
	 * ObjectChannel, depending on the message being aimed at 
	 * a Loggable channel or not.
	 *
	 */

	if ( Loggable::IsALoggableChannelName( channelName ) )
		return findObjectChannel( 
			Loggable::GetEmbeddedChannelName( channelName ) ) ;
	else
		return findBasicChannel( channelName ) ;
		
}



void LogAggregator::transferChannel( LogChannel & source, 
	LogChannel & target ) throw( LogException )
{


	CEYLAN_LOG( "LogAggregator::transferChannel, from " 
		+ source.getName() 
		+ " to " + target.getName() ) ;
		
	CEYLAN_LOG( "Source channel is: " + source.toString() ) ;
	CEYLAN_LOG( "Target channel is: " + target.toString() ) ;
		
	/*
	 * For each message of source channel, update its channel 
	 * and move it to the target channel.
	 *
	 */
	
	for ( list<LogMessage *>::iterator it = source._messages.begin(); 
		it != source._messages.end(); it++ )
	{
	
#if CEYLAN_DEBUG

		if ( (*it) == 0 )
			throw LogException( "LogAggregator::transferChannel: "
				"null pointer in channel list." ) ;	
					
#endif // CEYLAN_DEBUG
		

		LogMessage * movedMessage = *it ;
		CEYLAN_LOG( "LogAggregator::transferChannel: moving " 
			+ movedMessage->toString( Ceylan::low ) ) ;
				
		movedMessage->setChannelName( target.getName() ) ;
		target.addMessage( * movedMessage ) ;
		
		/*
		 * Ownership transferred to target channel, message pointers
		 * set to null to avoid possible double deallocation of
		 * transferred messages.
		 *
		 */
		 
		*it = 0 ;

	}
	
	/*
	 * After having nullified all message pointers in source list,
	 * remove this null pointers:
	 *
	 */
	source._messages.clear() ;
	
	// And finally remove this empty obsolete channel: 
	removeChannel( source ) ;
	
}



void LogAggregator::removeChannel( LogChannel & target ) throw()
{
	CEYLAN_LOG( "Removing channel " + target.getName() ) ;
	_channelList.remove( & target ) ;
	delete & target ;
}



void LogAggregator::store( LogMessage & message ) throw( LogException ) 
{
	
	CEYLAN_LOG( "LogAggregator::store: incoming message " 
		+ message.toString() ) ;
	
	/*
	 * Tests whether this message is an object one or not, and 
	 * manages it accordingly:
	 *
	 */
	
	if ( Loggable::IsALoggableChannelName( message.getChannelName() ) )
		storeObjectMessage( message ) ;
	else
		storeBasicMessage( message ) ;
		
	CEYLAN_LOG( "LogAggregator::store: done" ) ;
		
}		

	
				
LogChannel * LogAggregator::findBasicChannel( 
	const string & basicChannelName ) const throw( LogException )
{

	CEYLAN_LOG( "LogAggregator::findBasicChannel: "
		"searching basic channels for " + basicChannelName ) ;
			
	for ( list<LogChannel *>::const_iterator it = _channelList.begin(); 
		it != _channelList.end(); it++ )
	{

#if CEYLAN_DEBUG

		if ( (*it) == 0 )
			throw LogException( "LogAggregator::findBasicChannel: "
				"null pointer in channel list." ) ;	

#endif // CEYLAN_DEBUG
		
		ObjectChannel * objChannel = 
			dynamic_cast<ObjectChannel *>( (*it) ) ;			
		
		if ( objChannel == 0 )
		{
		
			CEYLAN_LOG( (*it)->getName() 
				+ " is a basic log channel." ) ;
			
			if ( (*it)->getName() == basicChannelName )
				return (*it) ;
		}
	}
	
	/*
	 * Better raise an exception on abnormal conditions:
	 * 

	throw LogAggregatorException( "No basic channel named " 
		+ basicChannelName + " found." ) ;

	*/
	return 0 ;
		
}
	
	
					
ObjectChannel * LogAggregator::findObjectChannel( 
	const string & nonPrefixedChannelName ) const throw( LogException )
{


	CEYLAN_LOG( "LogAggregator::findObjectChannel: "
		"searching object channels for " + nonPrefixedChannelName ) ;
			
	for ( list<LogChannel *>::const_iterator it = _channelList.begin(); 
		it != _channelList.end(); it++ )
	{
	
#if CEYLAN_DEBUG

		if ( (*it) == 0 )
			throw LogException( "LogAggregator::findObjectChannel: "
				"null pointer in channel list." ) ;	

#endif // CEYLAN_DEBUG
		
		ObjectChannel * objChannel = 
			dynamic_cast<ObjectChannel *>( (*it) ) ;			
		
		if ( objChannel != 0 )
		{
		
			CEYLAN_LOG( objChannel->getName() + " is an Object log channel." ) ;
			
			if ( objChannel->getName() == nonPrefixedChannelName )
				return objChannel ;
		}
	}
	
	/*
	 * Better raise an exception on abnormal conditions:
	 * 

	throw LogAggregatorException( "No object channel named " 
		+ nonPrefixedChannelName + " found." ) ;

	*/
	return 0 ;
		
}



void LogAggregator::createBasicChannelFrom( LogMessage & message ) 
	throw( LogException ) 
{

	/*
	 * Simply, creates the new channel and stores its first 
	 * message in it:
	 *
	 */
	
	LogChannel & newChannel = createBasicChannel( message.getChannelName() ) ;
	newChannel.addMessage( message ) ;
	
}



void LogAggregator::createLoggableChannelFrom( LogMessage & message ) 
	throw( LogException )
{

	// If it is not a smart aggregator, just store and forget: 
	if ( ! _beSmart )
	{
		CEYLAN_LOG( "LogAggregator::createLoggableChannelFrom: "
			"being not smart, store message inconditionnally "
			"as if it were basic." ) ;
		createBasicChannelFrom( message ) ;
		return ; 
	}
	
	
	/*
	 * We are therefore here in a smart aggregator which will fix 
	 * mangled Loggable names.
	 *
	 */
	
	CEYLAN_LOG( "LogAggregator::createLoggableChannelFrom: "
		"smart aggregator will try to find any previously "
		"created channel corresponding to this message aimed at "
		+ message.getChannelName() ) ;
		
	/*
	 * Remove the protocol prefix and separator (typically, loggable://) 
	 * to get the real channel name:
	 *
	 */
	string realChannelName = Loggable::GetEmbeddedChannelName(
		message.getChannelName() ) ;
		
	CEYLAN_LOG( "LogAggregator::createLoggableChannelFrom: "
		"channel name is " + realChannelName ) ;
		
	/*
	 * First, check that this incoming channel name corresponds 
	 * to an object channel.
	 *
	 * We may be in the case where previous messages had been sent
	 * with the mangled class name because of the Object constructor.
	 *
	 * Let's seek whether it is really the case and, if so, let's 
	 * move those messages to their right loggable channel 
	 * (i.e. the new one, with a non-mangled class name).
	 *
	 */
	 			 			 
	ObjectIdentifier * identifierFromMessage ;
	 
	try 
	{
	 
		identifierFromMessage = 
			& ObjectIdentifier::generateFromChannelName( realChannelName ) ;
			
	} 
	catch( const Identifier::IdentifierException & idEx )
	{
	 	 
		throw LogException( 
			"LogAggregator::createLoggableChannelFrom: "
			"the channel name of incoming message, "
			+ realChannelName + " is not an object channel name: "
			+ idEx.toString() + ": this shoud never happen." ) ;
	}
	
	CEYLAN_LOG( "Trying to match " + realChannelName 
		+ " with already existing channels." ) ;
	
	
	/*
	 * This message corresponds that an object channel which does 
	 * not exist already.
	 * 
	 * Second, scan existing channels to know whether there is one whose
	 * instance address matches: it would have been created with 
	 * a mangled class name for this object.
	 *
	 */ 
	for ( list<LogChannel *>::const_iterator it = _channelList.begin();
		it != _channelList.end(); it++ )
	{
	

#if CEYLAN_DEBUG

		if ( (*it) == 0 )
		throw LogException( 
			"LogAggregator::createLoggableChannelFrom: "
			"null pointer in channel list." ) ;		
			
#endif // CEYLAN_DEBUG

		CEYLAN_LOG( "Trying " + (*it)->getName() ) ;
		
		ObjectChannel * objChannel = 
			dynamic_cast<ObjectChannel *>( (*it) ) ;	
					
		if ( objChannel )
		{
		
			CEYLAN_LOG( (*it)->getName() + " is an object channel." ) ;
			
			/*
			 * This element of the list is an ObjectChannel, maybe 
			 * which had the mangled name ?
			 *
			 */
			
			ObjectIdentifier * id = & objChannel->getObjectIdentifier() ;
			
			CEYLAN_LOG( "This object channel identifier is " 
				+ id->toString() ) ;
			 
			if ( identifierFromMessage->differentButMatches( * id ) )
			{
			
				CEYLAN_LOG( (*it)->getName() 
					+ " matches the new channel, moving its "
					"messages to the new one." ) ;
					
				/*
				 * Yes, so transfer the messages after having created
				 * the channel:
				 *
				 */
				
				ObjectChannel & newChannel = createObjectChannel( message ) ;
				transferChannel( * objChannel, newChannel ) ;
				
				/*
				 * Do not forget to add this incoming message which
				 * triggered the transfer !
				 *
				 */
				newChannel.addMessage( message ) ;
				
				// Normally, only one channel should be to fix.
				
				return ;
			}
			
			
			// This element does not match, do nothing more with it.

			CEYLAN_LOG( (*it)->getName() + " does not match." ) ;

		}
		
		// Not an object channel, skip it.
		
		CEYLAN_LOG( (*it)->getName() + " is not an object channel." ) ;
		
	} // for ...
	
	// Deallocate the object identifer used for comparison purpose:
	delete identifierFromMessage ;
			  
	
	/*
	 * We went through the list and did NOT find anything, so let's
	 * create it in the next step, which is common with non-smart 
	 * aggregators.
	 *
	 */
			 
	CEYLAN_LOG( realChannelName 
		+ " did not match any other object channel, create it." ) ;
			 
	ObjectChannel & newObjectChannel = createObjectChannel( message ) ;	
	
	CEYLAN_LOG( "Adding message to newly created object channel." ) ;
	
	newObjectChannel.addMessage( message ) ;
	
	CEYLAN_LOG( "LogAggregator::createLoggableChannelFrom: done." ) ;	
			 
}				



void LogAggregator::storeBasicMessage( LogMessage & basicLogMessage ) 
	throw( LogException )
{	

	CEYLAN_LOG( "LogAggregator::storeBasicMessage: message aimed at " 
		+ basicLogMessage.getChannelName() ) ;

	// Maybe the relevant basic channel already exists ?

	LogChannel * channel = findBasicChannel( 
		basicLogMessage.getChannelName() ) ;
	
	if ( channel == 0 )
	{
	
		CEYLAN_LOG( "LogAggregator::storeBasicMessage: Channel " 
			+ basicLogMessage.getChannelName() 
			+ " not found, hence is to be created." ) ;
		createBasicChannelFrom( basicLogMessage ) ;	
		
		return ;
	}
	
	CEYLAN_LOG( "LogAggregator::storeBasicMessage: "
		"adding new message to already existing channel "
		+ channel->getName() ) ;
	channel->addMessage( basicLogMessage ) ;
		
}



void LogAggregator::storeObjectMessage( 
	LogMessage & objectLogMessage ) throw( LogException )
{
	
	// Maybe the relevant object channel already exists ?	
	
	// First, auto-correct any mangled class name:
	demangle( objectLogMessage ) ;
	
	const string targetChannelName = Loggable::GetEmbeddedChannelName(
		objectLogMessage.getChannelName() ) ;

	CEYLAN_LOG( 
		"LogAggregator::storeObjectMessage: message aimed at " 
		+ targetChannelName ) ;
	
	ObjectChannel * channel = findObjectChannel( targetChannelName ) ;
		
	if ( channel == 0 )
	{
	
		CEYLAN_LOG( "LogAggregator::storeObjectMessage: Channel " 
			+ targetChannelName + " not found." ) ;
		
		/*
		 * Object Channel not found. A channel is to be created, a 
		 * brand new or an unmangled form replacing a former object
		 * channel.
		 *
		 * Either this is a first message sent to a new object 
		 * channel, or this is the first unmangled message sent
		 * from a previously mangled object channel.
		 *
		 */
		createLoggableChannelFrom( objectLogMessage ) ;
	
		CEYLAN_LOG( "Message successfully stored in new object channel." ) ;
		
		return ;
	}
	
	
	/*
	 * Yes, object channel already available, let's use it and 
	 * add the incoming message.
	 *
	 */
	
	CEYLAN_LOG( "Object Channel " + targetChannelName + " found." ) ;
	
	/*
	 * Removing now useless protocol prefix of this object 
	 * message's header:
	 *
	 */
	objectLogMessage.setChannelName( 
		Loggable::GetEmbeddedChannelName( 
			objectLogMessage.getChannelName() ) ) ;
	
	channel->addMessage( objectLogMessage ) ;		
	
}



void LogAggregator::demangle( LogMessage & objectLogMessage ) throw()
{


	/*
	 * Isolates the class name and replace it with a demangled one.
	 *
	 * @example: loggable://sonata/PID-31655/ExampleOne/0x8052b18
	 *
	 * Get ExampleOne
	 *
	 */
	
	string channelName = objectLogMessage.getChannelName() ;
	
	//LogPlug::debug( "LogAggregator demangling channel name " + channelName ) ;
	
	string demangled ;
	Ceylan::Uint16 count = 0 ;
	
	// Skip everything till fourth '/':
	
	Ceylan::Uint16 sepCount = 0 ;
	
	while ( sepCount < 4 )
	{
	
		while ( channelName[ count ] != ObjectIdentifier::Separator )
		{
			demangled += channelName[ count ] ;
			count++ ;	
		}
	
	
		// Jump over '/':
		count++ ;
		demangled += ObjectIdentifier::Separator ;
	
		sepCount++ ;
	}
	

	// Here we are in the interesting word:
	string className ;
	
	while ( channelName[ count ] != ObjectIdentifier::Separator )
	{
		className += channelName[ count ] ;
		count++ ;	
	}
	

	//LogPlug::debug( "Demangling asked for " + className ) ;
	
	demangled += Ceylan::demangleSymbol( className ) ;
	
	while ( count < channelName.size() )
	{
		demangled += channelName[ count ] ;
		count ++ ;	
	}
	
	objectLogMessage.setChannelName( demangled ) ;
	
}



Ceylan::VerbosityLevels LogAggregator::getOverallVerbosityLevel() const throw()
{

	LevelOfDetail level ;
	
	// Level of detail globally overriden ?
	if ( _useGlobalLevelOfDetail )			
	{
		level = _globalLevelOfDetail ;
	} 
	else
	{
		level = DefaultLevelOfDetailForListener ;
	}
	
	
	// Now maps the selected level to a verbosity level:
	 
	Ceylan::VerbosityLevels res =
		ConvertListenerLevelOfDetailToVerbosityLevel( level ) ;
		
	CEYLAN_LOG( "LogAggregator::getOverallVerbosityLevel: "
		"level of detail is: " + Ceylan::toString( res ) ) ;
	
	return res ;

}


				 
Ceylan::VerbosityLevels LogAggregator::getMessageVerbosityLevel( 
	const LogMessage & message ) const throw()
{

	LevelOfDetail level ;
	
	
	// Level of detail globally overriden ?
	if ( _useGlobalLevelOfDetail )			
	{
		level = _globalLevelOfDetail ;
	} 
	else
	{
		level = message.getLevelOfDetail() ;
	}


	// Now maps the selected level to a verbosity level:
	 
	Ceylan::VerbosityLevels res =
			LogAggregator::ConvertMessageLevelOfDetailToVerbosityLevel(
		level ) ;
	
	CEYLAN_LOG( "LogAggregator::getMessageVerbosityLevel: "
		"level of detail is: " + Ceylan::toString( res ) ) ;
				
	return res ;			

}
					
					
					
Ceylan::VerbosityLevels 
		LogAggregator::ConvertListenerLevelOfDetailToVerbosityLevel(
	LevelOfDetail level ) throw()
{
	
	/*
	 * Maps the selected level to a verbosity level:
	 *
	 * @note: a switch/case/default test could not be used since case label 
	 * would not reduce to an integer constant: 'case
	 * MaximumLevelOfDetailForMessage:' cannot refer to an extern, in C/C++
	 * No comment.
	 *
	 */
	 
	/*
	 * If the listener LOD is MaximumLevelOfDetailForListener (0), only
	 * top-priority messages are taken into account, hence the verbosity
	 * is low, etc.
	 *
	 */
	if ( level == MaximumLevelOfDetailForListener )
		return Ceylan::low ;
	else if ( level < DefaultLevelOfDetailForListener )
		return Ceylan::medium ;
	else
		return Ceylan::high ;
		
}

	

Ceylan::VerbosityLevels 
		LogAggregator::ConvertMessageLevelOfDetailToVerbosityLevel(
	LevelOfDetail level ) throw()
{
	
	/*
	 * Maps the selected level to a verbosity level:
	 *
	 * @note: a switch/case/default test could not be used since case label 
	 * would not reduce to an integer constant: 'case
	 * MaximumLevelOfDetailForMessage:' cannot refer to an extern, in C/C++
	 * No comment.
	 *
	 */
	
	// For a message, a maximum LOD leads to a maximum verbosity: 
	if ( level == MaximumLevelOfDetailForMessage )
		return Ceylan::high ;
	else if ( level <= DefaultLevelOfDetailForMessage )
		return Ceylan::medium ;
	else
		return Ceylan::low ;
		
}



const string LogAggregator::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

	return string( "LogAggregator is up and running, is " )
		+ ( _beSmart ? "" : "not " )
		+ string( "smart, and currently has " )
		+ Ceylan::toString( 
			static_cast<Ceylan::Uint32>( _channelList.size() ) )
		+ " log channel(s) created" ;
		
}

