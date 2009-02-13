#ifndef CEYLAN_LOG_SOURCE_H_
#define CEYLAN_LOG_SOURCE_H_


#include "CeylanLog.h"               // for LevelOfDetail, LogException etc.
#include "CeylanTextDisplayable.h"   // for TextDisplayable etc.

#include <string>



namespace Ceylan
{


	namespace Log
	{
	
	
		// Link to the Log listener.
		class LogTransport ;
	
	
		/**
		 * This is the mother class for every potential sender of Log messages.
		 *
		 * Its job is to propagate incoming Log messages to the relevant 
		 * LogListener, through a LogTransport.
		 *
		 * @note The send methods, thanks to their parameters, create a log
		 * message object, whose ownership is transferred to the LogTransport,
		 * which is in charge of deallocating it when deemed appropriate.
		 *
		 * @see Log, LogTransport
		 *
		 */
		class CEYLAN_DLL LogSource : public TextDisplayable
		{
		
			public:
			
			
				/**
				 * Constructs a LogSource whose channel has for name
				 * <b>name</b>, with no registered LogTransport.
				 *
				 * @see setTransport
				 * 
				 */
				explicit LogSource( const std::string & name, 
					LevelOfDetail levelOfDetail 
						= DefaultLevelOfDetailForSource ) throw() ;
			
			
				/**
				 * Constructs a LogSource whose channel has for name
				 * <b>name</b>, linked to specified LogTransport.
				 * 
				 */
				LogSource( const std::string & name, LogTransport & transport,
					LevelOfDetail levelOfDetail 
						= DefaultLevelOfDetailForSource ) throw() ;
	
			
				/**
				 * Constructs a LogSource linked to specified LogTransport.
				 * 
				 * @note setChannelName should be called before any message 
				 * is sent from this LogSource's internal channel.
				 *
				 */
				explicit LogSource( LogTransport & transport, 
					LevelOfDetail levelOfDetail 
						= DefaultLevelOfDetailForSource ) throw() ;
				
				
				/// Basic virtual destructor.
				virtual ~LogSource() throw() ;
		


				/**
				 * Returns whether this Log source has a registered channel
				 * name.
				 *
				 */
				bool hasChannelName() const throw() ;				
				
				
				/// Sets this LogSource channel name.
				void setChannelName( const std::string & channelName ) throw() ;
		
		
				/// Returns this LogSource channel name.
				std::string getChannelName() const throw() ;
				
				
				
				/// Sets this LogSource level of detail of interest.
				void setLevelOfDetail( LevelOfDetail newLevel ) throw() ;
		
		
				/// Returns this LogSource channel name.
				LevelOfDetail getLevelOfDetail() const throw() ;
				
				
		
		        /**
		         * Sends <b>message</b> to this LogSource's internal channel,
				 * through known LogTransport.
		         *
		         * @param message the log message to send. Please avoid
				 * characters '<' and '>' since they have a special meaning
				 * for HTML log output. These characters used to be filtered 
				 * in HTML aggregators but it prevented messages to contain
				 * HTML tags on purpose, which proved to be convenient.
				 *
		         * @param levelOfDetail the level of detail of this message 
				 * (level 1 by default).
		         *
				 * @note This method cannot be const since it would do so 
				 * with Ceylan::Object's one, which has to be non-const 
				 * because of its possible need to forge an identifier and
				 * mutable is not used.
				 *
				 * @see send with implied internal channel
				 * 
		         */		
				virtual void send( 
					const std::string & message,
					LevelOfDetail levelOfDetail 
						= DefaultLevelOfDetailForMessage 
					) throw( LogException ) ; 
		
		
		        /**
		         * Sends <b>message</b> to the specified channel, through
				 * known LogTransport.
		         *
				 * @note This method is to be used when a message is to be sent
				 * to a channel different from the LogSource's internal one.
				 *
				 * @param channel the channel name which will identify the
				 * targeted Loglistener.
				 *
		         * @param message the log message to send. Please avoid
				 * characters '<' and '>' since they have a special meaning 
				 * for HTML log output. These characters used to be filtered
				 * in HTML aggregators but it prevented messages to contain
				 * HTML tags on purpose, which proved to be convenient.
				 *
		         * @param levelOfDetail the level of detail of this message 
				 * (level 1 by default).
		         *
				 * @see send with implied internal channel
				 * 
		         */		
				virtual void sendToChannel( 
					const std::string & channel,
					const std::string & message,
					LevelOfDetail levelOfDetail = DefaultLevelOfDetailForMessage
					) const throw( LogException ) ; 
			
				
				
				
				/// Sets a new Log transport for this LogSource.
				virtual void setTransport( LogTransport & newTransport ) 
					throw() ;
				
				
				/// Returns this LogSource's Log transport.
				virtual LogTransport * getTransport() const throw() ;
				
				
				/// Tells whether this LogSource has a registered Log transport.
				virtual bool hasTransport() const throw() ;


			
				/**
				 * Returns a user-friendly description of the state of this
				 * object.
				 *
				 * @param level the requested verbosity level.
				 *
				 * @note Text output format is determined from overall settings.
				 *
				 * @see TextDisplayable
				 *
				 */
				virtual const std::string toString( 
						Ceylan::VerbosityLevels level = Ceylan::high ) 
					const throw() ;

								
				/// The default level of detail of a Log source.
 				static const LevelOfDetail DefaultLevelOfDetailForSource = 10  ;
			
			
			
			
			protected:
			
			
				/**
				 * Internal method to send messages. Filters them out if 
				 * their level of detail is higher than the one of this log
				 * source.
				 *
				 */
				virtual void directSend( const std::string & channel, 
					const std::string & message,
					LevelOfDetail levelOfDetail 
						= DefaultLevelOfDetailForMessage ) 
					const throw( LogException ) ; 
				
				
				/// Stores this LogSource channel name.
				std::string _channelName ;
				
				
				/// The current level of detail of interest for this Log source.
				LevelOfDetail _level ;
				
				
				/**
				 * Suppresses the link between this LogSource and its Log
				 * transport.
				 *
				 */
				virtual void unlinkTransport() throw() ;

			
				/// The LogTransport to be used for sending messages.
				LogTransport * _transport ;



			private:
			
			
				/**
				 * Copy constructor made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 * 
				 */			 
				LogSource( const LogSource & source ) throw() ;
			
			
				/**
				 * Assignment operator made private to ensure that it 
				 * will be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 * 
				 */			 
				LogSource & operator = ( const LogSource & source ) throw() ;
				
	
		} ;
	
	
	}


}


#endif // CEYLAN_LOG_SOURCE_H_

