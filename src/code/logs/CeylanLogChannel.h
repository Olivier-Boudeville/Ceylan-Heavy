#ifndef CEYLAN_LOG_CHANNEL_H_
#define CEYLAN_LOG_CHANNEL_H_


#include "CeylanLog.h"             // for LogException
#include "CeylanTextDisplayable.h" // for inheritance

#include <string>
#include <list>



namespace Ceylan
{

	namespace Log
	{
	
	
		// Log messages are gathered into these Log channels.
		class LogMessage ;
		
		
		/**
	 	 * Log channels are thematic containers for log messages, 
		 * meant to be used by LogAggregators.
		 *
	 	 * @see LogAggregator
	 	 *
	 	 */
		class LogChannel : public TextDisplayable
		{
	
	
			/**
			 * Aggregators must able to access to messages of log
			 * channels.
			 *
			 */
			friend class LogAggregator ;
			
			
			/**
			 * Necessary since friend relationships are not inherited
			 * apparently.
			 *
			 */
			friend class LogAggregatorHTML ;
			
			
			public:
			
			
				/**
		 	 	 * Creates a named Log channel.
		 	 	 * 
		 		 */
				explicit LogChannel( const std::string & name ) throw() ;
			
	
				/// Basic virtual destructor.
				virtual ~LogChannel() throw() ;


				/**
				 * Add a new Log message to this channel.
				 *
				 * @note this method takes ownership of the specified 
				 * log message.
				 *
				 * @param message the log message to record
				 *
				 * @param check if true, raises a LogException if this
				 * added message's channel does not match this channel.
				 *
				 */
				virtual void addMessage( LogMessage & message, 
					bool check = true ) throw( LogException ) ;
				
				
				/**
				 * Returns this channel's name.
				 *
				 */
				virtual const std::string getName() const throw() ;
				
					
				/**
				 * Returns the number of messages this channel 
				 * currently gathered.
				 *
				 */
				virtual unsigned int getMessageCount() const throw() ;
					
											
	            /**
	             * Returns a user-friendly description of the state of 
				 * this object.
	             *
				 * @param level the requested verbosity level.
				 *
				 * @note Text output format is determined from 
				 * overall settings.
				 *
				 * @see TextDisplayable
				 *
	             */
				virtual const std::string toString( 
						Ceylan::VerbosityLevels level = Ceylan::high )
					const throw() ;
				
								
				
			protected:	


				/// This channel's name.
				std::string _name ;
								
								
				/**
				 * Chronologically-ordered set of messages for this
				 * Log channel.
				 *
				 */
				std::list<LogMessage *> _messages ;



			private:
			
			
				/**
				 * Copy constructor made private to ensure that it 
				 * will be never called.
				 * The compiler should complain whenever this 
				 * undefined constructor is called, implicitly or not.
				 *
				 */			 
				LogChannel( const LogChannel & source ) throw() ;
			
			
				/**
				 * Assignment operator made private to ensure that
				 * it will be never called.
				 *
				 * The compiler should complain whenever this 
				 * undefined operator is called, implicitly or not.
				 *
				 */			 
				LogChannel & operator = ( const LogChannel & source ) throw() ;
				
				
		} ;

	}

}

#endif // CEYLAN_LOG_CHANNEL_H_
