#ifndef CEYLAN_LOG_MESSAGE_H_
#define CEYLAN_LOG_MESSAGE_H_



#include "CeylanLog.h"              // for LevelOfDetail, Log::Exception
#include "CeylanTextDisplayable.h"  // for inheritance

#include <string>


namespace Ceylan
{


	// So that Log messages are ordered.
	class Timestamp ;


	namespace Log
	{
		

		/**
		 * This class encapsulates the concept of Log message.
		 *
		 * On future versions, a Log message could carry a content,
		 * which could be a string or anything else, thanks to
		 * Ceylan::Log::LogContent.
		 *
		 */
		class CEYLAN_DLL LogMessage : public TextDisplayable
		{
			
			
			public:			
			
			
				/**
				 * Constructor for a Log messsage.
				 *
				 * @param message the log message itself.
				 *
				 * @param channelName the name of the channel this message
				 * belongs to.
				 *
				 * @param levelOfDetail this message's level of detail.
				 *
				 * @param timestamp a timesptamp to record when this
				 * message was emitted. The Log message takes ownership 
				 * of the timestamp.				
				 *
				 */
				LogMessage( const std::string & message, 
					const std::string & channelName,
					LevelOfDetail levelOfDetail, 
					const Timestamp & timestamp ) throw() ;
				
				
				/**
				 * Simpler LogMessage constructor, whose timestamp is
				 * dated from this LogMessage creation.
				 *
				 * @throw UtilsException since timestamp creation may fail.
				 *
				 */
				LogMessage( const std::string & message, 
						const std::string & channelName,
						LevelOfDetail levelOfDetail 
							= MaximumLevelOfDetailForMessage) 
					throw( LogException ) ;
					
					
				/// Basic virtual destructor.
				virtual ~LogMessage() throw() ;	


				/// Returns this message's actual content.
				virtual const std::string getContent() const throw() ;


				/// Returns this message's channel.
				virtual const std::string getChannelName() const throw() ;
				
				
				/**
				 * Sets this message's channel name to 
				 * <b>newChannelName</b>.
				 *
				 */
				virtual void setChannelName( 
					const std::string & newChannelName ) throw() ;
				
				
				/// Returns this message's level of detail.				
				virtual LevelOfDetail getLevelOfDetail() const throw() ;
				
				
				/**
				 * Returns this message's timestamp, but keep 
				 * ownership of it.
				 *
				 */
				virtual const Timestamp & getTimestamp() 
					const throw( LogException ) ;
				
				
				/**
				 * Returns a user-friendly pre-formatted text of this
				 * log message.
				 *
				 */
				virtual const std::string getPreformattedText() 
					const throw() ;
				
				
				/**
				 * Returns a user-friendly description of the state
				 * of this object.
				 *
				 * @see TextDisplayable, Displayable
				 * @see Ceylan::VerbosityLevels
				 *
				 */
				virtual const std::string toString( 
						Ceylan::VerbosityLevels level = Ceylan::high )
					const throw() ;
		
			
			
			protected:
			
			
				/// The Log message itself.
				std::string _message ;
				
				/// The Log message channel name.
				std::string _channelName ;
				
				/// This Log message's level of detail.
				LevelOfDetail _levelOfDetail ;
				
				/// This Log message's timestamp.
				const Timestamp * _timestamp ;


			private:
		
		
				/**
				 * Copy constructor made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 * 
				 */			 
				LogMessage( const LogMessage & source ) throw() ;
			
			
				/**
				 * Assignment operator made private to ensure that it 
				 * will be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * operator is called, implicitly or not.
				 * 
				 */			 
				LogMessage & operator = ( const LogMessage & source ) throw() ;
						
		} ;
		
	}

}


#endif // CEYLAN_LOG_MESSAGE_H_
