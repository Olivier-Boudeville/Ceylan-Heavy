#ifndef CEYLAN_LOG_AGGREGATOR_H_
#define CEYLAN_LOG_AGGREGATOR_H_


#include "CeylanTextDisplayable.h"  // for inheritance
#include "CeylanLog.h"              // for LogException


#include <string>
#include <list>


namespace Ceylan
{

	
	namespace Log
	{
	
	
		// A container for basic Log messages.
		class LogChannel ;
		
		
		/**
		 * A container for Log messages coming from Loggable sources,
		 * Ceylan::Object.
		 *
		 */
		class ObjectChannel ;
		
		
		// What a LogAggregator aggregates.
		class LogMessage ;
		
		
		/**
		 * This abstract class is meant to store messages targeted at a set
		 * of channels which are conveyed thanks to a Log transport, and to
		 * generate a view of it.
		 *
		 * For the moment, Loggable and Object messages are managed as if they 
		 * were equivalent (whereas Object are specialized Loggable).
		 * 
		 * @note Smart aggregators can deal with mangled Object class 
		 * names, and auto-correct these names if they receive at least
		 * one non-mangled message. 
		 *
		 * As, apparently, from constructors <b>and</b> destructors, 
		 * class names, with gcc, are mangled, objects that did not 
		 * send a message from elsewhere remain mangled.
		 *
		 * @see LogSource, LogListener, LogTransport
		 *
		 */
		class CEYLAN_DLL LogAggregator : public TextDisplayable
		{
		
		
			public:
			
			
				/// Exception raised by aggregators.
				class LogAggregatorException : public LogException
				{
					public:
					
						explicit LogAggregatorException( 
							const std::string & message ) throw() ;	
							
						virtual ~LogAggregatorException() throw() ;
						
				} ;
				
			
			
				/**
				 * Constructs a LogAggregator, whose role is to transform
				 * log messages into browsable files, according to various
				 * encodings (raw, HTML, etc.).
				 *
				 * @param useGlobalLevelOfDetail tells whether log 
				 * channels levels of detail are to be overriden by 
				 * the aggregator-wide one.
				 *
				 * @param beSmart tells whether this aggregator should 
				 * be smart, i.e. should detect log messages whose class 
				 * name is mangled, and correct that so that they are 
				 * stored in the right channel. Note that instances sending
				 * logs from their destructor might prevent the smart 
				 * aggregator from overcoming the mangling issue.
				 *
				 * @note Smart aggregators might spend much more time 
				 * than basic ones when creating a new channel.
				 *
				 */
				explicit LogAggregator( bool useGlobalLevelOfDetail = true, 
					bool beSmart = true ) throw() ;
					
							
				/// Basic virtual destructor.
				virtual ~LogAggregator() throw() ;
		
				
				/**
				 * Creates, if possible, a new basic channel whose 
				 * name is <b>channelName</b>.
				 *
				 */
				virtual LogChannel & createBasicChannel( 
					const std::string & channelName ) throw( LogException ) ;

				
				/**
				 * Creates, if possible, a new object channel 
				 * corresponding to the specified Loggable message.
				 */
				virtual ObjectChannel & createObjectChannel( 
					LogMessage & message ) throw( LogException ) ;


				/**
				 * Aggregates all channel and log messages informations in the 
				 * implementation's fashion.
				 *
				 */
				virtual void aggregate() throw( LogAggregatorException ) = 0 ;
				
					
				/**
				 * Tells whether this aggregator has already a channel named
				 * <b>channelName</b>.
				 *
				 * @throw LogException if an inconsistency is detected 
				 * in channel list.
				 *
				 */
				virtual bool hasChannel( const std::string & channelName ) 
					const throw( LogException ) ;
					
					
				/**
				 * Finds, if any, a channel in LogAggregator's list of 
				 * channels whose name is <b>channelName</b>.
				 *
				 * @note if the specified channel name is an object 
				 * channel name (typically starting by 'loggable://'), 
				 * search will be only performed on object channels.
				 *
				 */
				virtual LogChannel & findChannel( 
						const std::string & channelName ) 
					const throw( LogException ) ;
				 
				 
				/**
				 * Transfers all the messages of <b>source</b> into
				 * <b>target</b>, updates their channel identifier so
				 * that it matches the one of <b>target</b>.
				 *
				 * This method helps correcting wrong channel names 
				 * due to mangled class names.
				 *
				 * @note As the ownership of transferred message is
				 * transferred to the target channel, the source message
				 * pointers are set to null so that, when source channel
				 * will be deallocated, messages will not be touched.
				 *
				 */
				virtual void transferChannel( LogChannel & source, 
					LogChannel & target ) throw( LogException ) ;
				 
				
				/// Removes completly a log channel. 
				virtual void removeChannel( LogChannel & target ) throw() ;
				
				
		        /**
		         * Classify and stores <b>message</b> internally, 
				 * according to the corresponding channels they contain.
		         *
		         * @param message the log message to be stored, the
				 * aggregator takes ownership of it.
		         *
		         */		
				virtual void store( LogMessage & message ) 
					throw( LogException ) ; 
			
			
	            /**
	             * Returns a user-friendly description of the state of 
				 * this object.
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
					

				/// Default value for the aggregator-wide level of detail.
				static const LevelOfDetail DefaultGlobalLevelOfDetail ;
				
				 
			
			protected:


				/**
				 * Finds, if any, a Log channel in LogAggregator's list of
				 * channels whose name is <b>channelName</b>.
				 *
				 * @note this method is dedicated to find basic log 
				 * channels, not object ones.
				 *
				 */
				virtual LogChannel & findBasicChannel( 
						const std::string & basicChannelName ) 
					const throw( LogException ) ;
			
			
				/**
				 * Finds, if any, an Object channel in LogAggregator's
				 * list of object channels whose short name is
				 * <b>channelName</b> (no protocol prefix or separator
				 * allowed).
				 *
				 * @note this method is dedicated to find object log 
				 * channels, not basic ones.
				 *
				 */
				virtual ObjectChannel & findObjectChannel( 
						const std::string & nonPrefixedChannelName ) 
					const throw( LogException ) ;
			
			
				/**
				 * Creates a basic (non protocol-prefixed) channel from
				 * specified message, which is automatically added to this
				 * new channel.
				 *
				 */
				virtual void createBasicChannelFrom( LogMessage & message )
					throw( LogException ) ;
				
				
				/**
				 * Creates a Loggable channel from specified message.
				 *
				 * @param message the incoming Loggable message which 
				 * triggers this channel creation.
				 *
				 * @note If the incoming message that would lead to create
				 * a channel A has the same address as past events which
				 * led to the creation of a channel B, then all the 
				 * messages in B will be transferred into a newly created
				 * channel A, no matter A and B. 
				 * If messages are sent from destructors, then the channel
				 * will be finally renamed as the base class, and not the
				 * specialized class being effectively used.
				 * So sending messages from destructors should be avoided.
				 *
				 */
				virtual void createLoggableChannelFrom( LogMessage & message ) 
					throw( LogException ) ;
				
				
		        /**
		         * Classify and stores basic log message 
				 * <b>basicLogMessage</b> internally, according to the
				 * corresponding channel it contains.
		         *
		         * @param message the basic log message to be stored, 
				 * the aggregator takes ownership of it.
		         *
		         */		
				virtual void storeBasicMessage( LogMessage & basicLogMessage ) 
					throw( LogException ) ; 
			
				
		        /**
		         * Classify and stores object log message
				 * <b>objectLogMessage</b> internally, 
				 * according to the corresponding channel it contains.
		         *
		         * @param objectLogMessage the object log message to 
				 * be stored, the aggregator takes ownership of it.
		         *
		         */		
				virtual void storeObjectMessage( 
					LogMessage & objectLogMessage ) throw( LogException ) ; 
			
			
				/**
				 * Corrects, if possible, any mangled class name in 
				 * specified message.
				 *
				 */
				virtual void demangle( LogMessage & objectLogMessage ) throw() ;


				
				/**
				 * Returns the most appropriate level of detail for 
				 * log channel output, depending only on the state of 
				 * the aggregator.
				 *
				 * Basically, maps a global or local level of detail 
				 * to a verbosity level.
				 *
				 */
				 virtual Ceylan::VerbosityLevels 
				 	getOverallVerbosityLevel() const throw() ;
				 
				 
				 /**
				  * Returns the most appropriate level of detail for 
				  * log message output, depending on the state of the
				  * aggregator and on the message's level of detail.
				  *
				  * Basically, maps a global or local level of detail 
				  * to a verbosity level.
			  	  *
				  */
				 virtual Ceylan::VerbosityLevels getMessageVerbosityLevel( 
				 		const LogMessage & message ) 
				 	const throw() ;			 
				
				
				
				/**
				 * Converts a level of detail of a log listener into
				 * a corresponding verbosity level. 
				 *
				 */
				static Ceylan::VerbosityLevels
					ConvertListenerLevelOfDetailToVerbosityLevel( 
						LevelOfDetail level ) throw() ; 
				
				
				/**
				 * Converts a level of detail of a log message into
				 * a corresponding verbosity level. 
				 *
				 */
				static Ceylan::VerbosityLevels
					ConvertMessageLevelOfDetailToVerbosityLevel( 
						LevelOfDetail level ) throw() ; 
				
				
#pragma warning( push )
#pragma warning( disable : 4251 )

				/// List of all known Log channels.
				std::list<LogChannel *> _channelList ;

#pragma warning( pop ) 
				
				
				/**
				 * Tells whether this aggregator should cope with mangled 
				 * class names.
				 *
				 */
				bool _beSmart ;
		
		
				/**
				 * Tells whether a global (aggregator-wide) level of 
				 * detail for channels should be used.
				 *
				 * @note if set, will override log channels levels of details.
				 *
				 */
				bool _useGlobalLevelOfDetail ; 
				
				
				/**
				 * Defines the aggregator-wide level of detail, used if
				 * _useGlobalLevelOfDetail is set.
				 *
				 */
				LevelOfDetail _globalLevelOfDetail ;

		
		
		private:
		
		
				/**
				 * Copy constructor made private to ensure that it will be
				 * never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 * 
				 */			 
				LogAggregator( const LogAggregator & source ) throw() ;
			
			
				/**
				 * Assignment operator made private to ensure that it 
				 * will be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * operator is called, implicitly or not.
				 * 
				 */			 
				LogAggregator & operator = ( const LogAggregator & source )
					throw() ;
				
				
				
		} ;
	
	
	}


}


#endif // CEYLAN_LOG_AGGREGATOR_H_
