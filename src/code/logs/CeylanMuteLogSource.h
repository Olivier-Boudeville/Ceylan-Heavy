#ifndef CEYLAN_MUTE_LOG_SOURCE_H_
#define CEYLAN_MUTE_LOG_SOURCE_H_


#include "CeylanLogSource.h"               // for inheritance and al

#include <string>



namespace Ceylan
{


	namespace Log
	{
	
	
		
	
		/**
		 * This is a specific child class of LogSource, whose role is to
		 * ignore log messages, in the context of the use of the null log
		 * plug.
		 *
		 * @see LogPlugNull, LogTransport
		 *
		 */
		class CEYLAN_DLL MuteLogSource : public LogSource
		{
		
			public:
			
			
				/**
				 * Constructs a MuteLogSource.
				 *
				 */
				explicit MuteLogSource() throw() ;
			
				
				
				/// Basic virtual destructor.
				virtual ~MuteLogSource() throw() ;
		


		        /**
		         * Sends <b>message</b> to this MuteLogSource's internal
				 * channel,knowing it will be ignored.
		         *
		         * @param message the log message to send, which will be 
				 * ignored here.
				 *
		         * @param levelOfDetail the level of detail of this message,
				 * which will be ignored here.
				 * 
		         */		
				virtual void send( 
					const std::string & message,
					LevelOfDetail levelOfDetail 
						= DefaultLevelOfDetailForMessage 
					) throw( LogException ) ; 
		
			
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

								


			private:
			
			
				/**
				 * Copy constructor made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 * 
				 */			 
				MuteLogSource( const MuteLogSource & source ) throw() ;
			
			
				/**
				 * Assignment operator made private to ensure that it 
				 * will be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 * 
				 */			 
				MuteLogSource & operator = ( const MuteLogSource & source )
					throw() ;
				
	
		} ;
	
	
	}


}


#endif // CEYLAN_MUTE_LOG_SOURCE_H_

