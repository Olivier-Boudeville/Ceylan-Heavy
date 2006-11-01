#ifndef CEYLAN_STREAM_SOCKET_H_
#define CEYLAN_STREAM_SOCKET_H_


#include "CeylanSocket.h"       // for inheritance

#include <string>



namespace Ceylan
{


	namespace Network
	{


		
		/**
		 * Simple connection-based socket I/O class.
		 *
		 * These are Internet sockets, not the ones used for UNIX IPC
		 * (interprocess communication) family.
		 *
		 * Designed to be subclassed.
		 *
		 * @see StreamClient
		 * @see StreamServer
		 *
		 */
		class CEYLAN_DLL StreamSocket: public Socket 
		{

			
			
			public:
		
				
				/// Mother class for all stream socket-related exceptions.
				class CEYLAN_DLL StreamSocketException: 
					public SocketException
				{ 
					public: 
					
						explicit StreamSocketException( 
							const std::string & reason ) throw() ;
						
						virtual ~StreamSocketException() throw() ; 
							
				} ;
		
		
				/**
				 * Client-side constructor for connection-based sockets, used
				 * also for anonymous stream sockets.
				 *
				 * @param blocking tells whether this socket should be
				 * created in blocking mode (the default) or in non-blocking
				 * mode.
				 *
				 * @param sacrificeThroughputToPacketTiming tells whether
				 * the snappiest possible response (packet timing) should be
				 * searched for (if true), even though it would be obtained at 
				 * the expense of usable network bandwidth. This would be done
				 * by deactivating the Nagle algorithm, which is seldom
				 * recommended except for remote GUI or multiplayer network games.
				 * Hence the default is false, and the Nagle algorithm is used.
				 *
				 * @see http://tangentsoft.net/wskfaq/intermediate.html#nagle-desc
				 *
				 * @throw SocketException if the operation failed.
				 *
				 */
				explicit StreamSocket( bool blocking = true, 
						bool sacrificeThroughputToPacketTiming = false ) 
					throw( SocketException ) ; 
		

				/**
				 * Server-side constructor for connection-based sockets.
				 *
				 * @param blocking tells whether this socket should be
				 * created in blocking mode (the default) or in non-blocking
				 * mode.
				 *
				 * @param sacrificeThroughputToPacketTiming tells whether
				 * the snappiest possible response (packet timing) should be
				 * searched for (if true), even though it would be obtained at 
				 * the expense of usable network bandwidth. This would be done
				 * by deactivating the Nagle algorithm, which is seldom
				 * recommended except for remote GUI or multiplayer network games.
				 * Hence the default is false, and the Nagle algorithm is used.
				 *
				 * @see http://tangentsoft.net/wskfaq/intermediate.html#nagle-desc
				 *
				 * @throw SocketException if the operation failed.
				 *
				 */
				StreamSocket( Port localPort, bool blocking = true,
						bool sacrificeThroughputToPacketTiming = false ) 
					throw( SocketException ) ;
		

				/// Virtual destructor.
				virtual ~StreamSocket() throw() ;
				
				
				/**
				 * Sets the blocking mode of this stream socket.
				 *
				 * @param newStatus if true, sets the socket in blocking mode,
				 * if false set to non-blocking mode. If the socket is 
				 * already in the target state, nothing is done.
				 *
				 * @throw NonBlockingNotSupportedException if the operation
				 * failed.
				 *
				 */
				virtual void setBlocking( bool newStatus )
					throw( NonBlockingNotSupportedException ) ;
	
		
            	/**
            	 * Returns an user-friendly description of the state of
				 * this object.
            	 *
				 * @param level the requested verbosity level.
				 *
				 * @note Text output format is determined from overall 
				 * settings.
				 *
				 * @see TextDisplayable
				 *
				 */
            	virtual const std::string toString( 
					Ceylan::VerbosityLevels level = Ceylan::high ) 
						const throw() ;
			
				
				
			protected:

				
				/** 
				 * Creates the stream (TCP) socket associated with the port
				 * <b>port</b>.
				 *
				 * @throw SocketException if the operation failed.
				 *
				 */
				virtual void createSocket( Port port ) 
					throw( SocketException )  ;
					

				/**
				 * Enables or disables the Nagle algorithm for this
				 * socket.
				 *
				 * @param activated if true, activates the algorithm,
				 * if false, deactivates it.
				 *
				 * By default the Nagle algorithm is activated, as it
				 * should be used on most situations.
				 *
				 */
				virtual void setNagleAlgorithmTo( bool activated ) 
					throw( StreamSocketException ) ;


				/**
				 * Tells whether the Nagle algorithm is requested to be
				 * deactivated, so that the snappiest possible response 
				 * (packet timing) is searched for (if true), even though 
				 * it would be obtained at the expense of usable network 
				 * bandwidth. 
				 *
				 * This is seldom recommended except for remote GUI or multiplayer
				 * network games.
				 *
				 * Hence the default is false, and the Nagle algorithm is used.
				 *
				 * @see http://tangentsoft.net/wskfaq/intermediate.html#nagle-desc
				 *
				 * Deactivating the Nagle algorithm is to be performed in socket
				 * child classes.
				 *
				 */
				bool _nagleAlgorithmDeactivated ;
				
		
			private:

				
				/**
				 * Copy constructor made private to ensure that it will 
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				StreamSocket( const StreamSocket & source ) throw() ;


				/**
				 * Assignment operator made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 *
				 */
				StreamSocket & operator = ( const StreamSocket & source )
					throw() ;

		} ;

	}
	
}	


#endif // CEYLAN_STREAM_SOCKET_H_
