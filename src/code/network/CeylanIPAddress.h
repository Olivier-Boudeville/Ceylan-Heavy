#ifndef CEYLAN_IP_ADDRESS_H_
#define CEYLAN_IP_ADDRESS_H_


#include "CeylanNetwork.h"          // for inheritance, NetworkAddressType
#include "CeylanTextDisplayable.h"  // for inheritance


#include <string>



namespace Ceylan
{
	

	namespace Network
	{


		/// Exception dedicated to the IP addressing.
		class IPAddressException : public NetworkException
		{
		
			public:
			
			
				explicit IPAddressException( const std::string message ) :
					NetworkException( message )
				{
				
				}	
			
		} ;
		


		/**
		 * Handles abstract IP (Internet Protocol) addresses.
		 *
		 * @todo Add informations from netinet/in.h (ex : IN_CLASSA).
		 *
		 */		
		class IPAddress : public TextDisplayable
		{

			public:
			
		
				/// Basic constructor.
				IPAddress() throw() ;
			
			
				/// Basic virtual destructor.
				virtual ~IPAddress() throw() ;
			
			
				/**
				 * Returns the actual type of the IP address, for example :
				 * IPv4.
				 *
				 */
				virtual NetworkAddressType getType() const throw() = 0 ;
				
				
	            /**
	             * Returns a user-friendly description of the state of 
				 * this object.
	             *
				 * @param level the requested verbosity level. For level 
				 * 'Ceylan::low', the exact standard notation for IP address
				 * will be returned, depending on the actual address type.
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
				
					
					
				/**
				 * Tells whether the specified string is a valid IP address.	
				 *
				 * @note For abstract IPAddress, always returns false.
				 *
				 */
				static bool IsValid( const std::string IPString ) throw() ;
			
			
			
			protected:
			
			
				/**
				 * Checks that internal IP is valid, raises a 
				 * NetworkException otherwise.
				 *
				 */
				virtual void validate() const throw( IPAddressException ) = 0 ;




			private:
				
			
				/**
				 * Copy constructor made private to ensure that it will 
				 * never be called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */			 
				IPAddress( const IPAddress & source ) throw() ;
			
			
				/**
				 * Assignment operator made private to ensure that it 
				 * will never be called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 * 
				 */			 
				IPAddress & operator = ( const IPAddress & source ) throw() ;
			
				
		} ;
		

	}
	
	
}

#endif // CEYLAN_IP_ADDRESS_H_
