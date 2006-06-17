#ifndef CEYLAN_INPUT_STREAM_H_
#define CEYLAN_INPUT_STREAM_H_


#include "CeylanSystem.h"     // for IOException
#include "CeylanStream.h"     // for inheritance


#include <list>
#include <string>



namespace Ceylan 
{


	namespace System 
	{
	
	
		/**
 		 * Abstract input stream class used in IPC.
 		 *
 		 * @see Socket, Pipe, File, AnonymousInputStream.
 		 *
		 * @note Select operations only available if the file descriptor
		 * feature is itself available.
		 *
		 * @note The virtual inheritance has been set for classes such as 
		 * System::File which are both input and output streams : had the 
		 * Stream class a data member, it would not be duplicated in a 
		 * System::File instance (it would be allocated only once).
		 *
 		 */
		class InputStream : public virtual Stream
		{


			public:
	
	
				/// Exception thrown when select() fails.
				class SelectFailedException : public IOException
				{
					public: 
						explicit SelectFailedException( 
								const std::string & message ) throw() : 
							IOException( message )
						{}
				} ;
	
	
				/// Basic constructor for InputStream, created not selected.
				InputStream() throw() ;
		
		
				/// Basic virtual destructor.
				virtual ~InputStream() throw() ;
		
		
				/// Tells if the stream has data to read.
				inline bool isSelected() const throw() ;
		
				/// Returns the stream's unique ID.
				virtual StreamID getInputStreamID() const = 0 ;



				// Static section.

				
				/**
				 * Blocks the calling thread until bytes become available
				 * on one or more streams in <b>is</b>.
				 *
				 * To see which are selected, use the <code>isSelected</code>
				 * method.
				 *
				 * @return the number of selected streams.
				 *
				 * @see the non-blocking version, Test.
				 *
				 * @throw SelectFailedException if the operation failed, for 
				 * example if the file descriptor feature is not available
				 * on this platform.
				 * 				 
				 */
				static Ceylan::Uint16 Select( std::list<InputStream*> & is ) 
					throw ( SelectFailedException ) ;
		
		
				/**
				 * Checks whether bytes become available on one or more 
				 * streams in <b>is</b>.
				 *
				 * To see which are selected, use the <code>isSelected</code>
				 * method.
				 *
				 * This method returns always immediatly. 
				 *
				 * @return the number of selected streams.
				 *
				 * @see the blocking version, Select. 
				 *
				 * @throw SelectFailedException if the operation failed, for 
				 * example if the file descriptor feature is not available
				 * on this platform.
				 *
				 */
				static Ceylan::Uint16 Test( std::list<InputStream*> & is )
					throw( SelectFailedException ) ;

	
	
			protected:
	
	
				/// Used to set the selection status.
				virtual void setSelected( bool newStatus ) throw() ;
		
	
			private:


				/**
				 * Copy constructor made private to ensure that it will 
				 * be never called.
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 * 
				 */			 
				InputStream( const InputStream & source ) throw() ;
			
			
				/**
				 * Assignment operator made private to ensure that it will 
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 * 
				 */			 
				InputStream & operator = ( const InputStream & source ) 
					throw() ;
	
	
				/// Stores the selected status.
				bool _isSelected ;
	
		} ;
		
		
		
		bool InputStream::isSelected() const throw()
		{
			return _isSelected ;
		}
		
			
	}
	
}


	

#endif // CEYLAN_INPUT_STREAM_H_
