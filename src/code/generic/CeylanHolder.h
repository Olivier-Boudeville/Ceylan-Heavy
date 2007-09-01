#ifndef CEYLAN_HOLDER_H_
#define CEYLAN_HOLDER_H_




namespace Ceylan
{
	
	
	
	/**
	 * An Holder instance drives the life-cycle of the object it holds.
	 * They allow to make objects obtained through pointers as if they were
	 * automatic variables.
	 *
	 * To call a method 'hello' on the held object, either use the holder 
	 * 'myHolder' as if it was a pointer to the held object: 
	 * 'myHolder->hello(...)' or use 'myHolder.get().hello()', at your
	 * convenience.
	 * 
	 * @note Somewhat mimics the behaviour of a shared pointer (with the
	 * '->' operator instead of the '.' one, which cannot be be overriden.
	 *
	 * @see Ceylan::CeylanCountedPointer for a far more complex shared pointer
	 * template.
	 *
	 * @example: 
	 * Holder<File> myFileHolder( File::Create(...) ) ;
	 * myFileHolder->lockForReading() ;
	 * As soon as the automatic variable myFileHolder goes out of scope 
	 * (ex: return or throw statement), it will be deleted and its held
	 * content (here, a File instance ) will be deleted too.
	 * Thus the file reference can be manipulated here as easily as if it was an
	 * automatic variable, thanks to the Holder template.
	 *
	 * @note No CEYLAN_DLL declaration for templates.
	 *
	 */
	template <typename Held>
	class Holder
	{
	
	
		public:
		
			
			/**
			 * Creates an Holder instance holding specified reference.
			 *
			 * @param heldReference the reference whose life-cycle will be
			 * set to match the one of this holder instance.
			 *
			 */
			explicit Holder( Held & heldReference ) throw() ;
  
 
 			/**
			 * Non-virtual destructor, deletes its held object.
			 *
			 */
 			void ~Holder() throw() ;
	
					
			/**
			 * Returns the held reference.
			 *
			 */
			Held & get() throw();
			
			
			/// Returns a reference to the held instance.
			Held & operator*() const ;

			/// Returns a pointer to the held instance.
			Held * operator->() const ;
		
		
		private:
		
		
			/// The held instance.
			Held * _held ;
			
		
			/**
			 * Copy constructor made private to ensure that it will never be 
			 * called.
			 * The compiler should complain whenever this undefined 
			 * constructor is called, implicitly or not.
			 * 
			 */			 
			Holder( const Holder & source ) throw() ;
			
			
			/**
			 * Assignment operator made private to ensure that it will be
			 * never called.
			 * The compiler should complain whenever this undefined operator
			 * is called, implicitly or not.
			 * 
			 */			 
			Holder & operator = ( const Holder & source ) throw() ;
			
			
	} ;



	/// Public section : implementation.


	template <typename Held>
	Holder<Held>::Holder( Held & heldReference ) throw() :
		_held( & heldReference )
	{
	
	}	

				
	template <typename Held>
	Holder<Held>::~Holder() throw()
	{

		delete _held ;
		
	}	
				
				
	template <typename Held>
	Held & Holder<Held>::get() throw()
	{

		return *_held ;
		
	}	
				

	Held & operator*() const
	{
		return get() ;
	}


	Held * operator->() const
	{
		return & get() ;
	}
	
	
}


#endif // CEYLAN_HOLDER_H_

