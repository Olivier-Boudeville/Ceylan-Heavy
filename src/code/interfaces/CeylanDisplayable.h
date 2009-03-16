#ifndef CEYLAN_DISPLAYABLE_H_
#define CEYLAN_DISPLAYABLE_H_


namespace Ceylan
{



	/// Verbosity levels, used for instance by toString methods.
	enum VerbosityLevels { low, medium, high } ;



    /**
     * Interface that every object which can be displayed should implement. 
     *
     * Most objects implement that interface. 
	 *
	 * Most of the time, they implement it indirectly, i.e. because they
	 * inherited it.
	 *
	 * @note Copy constructor and assignment operator cannot be private,
	 * because some Displayable instances need to rely on their copy
	 * constructor, for example for operators returning them by value.
	 * 
     */
    class CEYLAN_DLL Displayable
    {

        public:


            /// Void interface, just used to ensure strong typing.


			/// Do-nothing constructor.
			Displayable() throw()
			{
			
			}
			
			
			/// Do-nothing virtual destructor.
			virtual ~Displayable() throw()
			{
			
			}
			
			
			

		private:
		
		
			/**
			 * Copy constructor cannot be private since it is needed in 
			 * some cases (return by value).
			 *
			 * Displayable( const Displayable & source ) throw() ;
			 *
			 */			 
			
			
			/**
			 * Assignment operator should not be private since it is useful
			 * in some cases.
			 * 
			 * Displayable & operator = ( const Displayable & source ) throw() ;
	    	 *
			 */
			 
    } ;

}


#endif // CEYLAN_DISPLAYABLE_H_

