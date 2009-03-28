#ifndef CEYLAN_GENERIC_MVC_DEFINES_H_
#define CEYLAN_GENERIC_MVC_DEFINES_H_


#include "CeylanException.h"  // for inheritance


#include <string>



/*
 * @note In most cases, this generic (template-based) MVC version should be
 * preferred to the event-based MVC one (ex: based on Ceylan::MVC::Model).
 *
 */


/*
 * With the MVC framework, we have Controller -> Model -> View, with '->'
 * meaning "depending on".
 *
 * Therefore a model, in terms of declaration, could depend (only) on its 
 * views. 
 *
 * However, in terms of life-cycle management, having a model know all its
 * MVC-related components (views and controllers) is more convenient, as 
 * when the model determines it should be deallocated, it can manage
 * automatically its related objects (i.e. deallocate its views and 
 * controllers).
 *
 * All references detained by a MVC instance to other instances should be
 * 'const': if A -> B, then A will not post updates to B, on the contrary B
 * will pick information from A, thus A may reference B just for life-cycle
 * reasons ('const' will suffice), and B will only call 'const' methods of A
 * to get the informations it needs (hence 'const' should suffice again).
 *
 */
 
  
/*
 * Note that, at this level of abstraction, a MVC class does not have to know 
 * the specific class(es) it depends on (ex: a SingleViewGenericModel just
 * need a BaseView pointer, regardless of the actual view class).
 *
 * However, the point is that, as soon as these mother classes are subclassed, 
 * these actual subclasses will need to know the specific class they 
 * depend on, as they intend to call their class-specific methods.
 * Hence for example a SingleViewGenericModel would need to rely on a 
 * referencee to MySpecificView, rather than on a mere BaseView.
 *
 * Therefore, as soon as a MVC class can have a reference on another, we
 * indeed need to define a templated version of it, so that we keep the
 * actual type (class) of referenced instances.
 *
 */



// Allows to avoid complex header dependencies.


namespace Ceylan
{


	namespace MVC
	{
	
	
		class GenericMVCException : public Ceylan::Exception
		{
	
			public:
	
	
				GenericMVCException( const std::string & message ) throw() :
					Ceylan::Exception( message )
				{
	
				}
	
	
				virtual ~GenericMVCException() throw()
				{
	
				}
	
		} ;
	
	
	}

	
}	


#endif // CEYLAN_GENERIC_MVC_DEFINES_H_

