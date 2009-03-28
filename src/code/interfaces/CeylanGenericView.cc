#include "CeylanGenericView.h"

using namespace Ceylan ;
using namespace Ceylan::MVC ;


using std::string ;


/* 
 * In this file following classes are implemented:
 *
 *   - BaseView
 */



// BaseView section.


BaseView::BaseView() throw()
{

}



BaseView::~BaseView() throw()
{

}


const string BaseView::toString( Ceylan::VerbosityLevels level ) const throw()
{

	return "Base view" ;
	
}

