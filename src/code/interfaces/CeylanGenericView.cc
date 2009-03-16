#include "CeylanGenericView.h"

using namespace Ceylan ;

using std::string ;


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

