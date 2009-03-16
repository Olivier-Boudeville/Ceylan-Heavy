#include "CeylanGenericModel.h"

#include "CeylanGenericView.h"
#include "CeylanOperators.h"

using namespace Ceylan ;

using std::string ;


// BaseModel section.


BaseModel::BaseModel() throw()
{

}



BaseModel::~BaseModel() throw()
{

}


const string BaseModel::toString( Ceylan::VerbosityLevels level ) const throw()
{

	return "Base model" ;
	
}





// SingleViewGenericModel section.


SingleViewGenericModel::SingleViewGenericModel( const BaseView & view ) :
	_view( & view )
{

}

SingleViewGenericModel::SingleViewGenericModel() :
	_view( 0 )
{

}



SingleViewGenericModel::~SingleViewGenericModel() throw()
{

	if ( _view != 0 )
		delete _view ;
	
}



void SingleViewGenericModel::addView( const BaseView & view ) 
	throw( GenericMVCException )
{

	if ( _view != 0 )
		throw GenericMVCException( "SingleViewGenericModel::addView failed: "
			"a view was already registered." ) ;
	
	_view = & view ;
	
}
	


const string SingleViewGenericModel::toString( Ceylan::VerbosityLevels level )
	const throw()
{

	if ( _view != 0 )
	{
	
		// Beware to infinite recursion:
		if ( level == Ceylan::low )
			return "Single-view generic model owning a view" ;
		else	
			return "Single-view generic model owning following view: " 
				+ Ceylan::toString( _view ) ;
			
	}
	else
	{
	
		return "Single-view generic model with no view registered." ;
	
	}		
	
}

