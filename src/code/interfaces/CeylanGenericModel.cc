#include "CeylanGenericModel.h"

#include "CeylanGenericView.h"
#include "CeylanOperators.h"


using namespace Ceylan ;
using namespace Ceylan::MVC ;


using std::string ;



/* 
 * In this file following classes are implemented:
 *
 *   - BaseModel
 *   - NoViewModel
 *   - SingleViewModel
 *   - MultipleViewModel
 *
 */




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





// NoViewModel section.


NoViewModel::NoViewModel() throw()
{

}



NoViewModel::~NoViewModel() throw()
{

}



void NoViewModel::addView( const BaseView & view ) const
	throw( GenericMVCException )
{

	throw GenericMVCException( "NoViewModel::addView failed: "
		"no view supported by this model." ) ;
		
}


	
const string NoViewModel::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

	return "View-less model" ;
	
}





// Non-templated SingleViewModel section.



SingleViewModel::SingleViewModel( const BaseView & view ):
	_view( & view )
{

}



SingleViewModel::SingleViewModel():
	_view( 0 )
{

}



SingleViewModel::~SingleViewModel() throw()
{

	if ( _view != 0 )
	{
			
		// View owned here:
		delete _view ;
				
	}
		
}



void SingleViewModel::setView( const BaseView & view )
	throw( GenericMVCException )
{

	addView( view ) ;
	
}


	
void SingleViewModel::addView( const BaseView & view ) const 
	throw( GenericMVCException )
{

	if ( _view != 0 )
		throw GenericMVCException( "SingleViewModel::addView failed: "
			"a view was already registered." ) ;
			
	// Do as if this instance had not to be 'const':
	const_cast<SingleViewModel *>(this)->_view = & view ;
		
}


	
const string SingleViewModel::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

	if ( _view != 0 )
	{
	
		/*
		 * Beware to infinite recursion:
		 * (the view may display in turn this model)
		 *
		 */

		if ( level == Ceylan::low )
			return "Single-view controller-less model owning a view" ;
		else
			return "Single-view controller-less model owning " 
				+ _view->toString( level ) ;

	}		
	else
	{
	
		return "Single-view controller-less model "
			"not associated to any view" ;
	
	}
	
}





// Non-templated MultipleViewModel section.



MultipleViewModel::MultipleViewModel( const BaseView & view )
{

	_views.push_back( & view ) ;	
	
}



MultipleViewModel::MultipleViewModel( 
	const std::list<const BaseView *> & views )
{

	_views = views ;	
	
}



MultipleViewModel::MultipleViewModel()
{

}



MultipleViewModel::~MultipleViewModel() throw()
{

	deleteViews() ;
		
}



void MultipleViewModel::setViews( 
	const std::list<const BaseView *> & views ) throw( GenericMVCException )
{

	if ( ! _views.empty() )
		throw GenericMVCException( "MultipleViewModel::setViews failed: "
			"there was at least one view registered." ) ;
					
	_views = views ;
	
}


	
void MultipleViewModel::addView( const BaseView & view ) const
	throw( GenericMVCException )
{

	const_cast<MultipleViewModel *>( this )->_views.push_back( & view ) ;
		
}


	
const string MultipleViewModel::toString( Ceylan::VerbosityLevels level )
	const throw()
{

	if ( _views.empty() )
		return "Multiple-view controller-less model "
			"not associated to any view" ;
			
	/*
	 * Beware to infinite recursion:
	 * (the view may display in turn this model)
	 *
	 */

	if ( level == Ceylan::low )
		return "Multiple-view controller-less model owning " 
			+ Ceylan::toString( _views.size() ) + " view(s)" ;


	std::list<std::string> res ;
	
	for ( std::list<const BaseView *>::const_iterator it =
		_views.begin(); it != _views.end() ; it++ )
	{
	
		res.push_back( (*it)->toString( level ) ) ;
		
	}
				
	return "Multiple-view controller-less model "
		"associated to following view(s): "
				+ Ceylan::formatStringList( res ) ;
	
}



void MultipleViewModel::deleteViews()
{

	for ( std::list<const BaseView *>::const_iterator it =
		_views.begin(); it != _views.end(); it++ )
	{
			
		delete *it ;
				
	}
			
	_views.clear() ;

}

