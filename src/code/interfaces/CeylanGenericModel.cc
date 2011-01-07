/*
 * Copyright (C) 2003-2011 Olivier Boudeville
 *
 * This file is part of the Ceylan library.
 *
 * The Ceylan library is free software: you can redistribute it and/or modify
 * it under the terms of either the GNU Lesser General Public License or
 * the GNU General Public License, as they are published by the Free Software
 * Foundation, either version 3 of these Licenses, or (at your option)
 * any later version.
 *
 * The Ceylan library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License and the GNU General Public License
 * for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License and the GNU General Public License along with the Ceylan library.
 * If not, see <http://www.gnu.org/licenses/>.
 *
 * Author: Olivier Boudeville (olivier.boudeville@esperide.com)
 *
 */


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


BaseModel::BaseModel()
{

}



BaseModel::~BaseModel() throw()
{

}



const string BaseModel::toString( Ceylan::VerbosityLevels level ) const
{

	return "Base model" ;

}





// NoViewModel section.


NoViewModel::NoViewModel()
{

}



NoViewModel::~NoViewModel() throw()
{

}



void NoViewModel::addView( const BaseView & view ) const
{

	throw GenericMVCException( "NoViewModel::addView failed: "
		"no view supported by this model." ) ;

}



const string NoViewModel::toString( Ceylan::VerbosityLevels level ) const
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
{

	addView( view ) ;

}



void SingleViewModel::addView( const BaseView & view ) const
{

	if ( _view != 0 )
		throw GenericMVCException( "SingleViewModel::addView failed: "
			"a view was already registered." ) ;

	// Do as if this instance had not to be 'const':
	const_cast<SingleViewModel *>(this)->_view = & view ;

}



const string SingleViewModel::toString( Ceylan::VerbosityLevels level ) const
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
	const std::list<const BaseView *> & views )
{

	if ( ! _views.empty() )
		throw GenericMVCException( "MultipleViewModel::setViews failed: "
			"there was at least one view registered." ) ;

	_views = views ;

}



void MultipleViewModel::addView( const BaseView & view ) const
{

	const_cast<MultipleViewModel *>( this )->_views.push_back( & view ) ;

}



const string MultipleViewModel::toString( Ceylan::VerbosityLevels level ) const
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
