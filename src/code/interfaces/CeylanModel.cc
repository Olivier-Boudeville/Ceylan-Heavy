/* 
 * Copyright (C) 2003-2013 Olivier Boudeville
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


#include "CeylanModel.h"

#include "CeylanLogPlug.h"      // for LogPlug
#include "CeylanOperators.h"    // for Ceylan::toString

#include "CeylanView.h"         // for Ceylan::View
#include "CeylanController.h"   // for Ceylan::Controller

#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"       // for CEYLAN_DEBUG_EVENTS
#endif // CEYLAN_USES_CONFIG_H


#include <list>


using std::string ;
using std::list ;


using namespace Ceylan ;
using namespace Ceylan::Log ;




MVCEvent::MVCEvent( EventSource & source ) :
	Event( source )
{

}


MVCEvent::~MVCEvent() throw()
{

}




Model::Model() :
	CallableEventSource(), 
	CallerEventListener()
{

}



Model::~Model() throw()
{

#if CEYLAN_DEBUG_EVENTS
	LogPlug::debug( "Deleting Model " + toString() ) ;
#endif // CEYLAN_DEBUG_EVENTS
	
	unsubscribeFromAllControllers() ;
	
	// removeAllViews would be useless.
	
}



void Model::addView( View & newView )
{

	// Model will send events to views:
	add( newView ) ;
		
}



void Model::removeView( View & view )
{

	remove( view ) ;
	
}



void Model::removeAllViews()
{

	removeAllListeners() ;
	
}



void Model::subscribeToController( Controller & newController ) 
{

	// Model will listen to controller:
	subscribeTo( newController ) ;
		
}



void Model::unsubscribeFromController( Controller & controller ) 
{

	unsubscribeFrom( controller ) ;
	
}



void Model::unsubscribeFromAllControllers()
{

	unsubscribeFromAllSources() ;
	
}



const string Model::toString( Ceylan::VerbosityLevels level ) const
{

	string result ;
	
	if ( _sources.empty() )
		result = "MVC Model currently not subscribed to any controller, " ;
	else
		result = "MVC Model having currently subscribed to " 
			+ Ceylan::toString( 
				static_cast<Ceylan::Uint32>( _sources.size() ) ) 
			+ " controller(s), " ;
	
	if ( _listeners.empty() )
		result += "and having no registered view" ;
	else
		result += "and having " 
			+ Ceylan::toString( 
				static_cast<Ceylan::Uint32>( _listeners.size() ) ) 
			+ " registered view(s)" ;
	
	return result ;
				
}



void Model::notifyAllViews( const MVCEvent & newMVCEvent )
{

	notifyAllListeners( newMVCEvent ) ;
	
}

