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


#include "CeylanController.h"   


#include "CeylanModel.h"        // for Ceylan::Model

#include "CeylanLogPlug.h"      // for LogPlug
#include "CeylanOperators.h"    // for Ceylan::toString

#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"       // for CEYLAN_DEBUG_EVENTS
#endif // CEYLAN_USES_CONFIG_H

#include <list>



using std::string ;
using std::list ;


using namespace Ceylan ;
using namespace Ceylan::Log ;



Controller::Controller() :
	CallableEventSource()
{

}



Controller::Controller( Model & model ) :
	CallableEventSource()
{

	model.subscribeTo( * this ) ;
	
}



Controller::~Controller() throw()
{

	// Input device not owned.
	
#if CEYLAN_DEBUG_EVENTS
	LogPlug::debug( "Deleting Controller " + toString() ) ;
#endif // CEYLAN_DEBUG_EVENTS
	
}



const string Controller::toString( Ceylan::VerbosityLevels level ) const 
{
	
	if ( _listeners.empty() )
		return "MVC Controller has currently no model subscribed" ;
	
	return "MVC Controller has currently " 
		+ Ceylan::toString( static_cast<Ceylan::Uint32>( _listeners.size() ) )
		+ " model(s) subscribed" ;
				
}

