/* 
 * Copyright (C) 2003-2009 Olivier Boudeville
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


#include "CeylanInputDevice.h"   

#include "CeylanController.h"   // for Controller 

#include "CeylanLogPlug.h"      // for LogPlug
#include "CeylanOperators.h"    // for Ceylan::toString

#include <list>


using std::string ;
using std::list ;


using namespace Ceylan ;
using namespace Ceylan::Log ;



InputDevice::InputDevice() throw() :
	TextDisplayable(),
	_controller( 0 )
{

}


InputDevice::InputDevice( Controller & controller  ) throw() :
	TextDisplayable(),
	_controller( & controller )
{

}



InputDevice::~InputDevice() throw()
{
	// Controller not owned.	
}


bool InputDevice::isLinkedToController() const throw()
{
	return ( _controller != 0 ) ;
}


Controller & InputDevice::getController() const throw( EventException )
{

	if ( _controller == 0 )
		throw EventException( 
			"InputDevice::getController : not linked to any controller." ) ;
	
	return * _controller ;
		
}


void InputDevice::setController( Controller & controller ) 
	throw( EventException )
{

	if ( _controller != 0 )
		throw EventException( "InputDevice::setController : already linked with controller "
			+ _controller->toString() ) ;
	
	_controller = & controller ;		
	
}


bool InputDevice::dropController() throw() 
{

	if ( _controller != 0 )
	{
		_controller = 0 ;
		return true ;
	}
	
	return false ;
	
}


const string InputDevice::toString( Ceylan::VerbosityLevels level ) 
	const throw() 
{
	
	if ( _controller == 0 )
		return "Input device currently not linked to any controller" ;
	else
		return "Input device currently linked with following controller : "
			+ _controller->toString( level ) ;
				
}
