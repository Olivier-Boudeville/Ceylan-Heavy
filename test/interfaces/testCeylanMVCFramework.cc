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
 * Author: Olivier Boudeville (olivier (dot) boudeville (at) esperide (dot) com)
 *
 */


#include "Ceylan.h"

using namespace Ceylan ;
using namespace Ceylan::Log ;


#include <iostream>    // for cerr, endl
#include <exception>

#include <string>
using std::string ;




/// A MVC test event will be propagated.
class TestMVCEvent : public Ceylan::MVCEvent
{

public:


  TestMVCEvent( const std::string & message, EventSource & source )
	throw() :
	MVCEvent( source ),
	_message( message )
  {


  }


  virtual ~TestMVCEvent() throw()
  {

  }


  virtual const std::string toString(
	Ceylan::VerbosityLevels level = Ceylan::high ) const throw()
  {
	return "Test MVC event: " + _message ;
  }


private:

  std::string _message ;


} ;



/// This test model can be linked to views and controllers.
class AnObjectModel : public Ceylan::Model
{

public:


  AnObjectModel() throw() :
  _lastEvent( 0 )
  {

  }


  ~AnObjectModel() throw()
  {

	if ( _lastEvent != 0 )
	  delete _lastEvent ;

  }


  virtual void beNotifiedOf( const Event & newEvent ) throw()
  {

	/*
	 * This model relays all incoming events from controllers to views.
	 *
	 */
	notifyAllListeners( newEvent ) ;

  }


  virtual Event & getEventFor( const CallerEventListener & listener )
	throw( EventException )
  {
	if ( _lastEvent == 0 )
	  throw EventException(
		"AnObjectView::getEventFor: no event available." ) ;

	return * _lastEvent ;

  }


private:

  TestMVCEvent * _lastEvent ;

} ;



/// Views linked to the model.
class AnObjectView : public Ceylan::View
{

public:


  AnObjectView( AnObjectModel & model ) :
	View( model )
  {

  }


  virtual void beNotifiedOf( const Event & newEvent ) throw()
  {

	LogPlug::info( "I am an AnObjectView and "
	  "I am notified of following event: "
	  + newEvent.toString() ) ;

	// Render at each event received:
	renderModel() ;

  }



  virtual void renderModel() throw()
  {
	LogPlug::info( "Rendering of AnObjectView: " + toString() ) ;
  }



} ;



/**
 * This controller keeps the last event it sent, so that it can be used in
 * getEventFor is called.
 *
 */
class AnObjectController : public Ceylan::Controller
{


public:


  AnObjectController( AnObjectModel & model ) :
	Controller( model ),
	_lastEvent( 0 )
  {

  }


  ~AnObjectController() throw()
  {

	if ( _lastEvent != 0 )
	  delete _lastEvent ;

  }


  void sendEvent( Ceylan::Uint32 count )
  {

	if ( _lastEvent != 0 )
	  delete _lastEvent ;

	_lastEvent = new TestMVCEvent( "I am a MVC event whose count is "
	  + Ceylan::toString( count ), * this ) ;

	notifyAllListeners( * _lastEvent ) ;

  }


  virtual const Event & getEventFor(
	const CallerEventListener & listener ) throw( EventException )
  {

	if ( ! isRegistered( listener ) )
	  throw EventException( "AnObjectController::getEventFor: "
		"listener not registered." ) ;

	if ( _lastEvent == 0 )
	  throw EventException( "AnObjectController::getEventFor: "
		"no event available." ) ;

	return * _lastEvent ;

  }


private:

  TestMVCEvent * _lastEvent ;

} ;





/**
 * Test of Model-View-Controller (MVC) framework implementation.
 *
 * @see Event, CallableEventSource, CallerEventListener
 *
 */
int main( int argc, char * argv[] )
{

  {

	LogHolder logger( argc, argv ) ;

	try
	{

	  LogPlug::info( "Starting testing Model-View-Controller "
		"(MVC) framework." ) ;

	  LogPlug::info( "First create the model." ) ;
	  AnObjectModel model ;

	  LogPlug::info( "Model: " + model.toString() ) ;

	  LogPlug::info( "Then create its first view, and link them." ) ;
	  AnObjectView firstView( model ) ;

	  LogPlug::info( "Model: " + model.toString() ) ;
	  LogPlug::info( "First view: " + firstView.toString() ) ;

	  LogPlug::info( "Then create the model's first controller, "
		"and link them." ) ;
	  AnObjectController firstController( model ) ;

	  LogPlug::info( "Model: " + model.toString() ) ;
	  LogPlug::info( "First controller: " + firstController.toString() ) ;

	  LogPlug::info( "Now activate the controller twice "
		"and see how the view reacts." ) ;

	  firstController.sendEvent( 100 ) ;
	  firstController.sendEvent( 101 ) ;

	  LogPlug::info( "Add a second view, use the controller again." ) ;
	  AnObjectView secondView( model ) ;

	  LogPlug::info( "Model: " + model.toString() ) ;

	  firstController.sendEvent( 103 ) ;

	  LogPlug::info( "Add a second controller, and use it twice." ) ;
	  AnObjectController secondController( model ) ;
	  secondController.sendEvent( 200 ) ;
	  secondController.sendEvent( 201 ) ;

	  LogPlug::info( "Event requested, on behalf of the model, "
		"to the second controller is: "
		+ secondController.getEventFor( model ).toString() ) ;

	  LogPlug::info( "Unlink model from controllers." ) ;
	  model.unsubscribeFromAllControllers() ;

	  LogPlug::info( "Unlink views from model." ) ;
	  firstView.unsubscribeFromAllSources() ;
	  secondView.unsubscribeFromAllSources() ;

	  LogPlug::info( "End of Model-View-Controller (MVC) framework test." ) ;


	}

	catch ( const Ceylan::Exception & e )
	{
	  std::cerr << "Ceylan exception caught: "
				<< e.toString( Ceylan::high ) << std::endl ;
	  return Ceylan::ExitFailure ;

	}

	catch ( const std::exception & e )
	{
	  std::cerr << "Standard exception caught: "
				<< e.what() << std::endl ;
	  return Ceylan::ExitFailure ;

	}

	catch ( ... )
	{
	  std::cerr << "Unknown exception caught" << std::endl ;
	  return Ceylan::ExitFailure ;

	}

  }

  Ceylan::shutdown() ;

  return Ceylan::ExitSuccess ;

}
