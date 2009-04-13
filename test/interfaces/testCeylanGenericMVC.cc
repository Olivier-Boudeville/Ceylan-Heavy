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


#include "Ceylan.h"

using namespace Ceylan ;
using namespace Ceylan::Log ;
using namespace Ceylan::MVC ;


#include <iostream>    // for cerr, endl
#include <exception>

#include <string>
using std::string ;



/*
 * Here are tested all generic (usually template-based) MVC components of the
 * lightweight generic MVC framework:
 *
 *  - models:
 *    + NoViewGenericModel
 *    + SingleViewGenericModel (SingleViewGenericModel<View> not tested yet,
 * as usually not appropriate)
 *    + MultipleViewGenericModel (MultipleViewGenericModel<View> not tested yet,
 * as usually not appropriate) 
 *    + SingleControllerNoViewGenericModel<Controller>
 *
 *  - views:
 *    + 
 *  
 *  - controllers:
 *    + 
 *
 */



/// Testing NoViewModel.
class TestNoViewModel: public NoViewModel
{

	public:
	
		TestNoViewModel( Ceylan::Uint32 initialState ):
			NoViewModel(),
			_state( initialState )
		{
		
			LogPlug::info( "Creating TestNoViewModel test model." ) ;
			
		}


		~TestNoViewModel() throw()
		{
		
			// View shall be automatically deallocated.
			LogPlug::info( "Deleting TestNoViewModel test model." ) ;
			
		}
		
		Ceylan::Uint32 getTestingState() const throw()
		{
			
			return _state ;
		
		}
		
		void update()
		{
		
			_state += 1 ;
			
		}
		
	private:
	
		Ceylan::Uint32 _state ;				

} ;



/// Testing SingleViewModel.
class TestSingleViewModel: public SingleViewModel
{

	public:
	
		TestSingleViewModel( Ceylan::Uint32 initialState ):
			SingleViewModel(),
			_state( initialState )
		{
		
			LogPlug::info( "Creating TestSingleViewModel test model." ) ;
			
		}


		~TestSingleViewModel() throw()
		{
		
			// View shall be automatically deallocated.
			LogPlug::info( "Deleting TestSingleViewModel test model." ) ;
			
		}
		
		Ceylan::Uint32 getTestingState() const throw()
		{
			
			return _state ;
		
		}
		
		void update()
		{
		
			_state += 1 ;
			
		}
		
	private:
	
		Ceylan::Uint32 _state ;				

} ;




/**
 * Testing the SingleModelGenericView template with the SingleViewModel model.
 * This is a (generic) view associated to one model, which itself knows only
 * one view (this one).
 *
 */
class TestSingleModelWithSingleViewGenericView: 
	public SingleModelGenericView<TestSingleViewModel>
{

	public:
	
		TestSingleModelWithSingleViewGenericView( 
				const TestSingleViewModel & model ) :
			SingleModelGenericView<TestSingleViewModel>( model )
		{

			LogPlug::info( "Creating "
				"TestSingleModelWithSingleViewGenericView test view." ) ;
		
		}
		
		
		~TestSingleModelWithSingleViewGenericView() throw()
		{
		
			LogPlug::info( "Deleting "
				"TestSingleModelWithSingleViewGenericView test view." ) ;
			
		}
		
		
		void render()
		{
			
			LogPlug::info( "TestSingleModelWithSingleViewGenericView "
				"instance is rendering for " + _model->toString()
				+ ", whose state is " 
				+ Ceylan::toString( _model->getTestingState() ) ) ;
				
		}	
		
} ;




/// Testing MultipleViewModel.
class TestMultipleViewModel: public MultipleViewModel
{

	public:
	
		TestMultipleViewModel( Ceylan::Uint32 initialState ):
			MultipleViewModel(),
			_state( initialState )
		{
		
			LogPlug::info( "Creating TestMultipleViewModel test model." ) ;
			
		}


		~TestMultipleViewModel() throw()
		{
		
			// View shall be automatically deallocated.
			LogPlug::info( "Deleting TestMultipleViewModel test model." ) ;
			
		}
		
		Ceylan::Uint32 getTestingState() const throw()
		{
			
			return _state ;
		
		}
		
		void update()
		{
		
			_state += 1 ;
			
		}
		
	private:
	
		Ceylan::Uint32 _state ;				

} ;





/**
 * Testing the SingleModelGenericView template with the MultipleViewModel model.
 * This is a (generic) view associated to one model, which itself can know
 * any number of views.
 *
 */
class TestSingleModelWithMultipleViewGenericView: 
	public SingleModelGenericView<TestMultipleViewModel>
{

	public:
	
		TestSingleModelWithMultipleViewGenericView( 
				const TestMultipleViewModel & model ) :
			SingleModelGenericView<TestMultipleViewModel>( model )
		{

			LogPlug::info( "Creating "
				"TestSingleModelWithMultipleViewGenericView test view." ) ;
		
		}
		
		
		~TestSingleModelWithMultipleViewGenericView() throw()
		{
		
			LogPlug::info( "Deleting "
				"TestSingleModelWithMultipleViewGenericView test view." ) ;
			
		}
		
		
		void render()
		{
			
			LogPlug::info( "TestSingleModelWithMultipleViewGenericView "
				"instance is rendering for: " + _model->toString()
				+ ", whose state is " 
				+ Ceylan::toString( _model->getTestingState() ) ) ;
				
		}	
		
} ;









/**
 * Test of the generic lightweight implementation of the MVC framework.
 *
 * @see Ceylan::BaseModel, Ceylan::BaseView, Ceylan::BaseController.
 *
 */
int main( int argc, char * argv[] )
{

	LogHolder logger( argc, argv ) ;
	
    try
    {
		
		LogPlug::info( "Starting testing lightweight MVC framework." ) ;
		
		
		LogPlug::info( "Testing no view - one model." ) ;

		TestNoViewModel & myNoViewModel = * new TestNoViewModel( 1 ) ;
		
		myNoViewModel.update() ;
		myNoViewModel.update() ;

		delete & myNoViewModel ;
		
		
		
		LogPlug::info( "Testing one view - one model." ) ;

		TestSingleViewModel & mySingleViewModel = 
			* new TestSingleViewModel( 10 ) ;
				
		// Auto-registers to the model:
		TestSingleModelWithSingleViewGenericView & mySingleModelView =
			* new TestSingleModelWithSingleViewGenericView( 
				mySingleViewModel ) ;
				
		mySingleModelView.render() ;
		mySingleViewModel.update() ;
		mySingleModelView.render() ;
		
		delete & mySingleViewModel ;
		


		LogPlug::info( "Testing multiple views - one model." ) ;

		TestMultipleViewModel & myMultipleViewModel = 
			* new TestMultipleViewModel( 100 ) ;
			
		TestSingleModelWithMultipleViewGenericView & myFirstMultiView = * new
			TestSingleModelWithMultipleViewGenericView( myMultipleViewModel ) ;
		
		myFirstMultiView.render() ;
		myMultipleViewModel.update() ;
		myFirstMultiView.render() ;
		
		TestSingleModelWithMultipleViewGenericView & mySecondMultiView = * new
			TestSingleModelWithMultipleViewGenericView( myMultipleViewModel ) ;
			
		myFirstMultiView.render() ;
		mySecondMultiView.render() ;
		
		myMultipleViewModel.update() ;
		
		myFirstMultiView.render() ;
		mySecondMultiView.render() ;
		
		delete & myMultipleViewModel ;
		
		
		LogPlug::info( "End of lightweight MVC framework test." ) ;

		
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

    return Ceylan::ExitSuccess ;

}

