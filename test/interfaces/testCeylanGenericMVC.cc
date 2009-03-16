#include "Ceylan.h"

using namespace Ceylan ;
using namespace Ceylan::Log ;


#include <iostream>    // for cerr, endl
#include <exception>

#include <string>
using std::string ;



class TestModel: public SingleViewGenericModel
{

	public:
	
		TestModel( Ceylan::Uint8 initialState ):
			SingleViewGenericModel(),
			_state( initialState )
		{
		
			LogPlug::info( "Creating test model." ) ;
			
		}
		
		
		~TestModel() throw()
		{
		
			// View shall be automatically deallocated.
			LogPlug::info( "Deleting test model." ) ;
			
		}
		
		
		Ceylan::Uint8 getTestingState() const throw()
		{
			
			return _state ;
		
		}
		
		void update()
		{
		
			_state += 1 ;
			
		}
		
	private:
	
		Ceylan::Uint8 _state ;				

} ;




class TestView: public SingleModelGenericView<TestModel>
{

	public:
	
	
		TestView( TestModel & model ):
			SingleModelGenericView<TestModel>( model )
		{

			// Auto-registers in mother class constructor.
			LogPlug::info( "Creating test view." ) ;
		
		}
		
		
		~TestView() throw()
		{
		
			LogPlug::info( "Deleting test view." ) ;
		}
		
		
		void render()
		{
		
			/*
			 * This is the whole point of the template: being able to
			 * access the model directly as a TestModel instance.
			 *
			 */
			LogPlug::info( "Test view rendering model : state is "
				+ Ceylan::toNumericalString( _model->getTestingState() ) 
				+ "." ) ;
			
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
		
		TestModel & myModel = * new TestModel( 42 ) ;
		
		// Auto-registers to the model:
		TestView & myView = * new TestView( myModel ) ;
		
		myView.render() ;
		
		myModel.update() ;

		myView.render() ;
		
		// Both are destroyed:
		delete & myModel ;
		
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

