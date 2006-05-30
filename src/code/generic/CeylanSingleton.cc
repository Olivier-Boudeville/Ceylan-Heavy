#include "CeylanSingleton.h"


#include "CeylanLogPlug.h"
#include "CeylanOperators.h"



using std::string ;

using namespace Ceylan ;
using namespace Ceylan::Log ;


Singleton * Singleton::_InternalSingleton = 0 ;



SingletonException::SingletonException( const string & reason ) throw() :
	Ceylan::Exception( reason )
{

}


SingletonException::~SingletonException() throw()
{

}



Singleton & Singleton::GetSingleton() throw()
{

    if ( Singleton::_InternalSingleton == 0 )
    {
        LogPlug::debug( 
			"No Singleton available for getSingleton, creating new one" ) ;
        Singleton::_InternalSingleton = new Singleton() ;
    }
    else
    {
        LogPlug::debug( "Returning already constructed instance "
			"of Singleton, no creation" ) ;
    }

    LogPlug::debug( "Returning Singleton instance "
		+ Ceylan::toString( Singleton::_InternalSingleton ) ) ;

    return * Singleton::_InternalSingleton ;

}


void Singleton::DeleteSingleton() throw()
{

    if ( Singleton::_InternalSingleton != 0 )
    {
        LogPlug::debug( "deleteSingleton : effective deleting." ) ;
        delete Singleton::_InternalSingleton ;
		Singleton::_InternalSingleton = 0 ;
    }
    else
    {
        LogPlug::debug( "deleteSingleton : no deleting needed." ) ;
    }

}



// Protected section.


Singleton::Singleton() throw()
{
    LogPlug::debug( "Creation of a Singleton instance : "
		+ Ceylan::toString( this ) ) ;
}


Singleton::~Singleton() throw()
{
    LogPlug::debug( "Warning : destruction of a Singleton instance : "
		+ Ceylan::toString( this ) ) ;
}

