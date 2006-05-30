#include "CeylanRunnable.h"

using std::string ;

using namespace Ceylan::System ;



RunnableException::RunnableException( const string message ) throw() :
	SystemException( message )
{

}


RunnableException::~RunnableException() throw()
{

}



Runnable::Runnable() throw() : 
	_name()
{

}


Runnable::Runnable( const string & name ) throw() : 
	_name( name )
{

}


Runnable::~Runnable() throw()
{

}


const string & Runnable::getName() const throw()
{ 
	return _name ; 
}
				
