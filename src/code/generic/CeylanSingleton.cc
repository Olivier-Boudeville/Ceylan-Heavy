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


#include "CeylanSingleton.h"


#include "CeylanLogPlug.h"
#include "CeylanOperators.h"



using std::string ;

using namespace Ceylan ;
using namespace Ceylan::Log ;


Singleton * Singleton::_InternalSingleton = 0 ;



SingletonException::SingletonException( const string & reason ) :
	Ceylan::Exception( reason )
{

}



SingletonException::~SingletonException() throw()
{

}



Singleton & Singleton::GetSingleton()
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



void Singleton::DeleteSingleton()
{

    if ( Singleton::_InternalSingleton != 0 )
    {
        LogPlug::debug( "deleteSingleton: effective deleting." ) ;
        delete Singleton::_InternalSingleton ;
		Singleton::_InternalSingleton = 0 ;
    }
    else
    {
        LogPlug::debug( "deleteSingleton: no deleting needed." ) ;
    }

}



// Protected section.


Singleton::Singleton()
{

    LogPlug::debug( "Creation of a Singleton instance: "
		+ Ceylan::toString( this ) ) ;
		
}



Singleton::~Singleton() throw()
{

    LogPlug::debug( "Warning: destruction of a Singleton instance: "
		+ Ceylan::toString( this ) ) ;
		
}

