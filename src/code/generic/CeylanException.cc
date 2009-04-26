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


#include "CeylanException.h"

using std::string ;

using namespace Ceylan ;



Exception::Exception( const string & reason ) :
	_reason( reason )
{
    
}



Exception::~Exception() throw()
{

}



const string Exception::toString( VerbosityLevels level ) const
	
{

    return ( _reason ) ;
	
}



const char * Exception::what() const throw()
{

	return toString( Ceylan::high ).c_str() ;
	
}



std::ostream & operator << ( std::ostream & os, const Exception & e )
	
{

    return os << e.toString( Ceylan::high ) ;
	
}

