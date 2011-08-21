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


#include "CeylanResource.h"

#include "CeylanOperators.h"    // for string operators


using namespace Ceylan ;

using std::string ;



// Resource Exception.

ResourceException::ResourceException( const std::string & reason ) :
	Exception( "Resource exception: " + reason )
{

}


ResourceException::~ResourceException() throw()
{

}




// The Resource class itself, mostly empty.


Resource::Resource()
{

}



Resource::~Resource() throw()
{

}



const string Resource::toString( Ceylan::VerbosityLevels level ) const
{

	return "Ressource at address "
		+ Ceylan::toString( static_cast<const void *>( this ) ) ;

}
