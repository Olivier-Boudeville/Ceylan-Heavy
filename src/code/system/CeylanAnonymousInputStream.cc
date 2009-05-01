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


#include "CeylanAnonymousInputStream.h"

#include "CeylanOperators.h"
#include "CeylanLogPlug.h"


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for configure-time settings
#endif // CEYLAN_USES_CONFIG_H




using std::string ;
using std::list ;


using namespace Ceylan::System ;
using namespace Ceylan::Log ;



AnonymousInputStream::AnonymousInputStream( FileDescriptor fd ) :
	_fdes( fd )
{

#if CEYLAN_USES_FILE_DESCRIPTORS

#else // CEYLAN_USES_FILE_DESCRIPTORS

	throw StreamException( "AnonymousInputStream constructor: "
		"file descriptor feature not available." ) ;
		
#endif // CEYLAN_USES_FILE_DESCRIPTORS

}



AnonymousInputStream::~AnonymousInputStream() throw()
{

}



StreamID AnonymousInputStream::getInputStreamID() const
{

	return static_cast<StreamID>( _fdes ) ;
	
}



const std::string AnonymousInputStream::toString( 
	Ceylan::VerbosityLevels level ) const
{

	return "AnonymousInputStream whose file descriptor is "
		+ Ceylan::toString( _fdes ) ;
		
}

