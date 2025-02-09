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


#include "CeylanCountable.h"


#include "CeylanUtils.h"     // for emergencyShutdown


#include "CeylanOperators.h"
#include "CeylanLogLight.h"




using std::string ;

using namespace Ceylan ;


Countable::InstanceCount Countable::ReferenceCount = 0 ;
Countable::InstanceCount Countable::MaximumReferenceCount = 0 ;


const string Countable::LogPrefix = "[Instance count]" ;



Countable::Countable( bool verbose ) : 
	_verbose( verbose ) 
{

	ReferenceCount++ ;
	
	if ( ReferenceCount > MaximumReferenceCount )
		MaximumReferenceCount = ReferenceCount ;
		
	if ( _verbose )
	{
		CEYLAN_LOG( LogPrefix + " one more : " 
			+ Ceylan::toString( ReferenceCount )
			+ "/" + Ceylan::toString( MaximumReferenceCount ) ) ;
	}
			
}



Countable::~Countable() throw()
{

	if ( ReferenceCount == 0 )
		Ceylan::emergencyShutdown( 
			"Countable destructor : negative reference count detected." ) ;
		
	ReferenceCount-- ;
	
	if ( _verbose )
	{
		CEYLAN_LOG( LogPrefix + " one less : " 
			+ Ceylan::toString( ReferenceCount )
			+ "/" + Ceylan::toString( MaximumReferenceCount ) ) ;
	}
	
}



const string Countable::ToString( Ceylan::VerbosityLevels level )
{

	return "Current Countable count is " + Ceylan::toString( ReferenceCount )
		+ ", maximum was " + Ceylan::toString( MaximumReferenceCount ) ;
		
}



Countable::InstanceCount Countable::GetInstanceCount()
{

	return ReferenceCount ;
	
}



Countable::InstanceCount Countable::GetMaximumInstanceCount()
{

	return MaximumReferenceCount ;
	
}

