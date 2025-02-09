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


#include "CeylanHashable.h"


using std::string ;

#include "CeylanOperators.h"  // for toString



using namespace Ceylan ;



Hashable::Hashable() 
{

}



Hashable::~Hashable() throw() 
{

}



const string Hashable::toString( VerbosityLevels level ) const
{

	if ( level == Ceylan::low )
		return "Hashable object" ; 
	
	if ( level == Ceylan::medium )
		return "Hashable object whose weak hash code is " 
			+ Ceylan::toString( getWeakHashCode() ) ; 
	
	return "Hashable object whose strong hash code is " 
			+ Ceylan::toString( getStrongHashCode() ) ; 
			
}	
		


WeakHashCode Hashable::GetWeakHashCode( const std::string & stringToHash )
{

	WeakHashCode hash = 0 ;
 
	Ceylan::Uint32 charCount = 0 ;
	
	while ( stringToHash[charCount] )
	{

		hash <<= 1 ;

		hash = hash ^ stringToHash[charCount] ;
		charCount++ ;
		
	}

	return hash ;
	
}



StrongHashCode Hashable::GetStrongHashCode( const std::string & stringToHash )
{

	StrongHashCode hash = 0 ;
 
	Ceylan::Uint32 charCount = 0 ;
	
	while ( stringToHash[charCount] )
	{

		hash <<= 1 ;

		hash = hash ^ stringToHash[charCount] ;
		charCount++ ;
		
	}

	return hash ;
	
}

