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


#include "Ceylan.h"

#include <iostream>
#include <ctime>


/*
 * Most basic test to know whether compile chain is sane.
 *
 * It is linked to the Ceylan library even though it does not use it.
 *
 */

int main( int argc, char * argv[] )
{

	std::cout << "Hello, Ceylan world !" << std::endl ;
	
	std::cerr << "The size of a time_t variable is " << sizeof( time_t )
		<< " bytes, and the size of Ceylan::Sint32 is " 
		<< sizeof( Ceylan::Sint32 ) << " bytes." << std::endl ;

	/*
	 * Not used anymore :
	  
	std::cerr << "The size of a Float80 is " << sizeof( Ceylan::Float80  )
		<< " bytes." << std::endl ;
	 */
	
	std::cerr << "The size of a StringSize variable is " 
		<< sizeof( Ceylan::StringSize )
		<< " bytes, and the size of a ListSize variable is " 
		<< sizeof( Ceylan::ListSize ) << " bytes." << std::endl ;
	
	return Ceylan::ExitSuccess ;
	
}

