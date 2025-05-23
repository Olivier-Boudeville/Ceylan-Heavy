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


#include "CeylanMatrix.h"   


using std::string ;


using namespace Ceylan ;
using namespace Ceylan::Maths::Linear ;




Matrix::Matrix() 
{
	
}



Matrix::~Matrix() throw()
{

}



void Matrix::nullify()
{

	setAllElementsTo( 0 ) ;
	
}



void Matrix::setToIdentity()
{

	setToDiagonal( 1 ) ;
	
}



const string Matrix::toString( VerbosityLevels level ) const
{
	
	return "Abstract matrix"  ;	

}

