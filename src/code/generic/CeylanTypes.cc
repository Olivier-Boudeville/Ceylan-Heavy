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
 * Author: Olivier Boudeville (olivier.boudeville@esperide.com)
 *
 */


#include "CeylanTypes.h"


#include <limits>    // for numeric_limits


using namespace std ;



// Must be -128:
Ceylan::Sint8 Ceylan::Sint8Min     = numeric_limits<Ceylan::Sint8>::min() ;

// Must be 127:
Ceylan::Sint8 Ceylan::Sint8Max     = numeric_limits<Ceylan::Sint8>::max() ;



// Must be 0:
Ceylan::Uint8 Ceylan::Uint8Min     = numeric_limits<Ceylan::Uint8>::min() ;

// Must be 255:	
Ceylan::Uint8 Ceylan::Uint8Max     = numeric_limits<Ceylan::Uint8>::max() ;



// Should be -128:
Ceylan::Byte Ceylan::ByteMin       = numeric_limits<Ceylan::Byte>::min() ;

// Should be 127:	
Ceylan::Byte Ceylan::ByteMax       = numeric_limits<Ceylan::Byte>::max() ;




// Must be -32768:
Ceylan::Sint16 Ceylan::Sint16Min   = numeric_limits<Ceylan::Sint16>::min() ;

// Must be 32767:
Ceylan::Sint16 Ceylan::Sint16Max   = numeric_limits<Ceylan::Sint16>::max() ;


// Must be 0:
Ceylan::Uint16 Ceylan::Uint16Min   = numeric_limits<Ceylan::Uint16>::min() ;

// Must be 65535:	
Ceylan::Uint16 Ceylan::Uint16Max   = numeric_limits<Ceylan::Uint16>::max() ;



// Must be -2147483648:
Ceylan::Sint32 Ceylan::Sint32Min   = numeric_limits<Ceylan::Sint32>::min() ;

// Must be 2147483647:
Ceylan::Sint32 Ceylan::Sint32Max   = numeric_limits<Ceylan::Sint32>::max() ;


// Must be 0:
Ceylan::Uint32 Ceylan::Uint32Min   = numeric_limits<Ceylan::Uint32>::min() ;

// Must be 4294967294:
Ceylan::Uint32 Ceylan::Uint32Max   = numeric_limits<Ceylan::Uint32>::max() ;




// Depends on the platform:
Ceylan::SignedLongInteger Ceylan::SignedLongIntegerMin
	= numeric_limits<Ceylan::SignedLongInteger>::min() ;

// Depends on the platform:
Ceylan::SignedLongInteger Ceylan::SignedLongIntegerMax
	= numeric_limits<Ceylan::SignedLongInteger>::max() ;



// Depends on the platform:
Ceylan::UnsignedLongInteger Ceylan::UnsignedLongIntegerMin
	= numeric_limits<Ceylan::UnsignedLongInteger>::min() ;

// Depends on the platform:
Ceylan::UnsignedLongInteger Ceylan::UnsignedLongIntegerMax
	= numeric_limits<Ceylan::UnsignedLongInteger>::max() ;



/*
 * Actually with the STL, for floating-point values the min returns something
 * like epsilon (the smallest value that can be added to 1.0 to yield a
 * different value), whereas we want the lower bound.
 *
 * Hence min is deduced from max instead.
 *
 */
 

// Must be -3.4E-38f:
Ceylan::Float32 Ceylan::Float32Min = -numeric_limits<Ceylan::Float32>::max() ;

// Must be 3.4E38f:
Ceylan::Float32 Ceylan::Float32Max = numeric_limits<Ceylan::Float32>::max() ;



// Must be -1.7E-308:
Ceylan::Float64 Ceylan::Float64Min = -numeric_limits<Ceylan::Float64>::max() ;

// Must be 1.7E308:
Ceylan::Float64 Ceylan::Float64Max = numeric_limits<Ceylan::Float64>::max() ;



// Depends on the platform:
Ceylan::LongFloat Ceylan::LongFloatMin 
	= -numeric_limits<Ceylan::LongFloat>::max() ;

// Depends on the platform:
Ceylan::LongFloat Ceylan::LongFloatMax
	= numeric_limits<Ceylan::LongFloat>::max() ;



//Ceylan::Float80 Ceylan::Float80Min = -3.4E-4932 ;

// Should have been 3.4E4932:
//Ceylan::Float80 Ceylan::Float80Max = 1.7E308    ; 

