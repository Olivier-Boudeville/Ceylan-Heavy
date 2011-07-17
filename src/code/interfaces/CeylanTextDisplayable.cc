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


#include "CeylanTextDisplayable.h"

#include "CeylanStringUtils.h"      // for formatStringList



using std::list ;
using std::string ;

using namespace Ceylan ;



// Output format defaults to raw text.
TextDisplayable::TextOutputFormat TextDisplayable::_OutputFormat = rawText ;



const std::string TextDisplayable::ToString(
	list<TextDisplayable*> displayables, Ceylan::VerbosityLevels level )
{

	string res = "Description of TextDisplayable list is: " ;

	list<string> descriptionList ;

	for( list<TextDisplayable*>::const_iterator it = displayables.begin();
			it != displayables.end(); it++ )
		descriptionList.push_back( (*it)->toString( level ) ) ;

	return res + formatStringList( descriptionList ) ;

}



TextDisplayable::TextOutputFormat TextDisplayable::GetOutputFormat()
{

	return _OutputFormat ;

}



void TextDisplayable::SetOutputFormat( TextOutputFormat newOutputFormat )
{

	_OutputFormat = newOutputFormat ;

}



std::ostream & operator << ( std::ostream & os,
	const Ceylan::TextDisplayable & textDisplayable )
{

	return os << textDisplayable.toString( high ) ;

}
