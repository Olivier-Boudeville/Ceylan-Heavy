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


#ifndef CEYLAN_TEST_EXCEPTION_H_
#define CEYLAN_TEST_EXCEPTION_H_


#include "CeylanException.h"  // for inheritance


namespace Ceylan
{


	/**
	 * This Exception subclass is to be used only for test results.
	 *
	 */
    class CEYLAN_DLL TestException : public Ceylan::Exception
    {


        public:


            /**
             * Basic constructor.
             *
             * @param reason an explanation for this exception being raised.
             *
             */
            explicit TestException( const std::string & reason ) ;


            /// Basic virtual destructor.
            virtual ~TestException() throw() ;



    } ;

}


#endif // CEYLAN_TEST_EXCEPTION_H_

