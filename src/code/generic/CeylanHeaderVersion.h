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



#ifndef CEYLAN_HEADER_VERSION_H_
#define CEYLAN_HEADER_VERSION_H_

#include <string>



/*
 * This file is dedicated to the non-UNIX versions (Windows, Nintendo DS, etc.),
 * as on UNIX it is overwritten by the configure-time generated one.
 *
 * @note This version needs CeylanHeaderVersion.cc, whereas the UNIX one does
 * not.
 *
 */


namespace Ceylan
{


  /**
   * This is the libtool version of the Ceylan headers.
   *
   * Allows to detect run-time mismatches between the Ceylan headers a
   * program or a library was compiled with, and the actual Ceylan library
   * it is then linked to.
   *
   * @note Cannot declare here:
   * 'extern CEYLAN_DLL const std::string actualCeylanHeaderLibtoolVersion
   *  = CEYLAN_LIBTOOL_VERSION ;' because with Visual C++ it leads to
   * multiple definitions for actualCeylanHeaderLibtoolVersion.
   *
   */
  extern CEYLAN_DLL const std::string actualCeylanHeaderLibtoolVersion ;

}


#endif // CEYLAN_HEADER_VERSION_H_

