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


#ifndef CEYLAN_DOC_MAIN_PAGE_H_
#define CEYLAN_DOC_MAIN_PAGE_H_


// This is the main page of Ceylan's API documentation.

// This header file is designed to be parsed by doxygen.


/**
 * @mainpage Ceylan: Mutualization of Generic-Purpose High-Level Classes
 *
 * @section intro Overview
 *
 * Ceylan's purpose is to centralize generic and useful C++ code in order to
 * ease and improve our software development: write it only once, and write it
 * well (or die tryin').
 *
 * @section use Usage
 *
 * Use Ceylan.h and link your program or library with one of the following
 * Ceylan libraries:
 *<ul>
 *	<li>use Ceylan.h: <code>CPPFLAGS += 
 * -I&lt;CEYLAN_INSTALL_DIR&gt;/include/Ceylan</code></li>
 *	<li>use libCeylan.a, libCeylan.la or libCeylan.so variations: 
 *  <code>LIBS += -L&lt;CEYLAN_INSTALL_DIR&gt;/lib -lCeylan</code></li>
 *</ul>
 *
 * If the user code is autotools-based, the <a href="http://ceylan.svn.sourceforge.net/viewvc/ceylan/Ceylan/trunk/src/conf/build/m4/ceylan.m4.in?view=markup" target="_blank">ceylan.m4</a> macro, 
 * available in the <code>test</code> subdirectory, helps writing a 
 * <code>configure.ac</code> file which takes into account the linking
 * with Ceylan library. See the CEYLAN_PATH macro and its numerous built-in
 * sanity checks.
 * One can use Ceylan's numerous test cases as guidelines to implement, build
 * and run code using the Ceylan library.
 *
 * @subsection macro Usage of macros defined by Ceylan
 * 
 * You can set CEYLAN_DEBUG flags to control macros: this will activate debug
 * mode, more execution information will be displayed, and additionnal
 * checkings will be made in order to run a little safer (checking for example
 * run-time assertions), at the expense of a small performance loss.
 *
 * Debug facilities can be used by passing to the compiler -DCEYLAN_DEBUG, or
 * by adding it directly in <code>src/code/CeylanConfig.h</code>, or preferably
 * by using the configure dedicated <code>--enable-debug</code> option.
 *
 * Defining topic-specific debug symbols (ex: CEYLAN_DEBUG_NETWORK_CLIENTS)
 * allows the implementation to perform supplementary tests and to display more
 * accurate and detailed information about its run-time behaviour, as regard
 * to selected debug matters. 
 *
 *
 * @subsection details More information
 * 
 *
 * Browse Ceylan's <a href="http://ceylan.sourceforge.net/" target="_blank">official website</a> or our <a href="http://ceylan.esperide.com" target="_blank">mirror</a>.
 *
 * Information about the latest Ceylan release can be found <a href="http://ceylan.sourceforge.net/Ceylan-latest/" target="_blank">here</a>
 * (<a href="http://ceylan.esperide.com/Ceylan-latest/" target="_blank">mirror</a>).
 *
 * Support can be obtained from ceylan-support@lists.sourceforge.net (one 
 * must register first).
 *
 */


#endif // CEYLAN_DOC_MAIN_PAGE_H_

