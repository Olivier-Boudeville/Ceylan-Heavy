#ifndef CEYLAN_DOC_MAIN_PAGE_H_
#define CEYLAN_DOC_MAIN_PAGE_H_


// This is the main page of Ceylan's API documentation.

// This header file is designed to be parsed by doxygen.


/**
 * @mainpage Ceylan : mutualization of generic-purpose classes.
 *
 * @section intro Overview
 *
 * Ceylan's purpose is to centralize generic and useful C++ code in order to
 * ease and improve software development : write it only once, and write it
 * well (or die tryin').
 *
 * @section use Usage
 *
 * Use Ceylan.h and link your program or library with one of the following
 * Ceylan libraries :
 *<ul>
 *	<li>use Ceylan.h : <code>CPPFLAGS += 
 * -I&lt;CEYLAN_INSTALL_DIR&gt;/include/Ceylan</code></li>
 *	<li>use libCeylan.a, libCeylan.la or libCeylan.so variations : 
 *  <code>LIBS  += -L&lt;CEYLAN_INSTALL_DIR&gt;/lib -lCeylan</code></li>
 *</ul>
 *
 * If the user code is autotools-based, the <code>ceylan.m4</code> macro, 
 * available in <code>test</code> subdirectory, helps writing a 
 * <code>configure.ac</code> file which takes into account the linking
 * with Ceylan library. See the CEYLAN_PATH macro and its numerous built-in
 * sanity checks.
 *
 * @subsection macro Usage of macros defined by Ceylan
 * 
 * You can set CEYLAN_DEBUG flags to control macros : it will activate debug
 * mode, more execution informations will be displayed, and additionnal
 * checkings will be made in order to run a little safer (checking for example
 * run-time assertions), at the expense of a small performance loss.
 *
 * Use debug facilities, by passing to the compiler -DCEYLAN_DEBUG for instance,
 * or add it directly in <code>src/code/CeylanConfig.h</code>, or preferably
 * use the configure dedicated <code>--enable-debug</code> option.
 *
 * Defining debug symbols allows the implementation to perform supplementary
 * tests and to display more accurate and detailed informations about its
 * run-time behaviour, as regard to selected debug matters. 
 *
 *
 * @subsection details More informations
 * 
 *
 * Look at Ceylan's website, generated locally (under 
 * $CEYLAN_ROOT/src/doc/web/index.html),or preferably at its 
 * <a href="http://ceylan.sourceforge.net/" target="_blank">official site</a>.
 * A less up-to-date 
 * <a href="http://ceylan.esperide.com" target="_blank">mirror</a> is available
 * as well.
 *
 */


#endif // CEYLAN_DOC_MAIN_PAGE_H_
