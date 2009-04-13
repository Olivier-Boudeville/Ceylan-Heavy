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


#ifndef CEYLAN_LOG_INCLUDES_H_
#define CEYLAN_LOG_INCLUDES_H_


// Please include below all header files to be exported by module 'Log'.


#include "CeylanLog.h"
#include "CeylanLogAggregator.h"
#include "CeylanLogAggregatorConsole.h"
#include "CeylanLogAggregatorHTML.h"
#include "CeylanLogAggregatorRaw.h"
#include "CeylanLogChannel.h"
#include "CeylanLogHolder.h"
#include "CeylanLogListener.h"
#include "CeylanLogMessage.h"
#include "CeylanLogPlug.h"
#include "CeylanLogPlugClassical.h"
#include "CeylanLogPlugConsole.h"
#include "CeylanLogPlugHTML.h"
#include "CeylanLogSource.h"
#include "CeylanLogTransport.h"
#include "CeylanLogTransportListenerRaw.h"


/*
 * Private non-exported non-installed headers : 
 *   - CeylanLogLight.h : only for internal debugging purposes
 *   - CeylanLogAggregatorHTMLFragments.h : made to be included by 
 * some implementation files only (CeylanLogAggregatorHTML.cc)
 *
 */


#endif // CEYLAN_LOG_INCLUDES_H_
