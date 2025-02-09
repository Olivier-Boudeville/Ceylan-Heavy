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


#ifndef CEYLAN_DISPLAYABLE_H_
#define CEYLAN_DISPLAYABLE_H_


namespace Ceylan
{



	/// Verbosity levels, used for instance by toString methods.
	enum VerbosityLevels { low, medium, high } ;



    /**
     * Interface that every object which can be displayed should implement. 
     *
     * Most objects implement that interface. 
	 *
	 * Most of the time, they implement it indirectly, i.e. because they
	 * inherited it.
	 *
	 * @note Copy constructor and assignment operator cannot be private,
	 * because some Displayable instances need to rely on their copy
	 * constructor, for example for operators returning them by value.
	 * 
     */
    class CEYLAN_DLL Displayable
    {

        public:


            /// Void interface, just used to ensure strong typing.


			/// Do-nothing constructor.
			Displayable()
			{
			
			}
			
			
			/// Do-nothing virtual destructor.
			virtual ~Displayable()
			{
			
			}
			
			
			

		private:
		
		
			/**
			 * Copy constructor cannot be private since it is needed in 
			 * some cases (return by value).
			 *
			 * Displayable( const Displayable & source ) throw() ;
			 *
			 */			 
			
			
			/**
			 * Assignment operator should not be private since it is useful
			 * in some cases.
			 * 
			 * Displayable & operator = ( const Displayable & source ) throw() ;
	    	 *
			 */
			 
    } ;

}


#endif // CEYLAN_DISPLAYABLE_H_

