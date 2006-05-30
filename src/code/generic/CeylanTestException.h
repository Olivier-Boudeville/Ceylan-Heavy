#ifndef CEYLAN_TEST_EXCEPTION_H_
#define CEYLAN_TEST_EXCEPTION_H_


#include "CeylanException.h"  // for inheritance


namespace Ceylan
{


	/**
	 * This Exception subclass is to be used only for test results.
	 *
	 */
    class TestException : public Ceylan::Exception
    {


        public:


            /**
             * Basic constructor.
             *
             * @param reason an explanation for this exception being raised.
             *
             */
            explicit TestException( const std::string & reason ) throw() ;


            /// Basic virtual destructor.
            virtual ~TestException() throw() ;



    } ;

}


#endif // CEYLAN_TEST_EXCEPTION_H_
