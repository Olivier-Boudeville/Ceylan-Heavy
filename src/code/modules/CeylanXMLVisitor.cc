#include "CeylanXMLVisitor.h"

#include "CeylanOutputStream.h"     // for OutputStream
#include "CeylanXMLElement.h"       // for XLMMarkup, etc.



using namespace Ceylan ;
using namespace Ceylan::System ;
using namespace Ceylan::XML ;

using std::string ;





XMLVisitorException::XMLVisitorException( 
		const std::string & reason ) throw() : 
	XMLException( reason )
{

}


XMLVisitorException::~XMLVisitorException() throw()
{

}





// XMLVisitor section.




XMLVisitor::XMLVisitor() throw()
{

}


XMLVisitor::~XMLVisitor() throw()
{

}


const string XMLVisitor::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

	return "XML visitor" ;

}




// XMLSavingVisitor section.


XMLSavingVisitor::XMLSavingVisitor( System::OutputStream & output ) throw():
	_output( &output )
{

}


XMLSavingVisitor::~XMLSavingVisitor() throw()
{

	// _output stream not owned.
	
}


void XMLSavingVisitor::incrementHeight() throw()
{


	/*
	 * Does nothing, as the information is already stored in the markup stack,
	 * when the visit of an enclosing element occurred.
	 *
	 */
	
}


Ceylan::Height XMLSavingVisitor::getHeight() const throw()
{

	return _markupsToClose.size() ;
}


void XMLSavingVisitor::decrementHeight() throw()
{
	
	Height offset = getHeight() ;
	
	for ( Size s = 0; s < offset; s++ )
		_output->write( /* no char accepted */ "\t" ) ;
	
	_output->write( _markupsToClose.top() ) ;

	_markupsToClose.pop() ;
	
}


void XMLSavingVisitor::visit( const XMLMarkup & xmlMarkup ) 
					throw( VisitException )
{
	
	_markupsToClose.push( xmlMarkup.getClosingMarkup() ) ;

	Height offset = _markupsToClose.size() ;
		
	for ( Size s = 0; s < offset; s++ )
		_output->write( /* no char accepted */ "\t" ) ;
	
	_output->write( xmlMarkup.toString() ) ;
	
		
}					
