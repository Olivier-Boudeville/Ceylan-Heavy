#include "CeylanXMLVisitor.h"

#include "CeylanOutputStream.h"     // for OutputStream
#include "CeylanXMLElement.h"       // for XLMMarkup, etc.
#include "CeylanOperators.h"        // for toString
#include "CeylanLogPlug.h"          // for LogPlug



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


string XMLSavingVisitor::OffsetForMarkup = "\t" ;


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

	//Log::LogPlug::trace( "XMLSavingVisitor::incrementHeight" ) ;

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
	
	//Log::LogPlug::trace( "XMLSavingVisitor::decrementHeight" ) ;

	ClosingMarkup popped = _markupsToClose.top() ;
	_markupsToClose.pop() ;
	
	if ( popped.empty() )
		return ;
		
	Height offset = getHeight() ;
	
	for ( Size s = 0; s < offset; s++ )
		_output->write( OffsetForMarkup ) ;
	
	_output->write( popped + '\n') ;

	
}


void XMLSavingVisitor::visit( const XMLMarkup & xmlMarkup ) 
	throw( VisitException )
{
	
	/*
	Log::LogPlug::trace( "XMLSavingVisitor::visiting markup " 
		+ xmlMarkup.toString() ) ;
	*/
	
	Height offset = getHeight() ;

	_markupsToClose.push( xmlMarkup.getClosingMarkup() ) ;

		
	for ( Size s = 0; s < offset; s++ )
		_output->write( OffsetForMarkup ) ;
	
	_output->write( xmlMarkup.toString() + '\n' ) ;
	
		
}	


void XMLSavingVisitor::visit( const XMLText & xmlText ) 
	throw( VisitException )
{
	
	/*
	Log::LogPlug::trace( "XMLSavingVisitor::visiting text " 
		+ xmlText.toString() ) ;
	*/

	Height offset = getHeight() ;

	_markupsToClose.push( "" ) ;

		
	for ( Size s = 0; s < offset; s++ )
		_output->write( OffsetForMarkup ) ;
	
	_output->write( xmlText.toString() + '\n' ) ;
	
		
}	

				
const string XMLSavingVisitor::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

	string res = "XML saving visitor, " ;
	
	if ( _output != 0 )
		res += "using as output stream " + _output->toString( level ) ;
	else
		res += " with no specific output stream registered" ;
		
	if ( level == Ceylan::low )
		return res ;
	
	res += ". Markup stack " ;
	
	if ( _markupsToClose.empty() )
	{
		res += "is empty" ; 
	}
	else
	{
		res += "has " + Ceylan::toString( _markupsToClose.size() )
			+ " element(s)" ;
	}
		
	return res ;

}
