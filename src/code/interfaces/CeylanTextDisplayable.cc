#include "CeylanTextDisplayable.h"  

#include "CeylanStringUtils.h"      // for formatStringList



using std::list ;
using std::string ; 

using namespace Ceylan ;


// Output format defaults to raw text.
TextDisplayable::TextOutputFormat TextDisplayable::_OutputFormat = rawText ;


const std::string TextDisplayable::ToString( 
		list<TextDisplayable*> displayables, Ceylan::VerbosityLevels level ) 
	throw()
{

	string res = "Description of TextDisplayable list is : " ;
	
	list<string> descriptionList ;
	 
	for( list<TextDisplayable*>::const_iterator it = displayables.begin(); 
			it != displayables.end(); it++ )
		descriptionList.push_back( (*it)->toString( level ) ) ;
		
	return res + formatStringList( descriptionList ) ;
	
}


TextDisplayable::TextOutputFormat TextDisplayable::GetOutputFormat() throw()
{
	return _OutputFormat ;
}


void TextDisplayable::SetOutputFormat( TextOutputFormat newOutputFormat ) throw()
{
	_OutputFormat = newOutputFormat ;
}


std::ostream & operator << ( std::ostream & os, const Ceylan::TextDisplayable & textDisplayable )
	throw() 
{
    return os << textDisplayable.toString( high ) ;
}
	
