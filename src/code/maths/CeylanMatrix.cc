#include "CeylanMatrix.h"   


using std::string ;


using namespace Ceylan::Maths::Linear ;



Matrix::Matrix() throw() 
{
	
}


Matrix::~Matrix() throw() 
{
}


void Matrix::nullify() throw()
{
	setAllElementsTo( 0 ) ;
}


void Matrix::setToIdentity() throw()
{
	setToDiagonal( 1 ) ;
}


const string Matrix::toString( VerbosityLevels level ) const throw()
{
	
	return "Abstract matrix"  ;	

}

