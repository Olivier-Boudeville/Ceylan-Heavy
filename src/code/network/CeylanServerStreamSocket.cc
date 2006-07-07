#include "CeylanServerStream.h"



#include "Server.h"
#include "System.h"
#include "Thread.h"

extern "C" int errno;

extern "C"
{
#include <unistd.h>
}



ServerStreamSocket::ServerStreamSocketException::ServerStreamSocketException( 
		const std::string & reason ) throw():
	StreamSocketException( reason )
{

}


ServerStreamSocket::ServerStreamSocketException::~ServerStreamSocketException()
	throw()
{

}



Server::Server( int port, bool reuse ):
	Socket ( port ),
	_nfdes ( 0 ),
	_client(),
	_bind  ( false )
{
	if( reuse )
	{
		int i = 1;
		::setsockopt( getFileDescriptor(), SOL_SOCKET, SO_REUSEADDR, reinterpret_cast<char *>(&i), sizeof(i) );
	}
}

Server::~Server()
{
	::close( _nfdes );
}

bool Server::prepareToAccept()
{
	if( ! _bind )
	{

		getAddress().sin_addr.s_addr = htonl( INADDR_ANY );
 
 		int i = 0;
		for( ; i < 5; i++ )
		{
			if( ! ::bind( getFileDescriptor(), (sockaddr*)(& getAddress()), sizeof( sockaddr_in ) ) ) break;
			Thread::sleep( 1 );
		}
		
		if( i == 5 )
        	{
			setError( errno );
			bindFailed();
			return false;
        	}

		if( ::listen( getFileDescriptor(), 10 ) == -1 )
		{
			listenFailed();
			setError( errno );
			return false;
		}
		
		_bind = true;
	
	}
	return true;
}

bool Server::accept()
{
	prepareToAccept();
	
	socklen_t sz = (socklen_t)sizeof( _client );
	
	_nfdes = ::accept( getFileDescriptor(), (sockaddr*)(& _client), &sz );
	if( _nfdes == -1 )
	{
		setError( errno );
		acceptFailed();
		return false;
	}
	else
	{
		accepted();
	}
	return _nfdes > 0;
}

int Server::getFD() const
{
	return _nfdes;
}

void Server::accepted()
{
}

void Server::bindFailed()
{
	cerr << "bind@Server::prepareToAccept() failed: " << System::explainError( getError() ) << endl;
}

void Server::listenFailed()
{
	cerr << "listen@Server::prepareToAccept() failed: " << System::explainError( getError() ) << endl;
}

void Server::acceptFailed()
{
	cerr << "accept@Server::accept() failed: " << System::explainError( getError() ) << endl;
}
