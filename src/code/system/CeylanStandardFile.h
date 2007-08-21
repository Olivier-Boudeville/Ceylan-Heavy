#ifndef CEYLAN_STANDARD_FILE_H_
#define CEYLAN_STANDARD_FILE_H_


#include "CeylanFile.h"          // for inheritance



/**
 * Fallback file implementation, always included to avoid dependency on a 
 * config.h-style header.
 *
 * #include <iosfwd> cannot be used since we need real (not forward)
 * declarations.
 *
 */
#include <fstream>               // for std::fstream, ios_base
#include <ctime>                 // for time_t
#include <string>




namespace Ceylan
{


	namespace System
	{



		/**
		 * Encapsulates standard files, as provided by usual operating systems,
		 * i.e. based on file descriptors on the UNIX platforms, otherwise, for
		 * example on Windows, using the standard C++ library (std::fstream
		 * and al).
		 *
		 * Actual files should be created and opened with respectively the
		 * File::Create and File::Open factories.
		 *
		 * @note Depending on the platform support, some primitives may not
		 * be available, which results in FileException being raised
		 * whenever called. The reason for that is either the underlying
		 * platform is unable to provide these features, or the Ceylan
		 * porting effort did not manage them for the moment.
		 *
		 * @see following feature symbols to spot the actual support 
		 * beforehand:
		 *   - Features::areAdvancedFileAttributesSupported
		 *   - Features::areSymbolicLinksSupported
		 *   - Features::areFileDescriptorsSupported
		 *   - Features::areFileLocksSupported
		 *
		 * @note This class has restricted capabilities if the file 
		 * descriptor feature is not available.
		 * 
		 * Not all methods are available on all platforms. Hence when a feature
		 * is disabled, they raise a FeatureNotAvailableException.
		 *
		 */
		class CEYLAN_DLL StandardFile: public File
		{
		
				
			/**
			 * Opaque handle for forward-declared but undefined struct
			 * pointer to system permission flag, used to avoid
			 * including system-specific headers which define for 
			 * example mode_t.
			 *
			 * Otherwise the API exposed by Ceylan would depend on these
			 * headers, then on a config.h that should then be installed
			 * but may clash with others, and so on.
			 *
			 */
			struct SystemSpecificPermissionFlag ;



			public:



				/// Mother class for all exceptions related to standard files.
				class CEYLAN_DLL StandardFileException: public FileException
				{ 
					public: 
					
						explicit StandardFileException( 
							const std::string & reason ) throw() ;
						
						virtual ~StandardFileException() throw() ; 
							
				} ;

	
	
				class ConversionFailed: public StandardFileException
				{ 
				
					public: 
					
						explicit ConversionFailed( 
							const std::string & reason ) throw() ; 
							
				} ;


				/**
				 * Closes the file for read/write actions.
				 *
				 * @return true iff an operation had to be performed.
				 *
				 * @throw CloseException if the close operation failed, 
				 * including if the file was not already opened.
				 *
				 */
				virtual bool close() throw( Stream::CloseException ) ;


				/**
				 * Sends the file content to the <b>fd</b> file descriptor
				 * stream.
				 *
				 * @throw FeatureNotAvailableException if the file descriptor
				 * feature is not available.
				 *		 
				 */
				virtual void serialize( FileDescriptor fd ) const
					throw( Features::FeatureNotAvailableException ) ;



				/**
				 * Saves the file under a new name.
				 *
				 * @param newName the name of the newly copied file.
				 *
				 * @throw FileException if the operation failed.
				 *
				 */
				virtual void saveAs( const std::string & newName )
					throw( FileException ) ;



				// Locking section.

				 
				/**
				 * Locks the file for reading.
				 *
				 * @throw ReadLockingFailed if the operation failed or if the
				 * file lock feature is not available.
				 *		 
				 */
				virtual void lockForReading() const 
					throw( ReadLockingFailed ) ;


				/**
				 * Unlocks the file for reading.
				 *
				 * @throw ReadUnlockingFailed if the operation failed or if the
				 * file lock feature is not available.
				 *		 
				 */
				virtual void unlockForReading() const 
					throw( ReadUnlockingFailed ) ;


				/**
				 * Locks the file for writing.
				 *
				 * @throw WriteLockingFailed if the operation failed or if the
				 * file lock feature is not available.
				 *		 
				 */
				virtual void lockForWriting() const 
					throw( WriteLockingFailed ) ;


				/**
				 * Unlocks the file for writing.
				 *
				 * @throw WriteUnlockingFailed if the operation failed or if the
				 * file lock feature is not available.
				 *		 
				 */
				virtual void unlockForWriting() const 
					throw( WriteUnlockingFailed ) ;


				/**
				 * Tells whether the file is locked.
				 *
				 * @return true if the lock feature is available and the 
				 * file is locked, otherwise returns false, i.e. if the file
				 * is locked or if the lock feature is not available.
				 *		 
				 */
				virtual bool isLocked() const throw() ;



				/**
				 * Returns the file size, in bytes.
				 *
				 * @see GetSize
				 *
				 */
				virtual Size size() const 
					throw( FileSystemManager::CouldNotStatEntry ) ;



				/**
				 * Reads up to maxLength bytes from this file to specified
				 * buffer.
				 *
				 * @param buffer the buffer where to store read bytes. 
				 * Its size must be at least maxLength bytes.
				 *
				 * @param maxLength the maximum number of bytes that should 
				 * be read.
				 *
				 * @return The number of bytes actually read, which should
				 * be maxLength or lower.
				 *
				 * @throw ReadFailed if a read error occurred.
				 *
				 * @note May be unable to read the full content of a file 
				 * if the file was open without the 'Binary' flag (hence
				 * in text mode) and if in the file content it occurs
				 * that accidentally some bytes form an 'end of file'
				 * marker (despite some bytes remain to be read past
				 * this marker).
				 *
				 */
		 		virtual Size read( Ceylan::Byte * buffer, Size maxLength ) 
					throw( InputStream::ReadFailedException ) ;

				
				// readExactLength inherited.
								
				
				/**
				 * Writes message to this file.
				 *
				 * @param message the message to write to this file.
				 *
				 * @return The number of bytes actually written, which 
				 * should be equal to the size of the string or lower.
				 *
				 * @throw WriteFailed if a write error occurred.
				 *
				 */
				virtual Size write( const std::string & message ) 
					throw( OutputStream::WriteFailedException ) ;


				/**
				 * Writes up to maxLength bytes from the specified buffer
				 * to this file.
				 *
				 * @param buffer the buffer where to find bytes that must
				 * be written to this file.
				 * Its size must be at least maxLength bytes.
				 *
				 * @return The number of bytes actually written, which 
				 * should be equal to maxLength.
				 *
				 * @throw WriteFailed if a write error occurred.
				 *
				 */
				virtual Size write( const Ceylan::Byte * buffer, 
						Size maxLength ) 
					throw( OutputStream::WriteFailedException ) ;




				/**
				 * Returns the latest change time of this standard file.
				 *
				 * @throw FileSystemManager::GetChangeTimeFailed if the 
				 * operation failed, or is not supported.
				 *
				 */
				virtual time_t getLastChangeTime() const 
					throw( FileSystemManager::GetChangeTimeFailed ) ;


				/**
				 * Returns the stream id, its file descriptor.
				 *
				 * @throw FeatureNotAvailableException if the file descriptor
				 * feature is not available.
				 *
				 */
				FileDescriptor getFileDescriptor() const 
					throw( Features::FeatureNotAvailableException ) ;


				/**
				 * Removes this file from disk.
				 *
				 * Closes it if necessary. No other operation should be 
				 * performed 
				 *
				 * @throw RemoveFailed if the operation failed or is not
				 * supported on this platform.
				 *
				 */
				virtual void remove() 
					throw( FileSystemManager::RemoveFailed ) ;



				// Interface implementation.


				/**
				 * Returns this file's ID.
				 *
				 * @return the StreamID or -1 if nothing appropriate can
				 * be returned with the available features.
				 *
				 */
				virtual StreamID getStreamID() const throw() ;


            	/**
            	 * Returns an user-friendly description of the state of
				 * this object.
            	 *
				 * @param level the requested verbosity level.
				 *
				 * @note Text output format is determined from overall 
				 * settings.
				 *
				 * @see TextDisplayable
				 *
				 */
            	virtual const std::string toString( 
					Ceylan::VerbosityLevels level = Ceylan::high ) 
						const throw() ;


				
				/**
				 * Interprets the current state of the specified ifstream.
				 *
				 */
				static std::string InterpretState( 
					const std::ifstream & inputFile ) throw() ;
				
				
				/**
				 * Interprets the current state of the specified fstream.
				 *
				 * @note Despite a fstream should be a ifstream, since
				 * it is supposed to be one of its child classes, the
				 * InterpretState for ifstream cannot be used with 
				 * fstream instances.
				 *
				 */
				static std::string InterpretState( 
					const std::fstream & inputFile ) throw() ;
								


			protected:



				/**
				 * Constructs a standard file reference object.
				 * By default, it creates a new file on disk, if the name
				 * corresponds to an already-existing file, it will be
				 * truncated and overwritten.
				 *
				 * @param name the name of the file.
				 *
				 * @param openFlag the flag describing the opening mode.
				 *
				 * @param permissionFlag the flag describing the
				 * requested permissions, if this file is to be created.
				 * Otherwise (if the file already exists), this parameter
				 * is ignored.
				 *
				 * @see OpeningFlag, PermissionFlag
				 *
				 * @note If not specifically set, the file is open in text 
				 * mode: one should not forget to add the Binary flag. 
				 * The mistake can be detected when basic read() returns less
				 * than the requested size, or when readExactLength() never
				 * terminates.
				 *
				 * @note This constructor should not be called directly, the
				 * File factories (File::Create and File::Open) should be 
				 * used instead, as they allow to write code really independant
				 * from the running platform, not having to choose between the
				 * per-platform constructors.
				 *
				 */
				explicit StandardFile( const std::string & name, 
						OpeningFlag openFlag = CreateToWriteBinary,
						PermissionFlag permissionFlag = OwnerReadWrite ) 
					throw( CouldNotOpen ) ;



				/**
				 * Constructs a standard file with size <b>length</b> from file
				 * descriptor <b>fd</b>.
				 *
				 * @param name the name of the file.
				 *
				 * @param length the file size.
				 *
				 * @param fd the file descriptor to use.
				 *
				 * @param permissionFlag the flag describing the
				 * requested permissions, if this file is to be created.
				 *
				 * @note Very useful to copy files from streams: socket, 
				 * file, pipe.
				 *
				 * @throw Various exception on failure, including
				 * FeatureNotAvailableException if the file descriptor
				 * feature is not available.
				 *
				 * @note This constructor should not be called directly, the
				 * File factories (File::Create and File::Open) should be 
				 * used instead, as they allow to write code really independant
				 * from the running platform, not having to choose between the
				 * per-platform constructors.
				 *
				 */
				StandardFile(   
						const std::string & name, 
						Size length, 
						FileDescriptor fd,
						PermissionFlag permissionFlag = OwnerReadWrite )
					throw( CouldNotCreate, ReadFailed, WriteFailed,
						Features::FeatureNotAvailableException ) ;



				/**
				 * Destroys the file reference object, does not remove the
				 * file itself.
				 *
				 * @note Will close automatically the file if needed.
				 *
				 * To remove the file from disk, use remove().
				 *
				 * @see remove
				 *
				 */
				virtual ~StandardFile() throw() ;
								 


				/// Tries to reopen file.
				void reopen() throw( CouldNotOpen ) ;


				/// Interprets the current state of this file.
				std::string interpretState() const throw() ;
				


				/**
				 * Converts specified Ceylan opening flag into a valid 
				 * opening flag to be used with file descriptor 
				 * functions.
				 *
				 * @param openFlag the Ceylan opening flag to convert 
				 * to lower-level flag.
				 *
				 * @return the converted opening flag.
				 *
				 * @throw ConversionFailed if the mapping failed, or
				 * FeatureNotAvailableException is the file descriptor 
				 * feature is not available.
				 *
				 */
				static int ConvertToFileDescriptorOpenFlag( 
						OpeningFlag openFlag ) 
					throw( ConversionFailed,
						Features::FeatureNotAvailableException ) ;  
				
				 
				/**
				 * Converts specified Ceylan permission flag into a valid 
				 * permission flag to be used with file descriptor 
				 * functions.
				 *
				 * @param permissionFlag the Ceylan permission flag to 
				 * convert to lower-level flag.
				 *
				 * @param returned the structure that will be filled by 
				 * this method.
				 *
				 * @return nothing, but the effective result is in
				 * the parameter 'returned'.
				 *
				 * @throw ConversionFailed if the mapping failed, or
				 * FeatureNotAvailableException is the file descriptor 
				 * feature is not available.
				 *
				 */
				static void ConvertToFileDescriptorPermissionFlag( 
						PermissionFlag permissionFlag,
						struct SystemSpecificPermissionFlag & returned ) 
					throw( ConversionFailed,
						Features::FeatureNotAvailableException ) ;  
				



				/**
				 * Converts specified Ceylan opening flag into a valid 
				 * opening flag to be used with C++ streams.
				 *
				 * @param openFlag the Ceylan opening flag to convert 
				 * to lower-level flag.
				 *
				 * @param filename the filename these flags applied to,
				 * since it might be needed to check whether the file
				 * already exists, in the case no special creation is
				 * requested.
				 *
				 * @return the converted opening flag.
				 *
				 * @throw ConversionFailed if the mapping failed.
				 *
				 */
				static std::ios_base::openmode ConvertToStreamOpenFlag( 
						OpeningFlag openFlag,
						const std::string & filename ) 
					throw( ConversionFailed ) ;  
				
				
				/**
				 * Converts specified Ceylan permission flag into a valid 
				 * permission flag to be used with C++ streams.
				 *
				 * @param permissionFlag the Ceylan permission flag to 
				 * convert to lower-level flag.
				 *
				 * @return the converted permission flag.
				 *
				 * @throw ConversionFailed if the mapping failed.
				 *
				 * @note Currently disabled since permission specifications 
				 * are not supported by the underlying C++ stream layer.
				 *
				static FIXME ConvertToStreamPermissionFlag( 
						PermissionFlag permissionFlag ) 
					throw( ConversionFailed ) ;  
				 */



			private:



				/**
				 * Copy constructor made private to ensure that it will 
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				StandardFile( const StandardFile & source ) throw() ;


				/**
				 * Assignment operator made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 *
				 */
				StandardFile & operator = ( const StandardFile & source )
					throw() ;


				/**
				 * Internal file descriptor, used if this feature is 
				 * available.
				 *
				 */
				FileDescriptor _fdes ;


				/**
				 * Internal file input/output stream, used in case the
				 * file descriptor feature is not available.
				 *
				 */
				std::fstream _fstream ;
					


				/// Transfers bytes from a file descriptor to another.
				static void FromFDtoFD( FileDescriptor from, 
						FileDescriptor to, Size length )
					throw( IOException, 
						Features::FeatureNotAvailableException ) ;


		} ;
		
	}

}


#endif // CEYLAN_STANDARD_FILE_H_
