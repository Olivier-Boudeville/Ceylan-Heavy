#ifndef CEYLAN_FILE_H_
#define CEYLAN_FILE_H_


#include "CeylanSystem.h"        // for Size, SignedSize, etc.
#include "CeylanInputStream.h"   // for inheritance
#include "CeylanOutputStream.h"  // for inheritance
#include "CeylanFeatures.h"      // for FeatureNotAvailableException



/**
 * Fallback file implementation, always included to avoid dependency on a 
 * config.h-style header.
 *
 * #include <iosfwd> cannot be used since we need real (not forward)
 * declarations.
 *
 */
#include <fstream>               // for std::fstream, ios_base

#include <string>




namespace Ceylan
{


	namespace System
	{



		/**
		 * File opening openFlags, they can be OR'ed.
		 *
		 * At least one of the { Read ; Write } pair must be set.
		 *
		 */
		typedef Ceylan::Uint16 OpeningFlag ;



		/**
		 * File creation abstract permissions, to be used in case a new 
		 * file is created.
		 *
		 * Permissions can be OR'ed.
		 *
		 * Actual permissions can be modified by the umask of the process,
		 * according to (actualPermissionFlag & ~umask).
		 *
		 * @note When the file descriptor and/or the advanced file attribute
		 * features are not enabled, the permissions which are not managed
		 * are ignored.
		 *
		 */
		typedef Ceylan::Uint16 PermissionFlag ;



		/**
		 * Simple file object manipulation class.
		 *
		 * @see Directory
		 *
		 * @note Depending on the platform support, some primitives may not
		 * be available, which results in FileException being raised
		 * whenever called. The reason for that is either the underlying
		 * platform is unable to provide these features, or the Ceylan
		 * porting effort did not manage them for the moment.
		 *
		 * @see following feature symbols to spot the actual support 
		 * beforehand :
		 *   - Features::areAdvancedFileAttributesSupported
		 *   - Features::areSymbolicLinksSupported
		 *   - Features::areFileDescriptorsSupported
		 *   - Features::areFileLocksSupported
		 *
		 * @note This class has restricted capabilities if the file 
		 * descriptor feature is not available.
		 * 
		 * Not all methods are available on all platforms. Hence
		 * when a feature is disabled, they raise a 
		 * FeatureNotAvailableException.
		 *
		 */
		class File: public InputStream, public OutputStream
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



				/// Mother class for all file-related exceptions.
				class FileException: public SystemException
				{ 
					public: 
					
						explicit FileException( 
								const std::string & reason ) throw() ; 
				} ;



				// Full declarations preferred to preprocessor macros.
				
				
				class CouldNotOpen: public FileException
				{ 
					public: 
					
						explicit CouldNotOpen( 
								const std::string & reason ) throw() ; 
				} ;


				class CouldNotCreate: public FileException
				{ 
					public: 
					
						explicit CouldNotCreate( 
								const std::string & reason ) throw() ; 
				} ;


				class CouldNotStatFile: public FileException
				{ 
					public: 
						
						explicit CouldNotStatFile( 
							const std::string & reason ) throw() ; 
				} ;


				class ReadFailed: public FileException
				{ 
					public: 
					
						explicit ReadFailed( 
								const std::string & reason ) throw() ; 
				} ;


				class WriteFailed: public FileException
				{ 
					public: 
					
						explicit WriteFailed( 
								const std::string & reason ) throw() ; 
				} ;


				class ReadLockingFailed: public FileException
				{ 
					public: 
					
						explicit ReadLockingFailed( 
							const std::string & reason ) throw() ; 
				} ;


				class ReadUnlockingFailed: public FileException
				{ 
					public: 
					
						explicit ReadUnlockingFailed( 
							const std::string & reason ) throw() ; 
				} ;


				class WriteLockingFailed: public FileException
				{ 
					public: 
					
						explicit WriteLockingFailed( 
							const std::string & reason ) throw() ; 
				} ;


				class WriteUnlockingFailed: public FileException
				{ 
					public: 
					
						explicit WriteUnlockingFailed( 
							const std::string & reason ) throw() ; 
				} ;


				class TouchFailed: public FileException
				{ 
				
					public: 
					
						explicit TouchFailed( 
								const std::string & reason ) throw() ; 
				} ;


				class AlreadyOpened: public FileException
				{ 
				
					public: 
					
						explicit AlreadyOpened( 
								const std::string & reason ) throw() ; 
				} ;


				class RemoveFailed: public FileException
				{ 
				
					public: 
					
						explicit RemoveFailed( 
								const std::string & reason ) throw() ; 
				} ;


				class GetChangeTimeFailed : public FileException
				{ 
				
					public: 
					
						explicit GetChangeTimeFailed( 
							const std::string & reason ) throw() ; 
				} ;


				class CouldNotDuplicate : public FileException
				{ 
				
					public: 
					
						explicit CouldNotDuplicate( 
							const std::string & reason ) throw() ; 
				} ;


				class UnlinkFailed : public FileException
				{ 
				
					public: 
					
						explicit UnlinkFailed( 
								const std::string & reason ) throw() ; 
				} ;


				class SymlinkFailed : public FileException
				{ 
				
					public: 
					
						explicit SymlinkFailed( 
								const std::string & reason ) throw() ; 
				} ;


				class CloseFailed: public FileException
				{ 
				
					public: 
					
						explicit CloseFailed( 
							const std::string & reason ) throw() ; 
							
				} ;


				class MoveFailed: public FileException
				{ 
				
					public: 
					
						explicit MoveFailed( 
							const std::string & reason ) throw() ;
							 
				} ;


				class CopyFailed: public FileException
				{ 
				
					public: 
					
						explicit CopyFailed( 
							const std::string & reason ) throw() ; 
							
				} ;


				class ConversionFailed: public FileException
				{ 
				
					public: 
					
						explicit ConversionFailed( 
							const std::string & reason ) throw() ; 
							
				} ;


				
				// Allows read operations on the opened file. 
				static const OpeningFlag Read ;
				
				// Allows write operations on the opened file.
				static const OpeningFlag Write ;
				
				
				// Creates the file, if it does not exist.
				static const OpeningFlag Create ;
				
				
				// Truncates file to length zero when opening.
				static const OpeningFlag Truncate ;
				
				// Seeks to the end of the file before each write operation.
				static const OpeningFlag Append ;
				
								
				// Opens the file in binary rather than in text mode.
				static const OpeningFlag Binary ;
				
				// Does not open the underlying file object.
				static const OpeningFlag DoNotOpen ;
				
				
				/*
				 * Creates the file for writing, it is a convenience 
				 * shortcut equal to the very common combination :
				 * 'Read | Write | Create | Truncate | Binary'.
				 *
				 */
				static const OpeningFlag CreateToWriteBinary ;
				
								
				
				/**
				 * Opens the file in non-blocking mode, when possible.
				 *
				 * Neither the opening nor any subsequent operations on
				 * this file will cause the calling process to wait.
				 *
				 * @note Only taken into account if the advanced file 
				 * attribute feature is enabled.
				 *
				 */
				static const OpeningFlag NonBlocking ;
				
				
				/**
				 * Opens the file in synchronous mode.
				 *
				 * Any write operation on this file will block the
				 * calling process until the data has been physically 
				 * written to the underlying hardware.
				 *
				 * @note Mostly taken into account if the advanced file 
				 * attribute feature is enabled, even though our default C++
				 * file stream-base implementation tries to manage it as well.
				 *
				 */
				static const OpeningFlag Synchronous ;
				


				/*
				 * Open flags for 'Owner'.
				 *
				 */


				/// Allows its owner to read the created file.
				static const PermissionFlag OwnerRead ;
				
				/// Allows its owner to write to the created file.
				static const PermissionFlag OwnerWrite ;

				/// Allows its owner to execute the created file.
				static const PermissionFlag OwnerExec ;
				
				
				/**
				 * Provided for convenience : 
				 * OwnerReadWrite = OwnerRead | OwnerWrite.
				 *
				 */
				static const PermissionFlag OwnerReadWrite ;
				
				
				/**
				 * Provided for convenience : 
				 * OwnerReadWriteExec = OwnerReadWrite | OwnerExec.
				 *
				 */
				static const PermissionFlag OwnerReadWriteExec ;
				



				/*
				 * Open flags for 'Group'.
				 *
				 * @note They are only taken into account if the advanced 
				 * file attribute feature is enabled.
				 *
				 */


				/**
				 * Allows the group to read the created file.
				 *
				 * @note Only taken into account if the advanced file 
				 * attribute feature is enabled.
				 *
				 */
				static const PermissionFlag GroupRead ;
				
				
				/**
				 * Allows the group to write the created file.
				 *
				 * @note Only taken into account if the advanced file 
				 * attribute feature is enabled.
				 *
				 */
				static const PermissionFlag GroupWrite ;


				/**
				 * Allows the group to execute the created file.
				 *
				 * @note Only taken into account if the advanced file 
				 * attribute feature is enabled.
				 *
				 */
				static const PermissionFlag GroupExec ;
				
				
				/**
				 * Provided for convenience : 
				 * GroupReadWrite = GroupRead | GroupWrite.
				 *
				 * @note Only taken into account if the advanced file 
				 * attribute feature is enabled.
				 *
				 */
				static const PermissionFlag GroupReadWrite ;
				
				
				/**
				 * Provided for convenience : 
				 * GroupReadWriteExec = GroupReadWrite | GroupExec.
				 *
				 * @note Only taken into account if the advanced file 
				 * attribute feature is enabled.
				 *
				 */
				static const PermissionFlag GroupReadWriteExec ;
				


				/*
				 * Open flags for 'Others'.
				 *
				 * @note They are only taken into account if the advanced 
				 * file attribute feature is enabled.
				 *
				 */


				/**
				 * Allows the others (not owner, not group) to read the 
				 * created file.
				 *
				 * @note Only taken into account if the advanced file 
				 * attribute feature is enabled.
				 *
				 */
				static const PermissionFlag OthersRead ;
				
				
				/**
				 * Allows the others (not owner, not group) to write
				 * to the created file.
				 *
				 * @note Only taken into account if the advanced file 
				 * attribute feature is enabled.
				 *
				 */
				static const PermissionFlag OthersWrite ;


				/**
				 * Allows the others (not owner, not group) to execute
				 * the created file.
				 *
				 * @note Only taken into account if the advanced file 
				 * attribute feature is enabled.
				 *
				 */
				static const PermissionFlag OthersExec ;
				
				
				/**
				 * Provided for convenience : 
				 * OthersReadWrite = OthersRead | OthersWrite.
				 *
				 * @note Only taken into account if the advanced file 
				 * attribute feature is enabled.
				 *
				 */
				static const PermissionFlag OthersReadWrite ;
				
				
				/**
				 * Provided for convenience : 
				 * OthersReadWriteExec = OthersReadWrite | OthersExec.
				 *
				 * @note Only taken into account if the advanced file 
				 * attribute feature is enabled.
				 *
				 */
				static const PermissionFlag OthersReadWriteExec ;
				
		

				/**
				 * Constructs a file reference object.
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
				 * @example :
				 * <pre>
				 * File f( "myfilename", Read ) ;
				 * ...
				 * f.read( buf, 100 ) ;
				 * ...
				 * </pre>
				 *
				 */
				explicit File( const std::string & name, 
						OpeningFlag openFlag = CreateToWriteBinary,
						PermissionFlag permissionFlag = OwnerReadWrite ) 
					throw( CouldNotOpen ) ;



				/**
				 * Constructs a file with size <b>length</b> from file
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
				 * @note Very useful to copy files from streams : socket, 
				 * file, pipe.
				 *
				 * @throw Various exception on failure, including
				 * FeatureNotAvailableException if the file descriptor
				 * feature is not available.
				 *
				 */
				File(   const std::string & name, 
						Size length, 
						FileDescriptor fd,
						PermissionFlag permissionFlag = OwnerReadWrite )
					throw( CouldNotCreate, ReadFailed, WriteFailed,
						Features::FeatureNotAvailableException ) ;



				/**
				 * Destroys the file reference object, does not remove the
				 * file itself.
				 *
				 * To remove the file from disk, use remove().
				 *
				 * @see remove
				 *
				 */
				virtual ~File() throw() ;


				/// Returns this file's name.
				inline const std::string & getName() const throw()
				{ return _name; }


				/**
				 * Closes the file for read/write actions.
				 *
				 */
				virtual void close() throw( CloseFailed ) ;


				/**
				 * Removes that file from disk.
				 *
				 */
				virtual void remove() throw( RemoveFailed ) ;


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



				 
				/**
				 * Locks the file for reading.
				 *
				 * @throw FeatureNotAvailableException if the file lock
				 * feature is not available.
				 *		 
				 */
				virtual void lockForReading() const 
					throw( ReadLockingFailed,
						Features::FeatureNotAvailableException ) ;


				/**
				 * Unlocks the file for reading.
				 *
				 * @throw FeatureNotAvailableException if the file lock
				 * feature is not available.
				 *		 
				 */
				virtual void unlockForReading() const 
					throw( ReadUnlockingFailed,
						Features::FeatureNotAvailableException ) ;


				/**
				 * Locks the file for writing.
				 *
				 * @throw FeatureNotAvailableException if the file lock
				 * feature is not available.
				 *		 
				 */
				virtual void lockForWriting() const 
					throw( WriteLockingFailed,
						Features::FeatureNotAvailableException ) ;


				/**
				 * Unlocks the file for writing.
				 *
				 * @throw FeatureNotAvailableException if the file lock
				 * feature is not available.
				 *		 
				 */
				virtual void unlockForWriting() const 
					throw( WriteUnlockingFailed,
						Features::FeatureNotAvailableException ) ;


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
				 * Returns the file size.
				 *
				 * @see getSize
				 *
				 */
				Size size() const throw( CouldNotStatFile ) ;


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
				 */
		 		Size read( char * buffer, Size maxLength ) 
					throw( ReadFailed ) ;


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
				Size write( const std::string & message ) 
					throw( File::WriteFailed ) ;


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
				Size write( const char * buffer, Size maxLength ) 
					throw( WriteFailed ) ;


				/**
				 * Tries to open file, useful if it was created with the
				 * DoNotOpen openFlag.
				 *
				 */
				virtual void open( 
						OpeningFlag openFlag = CreateToWriteBinary, 
						PermissionFlag permissionFlag = OwnerReadWrite )
					throw( AlreadyOpened, CouldNotOpen ) ;


				/// Returns the latest change time.
				time_t getLastChangeTime() const 
					throw( GetChangeTimeFailed ) ;


				/**
				 * Returns the stream id, its file descriptor.
				 *
				 * @throw FeatureNotAvailableException if the file descriptor
				 * feature is not available.
				 *
				 */
				FileDescriptor getDescriptor() const 
					throw( Features::FeatureNotAvailableException ) ;



				// Interface implementation.


				/**
				 * Returns this file's ID.
				 *
				 * @return the StreamID or -1 if nothing appropriate can
				 * be returned with the available features.
				 *
				 */
				virtual StreamID getStreamID() const throw() ;


				/// Returns this file's file descriptor.
				virtual StreamID getInputStreamID() const throw() ;

				/// Returns this file's file descriptor.
				virtual StreamID getOutputStreamID() const throw() ;


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



				// Static section.


				/**
				 * Duplicates the file descriptor.
				 *
				 * @throw CouldNotDuplicate if the operation failed or
				 * FeatureNotAvailableException if the file descriptor
				 * feature is not available.
				 *
				 */
				static FileDescriptor Duplicate( FileDescriptor fdes ) 
					throw( CouldNotDuplicate,
						Features::FeatureNotAvailableException ) ;


				/**
				 * Tells whether the entry <b>name</b> exists.
				 *
				 * This entry will be deemed existing, even if it is a 
				 * directory and not a file.
				 *
				 * @throw CouldNotStatFile if the existence test failed
				 * with no answer.
				 *
				 */
				static bool Exists( const std::string & name ) 
					throw( CouldNotStatFile ) ;


				/**
				 * Tells whether the regular file or symbolic link 
				 * <b>name</b> exists.
				 *
				 * This method will work as expected whether the 
				 * symbolic link feature is enabled or not.
				 *
				 * @throw CouldNotStatFile if the existence test failed
				 * with no answer.
				 *
				 */
				static bool ExistsAsFileOrSymbolicLink( 
					const std::string & name ) throw( CouldNotStatFile ) ;


				/// Creates a symbolic link on disk.
				static void MakeSymbolicLink( const std::string & path,
					const std::string & linkname ) throw( SymlinkFailed ) ;
					

				/**
				 * Removes the link from the disk.
				 *
				 * If the file is not a symbolic link, the file itself 
				 * is removed.
				 *
				 */
				static void Unlink( const std::string & name ) 
					throw( UnlinkFailed ) ;


				/// Renames the file on disk.
				static void Move( const std::string & name,
					const std::string & newName ) throw( MoveFailed ) ;


				/// Copies the file on disk.
				static void Copy( const std::string & name,
					const std::string & newName ) throw( CopyFailed ) ;


				/**
				 * Returns the file size, static version.
				 *
				 * @see size
				 *
				 */
				static Size GetSize( const std::string & name ) 
					throw( CouldNotStatFile ) ;


				/**
				 * Updates the last access and modification times of 
				 * specified file.
				 *
				 * @note On contrary to the UNIX command touch, if the
				 * specified file does not exist, it will not be created.
				 * A TouchFailed exception would be raised instead.
				 *
				 * @see File constructors to create empty files.
				 *
				 */
				static void Touch( const std::string & name ) 
					throw( TouchFailed ) ;


				/**
				 * Takes specified <b>name</b> and tries to transform it 
				 * so that the result should be a valid name, from a file
				 * system's point of view.
				 *
				 */
				static const std::string TransformIntoValidFilename( 
					const std::string & name ) throw() ;





			protected:



				/// Tries to reopen file.
				void reopen() throw( CouldNotOpen ) ;


				/// Interprets the current state of this file.
				std::string interpretState() const throw() ;
				
				
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
				
				


				/// Describes buffer size for usual I/O operations.
				static const Size UsualBufferSize ;
				
				/// Describes buffer size for significant I/O operations.
				static const Size BigBufferSize ;
				


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
				 * @note Currently disabled since not supported by the
				 * underlying C++ stream layer.
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
				File( const File & source ) throw() ;


				/**
				 * Assignment operator made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 *
				 */
				File & operator = ( const File & source ) throw() ;


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
					

				/// Name of the file.
				std::string _name ;

				/// Flags used for opening.
				OpeningFlag _openFlag ;

				/// Permissions used for opening.
				PermissionFlag _permissions ;


				/**
				 * Bit field for access locks.
				 *
				 * Not used by current implementation.
				 *
				 */
				bool _lockedForReading: 1, _lockedForWriting: 1 ;


				/// Transfers bytes from a file descriptor to another.
				static void FromFDtoFD( FileDescriptor from, 
						FileDescriptor to, Size length )
					throw( ReadFailed, WriteFailed,
						Features::FeatureNotAvailableException ) ;


		} ;
		
	}

}


#endif // CEYLAN_FILE_H_
