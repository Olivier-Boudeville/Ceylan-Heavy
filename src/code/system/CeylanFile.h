#ifndef CEYLAN_FILE_H_
#define CEYLAN_FILE_H_


#include "CeylanSystem.h"              // for Size, SignedSize, etc.
#include "CeylanTypes.h"               // for Ceylan::Byte
#include "CeylanInputOutputStream.h"   // for inheritance
#include "CeylanFileSystemCommon.h"    // for FileManagementException and al

#include <string>




namespace Ceylan
{


	namespace System
	{

		
		/*
		 * Each File child class is linked to a corresponding filesystem
		 * manager child class.
		 *
		 */
		class FileSystemManager ;
		
		
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
		 * Abstract file mother class, so that programs can always manipulate
		 * Ceylan::File instances, whereas per-platform specialized classes are
		 * actually used by the system.
		 *
		 * Actual files should be created and opened with respectively the
		 * File::Create and File::Open factories.
		 * If it allows to fully hide the actual child class being used, an
		 * abstract File class hinders from using such File instances as 
		 * automatic variables: 'File myFile(...)' cannot exist, thus 
		 * 'File & myFile = File::Create(...)' as to be used instead. 
		 * The drawback of this approach is that the life-cycle of the instance
		 * has to be managed explicitly, with for example a 'delete &myFile'
		 * to be placed before each return or throw statement.
		 * To restore file handling through automatic variable (which are
		 * automatically deallocated in the relevant cases), the ResourceHolder
		 * class is available.
		 * It should be used that way:
		 * 'Holder<File> myFileHolder( File::Open(...) ) ;', then
		 * 'myFileHolder.get().lockForWriting()' can be used for example.
		 *
		 * @see Directory, FileSystemManager
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
		 *   - Features::areFileLocksSupported
		 * 
		 * @note To specify all exceptions that may be thrown by file
		 * manipulation, use System::SystemException, the closest mother class
		 * of FileException, InputStream::ReadFailedException and
		 * OutputStream::WriteFailedException. 
		 * 
		 */
		class CEYLAN_DLL File : public InputOutputStream
		{
	

			public:


				class CEYLAN_DLL RemoveFailed: public FileException
				{
					public:
					
						explicit RemoveFailed( const std::string & message )
								throw():
							FileException( message )
						{
						
						}	
						
				} ;


				class CEYLAN_DLL MoveFailed: public FileException
				{
					public:
					
						explicit MoveFailed( const std::string & message )
								throw():
							FileException( message )
						{
						
						}	
						
				} ;


				class CEYLAN_DLL CopyFailed: public FileException
				{
					public:
					
						explicit CopyFailed( const std::string & message )
								throw():
							FileException( message )
						{
						
						}	
						
				} ;


				class CEYLAN_DLL CreateFailed: public FileException
				{ 
					public: 
					
						explicit CreateFailed( 
								const std::string & reason ) throw() ; 
				} ;


				class CEYLAN_DLL OpenFailed: public FileException
				{ 
				
					public: 
					
						explicit OpenFailed( 
								const std::string & reason ) throw() ; 
				} ;


				class CEYLAN_DLL AlreadyOpened: public FileException
				{ 
				
					public: 
					
						explicit AlreadyOpened( 
								const std::string & reason ) throw() ; 
				} ;


				class CEYLAN_DLL LookupFailed: public FileException
				{
					public:
					
						explicit LookupFailed( 
								const std::string & message ) throw():
							FileException( message )
						{
						
						}	
						
				} ;



				class CEYLAN_DLL ReadLockingFailed: public FileException
				{ 
					public: 
					
						explicit ReadLockingFailed( 
							const std::string & reason ) throw() ; 
				} ;


				class CEYLAN_DLL ReadUnlockingFailed: public FileException
				{ 
					public: 
					
						explicit ReadUnlockingFailed( 
							const std::string & reason ) throw() ; 
				} ;


				class CEYLAN_DLL WriteLockingFailed: public FileException
				{ 
					public: 
					
						explicit WriteLockingFailed( 
							const std::string & reason ) throw() ; 
				} ;


				class CEYLAN_DLL WriteUnlockingFailed: public FileException
				{ 
					public: 
					
						explicit WriteUnlockingFailed( 
							const std::string & reason ) throw() ; 
				} ;



				class CEYLAN_DLL CloseFailed: public FileException
				{ 
				
					public: 
					
						explicit CloseFailed( 
							const std::string & reason ) throw() ; 
							
				} ;

				
				class CEYLAN_DLL FileTouchFailed: public FileException
				{ 
				
					public: 
					
						explicit FileTouchFailed( 
							const std::string & reason ) throw() ; 
							
				} ;

				
				




				// Allows read operations on the opened file. 
				static const OpeningFlag Read ;
				
				// Allows write operations on the opened file.
				static const OpeningFlag Write ;
				
				
				/**
				 * Creates the file, if it does not exist.
				 *
				 * Could not be named 'Create' (static method of the same
				 * name exists already), thus CreateFile is used.
				 *
				 */
				static const OpeningFlag CreateFile ;
				
				
				// Truncates file to length zero when opening.
				static const OpeningFlag TruncateFile ;
				
				// Seeks to the end of the file before each write operation.
				static const OpeningFlag AppendFile ;
				
								
				// Opens the file in binary rather than in text mode.
				static const OpeningFlag Binary ;
				
				// Does not open the underlying file object.
				static const OpeningFlag DoNotOpen ;
				
				
				/*
				 * Creates the file for writing, it is a convenience 
				 * shortcut equal to the very common combination:
				 * 'Read | Write | CreateFile | TruncateFile | Binary'.
				 *
				 */
				static const OpeningFlag CreateToWriteBinary ;
				
				
				/*
				 * Opens the file for reading, it is a convenience 
				 * shortcut equal to the very common combination:
				 * 'Read | Binary'.
				 *
				 */
				static const OpeningFlag OpenToReadBinary ;
				
								
				
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
				 * Provided for convenience: 
				 * OwnerReadWrite = OwnerRead | OwnerWrite.
				 *
				 */
				static const PermissionFlag OwnerReadWrite ;
				
				
				/**
				 * Provided for convenience: 
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
				 * Provided for convenience: 
				 * GroupReadWrite = GroupRead | GroupWrite.
				 *
				 * @note Only taken into account if the advanced file 
				 * attribute feature is enabled.
				 *
				 */
				static const PermissionFlag GroupReadWrite ;
				
				
				/**
				 * Provided for convenience: 
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
				 * Provided for convenience: 
				 * OthersReadWrite = OthersRead | OthersWrite.
				 *
				 * @note Only taken into account if the advanced file 
				 * attribute feature is enabled.
				 *
				 */
				static const PermissionFlag OthersReadWrite ;
				
				
				/**
				 * Provided for convenience: 
				 * OthersReadWriteExec = OthersReadWrite | OthersExec.
				 *
				 * @note Only taken into account if the advanced file 
				 * attribute feature is enabled.
				 *
				 */
				static const PermissionFlag OthersReadWriteExec ;
				
							
							
				// Static section.


				/**
				 * Tells whether the regular file or symbolic link 
				 * <b>filename</b> exists (and is not a directory).
				 *
				 * @param filename the filename to look-up.
				 *
				 * This method will work as expected whether the 
				 * symbolic link feature is enabled or not.
				 *
				 * @throw FileException, including File::LookupFailed if the
				 * operation failed or is not supported on this platform, or
				 * FileDelegatingException if the relevant filesystem manager
				 * could not be retrieved.
				 *
				 */
				static bool ExistsAsFileOrSymbolicLink( 
						const std::string & filename ) const 
					throw( FileException ) ;


				/**
				 * Removes the file or symbolic link from the filesystem.
				 *
				 * @param filename the filename to remove.
				 *
				 * @throw FileException, including File::RemoveFailed if the
				 * operation failed or is not supported on this platform, or
				 * FileDelegatingException if the relevant filesystem manager
				 * could not be retrieved.
				 *
				 */
				static void Remove( const std::string & filename ) 
					throw( FileException ) ;
				
				
				/**
				 * Creates a symbolic link on filesystem.
				 *
				 * @param linkTarget the full path of the entry the new link
				 * should point to.
				 *
				 * @param linkName the filename of the link to create.
				 *
				 * @throw FileException, including SymlinkFailed if the
				 * operation failed or is not supported on this platform, or
				 * FileDelegatingException if the relevant filesystem manager
				 * could not be retrieved.
				 *
				 */
				static void CreateSymbolicLink( const std::string & linkTarget,
					const std::string & linkName ) throw( FileException ) ;


				/**
				 * Moves specified file on filesystem.
				 *
				 * A special case of file moving is file renaming.
				 *
				 * @param sourceFilename the filename of the file to be moved.
				 *
				 * @param targetFilename the target filename of the moved file.
				 *
				 * @throw FileException, including File::MoveFailed if the
				 * operation failed or is not supported on this platform, or
				 * FileDelegatingException if the relevant filesystem manager
				 * could not be retrieved.
				 *
				 */
				static void Move( const std::string & sourceFilename,
						const std::string & targetFilename ) 
					throw( FileException ) ;


				/**
				 * Copies the file on filesystem.
				 *
				 * @param sourceFilename the filename of the file to be copied.
				 *
				 * @param targetFilename the new filename of the copied file.
				 *
				 * @throw CopyFailed if the operation failed or is not
				 * supported on this platform, or FileDelegatingException if 
				 * the relevant filesystem manager could not be retrieved.
				 *
				 */
				static void Copy( const std::string & sourceFilename,
						const std::string & targetFilename ) 
					throw( FileException ) ;


				/**
				 * Returns the size, in bytes, of the specified file.
				 *
				 * @param filename the filename whose size is searched.
				 *
				 * @throw File::LookupFailed if the operation failed or is not
				 * supported on this platform, or FileDelegatingException if 
				 * the relevant filesystem manager could not be retrieved.
				 *
				 */
				static Size GetSize( const std::string & filename ) 
					throw( FileException ) ;


				/**
				 * Takes specified <b>rawFilename</b> and tries to transform it 
				 * so that the result should be a valid name, from the
				 * filesystem's point of view.
				 *
				 * @param rawFilename the filename to convert
				 *
				 * @return the converted filename
				 *
				 */
				static std::string TransformIntoValidFilename( 
					const std::string & rawFilename ) throw( FileException ) ;


				/**
				 * Updates the last access and modification times of 
				 * specified file.
				 *
				 * This is not expected to work for directories.
				 *
				 * @param filename the filename of the file whose times must
				 * be updated.
				 *
				 * @note On contrary to the UNIX command touch, if the
				 * specified file does not exist, it will not be created.
				 * A TouchFailed exception would be raised instead.
				 *
				 * @see File::Create to create empty files.
				 *
				 * @throw FileTouchFailed if the operation failed or is not
				 * supported on this platform, or FileDelegatingException if 
				 * the relevant filesystem manager could not be retrieved.
				 *
				 */
				static void Touch( const std::string & filename ) 
					throw( FileException ) ;


				/**
				 * Tells whether the two specified files have exactly the same
				 * content (byte-wise).
				 *
				 * @param firstFilename the filename of the first file to
				 * compare.
				 *
				 * @param secondFilename the filename of the second file to
				 * compare.
				 *
				 * @return true iff these files exists and have exactly the 
				 * same content.
				 *
				 * @throw DiffFailed if the operation failed or is not
				 * supported on this platform, or FileDelegatingException if 
				 * the relevant filesystem manager could not be retrieved.
				 *
				 */
				static bool Diff( const std::string & firstFilename,
						const std::string & secondFilename ) 
					throw( FileException ) ;


				
				
				// Factory section.
				
		
				/**
				 * Returns a File reference on a newly created file.
				 *
				 * By default, it creates a new file on disk. If the name
				 * corresponds to an already-existing file, it will be
				 * truncated and overwritten.
				 *
				 * @param filename the name of the file.
				 *
				 * @param createFlag the flag describing the creation mode.
				 *
				 * @param permissionFlag the flag describing the requested
				 * permissions. On platforms that do not manage permissions,
				 * this parameter will be ignored.
				 *
				 * @see OpeningFlag, PermissionFlag
				 *
				 * File factory, to be used instead of a specific File subclass
				 * constructor, so that it can return a File instance that is
				 * actually a specialized one (ex: a StandardFile, not an
				 * abstract File).
				 *
				 * @throw CreateFailed if the operation failed or is not
				 * supported on this platform, or FileDelegatingException if 
				 * the relevant filesystem manager could not be retrieved.
				 *
				 * @example:
				 * <pre>
				 * File & myFile = File::Create( "myfilename" ) ;
				 * ...
				 * myFile.write( "Hello Ceylan!" ) ;
				 * ...
				 * </pre>
				 *
				 * @note Ceylan::Holder can be used as well to simplify the
				 * management of the lifecycle of File instances.
				 *
				 */
				static File & Create( const std::string & filename, 
						OpeningFlag createFlag = CreateToWriteBinary,
						PermissionFlag permissionFlag = OwnerReadWrite ) 
					throw( FileException ) ;

				
				
				/**
				 * Returns a File reference on specified already-existing file,
				 * which will be opened with specified settings.
				 *
				 * @param filename the name of the file.
				 *
				 * @param openFlag the flag describing the opening mode.
				 *
				 * @see OpeningFlag
				 *
				 * File factory, to be used instead of a specific File subclass
				 * constructor, so that it can return a File instance that is
				 * actually a specialized one (ex: a StandardFile, not an
				 * abstract File).
				 *
				 * @throw OpenFailed if the operation failed or is not
				 * supported on this platform, or FileDelegatingException if 
				 * the relevant filesystem manager could not be retrieved.
				 *
				 * @example:
				 * <pre>
				 * File & myFile = File::Open( "myfilename.dat" ) ;
				 * ...
				 * myFile.read( buf, 100 ) ;
				 * ...
				 * </pre>
				 *
				 * @note Ceylan::Holder can be used as well to simplify the
				 * management of the lifecycle of File instances.
				 *
				 */
				static File & Open( const std::string & filename, 
						OpeningFlag openFlag = OpenToReadBinary ) 
					throw( FileException ) ;

				
				/**
				 * Destroys the file reference object, does not remove the
				 * file itself.
				 *
				 * @note Will close automatically the file if needed.
				 *
				 * To remove the file itself (from disk), use Remove.
				 *
				 * @see Remove
				 *
				 * This destructor has to be public, so that instances created
				 * from factories can be deleted by the caller.
				 *
				 */
				virtual ~File() throw() ;



				/// Returns this file's name.
				const std::string & getName() const throw() ;


				/**
				 * Closes the file for read/write actions.
				 *
				 * @return true iff an operation had to be performed.
				 *
				 * @throw CloseException if the close operation failed, 
				 * including if the file was not already opened.
				 *
				 */
				virtual bool close() throw( Stream::CloseException ) = 0 ;


				/**
				 * Saves the file under a new name.
				 *
				 * @param newName the name of the newly copied file.
				 *
				 * @throw FileException if the operation failed.
				 *
				 */
				virtual void saveAs( const std::string & newName )
					throw( FileException ) = 0 ;




				// Locking section.
				
				 
				/**
				 * Locks the file for reading.
				 *
				 * @throw ReadLockingFailed if the operation failed or if the
				 * file lock feature is not available.
				 *		 
				 * This default implementation, meant to be overriden, throws
				 * this exception if called.
				 *
				 */
				virtual void lockForReading() const throw( ReadLockingFailed ) ;


				/**
				 * Unlocks the file for reading.
				 *
				 * @throw ReadUnlockingFailed if the operation failed or if the
				 * file lock feature is not available.
				 *		 
				 * This default implementation, meant to be overriden, throws
				 * this exception if called.
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
				 * This default implementation, meant to be overriden, throws
				 * this exception if called.
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
				 * This default implementation, meant to be overriden, throws
				 * this exception if called.
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
				 * This default implementation, meant to be overriden, returns
				 * always false.
				 *
				 */
				virtual bool isLocked() const throw() ;




				/**
				 * Returns the file size, in bytes.
				 *
				 * @see GetSize
				 *
				 * @throw StatEntryFailed if the file metadata could not 
				 * be accessed or if the operation is not supported on this
				 * platform.
				 *
				 */
				virtual Size size() const throw( StatEntryFailed ) = 0 ;



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
					throw( InputStream::ReadFailedException ) = 0 ;


				/**
				 * Reads exactly exactLength bytes from this file to specified
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
				 * @note May never terminate if the file was open without
				 * the 'Binary' flag (hence in text mode) and if in the 
				 * file content it occurs that accidentally some bytes 
				 * form an 'end of file' marker (despite some bytes remain
				 * to be read past this marker).
				 *
				 */
		 		virtual void readExactLength( Ceylan::Byte * buffer, 
						Size exactLength ) 
					throw( InputStream::ReadFailedException ) ;


				/**
				 * Tells whether there is data available on input.
				 *
				 * This methods returns always true for files.
				 *
				 */
				virtual bool hasAvailableData() const throw() ;
				
				
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
					throw( OutputStream::WriteFailedException ) = 0 ;


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
					throw( OutputStream::WriteFailedException ) = 0 ;



				/**
				 * Tries to open file, useful if it was created with the
				 * DoNotOpen openFlag.
				 *
				 */
				virtual void open( 
						OpeningFlag openFlag = CreateToWriteBinary, 
						PermissionFlag permissionFlag = OwnerReadWrite )
					throw( AlreadyOpened, OpenFailed ) ;



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
				virtual void remove() throw( RemoveFailed ) = 0 ;



				// Interface implementation.


				/**
				 * Returns this file's ID.
				 *
				 * @return the StreamID or -1 if nothing appropriate can
				 * be returned with the available features.
				 *
				 */
				virtual StreamID getStreamID() const throw() = 0 ;


				/**
				 * Returns this file descriptor for this file, or -1 if 
				 * the file descriptor feature is not available.
				 *
				 */
				virtual StreamID getInputStreamID() const throw() ;


				/**
				 * Returns this file descriptor for this file, or -1 if 
				 * the file descriptor feature is not available.
				 *
				 */
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


				/// Describes buffer size for usual I/O operations.
				static const Size UsualBufferSize ;
				
				/// Describes buffer size for significant I/O operations.
				static const Size BigBufferSize ;
				



			protected:



				/**
				 * Constructs a file reference object.
				 *
				 * By default, it creates a new file on disk, if the name
				 * corresponds to an already-existing file, it will be
				 * truncated and overwritten.
				 *
				 * @param name the name of the file.
				 *
				 * @param openFlag the flag describing the opening mode.
				 *
				 * @param permissionFlag the flag describing the requested
				 * permissions, if this file is to be created.
				 * Otherwise (if the file already exists), this parameter
				 * is ignored.
				 *
				 * @see OpeningFlag, PermissionFlag
				 *
				 * @example:
				 * <pre>
				 * File f( "myfilename", Read | Binary ) ;
				 * ...
				 * f.read( buf, 100 ) ;
				 * ...
				 * </pre>
				 *
				 * @note If not specifically set, the file is opened in 
				 * text mode: one should not forget to add the Binary flag. 
				 * The mistake can be detected when basic read() returns less
				 * than the requested size, or when readExactLength() never
				 * terminates.
				 *
				 */
				explicit File( const std::string & name, 
						OpeningFlag openFlag = CreateToWriteBinary,
						PermissionFlag permissionFlag = OwnerReadWrite ) 
					throw( OpenFailed ) ;



				/**
				 * Returns the filesystem manager that corresponds to the 
				 * actual File child class.
				 *
				 * @throw FileDelegatingException if the operation failed.
				 *
				 */
				virtual FileSystemManager & getCorrespondingFileSystemManager()
					const throw( FileDelegatingException ) = 0 ;
					


				/// Tries to reopen file.
				virtual void reopen() throw( OpenFailed ) = 0 ;

				/// Interprets the current state of this file.
				virtual std::string interpretState() const throw() = 0 ;
								


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

				

		} ;
		
	}

}


#endif // CEYLAN_FILE_H_

