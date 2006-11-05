Created on 2006, September 22.



	How to build Ceylan on Windows



Here the target platform is Windows XP Home edition.

The recommended build tool chain is Visual Studio 2005, Express edition.
It can be downloaded free of charge from [1].
It can be used freely, including for commercial use.

To load the project in this editor, just double-click on the Ceylan-x.y.sln
[2].

The Ceylan version comes with a configuration file dedicated to Windows [3]
which enables only the basic features provided by Ceylan : the core features
(ex : file management), and the ones that are available under all Windows 
platforms.

Some other features, such as multithreading, networking, plugin support,
etc. are not readily available on a vanilla Windows installation, they are
therefore initially deactivated.

To activate them, one has first to declare they are requested, by specifying
in [3] the relevant symbol (ex : #define CEYLAN_USES_THREADS 1).

Then the relevant packages have to be installed and declared to the IDE.


Lacking features in vanilla Windows builds :
	- network support (socket, gethostname, etc.) [CEYLAN_USES_NETWORK]
	- plugin support [CEYLAN_USES_PLUGINS]
	- symbolic links 


There is an issue with Windows DLL and templates, notably STL ones. 
See : 
	- http://www.unknownroad.com/rtfm/VisualStudio/warningC4251.html
	- http://support.microsoft.com/kb/q168958/

This led us to define pragmas around some template constructs in header files, ex :

"""

/* 
 * Takes care of the awful issue of Windows DLL with templates.
 *
 * @see Ceylan's developer guide and README-build-for-windows.txt 
 * to understand it, and to be aware of the associated risks. 
 * 
 */
#pragma warning( push )
#pragma warning( disable : 4251 )

	std::list<MyClass *> _instances ;

#pragma warning( pop ) 
"""

During the build on Windows of the Ceylan library, CEYLAN_DLL is expected to 
be equal to "__declspec( dllexport )" 
(ex : "CEYLAN_DLL=__declspec( dllexport )").

During the build on Windows of code using the Ceylan library, CEYLAN_DLL is 
expected to be equal to "__declspec( dllimport )" 
(ex : "CEYLAN_DLL=__declspec( dllimport )"

Special management for the "extern" keyword, with for example :
"CEYLAN_EXTERN_TEMPLATE template class CEYLAN_DLL std::list<Ceylan::EventListener *> ;"
does not seem necessary.

Each test must define :
	- that it references the Ceylan library : in 'Project Dependencies', check Ceylan-x.y-library
	- in 'Configuration Properties'-> C++ -> General -> 'Additional include directories': "$(ProjectDir)../src/code";"$(ProjectDir)../src/code/generic";"$(ProjectDir)../src/code/interfaces";"$(ProjectDir)../src/code/logs";"$(ProjectDir)../src/code/maths";"$(ProjectDir)../src/code/middleware";"$(ProjectDir)../src/code/modules";"$(ProjectDir)../src/code/network";"$(ProjectDir)../src/code/system"
	- in 'Configuration Properties'-> C++ -> 'Preprocessor Definitions' : "CEYLAN_DLL=__declspec( dllimport )";

In the linker settings, 'Generate Debug informations' may be set to yes.  


For numerous functions (such as gethostname), specific headers are required (ex : winsock2.h), 
they are provided with a SDK (Software Development Kit) that has to be installed separatly from the IDE (Visual Express).

The goal is to be able to build Win32 applications, not only .Net ones. 
  
See [5] and [6], most recent SDK at the time of this writing is 'Windows Server 2003 R2 Platform SDK Full Download', 
see [7] and choose to download the 'PSDK-x86.exe' file. Execute it and select the 'typical' installation, which will last
for a long time.

After having installing this PSDK (Platform-SDK), I noticed a link in the Visual Express start page [8] that explains what is to 
be done to complete the installation. I preferred to update the configuration file which in my case was located at [9].
I searched [10] for vccomponents.dat but did not find any.

After having applied all the steps, there is still a trap : as soon as you include for example winsock2.h in an extern "C" clause,
you encounter numerous errors about anonymous structs in winnt.h. According to [11], you need to enable the Microsoft-specific language
extensions, in C/C++ -> Language. It worked for me, even if it is a pity one could not stick with real ANSI/C.

Last thing that should be done is to make sure that the help for the PSDK is
available from the general Visual Express help, as by default it will be not
be automatically integrated. One just has to enter in the help browser URL line
the following one [12], select the appropriate PSDK documentation and restart its
IDE once again. To test the result, search for gethostname, the one defined by
winsock should be found, whereas without the PSDK help one the .Net-defined
GetHostName is referenced. Another useful tip is to select other help sources
than the local one : local, then online for example [13].

A windows executable needs some preprocessor symbols defined so that it can be built against the Ceylan library :
"CEYLAN_DLL=__declspec( dllimport )";"CEYLAN_RUNS_ON_WINDOWS=1"
This can be pasted in C/C++ -> Preprocessor -> Preprocessor Definitions.

To add a new test :
	- open the Ceylan solution
	- add a new project to it
	- choose project type : Visual C++ -> General -> Empty Project
	- choose project name (ex : generic-testCeylanBasicResourceManager for source in generic/testCeylanBasicResourceManager.cc)
	- choose project location in the relevant module (ex : trunk/test/generic for testCeylanBasicResourceManager)
	- remove default directories (Header, Resource, Source Files)
	- add in project an existing element (ex : testCeylanBasicResourceManager.cc)
	- go to the project properties, in Common Properties -> References, 
click on 'Add New Reference' and select 'Ceylan-x.y library'
	- in 'configuration properties' :
		* set in General -> Output Directory : ".."
		* add in C/C++ -> General -> Additional Include Directories :
"$(SolutionDir)code";"$(SolutionDir)code/generic";"$(SolutionDir)code/interfaces";"$(SolutionDir)code/logs";"$(SolutionDir)code/maths";"$(SolutionDir)code/middleware";"$(SolutionDir)code/modules";"$(SolutionDir)code/network";"$(SolutionDir)code/system"
		* add in C/C++ -> Preprocessor -> Preprocessor Definitions :
"CEYLAN_DLL=__declspec( dllimport )";"CEYLAN_RUNS_ON_WINDOWS=1"
		* choose in C/C++ -> Code Generation -> Runtime Library :
"Multi-threaded Debug DLL (/MDd)". Selecting "Multi-threaded DLL (/MD)", "Multi-threaded Debug (/MTd)" or "Multi-thread (/MT)" leads to a direct crash.
		* choose in Linker -> Debugging :
"Generate Debug Info : Yes (/DEBUG)"
	- build the project
	- from a cygwin terminal, go to C:\Documents and Settings\sye\Mes documents\Ceylan\trunk\src\Debug\ for example, 
and run testCeylanBasicResourceManager.exe; echo $?
	

test*.vcproj files could be generated automatically, but it would not be far more convenient (generate a GUID, register to the solution, etc.)
 
To run a test, the Ceylan DLL file (Ceylan-x.y.dll) must be found, the safest 
solution is to add the directory in which the DLL is generated (trunk/src/Debug)
in the PATH environment variable, or to run the test from this directory 
(ex : ../../test/network/network-testCeylanSequentialServerStream --consolePlug)

The recommended way of testing the Ceylan library is to run the testing suite 
for a Cygwin terminal, since the script playTests.sh can be run on all supported
platforms, including Cygwin-based Windows (note that Cygwin is used for the 
shell and UNIX commands it provides, none of its libraries is used by Ceylan) :
"""
cd trunk/test
./playTests.sh
"""

The ldd tool is lacking on the Windows platform, one may use [14] instead.

[1]  http://msdn.microsoft.com/vstudio/express/
[2]  for example, trunk/src/Ceylan-0.3.sln
[3]  trunk/src/code/CeylanConfigForWindows.h
[4]  http://sources.redhat.com/pthreads-win32/
[5]  http://msdn2.microsoft.com/en-us/visualc/aa336404.aspx
[6]  http://www.microsoft.com/downloads/details.aspx?familyid=EBA0128F-A770-45F1-86F3-7AB010B398A3&displaylang=en
[7]  http://www.microsoft.com/downloads/details.aspx?FamilyId=0BAF2B35-C656-4969-ACE8-E4C0C0716ADB&displaylang=en
[8]  http://msdn.microsoft.com/vstudio/express/visualc/usingpsdk/default.aspx
[9]  C:\Program Files\Microsoft Visual Studio 8\VC\vcpackages\VCProjectEngine.Dll.Express.Config
[10] C:\Documents and Settings\sye\Application Data\Microsoft\VCExpress
[11] http://community.vietfun.com/printthread.php?t=279103
[12] ms-help://MS.VSExpressCC.v80/dv_vsexpcc/local/CollectionManagerExpress.htm
[13] see Tools -> Options -> Help -> Online
[14] http://www.dependencywalker.com/




The solution and the main projects could not be stored in the expected trunk/src/conf/build directory, as it leads 
to having all imported files in the same virtual directory, whereas we want to keep the original module-based 
directory structure.