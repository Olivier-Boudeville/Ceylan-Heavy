package ceylan.parser;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.lightysoft.logmx.business.ParsedEntry;
import com.lightysoft.logmx.mgr.LogFileParser;



/**
 * Ceylan LogMX Parser able to parse a log file with multi-line support and
 * Relative Date support.
 *
 * @see Ceylan traces
 *
 * Here is an example of log file suitable for this parser:
 * """
 * <0.32.0>|AnotherObject-18|Actor.AnotherObject|3168318240218|08/04/2008
 *   04:41:24|mynode@my.machine.org|Simulation.Event.MyEvent|2|No answer from 
 * my object
 * """
 *
 * @see TraceSample.txt
 *
 */
public class CeylanParser extends LogFileParser 
{

    /** Current parsed log entry */
    private ParsedEntry entry = null;


    /** Entry date format */
    private final static SimpleDateFormat DatePattern = new SimpleDateFormat(
        "dd/MM/yyyy HH:mm:ss" ) ;

    /** Pattern to match the beginning of a trace, like '<0.31.0>|':  */
    private final static Pattern TraceBeginPattern =
		Pattern.compile("^<\\d++\\.\\d++\\.\\d++>\\|.*$");


    /** 
     * Returns the name of this parser
     * @see com.lightysoft.logmx.mgr.LogFileParser#getParserName()
     */
    public String getParserName() 
	{
        return "Ceylan Parser" ;
    }


    /**
     * Returns the supported file type for this parser
     * @see com.lightysoft.logmx.mgr.LogFileParser#getSupportedFileType()
     */
    public String getSupportedFileType() 
	{
        return "LogMX Ceylan log files" ;
    }


    /**
     * Process the new line of text read from file 
     * @see com.lightysoft.logmx.mgr.LogFileParser#parseLine(java.lang.String)
     */
    protected void parseLine(String line) throws Exception 
	{
	
        // If end of file, records last entry if necessary, and exits:
        if (line == null) 
		{
            if (entry != null) 
			{
                addEntry(entry) ;
            }
            return ;
        }

        Matcher matcher = TraceBeginPattern.matcher(line) ;
		
        if ( matcher.matches() ) 
		{
		
			// We are at the beginning of a trace.
	
			/* '|' is the field separator: */
            String[] fields = line.split( "\\|" ) ;

            if (entry != null)
			{ 
				// Records previous found entry if exists:
                addEntry(entry) ;
            }

			entry = createNewEntry() ;
			
			/*
			 * field #0: technical identifier   -> in entry Thread
			 * field #1: name of the emitter    -> added to emitter
			 * categorization
			 * field #2: emitter categorization -> in entry Emitter
			 * field #3: simulation time        -> in entry Date
			 * field #4: user time              -> stored in message
			 * field #5: emitter location       -> stored in message
			 * field #6: message categorization -> stored in message
			 * field #7: priority               -> in entry Level
			 * field #8: message                -> in entry Message
			 * prefixed by: 
			 * [message categorization][user time][location]
			 *
			 */
		 			
			entry.setThread( fields[0].trim() ) ;
			entry.setEmitter( fields[2].trim() + "." + fields[1].trim() ) ;		
			entry.setDate( fields[3].trim() );		
			entry.setLevel( fields[7].trim() ) ;
		
			/*
			 * Inserts spaces to allow line-breaking, and end-of-line to
			 * separate additional fields from actual message:
			 *
			 */	
			entry.setMessage( "[" + fields[6].trim() + "] [" + fields[4].trim() 
			+ "] [" + fields[5].trim() + "]\n" + fields[8].trim() ) ;

			/*
			 * Relative timestamp is also the simulation time here:
			 * (simulation time used here)
			 *
			 * @note If the simulation time is too high (more than a 
			 * 32-bit int ?), the parser will not be able to manage it.
			 *
			 * See Integer.parseInt in getRelativeEntryDate.
			 *
			 */
			entry.setExtraInfo( fields[3].trim() );	
		
      } 
	  else if (entry != null) 
	  {
            // Appending this line to previous entry's text:
            entry.setMessage( entry.getMessage() + "\n" + line ) ;
      }
  
	}
	
	  
	  
    /**
     * Returns the relative timestamp of given entry (if entry's ExtraInfo
	 * contains "1265", 
     * it means "T0 + 1265 ms", so simply return "new Date(1265)")
	 *
     * @see com.lightysoft.logmx.mgr.LogFileParser,
	 * getRelativeEntryDate(com.lightysoft.logmx.business.ParsedEntry)
	 *
     */
    public Date getRelativeEntryDate(ParsedEntry pEntry) throws Exception
	{
        return new Date( 
			Integer.parseInt( pEntry.getExtraInfo().toString() ) ) ;
    }
	
	  
    /**
     * Returns the Date object for the given entry 
     * @see com.lightysoft.logmx.mgr.LogFileParser, 	
	 * getAbsoluteEntryDate(com.lightysoft.logmx.business.ParsedEntry)
     */
    public Date getAbsoluteEntryDate(ParsedEntry pEntry) throws Exception 
	{
	
        return DatePattern.parse( pEntry.getDate() ) ; 
		
    }

}
