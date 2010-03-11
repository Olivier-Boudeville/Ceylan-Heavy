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
 * Here is an example of log file suitable for this parser:
 * """
 * <0.31.0>|Object-17|Actor.Theme.Object.SomeObject|3168318240218
 *   |08/04/2008 04:41:24|ceylan_test@myhost.org
 *   |Execution.Topic.SpecificEvent|2|No answer from Object XYZ
 * """
 *
 * @see erlang/traces/conf/logmx/TraceSample.txt
 *
 */
public class CeylanTraceParser extends LogFileParser 
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
        return "Ceylan Trace Parser" ;
    }


    /**
     * Returns the supported file type for this parser
     * @see com.lightysoft.logmx.mgr.LogFileParser#getSupportedFileType()
     */
    public String getSupportedFileType() 
	{
        return "LogMX Ceylan trace files" ;
    }


    /**
     * Process the new line of text read from file 
     * @see com.lightysoft.logmx.mgr.LogFileParser#parseLine(java.lang.String)
     */
    protected void parseLine(String line) throws Exception 
	{
	
        // If end of file, records last entry if necessary, and exits:
        if ( line == null ) 
		{
            if ( entry != null ) 
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

            if ( entry != null )
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
			 * field #3: Execution time        -> in entry Date
			 * field #4: user time              -> stored in message
			 * field #5: emitter location       -> stored in message
			 * field #6: message categorization -> stored in message
			 * field #7: priority               -> in entry Level
			 * field #8: message                -> in entry Message
			 * prefixed by: 
			 * [message categorization][user time][location]
			 * Next fields (if ever '|' is in message) are added to message.
			 */
		 			
			entry.setThread(  fields[0].trim() ) ;
			entry.setEmitter( fields[2].trim() + "." + fields[1].trim() ) ;		
			entry.setDate(    fields[3].trim() );		
			entry.setLevel(   fields[7].trim() ) ;
		
			// From field #8 to all ones that may remain:
			String remainingFields = "" ;
			
			Integer remainingFieldsCount = fields.length - 8 ;
			
			// Puts back the '|':
			for ( Integer i = 0; i < remainingFieldsCount; i++ )
			{
				remainingFields += fields[i+8].trim() ;
				if ( i != remainingFieldsCount-1 )
					remainingFields += "|" ;
			}
			  			 
			/*
			 * Inserts spaces to allow line-breaking, and end-of-line to
			 * separate additional fields from actual message:
			 *
			 */	
			entry.setMessage( "[" + fields[6].trim() + "] [" + fields[4].trim() 
			+ "] [" + fields[5].trim() + "]\n" + remainingFields ) ;

			// Relative timestamp is also the execution time here:
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

