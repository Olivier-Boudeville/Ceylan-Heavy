#ifndef CEYLAN_LOG_AGGREGATOR_HTML_FRAGMENTS_H_
#define CEYLAN_LOG_AGGREGATOR_HTML_FRAGMENTS_H_

#include <string>


/**
 * This is the place where HTML fragments for LogAggregatorHTML templates
 * should be stored.
 *
 * @see LogAggregatorHTML
 *
 */
 
 
 
/// Change "LogSystem.html" and "MainLog.html" if HTMLPageSuffix changes.
const std::string LogAggregatorHTML::FrameSet = 
	"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\""
	"\"http://www.w3.org/TR/html4/frameset.dtd\">"
	"<html lang=\"EN\">"
	"<head>"
 	"<meta content=\"Ceylan Log system (see http://osdl.sourceforge.net)\" name=\"generator\">"
	"<title>Ceylan - Log browser for ST_CALLER_DESCRIPTION</title>"
	"<frameset cols=\"30%,*\" frameborder=\"1\" border=\"1\" framespacing=\"0\">"
  	"<frame name=\"toolbar\" src=\"LogSystem.html\" marginheight=\"0\" "
  	"frameborder=\"1\" marginwidth=\"0\" scrolling=\"auto\">"
  	"<frame name=\"mainFrame\" src=\"MainLog.html\" marginheight=\"0\" "
  	"frameborder=\"1\" marginwidth=\"30\" scrolling=\"auto\">"
  	"<noframes>"
    "	<body>"
    "  	<h1>Frames not supported</h1>"
    "  	<p>This log document is designed to be viewed using frame"
    "  	features.<br>"
    "  	If you are seeing this message, please consider upgrading to a"
    "  	frame-compatible browser. We recommend <a href="
    "  	\"http://www.mozilla.org\">Mozilla</a>.</p>"
    "	</body>"
  	"</noframes>"
	"</frameset>"
	"</html>" ;
	
	
	
const std::string LogAggregatorHTML::DefaultPageHeader = 
	"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\""
	"http://www.w3.org/TR/REC-html40/loose.dtd\">"
	"<html lang=\"EN\">"
	"<head>"
 	"<meta content=\"Ceylan Log system (see http://osdl.sourceforge.net)\" name=\"generator\">"
	"<title>Ceylan - Log browser menu</title>"
	"<meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\">"
	"<meta name=\"description\" content=\"Ceylan log browser\">"
  	"<meta name=\"keywords\" content=\"Ceylan, log, menu\">"
 	"</head>"
	"<body><br>"
	"<h1>Welcome to the Ceylan System Log Browser</h1><br>"
	"<p>Please pick a channel of interest on the left panel to view its log.</p>" ;


const std::string LogAggregatorHTML::DefaultPageFooter = 
	"</ul><br><br><br><hr>"
	"</body></html>" ;


	
const std::string LogAggregatorHTML::MenuHeader = 
	"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\""
	"http://www.w3.org/TR/REC-html40/loose.dtd\">"
	"<html lang=\"EN\">"
	"<head>"
 	"<meta content=\"Ceylan Log system (see http://osdl.sourceforge.net)\" name=\"generator\">"
	"<title>Ceylan - Log browser menu</title>"
	"<meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\">"
	"<meta name=\"description\" content=\"Ceylan log browser\">"
  	"<meta name=\"keywords\" content=\"Ceylan, log, menu\">"
 	"</head>"
	"<body>"
	"<br><center><table summary=\"Log channel browser\" border=\"1\">" 
	"<tr><th width=\"20\">Log count</th><th>Channel</th>" ;
	
	
const std::string LogAggregatorHTML::MenuFooter = 
	"</table></center>"
	"</body></html>" ;



const std::string LogAggregatorHTML::ChannelHeader = 
	"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\""
	"http://www.w3.org/TR/REC-html40/loose.dtd\">"
	"<html lang=\"EN\">"
	"<head>"
 	"<meta content=\"Ceylan Log system (see http://osdl.sourceforge.net)\" name=\"generator\">"
	"<title>Ceylan - Channel log for ST_CHANNEL_NAME</title>"
	"<meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\">"
	"<meta name=\"description\" content=\"Ceylan log\">"
  	"<meta name=\"keywords\" content=\"Ceylan, log, channel\">"
	"<style type=\"text/css\">"
	"div.title {text-align: center; font-size:35px; color:green}"
	"</style>"
 	"</head>"
	"<a name=\"_top_\"></a>"
	"<body><br><br>"
	"<div class=\"title\">Channel ST_CHANNEL_NAME</div><p><ul>" ;
  
  
const std::string LogAggregatorHTML::ChannelFooter = 
	"</ul></p><br><br><br><hr>"
	"<center><a href=\"#_top_\">Top</a><br><br><em>Aggregation time :"
	" ST_AGGREGATION_DATE</em></center>"
	"<center><a href=\"MainLog.html\">Back to main page</a></center>"
	"</body></html>" ;




#endif // CEYLAN_LOG_AGGREGATOR_HTML_FRAGMENTS_H_
