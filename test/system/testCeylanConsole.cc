/* 
 * Copyright (C) 2003-2009 Olivier Boudeville
 *
 * This file is part of the Ceylan library.
 *
 * The Ceylan library is free software: you can redistribute it and/or modify
 * it under the terms of either the GNU Lesser General Public License or
 * the GNU General Public License, as they are published by the Free Software
 * Foundation, either version 3 of these Licenses, or (at your option) 
 * any later version.
 *
 * The Ceylan library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License and the GNU General Public License
 * for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License and the GNU General Public License along with the Ceylan library.
 * If not, see <http://www.gnu.org/licenses/>.
 *
 * Author: Olivier Boudeville (olivier.boudeville@esperide.com)
 *
 */


#include "Ceylan.h"
using namespace Ceylan ;
using namespace Ceylan::Log ;
using namespace Ceylan::System ;


#include <iostream>

#include <string>
using std::string ;




/**
 * Test of Ceylan console management.
 *
 * @see Ceylan::System::Console.
 *
 */
int main( int argc, char * argv[] )
{


	LogHolder logger( argc, argv ) ;


    try
    {

		bool isBatch = false ;

		std::string executableName ;
		std::list<std::string> options ;

		Ceylan::parseCommandLineOptions( executableName, options, argc, argv ) ;
		
		std::string token ;
		bool tokenEaten ;
		
		
		while ( ! options.empty() )
		{
		
			token = options.front() ;
			options.pop_front() ;

			tokenEaten = false ;
						
			if ( token == "--batch" )
			{
				LogPlug::info( "Batch mode selected" ) ;
				isBatch = true ;
				tokenEaten = true ;
			}
									
			if ( token == "--online" )
			{
				LogPlug::info( "Online option ignored" ) ;
				tokenEaten = true ;
			}
									
			if ( LogHolder::IsAKnownPlugOption( token ) )
			{
				// Ignores log-related (argument-less) options.
				tokenEaten = true ;
			}
			
			
			if ( ! tokenEaten )
			{
				throw Ceylan::CommandLineParseException( 
					"Unexpected command line argument : " + token ) ;
			}
		
		}


		LogPlug::info( "Testing Ceylan's console support." ) ;

		System::Console MyConsole ;
		
		LogPlug::info( "Console state on creation: " + MyConsole.toString() ) ;
			
		MyConsole.addInBuffer( "Universal Declaration of Human Rights" ) ;
		
		MyConsole.addInBuffer( "Adopted and proclaimed by General Assembly resolution 217 A (III) of 10 December 1948" ) ;
		
		MyConsole.addInBuffer( "Article 1.\nAll human beings are born free and equal in dignity and rights. They are endowed with reason and conscience and should act towards one another in a spirit of brotherhood." ) ;

 		MyConsole.addInBuffer( "Article 2.\nEveryone is entitled to all the rights and freedoms set forth in this Declaration, without distinction of any kind, such as race, colour, sex, language, religion, political or other opinion, national or social origin, property, birth or other status. Furthermore, no distinction shall be made on the basis of the political, jurisdictional or international status of the country or territory to which a person belongs, whether it be independent, trust, non-self-governing or under any other limitation of sovereignty." ) ;

		MyConsole.addInBuffer( "Article 3.\nEveryone has the right to life, liberty and security of person." ) ;
		
		MyConsole.addInBuffer( "Article 4.\nNo one shall be held in slavery or servitude; slavery and the slave trade shall be prohibited in all their forms." ) ;
		MyConsole.addInBuffer( "Article 5.\nNo one shall be subjected to torture or to cruel, inhuman or degrading treatment or punishment." ) ;
		
		MyConsole.addInBuffer( "Article 6.\nEveryone has the right to recognition everywhere as a person before the law." ) ;
		
		MyConsole.addInBuffer( "Article 7.\nAll are equal before the law and are entitled without any discrimination to equal protection of the law. All are entitled to equal protection against any discrimination in violation of this Declaration and against any incitement to such discrimination." ) ;
		
		MyConsole.addInBuffer( "Article 8.\nEveryone has the right to an effective remedy by the competent national tribunals for acts violating the fundamental rights granted him by the constitution or by law." ) ;
		
		MyConsole.addInBuffer( "Article 9.\nNo one shall be subjected to arbitrary arrest, detention or exile." ) ;

		MyConsole.addInBuffer( "Article 10.\nEveryone is entitled in full equality to a fair and public hearing by an independent and impartial tribunal, in the determination of his rights and obligations and of any criminal charge against him." ) ;
		
		MyConsole.addInBuffer( "Article 11.\n(1) Everyone charged with a penal offence has the right to be presumed innocent until proved guilty according to law in a public trial at which he has had all the guarantees necessary for his defence.\n(2) No one shall be held guilty of any penal offence on account of any act or omission which did not constitute a penal offence, under national or international law, at the time when it was committed. Nor shall a heavier penalty be imposed than the one that was applicable at the time the penal offence was committed." ) ;

		MyConsole.addInBuffer( "Article 12.\nNo one shall be subjected to arbitrary interference with his privacy, family, home or correspondence, nor to attacks upon his honour and reputation. Everyone has the right to the protection of the law against such interference or attacks." ) ;
		
		MyConsole.addInBuffer( "Article 13.\n(1) Everyone has the right to freedom of movement and residence within the borders of each state.\n(2) Everyone has the right to leave any country, including his own, and to return to his country." ) ;
		
		MyConsole.addInBuffer( "Article 14.\n(1) Everyone has the right to seek and to enjoy in other countries asylum from persecution.\n(2) This right may not be invoked in the case of prosecutions genuinely arising from non-political crimes or from acts contrary to the purposes and principles of the United Nations." ) ;
		
		MyConsole.addInBuffer( "Article 15.\n(1) Everyone has the right to a nationality.\n(2) No one shall be arbitrarily deprived of his nationality nor denied the right to change his nationality." ) ;
		
		MyConsole.addInBuffer( "Article 16.\n(1) Men and women of full age, without any limitation due to race, nationality or religion, have the right to marry and to found a family. They are entitled to equal rights as to marriage, during marriage and at its dissolution.\n(2) Marriage shall be entered into only with the free and full consent of the intending spouses.\n(3) The family is the natural and fundamental group unit of society and is entitled to protection by society and the State." ) ;

		MyConsole.addInBuffer( "Article 17.\n(1) Everyone has the right to own property alone as well as in association with others.\n(2) No one shall be arbitrarily deprived of his property." ) ;
		
		MyConsole.addInBuffer( "Article 18.\nEveryone has the right to freedom of thought, conscience and religion; this right includes freedom to change his religion or belief, and freedom, either alone or in community with others and in public or private, to manifest his religion or belief in teaching, practice, worship and observance." ) ;
		
		MyConsole.addInBuffer( "Article 19.\nEveryone has the right to freedom of opinion and expression; this right includes freedom to hold opinions without interference and to seek, receive and impart information and ideas through any media and regardless of frontiers." ) ;
		
		MyConsole.addInBuffer( "Article 20.\n(1) Everyone has the right to freedom of peaceful assembly and association.\n(2) No one may be compelled to belong to an association." ) ;
		
		MyConsole.addInBuffer( "Article 21.\n(1) Everyone has the right to take part in the government of his country, directly or through freely chosen representatives.\n(2) Everyone has the right of equal access to public service in his country.\n(3) The will of the people shall be the basis of the authority of government; this will shall be expressed in periodic and genuine elections which shall be by universal and equal suffrage and shall be held by secret vote or by equivalent free voting procedures." ) ;
		
		MyConsole.addInBuffer( "Article 22.\nEveryone, as a member of society, has the right to social security and is entitled to realization, through national effort and international co-operation and in accordance with the organization and resources of each State, of the economic, social and cultural rights indispensable for his dignity and the free development of his personality." ) ;
		
		MyConsole.addInBuffer( "Article 23.\n(1) Everyone has the right to work, to free choice of employment, to just and favourable conditions of work and to protection against unemployment.\n(2) Everyone, without any discrimination, has the right to equal pay for equal work.\n(3) Everyone who works has the right to just and favourable remuneration ensuring for himself and his family an existence worthy of human dignity, and supplemented, if necessary, by other means of social protection.\n(4) Everyone has the right to form and to join trade unions for the protection of his interests." ) ;
		
		MyConsole.addInBuffer( "Article 24.\nEveryone has the right to rest and leisure, including reasonable limitation of working hours and periodic holidays with pay." ) ;
		
		MyConsole.addInBuffer( "Article 25.\n(1) Everyone has the right to a standard of living adequate for the health and well-being of himself and of his family, including food, clothing, housing and medical care and necessary social services, and the right to security in the event of unemployment, sickness, disability, widowhood, old age or other lack of livelihood in circumstances beyond his control.\n(2) Motherhood and childhood are entitled to special care and assistance. All children, whether born in or out of wedlock, shall enjoy the same social protection." ) ;
		
		MyConsole.addInBuffer( "Article 26.\n(1) Everyone has the right to education. Education shall be free, at least in the elementary and fundamental stages. Elementary education shall be compulsory. Technical and professional education shall be made generally available and higher education shall be equally accessible to all on the basis of merit.\n(2) Education shall be directed to the full development of the human personality and to the strengthening of respect for human rights and fundamental freedoms. It shall promote understanding, tolerance and friendship among all nations, racial or religious groups, and shall further the activities of the United Nations for the maintenance of peace.\n(3) Parents have a prior right to choose the kind of education that shall be given to their children." ) ;
		
		MyConsole.addInBuffer( "Article 27.\n(1) Everyone has the right freely to participate in the cultural life of the community, to enjoy the arts and to share in scientific advancement and its benefits.\n(2) Everyone has the right to the protection of the moral and material interests resulting from any scientific, literary or artistic production of which he is the author." ) ;

		MyConsole.addInBuffer( "Article 28.\nEveryone is entitled to a social and international order in which the rights and freedoms set forth in this Declaration can be fully realized." ) ;
		
		MyConsole.addInBuffer( "Article 29.\n(1) Everyone has duties to the community in which alone the free and full development of his personality is possible.\n(2) In the exercise of his rights and freedoms, everyone shall be subject only to such limitations as are determined by law solely for the purpose of securing due recognition and respect for the rights and freedoms of others and of meeting the just requirements of morality, public order and the general welfare in a democratic society.\n(3) These rights and freedoms may in no case be exercised contrary to the purposes and principles of the United Nations." ) ;
		
		MyConsole.addInBuffer( "Article 30.\nNothing in this Declaration may be interpreted as implying for any State, group or person any right to engage in any activity or to perform any act aimed at the destruction of any of the rights and freedoms set forth herein." ) ;

		MyConsole.addInBuffer( "Article 31.\nThis is just a test for very long words: Yourobjectistosavetheworld,whilestillleadingapleasantlife. This should show some word-cutting." ) ;
		
				
		MyConsole.render() ;
		
		LogPlug::info( "Console state after first render: " 
			+ MyConsole.toString() ) ;
		
		
		if ( isBatch )
		{
		
			LogPlug::info( "Non-interactive test mode, "
				"no scheduling granularity computed, test finished." ) ;
				
			return Ceylan::ExitSuccess ;
			
		}	
		
		display( "Hit 'r' for line up, 'c' for line down, "
			"'u' for previous text, 'n' for next text, "
			"'h' for layout change, 'q' to quit." ) ;
			
			
		bool quit = false ;
		
		do
		{
		
			
			switch( getChar() )
			{

			
				// Pseudo up arrow:
				case 'r':
					MyConsole.jumpPreviousLine() ;
					MyConsole.render() ;
					break ;
					
				// Pseudo down arrow:
				case 'c':
					MyConsole.jumpNextLine() ;
					MyConsole.render() ;
					break ;
					
				// Pseudo left arrow: d
				// Pseudo right arrow: f
				
				// Pseudo X key:
				case 'u':
					MyConsole.jumpPreviousText() ;
					MyConsole.render() ;
					break ;
					
				// Pseudo B key:
				case 'n':
					MyConsole.jumpNextText() ;
					MyConsole.render() ;
					break ;
						
				// Pseudo Y key:
				case 'h':
					TextBuffer::TextLayout layout ;
				
					switch( MyConsole.getTextLayout() )
					{
				
						case TextBuffer::Raw:
							layout = TextBuffer::WordWrapped ;
							break ;
						
						case TextBuffer::WordWrapped:
							layout = TextBuffer::Justified ;
							break ;
				
						case TextBuffer::Justified:
							layout = TextBuffer::Raw ;
							break ;
						
						default:
							layout = TextBuffer::Raw ;
							break ;						
				
					}
						
					MyConsole.setTextLayout( layout ) ;
					MyConsole.render() ;
					break ;
					
				// Pseudo A key: j
				
				case 'q':
					quit = true ;
					break ;	
			
			}
			
		}
		while( ! quit ) ; 
		
		LogPlug::info( "End of console test." ) ;

	}
	
    catch ( const Ceylan::Exception & e )
    {
        std::cerr << "Ceylan exception caught : "
        	<< e.toString( Ceylan::high ) << std::endl ;
		return Ceylan::ExitFailure ;

    }

    catch ( const std::exception & e )
    {
        std::cerr << "Standard exception caught : " 
			 << e.what() << std::endl ;
		return Ceylan::ExitFailure ;

    }

    catch ( ... )
    {
        std::cerr << "Unknown exception caught" << std::endl ;
		return Ceylan::ExitFailure ;

    }

    return Ceylan::ExitSuccess ;

}
