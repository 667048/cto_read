{smcl}
{* *! version 0.02 24aug2023}{...}
{hline}
{cmd:help cto_read}
{hline}

{title:Title}

{p2colset 5 17 19 2}{...}
{p2col :{hi:cto_read} {hline 2}}Clean SurveyCTO rawdata{p_end}
{p2colreset}{...}


{title:Syntax}

{p 8 17 2}
{cmd:cto_read}
{cmd:,}
{opt inst:name(filepath)}
{opt data:name(filepath)}
{opt do:file(filepath)}
[{it:options}]

{synoptset 27 tabbed}{...}
{synopthdr}
{synoptline}
{syntab :Necessary}
{synopt :{opt inst:name(filepath)}}filepath to xlsform survey instrument{p_end}
{synopt :{opt data:name(filepath)}}filepath to comma-delimited SurveyCTO-exported rawdata{p_end}
{synopt :{opt do:file(filepath)}}filepath to cleaning dofile to be created{p_end}

{syntab :Optional}
{synopt :{opt american}}read dates in American MMDDYYYY format, rather than DDMMYYYY{p_end}
{synopt :{opt reshape:file(filepath)}}create a second dofile to reshape data to 
long format, saving the dofile to {it:filepath}{p_end}
{synopt :{opt identifiers(namelist)}}specify survey-level variables to be copied to {it:all}
 reshaped datasets{p_end}


{title:Description}

{pstd}
{cmd:cto_read} is a Stata module designed to facilitate the processing of survey
 data collected using SurveyCTO, and in particular, for surveys using instruments
 that have been designed using an xlsform. Using the xlsform, as well as the 
survey rawdata produced by SurveyCTO, cto_read produces a well-linted
dofile designed to label, format, and otherwise perform systematic cleaning of the rawdata.

{pstd}
Optionally, it can also systematically produce a dofile to reshape rawdata to long format
datasets linked by index variables. 

{title:Remarks}

{pstd}
Using {cmd:cto_read} obviates two pitfalls of development data collection:

{pstd}
1. Relying on sleep-deprived RAs to manually and inefficiently 
clean data (whilst introducing human error and/or spaghetti code)

{pstd}
2. Using SurveyCTO's own auto-generated dofile (which, whilst useful, has its own limitations) 

{pstd}
Data cleaning is an order of magnitude easier if you systematise your approach to each
variable according to meticulously prespecified criteria such as variable format, 
logical constraints, and abbreviated labels. That's precisely what an xlsform does, 
and we should be able to leverage that effort to create tailored cleaning dofiles.
That is the philosophy behind {cmd:cto_read}. It means no duplication of effort once
the xlsform has been built, but it also means a rigorous and systematic approach
to data cleaning that leaves you with clearly interpretable survey data. It
complements an automated workflow, as {cmd:cto_read} can be called within a "master" 
project dofile before the dofile(s) it produces to maintain an up-to-date database.

{pstd}
An alternative use of {cmd:cto_read} is to generate a template dofile that can then be 
manually edited by the user. This may be preferable if, for example, a variable that is
preloaded should always be treated as a numeric variable, rather than string. By default,
{cmd:cto_read} will treat preloaded variables as strings. However, this use may be 
inconvenient in a context where the data is regularly updated - the command 
uses the rawdata to store the maximum number of repeats for each survey 
repeat group, and anticipates the names of the variables to be cleaned based
on this information. If the observed maximum number of repeats for a particular
repeat group increases, this will imply a need to update the list of variables
to be cleaned. Secondly, survey designers are only human, and may need to issue 
several versions of a survey before they are content that it is running as designed.
Each time the survey instrument is updated, this will require a change in the code to
clean it.

{pstd}
At this time, the behaviour of {cmd:cto_read} with respect to nested repeat groups
is to correctly anticipate variable names (and then clean these variables) up to an 
infinite number of nests. However, for long-format data aficionados such as myself,
the {cmd:reshapefile()} option will only reshape repeat groups at one level of nesting,
that is, a repeat group within one other repeat group. If you're trying to reshape a repeat
group within a repeat group within {it:another} repeat group, you'll have to do that on your own.
This is partly because coding the dofile to correctly deal with multiple layers of nesting
is complicated. But, in my defence, it's also because so many layers of nesting is probably
suboptimal. Imagine a survey submitted at community-level, with a repeat group for each
household, and a repeat group within this for each person in the household. {cmd:reshapefile()}
will correctly deal with this reshaping for you to produce separate community-, household- and 
person-level datasets, as the person-level data is nested within 1 other repeat group 
(the household-level). Complicating the matter further by adding another repeat group
more granular than person-level seems like a strange setup for a survey - in 
this particular example, it would probably be best to run the survey
at household-, rather than community-level. 

{pstd}
I highly recommend that, when designing your xlsform, you add a column with the heading 
{it:label::Stata}. The standard behaviour of this command is to save the full text of 
the actual question (stored in either a column titled {it:label} or {it:label::English(en)}
as a note to the variable, but to look for an alternative variable label stored
in the {it:label::Stata} column. It is often a bad idea to label your variable with the full
text of the question it describes - not least because the question usually runs over Stata's
80-character variable label limit. By using the xlsform to set proper variable labels, you'll
find it much easier to navigate your cleaned dataset.


{title:Author}

{pstd}
Michael Rozelle, Wageningen University and Research, The Netherlands. 
Email: {browse "mailto:michael.rozelle@wur.nl":michael.rozelle@wur.nl}
{p_end}