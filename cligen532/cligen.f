c
c
c     Cligen version 5.32.1  04/06/2022  Roger Lew
c       - Fix for leap years to include years divisible by 400 but not
c          divisible by 100
c
c     Cligen version 5.32  03/14/2013 Jim Frankenberger
c       - using observed option (type=6) the tpeak variable
c         was being generated based only the cligen predicted days
c         with precip so most days with observed data had tpeak=0.
c         Updated to make sure type 6 input has the same distribution
c         of tpeak as generated precip.
c
c       Cligen version 5.31  01/31/2013 Jim Frankenberger
c        - Corrected solar radiation, it was not using standard
c          deviations from input par file. Increased year field to 5
c          digits for 10000 year+ runs.
c
c       Cligen version 5.30002. 09/14/09 Fred Fox
c        - Extended character string length for reading command line
c          arguments to allow longer output path length on linux.
c          Should now allow paths as long as WEPS. Path length on
c          Windows was already longer than maximum allowed.
c
c	Cligen version 5.30001. 06/30/09 Bill Rust
c	 - Changed order of reading in wet/wet & wet/dry
c	   Equivalence statement had interleaved the values
c	   making them incorrect
c	   Affected only Yoder-Foster & Fourier interpolation
c
c       Cligen version 5.3. 01/15/2008 Jim Frankenberger
c        - Corrected dew point calculation for a type 6 run.
c          Dew point was not being calculated from tmin and tmax values
c          read from the observed input.
c
c       Cligen version 5.22564.  10/26/2004.  C. R. Meyer
c        - In SR R5MONB variable WI(I) which is used to calculate AI,
c          was getting hosed when F went negative.  This would mess up Ip
c          for very dry climates where the average number of rainfall
c          events is less than 0.5 .
c
c       Cligen version 5.22563.  10/21/2004.  C. R. Meyer
c        - In SR USR_OPT variable NDFLAG was not being initialized unless
c          the type of run was 6 or 8.  Added a default "else" to the loop
c          to set it to zero.  Thanks to Larry Wagner & Fred Fox of WERU 
c          for pointing out this error.
c
c       Cligen version 5.22562.  10/01/2004.  C. R. Meyer
c        - Declared av_len to be integer in commandX.inc.  Note "command5.inc"
c          is no longer used -- "command6.inc" replaces it.
c
c       Cligen version 5.22561.  09/30/2004.  C. R. Meyer
c        - Using the Lahey compilers for Linux and Windows, a typo in SR RYF1
c          was generating a subscript-out-of-range error, unlike the GNU
c          Win-32 compiler.  Many thanks to Fred Fox and Larry Wagner of
c          ARS-WERU for their patience and persistence in pointing out that 
c          error.
c
c       Cligen version 5.2256.  07/02/2004.  C. R. Meyer
c        - For the Bloomington, Indiana site the Gamma distribution could 
c          not produce the desired level of quality with a lot size of just 
c          20 numbers.  An infinite loop resulted.  The lot size was increased 
c          to 30.
c
c       Cligen version 5.2255.  05/05/2004 - 05/06/04.  C. R. Meyer
c        - Running Excel with varying bin sizes from 10 to 20 showed that
c          the Chi-square test gave very inconsistent results in evaluating
c          goodness of fit for 30 year runs at 7 stations.  Reading articles
c          on the Internet revealed that some authors say Chi-Sq is only
c          applicable to discrete distributions -- for continuous distros
c          one should use the Kolmogorov-Smirnov test.  One problem: I
c          could not find D-value for P=50%; ie, P(50).  Equation is
c          P(D) = sum((-1^k)*exp(-2*k^2*D^2), k=-infinity, +infinity .
c          Wrote routine KS_test, using D-value derived interatively
c          using a spreadsheet (0.8276).
c
c       Cligen version 5.2254.  04/03/2004 - 04/05/04.  C. R. Meyer
c        - Fixed bug in DSTG that sometimes resulted in an infinite loop.
c          Removed third parameter (rn1) from DSTG, since it is in common
c          block crandom3.inc .
c
c       Cligen version 5.2253.  11/07/2003 - 01/07/04.  C. R. Meyer
c        - Changed generation of daily temperature values so that monthly
c          means & SD's of neither Tmin or Tmax are altered "up front", but
c          day-by-day as Tmin, Tmax, & Tdew are generated together for that
c          day.  Daily values are based on the _difference_ from Tmin or
c          Tmax -- whichever one has the smaller SD for that day.  This
c          approach developed after a suggestion by Fred Fox.
c
c     NOTE: With this version there are several mathematically ingenious features
c           incorporated in Cligen.  a) quality control on the random number 
c           generator, which is totally necessary and to date not known to be 
c           provided in any other stochastic model;  b) the generic random number 
c           generation scheme used for the Gamma, which can be employed for _any_
c           _function_ that can be utilized between x and f(x) max-min limits;
c           c) use of _monthly_ distributions of Tmin, Tmax, and Tdew to generate
c           related _daily_ values of these parameters which are based on each 
c           other, and reproduce the monthly means and SD's.
c
c       Cligen version 5.224.  10/20-20/2003.  C. R. Meyer
c	Changes:
c        - Changed generation of daily Tmin value from being independently 
c          generated, to being generated based on Tmax minus the _difference_
c          Tmax-Tmin.  This makes the daily Tmin a function of the daily Tmax
c          and can be accomplished using the same inputs.
c
c       Cligen version 5.223.  10/08-16/2003.  C. R. Meyer
c	Changes:
c        - Added QC code to function DSTG, which is involved in calculating
c          individual storm durations and peak intensities.
c        - In DSTG used *TWO* uniform deviates to generate each Gamma
c          deviate, instead of one as has been incorrectly used up to now.
c        - Obsoleted crandom2.inc, replacing it with crandom3.inc.
c
c       Cligen version 5.222.  09/23/2003.  C. R. Meyer
c	Changes:
c        - Reverted from "xx=rn1*5.795" to "xx=rn1*ai" in function DSTG.
c          Storm duration was getting hosed.
c
c       Cligen version 5.221.  09/23/2003.  C. R. Meyer
c	Changes:
c        - Disabled use of file "rand_nrs" which exists solely to verify
c          quality of random numbers.
c
c       Cligen version 5.220.  08/04/2003.  C. R. Meyer
c	Changes:
c        - Implemented a Chi-square test with up to 20 bins to test the RNG.
c        - Corrected a long-standing minor coding bug in 10,000th
c          (unsuccessful) retry.
c
c       Cligen version 5.213.  08/01/2003.  C. R. Meyer
c	Changes:
c        - In DSTG the maximum-x allowed for the Gamma function was originally
c          restricted to 'ai', the ratio of "total rain" to "30-minute rain".
c          Ai is different for each site & month combination.  Chi-square tests
c          on 5 cases where ai ranged between 1.49 and 2.25 show this virtually
c          guarantees a poor match between inputs and outputs (P => 0.98 in all
c          5 cases tested).  Integrating the function using Maple reveals that
c          it continues to accrue area under the curve until 'x' reaches 5.795,
c          reaching 99 percent by x = 2.9.  So the maximum x used in DSTG was
c          changed from ai to a constant 5.795, which yields P < .625 for the
c          same 5 cases.  This calculation impacts Storm Duration and Ipeak.
c
c       Note: In "Evaluation of CLIGEN precipitation parameters and their
c             implication on WEPP runoff and erosion prediction", Trans. ASAE,
c             Vol. 46(2):311-320, Zhang and Garbrecht seemed to feel CLIGEN
c             V-5.107's main problem was the single storm characteristics.
c             They specifically mentioned storm duration, peak intensity, and
c             Tpeak which are impacted by these most recent changes to CLIGEN
c             (V-5.212 & V-5.213).  The areas that C. R. Meyer had already
c             addressed, seemed to be functioning satisfactorily.
c
c       Cligen version 5.212.  07/24/2003.  C. R. Meyer
c	Changes:
c        - RANSET was radically recoded for Tpeak which chi-square tests 
c          revealed was _not_ uniformly distributed.  Tpeak values are now
c          only generated for days with precip, instead of generating them
c          for all days and then using the values only for days with precip.
c          The RANSET code was also generally cleaned up.
c        - Added comments to DSTG explaining generation of random Gamma
c          deviates.  This is a general approach which can be used with *ANY*
c          PDF.  It requires you to pick a maximum and minimum value for X
c          and f(X).  It requires a uniform random number generator which
c          functions correctly.  Without using a quality control filter,
c          we have not found one which does.  It is also iterative, so you
c          cannot predict exactly how many values you'll have to generate.
c
c       Cligen version 5.200.  03/13/2003.  C. R. Meyer
c	Changes:
c        - At line #1061 (SR CLGEN), added limits to keep the monthly skew
c          coefficient for precipitation from getting larger than 4.5 or
c          smaller than -4.5 .  This is necessary because the Pearson Type-III
c          equation used generates negative precip values, which are converted
c          to the default value of 0.01 inch (0.3 mm).  With high skew values,
c          90+ percent of the generated values can be negative.
c
c       Cligen version 5.111.  10/16&17/2002.  C. R. Meyer
c	Changes:
c        - Added some prompts for users running Cligen interactively using the
c          "old" datasets, advising use of the F. S. data.  Also prompt with
c          instructions regarding how to _fix_ the problem, if "stations" file
c          or state file are missing.  Added prompt for international users
c          to consult "Survey of Climatology", 1982, by Griffiths & Driscoll.
c         (All changes below involve "option 6" which uses observed precip
c          and temperature data from one station, in conjunction with station
c          parameters for another (similar) station.)
c        - Allow years to have more than 2 digits in the output file.
c        - Allow runs of more or less than 100 years.  100 years becomes
c          the default.
c        - Allows user to specify beginning year.  If unspecified, uses
c          first year listed in observed data file.
c        - Allow option-6 runs from the command line.  This entails adding
c          a new switch, capital-O, for the observed data filename.  Note:
c          "command4.inc" is no longer used -- "command5.inc" replaces it.
c
c       Cligen version 5.110.  10/26/2001.  C. R. Meyer
c	Changes:
c        - Records any command line parameters by appending them to the
c          fifth line of the output file.  Note: "command3.inc" is no
c          longer used -- "command4.inc" replaces it.
c        - Eliminates infinite loop caused by specifying an output file
c          that already exists.
c
c	Cligen version 5.109.  10/11/2001.  C. R. Meyer
c	Changes:
c	 - Increases length of permissible command line argument (argv)
c          from 60 to 256 characters.  This permits longer file pathnames
c          to be used on the command line.  Note: "command2.inc" is no
c          longer used -- "command3.inc" is used instead.
c
c	Cligen version 5.108.  8/29/2001.  C. R. Meyer
c	Changes:
c	 - Corrects problem with temperature dewpoints, under option 6.
c	   Added interpolation code to SR clgen for when msim=0.  Previously
c	   overlooked.
c
c       Cligen version 5.107.  5/11-17/01.  C. R. Meyer
c       Changes:
c        - There are now two cases handled for station data inputs:
c           1) Runs where _both_ a state and station index are specified.
c              If an input file is specified, it is used.  If not, the
c              corresponding state file is used.  This case applies to
c              files of concatenated FS data.
c           2) Other runs with an input filename.  In this case it is assumed
c              to be a single-station parameter file, and the _first_ station
c              found is used.
c        - Changed code in SR "sta_dat" that stupidly blows past exactly 
c          81 lines when Station-ID and State don't match.  (Approx. line 
c          number: 1913.)  Revised code reads each line until it finds a
c          match to both state and station.  This lets Cligen read files
c          of concatenated FS data without WERU's "fs_flag".
c
c       Cligen version 5.106.  5/4/01. C. R. Meyer
c       Changes:
c        - Accepts filenames which contain blanks & slashes.  (Must enclose 
c          filenames in double-quotes under MS-Windows.)
c        - Reversed meaning of '-H' command line option.
c        - Cull out options not preceeded by dashes.
c        - Program termination message changed.
c        - Syntax error corrected for "unknown option".
c
c       Cligen version 5.105.  4/17/01.  C. R. Meyer
c       Changes:
c        - There is one place where Cligen is iterative -- in the QC process
c          applied to the RNG (SR RANSET).  The only exit was production of
c          a distribution meeting the acceptance criteria.  Not guaranteed!
c          For Yuma, AZ and Wupatki Natl. Mon., AZ, the program got hung in
c          an infinite loop as it tried to produce acceptable standard normal
c          deviates for Wind Velocity and Precip.  (Observation: It was possible
c          to greatly improve the situation for WV by changing the random seed
c          k8(4) from 31 to 41, but this is not the solution pursued.)  A
c          counter "iredo" was inserted in the loop, and when it reaches
c          10,000 an error is printed to the screen, and an exit is effected
c          from the loop.
c        - In SR CDFCHI initialized PORQ to zero with a data statement.
c
c       Cligen version 5.104.  4/5/01.  C. R. Meyer
c       Changes:
c        (With the addition of command line argument capability, Cligen
c         was modified to run: totally in command line mode; totally
c         interactively; or a mixture of the two.  A couple of interactive
c         options had been disabled in the process.  If the value of
c         "numarg" is zero, the mode is totally interactive.  Now, for
c         that mode behavior again duplicates V-4.2, given the same inputs.)
c        - Asks if user wants to make another run.
c        - Asks if user wants to see Station Parameters.
c
c	Cligen version 5.103.  4/5/01.  C. R. Meyer
c       Changes: 
c        (All the changes made are purely cosmetic.  The outputs produced
c         remain totally unchanged.)
c        - Within an MS-Windows DOS window, a space is incorrectly prepended
c          to each line.  The station parameters appear double-spaced
c          because each line of data requires *two* lines for display.
c          The first half of the stations to scroll off the screen before they
c          can be read.  Write formats were altered to produce single-spaced
c          screen outputs.
c        - Some debugging statements were commented out, which displayed
c          parameters to the screen, like Station_ID, State, and Ibyear
c          (beginning year).
c        - Some changes to facilitate more global version number changes, by
c          changing the value of the variable "version".
c
c       Cligen version 5.102.  3/20/01.  C. R. Meyer
c       Changes:
c        - Correction to +/- 10.0 SD range check in DSTN1.
c        - RANSET now uses NTD, because NT only reflects whether the _initial_
c           year is a leap year, not the current one.
c        - RANSET uses ELLX to save the value of ELL (whether yesterday had
c           precipitation or not) for the current call, in case rainfall
c           amounts are re-generated.
c        - RANSET uses LST_RX to save the value of LAST_X (value from previous
c           day) in the event of a re-do.
c
c       Cligen version 5.101.  2/6/01.  Changes made to SR RYF1 to accomodate
c       occurance of three identical consecutive monthly values.
c
c	Cligen source as of 01/25/2001.  Changed version number to "5.1".
c
c	Changes made as of 11/08/2000:  Modified to permit use of single
c       station ".par" input file with simulation types other than continuous.
c       Also changed version number to "4.2c".  Added "-rxxx" "-Ix" to first
c       of output file, so random number cycles and interpolation used could
c       be re-created.  Commented lines that produce files containing results
c       of CI tests on the means & SD'.
c
c       Changes made as of 8/30/2000:  "-h" option added (equivalent to
c       existing "-?" help option).  Corrected definitions for PRW(1&2)
c       which had been reversed.  Added interpolation -- linear, Fourier,
c       and one to preserve the monthly means.  For INDY max temp the 
c       worst deviation between the monthly average of daily values and
c       average monthly values, for this latter scheme was 0.002 degree 
c       F for an individual month, and 0.0003 overall.
c
c       This version includes one of Bofu Yu's corrections that had 
c       been missed earlier.  The code is verified to produce results
c       identical to Bofu's, using the 11/11/99 release of CLIGEN,
c       which is recoded to the WEPP coding convention, but which does
c       not include the RNG-QC code.
c
c       Adds command line options including choice of a State & Station
c       from the CLIGEN climate files, or use of a single input file.
c       Also accepts arguments specifying starting year, duration of run,
c       output file name, output header info, output version info, and
c       modify the random number seed.
c
c       This is done through use of the UNIX & GNU "getarg" & "iargc" options.
c       Options are also provided for the Watcom and  BSD(?) UNIX compilers.
c       (Simply uncomment the 2 lines approriate for your compiler.)
c       WERU functions nargs() & getarg() are provided for use with Lahey
c       which has no direct means for returning argument count, or specific
c       arguments.  (This code was significantly modified and tested with 
c       Lahey 4.0 F-77 -- a 1989 product, which, incidentally _has_ a built
c       in nargs() function, but it only works for SUBprograms.)
c
c       This version also tests both the mean and the variance of the
c       standard normal deviates generated, using a prescribed value for
c       each ("thresh" & "thres2") set here at 50 percent.  Calculations 
c       are performed by parameter, by month, on the population of numbers
c       generated to the current point in the simulation.
c
c      	CLIGEN V-4.2 with the following differences:
c        - Recoded by the WEPP F-77 Coding Conventions.  The logic
c          is greatly simplified; the structure of the code is much
c          improved; and the in-line documentation is greatly expanded.
c        - Includes Bofu Yu's corrections to make rainfall intensity 
c          responsive to latitude.  Routines ALPH and R5MON were 
c          replaced.  Constants DUR in DAY_GEN, and XN1 in DSTG were
c          altered.
c        - Cligen's outputs start with 9 uniform random distributions
c          and subsequent corresponding standard normal distributions.
c          Parameters derived from historic data are scaled from these
c          to produce daily values.  Statistical testing demonstrated
c          that more often than chance would dictate, the starting
c          distributions were doing a poor job of reproducing the numbers
c          they originated from.  A feedback loop was added to apply
c          "quality control" to the distributions are they are being 
c          produced.
c
c       To compile under UNIX execute:
c         "f77 -o cligen cligen.f"
c
c       To compile for W-95/98/NT:
c         Use the GNU MinGW-32 compiler.
c         From the W-9x command line execute:
c          "g77_setup.bat"
c          "g77 -o cligen cligen.f"
c
c --------------------------------------------------------------------------
c
c     program Cligen. WEPP Water Erosion Project Durant, OK.  Version 5.1
c     Please address inquiries to
c
c         WEPP Technical Support
c         USDA-ARS-NSERL
c         275 South Russell St.
c         West Lafayette, IN 47907-2077
c         Phone 765 494-8673
c
c --------------------------------------------------------------------------
c
c     (Radically) Recoded by Charles R. Meyer   August - November 1999.
c     e-mail: meyerc@ecn.purdue.edu  (Same surface mail as above.)
c     Note that questionable lines contain the string 'XXX'.
c     MANY variable definitions provided by David Hall, USFS, Moscow, ID.
c
c              Structure of Recoded CLIGEN:
c      
c      Main---sta_dat---header
c           |         |-sta_name
c           |         |-sta_parms
c           |   
c           |-r5mon
c           |-*randn
c           |-usr_opt
c           |-sing_stm
c           |-wxr_gen---*jdt
c                     |-day_gen---jlt
c                     |         |-clgen---*dstn1
c                     |         |       |-*randn
c                     |         |       |---ranset
c                     |         |         |-*randn
c                     |         |
c                     |         |-windg---*dstn1
c                     |         |-alph---*dstg--*randn
c                     |         |-timepk
c                     | 
c                     |-opt_calc---clmout
c       
c       
c      * -- denotes function. 
c       
c      Note: NRMD does not seem to be used.
c       
c --------------------------------------------------------------------------
c     DSTN1 requires a pair of random numbers (supplied by RANDN).  A scheme
c     of "reuse" was instituted in which the first random number for each
c     pair is generated at the beginning of the run.  Then during the run,
c     the second is generated; DSTN1 is called; and the second number replaces
c     the first.  For example to generate daily maximum temperature values,
c     initially v1 = randn(k2); then v2 = randn(k2); DSTN1 is called with
c     arguments v1 & v2; v1 = v2; and the loop repeats.  Each population of
c     random numbers is generated from its own set of seeds, making it progress
c     independently of the others, AND ensuring that a random sequence was
c     used for each parameter without omitting values.  (Note that subsequent
c     days for a given parameter are NOT independent of each other!)  Below is
c     a summary of the parameters ane their use.
c     -- CRM -- 4/25/2000.
c
c             Used_For:    Deviate(s):   RandomSeeds:
c            ----------------------------------------
c              MaxTemp      v1, v2           k2
c              MinTemp      v3, v4           k3
c             Radiation     v5, v6           k4
c              Precip       v7, v8           k5
c              WindVel      v9, v10          k8
c                TDP        v11, v12         k9
c            ----------------------------------------
c             PrecipProb    vv               k1
c               DSTG        rn1, rn          k7
c              WindDir      fx               k6
c             TimeToPeak    z                k10
c
c --------------------------------------------------------------------------
c      
c     Version 4.2 April 1997  West Lafayette, IN.
c     Correction of version numbers.
c     Multiple year generation, output file, and summary added
c     Dewpoint temperature added with wind speed and velocity
c     using mean standard deviation and skew coefficient.
c     Change made to place parameter in state files (i.e. al Alabama parms).
c     All parameters rainfall, temperature, radiation, dewpoint temperature,
c     wind speed and direction are combined into one set with 82 lines of data.
c     Storm duration calculation change (4.607 to 9.210).
c     Weighting factor removed from Maximum, Minimum, Dew Point Temperature and
c     Solar Radiation.
c     Addition of formatting to allow use of interpolated station file.
c
c     + + + COMMON BLOCKS + + +
      include 'cbk1.inc'
c      write: pi2
c     pi2         - Pi * 2; ie, a full rotation.
c
      include 'cbk4.inc'
c      read: iopt
c      write: nt
c     iopt        - Weather Generator Options:
c                    1 - Single Year Simulation - Screen Output
c                    2 - Multiple Year - Screen Output',/,
c                    3 - Multiple Year Simulation - CREAMS - GLEAMS Output File
c                    4 - Selected Single Storm WEPP - Output File
c                    5 - Multiple Year - WEPP Output File
c                    6 - Read Observed P and Temp and Generate Missing Data
c                    7 - Single Design Storm - TR 55 Storm Type WEPP Output Filec                    8 - Exit Weather Generator Program
c     nt          - Set to 1 if IYEAR is not a leap year: otherwise, zero.
c
      include 'cbk7.inc'
c      read: prw,k1,k2,k3,k4,k5,k7,k8,k9
c      write: v1,v3,v5,v7,v9,v11,yls,ylc,pit,nsim,msim,l
c     prw(12,1)   - monthly probability of wet day after wet day
c     prw(12,2)   - monthly probability of wet day after dry day
c     k1 ... k9   - Seeds for random number generation.
c     v1 ... v11  - Random numbers used to generate various daily values.
c     yls         - ??? -- Used to compute CH and YS. sin(ylt/clt) sin(latitude)c     ylc         - ??? -- Used to compute CH and YC. cos(ylt/clt) cos(latitude)c     pit         - ??? -- Used to compute SD.  Defined as pit=58.13
c     nsim        - Set to one under option-6 if precip data is missing.
c     msim        - Set to one under option-6 if temperature data is missing.
c     l           - Set to either 1 or 2; linked to nsim 0 or 1; selects PRW.
c
c
      include 'cbk5.inc'
c      write: sml
c     sml         - Used to compute R1
c
      include 'cbk9.inc'
c      write: ab1,rn1
c      modify: wi,ab
c     ab1         - Set to 1.0-ab, and used to calculate AI
c     rn1         - ??? -- used for precip gamma dsn
c     wi          - Average Maximum .5 Hour Precip. Intensity (by month)
c     ab          - Set to 0.02083, and used to calculate AI
c
      include 'crandom3.inc'
c      (Referenced here to "save" the variables until CLIGEN exits.)
c
      include 'command6.inc'
      include 'cinterp.inc'
c
c
c     + + + LOCAL VARIABLES + + +
CC    integer numarg
      character*512 argv
      integer ti(4)
      real sumpp(13),sumptx(12),sumptm(12),sumprd(12),sumpdr(12)
      real smy(12),wgt(3),tymax(4),timpkd(0:12),tmpcmx(12),tmpcmn(12)
      integer elev,years,moveto
      character*1 yc
c     character*6 nstat
      character*41 stidd
c
c     + + + LOCAL DEFINITIONS + + +
c     numarg      - number of arguments passed on the command line.
c     argv        - a specified arg from the list of command line arguments.
c     infile      - cligen db input file name (interactive mode)
c     odfile      - observed data file for option 6.
c     ti          - variable for setting random seed from system clock.
c     dohedr      - flag which tells whether to add a WEPS header.
c     yc          - 1-character user response (y/n).
c     outfil      - Cligen Output (.cli) file name.
c     xx          - latitude / 57.296; ie, in radians
c     vv          - random deviate (Probability of Precip today).
c     nt          - 0 or 1 ("leap year?" for jdt) (iopt = 4, 7)
c     r5max       - "max of monthly maximum .5-hr rain"
c
c      Variables Passed to other Modules:
c     clt         - 57.296 180/pi: deg -> radians convert; deg/clt -> radian
c     damt        - Design Storm Amount in Inches for Single Storm.
c     elev        - Station Elevation above Sea Level (whole number of meters)
c     jd          - Day of the Storm.
c     igcode      - wind information/ET equation flag
c                      0 -- wind information exists: use Penman ET equation
c                      1 -- no wind information exists: use Priestly-Taylor
c                           ET equation
c     istate      - Numeric Climate Code of Desired State.
c     index       - 4-digit numeric station index.
c     ioyr        - first # of "infile" (-> ibyear) (iopt 6)
c     itype       - integer value [1..4] to set single storm parameters.
c     iyear       - Beginning Simulation Year.
c     moveto      - A global flag.  If set to 'XX' it means "goto XX".
c     ntd1        - julian date of jd, mo (iopt = 4, 7)
c     numyr       - number of years to simulate
c     smy         - Observed Monthly Average Precipitation (mm)
c     stidd       - 41-character alphanumeric station name.
c     sumpp(13)   - "prcp" (average monthly values for numyr years)
c                     (13: average annual precipitation)
c     sumptx(12)  - "tmax" (average monthly values for numyr years)
c     sumptm(12)  - "tmin" (average monthly values for numyr years)
c     sumprd(12)  - "rad" (average monthly values for numyr years)
c     sumpdr(12)  - "dur" (average monthly values for numyr years)
c     timpkd      - The 12 interval time to peak accummulated distribution
c                   parameters for the station.  Cumulative distribution of 
c                   computed time to peak rainfall intensity values based on
c                   NWS 15-minute rainfall data (section 2.1.4 WEPP tech 1995)
c     tmpcmx      - Observed Monthly Average Max Temperature (C)
c     tmpcmn      - Observed Monthly Average Min Temperature (C)
c     tp6         - maximum 6 hour precipitation depth (inches).
c     tymax(4)    - upper limit of r5p (based on itype)
c     usdur       - [User Supplied] Storm Duration in Hours for Single Storm.
c     ustpr       - [User Supplied] Time to Peak Intensity (% Duration e.g. .4).
c     uxmav       - Maximum Intensity Inches/Hour for Single Storm.
c     version     - CLIGEN version (ie, 5.103)
c     wgt(3)      - 3 wind station weights used for triangulation -- weighting
c                    factor for wind stations used for interpolation
c     xm          - number of days in the month of interest
c     years       - Years of Record at the Station.
c     ylt         - Station Degrees Latitude (+ is N, - is S).
c     yll         - Station Degrees Longitude (+ is E, - is W).
c
c     + + + SUBROUTINES CALLED + + +
c     sta_dat
c     r5mon
c     usr_opt
c     sing_stm
c     wxr_gen
c
c     + + + FUNCTION DECLARATIONS + + +
      real randn
c
c     + + + DATA INITIALIZATIONS + + +
      data tymax/180.34,154.94,307.34,330.2/
c
c     + + + OUTPUT FORMATS + + +
 2000 format(/22x,'Average Values for ',i2,' Years'/)
 2010 format(1x,'elem',' yr','   J     F     M     A     M     J',
     1                      '     J     A     S     O     N     D'/)
 2020 format(1x,'prcp   ',12f6.2)
 2030 format(1x,'tmax   ',12f6.2)
 2040 format(1x,'tmin   ',12f6.2)
 2050 format(1x,'rad    ',12f6.1)
 2060 format(1x,'dur    ',12f6.2)
 2070 format(/1x,'Average Annual Precipitation for ',i2,
     1' Years =',f6.2,a30/)
 2080 format(/1x,'Do you want to continue (y/n)? ')
c
c     + + + END SPECIFICATIONS + + +
      version=5.3210
c
c *************************************************************
c **************** BEGIN COMPILER-SPECIFIC CODE ***************
c *************************************************************
c     NOTE: Functions to determine the number of command line
c           arguments, and to return the individual arguments
c           are specific to the compiler used.  For Lahey they
c           apparently do not exist.  The functions narg() and
c           argopt() are included here to correct that deficiency.
c           Several popular compilers are supported by this code.
c           Simply locate the compiler of choice and uncomment
c           the related lines.
c
c     NOTE: Verify that for your compiler the number of command
c           line arguments does not include the command itself;
c           ie, for a command without arguments, the value returned
c           is zero.  If it is "one", simply subtract one from 
c           "numarg".
c     
c
c ----Determine Number of Command Line Arguments.
c ---- UNIX (BSD(?)):
c     numarg = nargs()
c ---- Lahey:
c     numarg = narg()
c ---- Solaris (System-V UNIX), GNU, & Watcom:
      numarg = iargc()
c     write(*,*) 'NUMARG:', numarg
c
c ---- Process Each Command Line Option Specified.
c ----- *** L1 IF ***
      if (numarg .gt. 0) then
        do 09 i = 1, numarg
c ------Read Each Command Line Argument.
c ------ BSD(?) UNIX:
c         call getarg(i, argv, ii)
c ------ Lahey:
c         call argopt(i, argv)
c ------ Watcom:
c         ii = igetarg(i, argv)
c ------ Solaris (System-V UNIX) & GNU:
          call getarg (i, argv)
c *************************************************************
c ***************** END COMPILER-SPECIFIC CODE ****************
c *************************************************************
c
c *********** Append COMMAND LINE ARGUMENTS to arg_v **********
          if(i .ne. 1) then
c ----------- Append current argument to arg_v.
            write(arg_v(av_len+1:), '(a)') argv
c ----------- Find length of string.
            av_len = 1025
 1001       av_len = av_len - 1
            if(arg_v(av_len:av_len) .eq. ' ' .and. av_len .gt. 1) 
     1        goto 1001
            av_len = av_len + 1
          else
            write(arg_v, '(a)') argv
            av_len = 1025
 1002       av_len = av_len - 1
            if(arg_v(av_len:av_len) .eq. ' ' .and. av_len .gt. 1) 
     1        goto 1002
            av_len = av_len + 1
c ----------- Find length of string.
          endif

C     write(*,*) arg_v(1:av_len)
c
c ************ BEGIN PARSING COMMAND LINE ARGUMENTS ***********
c
c         Make sure all options start with '-'
          if(argv(1:1) .ne. '-') then
            write(*,*) 'Option ignored, no option flag: ', argv
c
c         State Number:
          else if(argv(2:2) .eq. 'S') then
            read(argv(3:),'(i2)') istate
c
c         Station Number:
          else if(argv(2:2) .eq. 's') then
            read(argv(3:),*) index
c
c         Random Seed Value:
          else if(argv(2:2) .eq. 'r') then
c ---------- use default seed values
            if(argv(3:).eq.'0') then
               continue
c ------- use totally random seeds (based on system clock)
            else if(argv(3:4).eq.'-1') then
C             call gettim(ti(1),ti(2),ti(3),ti(4))
C             ti(3)=ti(4)+ti(3)*7+ti(2)*5+ti(1)*3
C             ti(3)=ti(3)-(ti(3)/199)*199
C             if (ti(3).lt.0) ti(3)=0-ti(3)
C             do 2 j=1,ti(3)
C               fx=randn(k1)
C               fx=randn(k2)
C               fx=randn(k3)
C               fx=randn(k4)
C               fx=randn(k5)
C               fx=randn(k6)
C               fx=randn(k7)
C               fx=randn(k8)
C               fx=randn(k9)
 2            continue
c -------- use specified seed value (to discard the designated number of RN's)
            else
              read(argv(3:),*) irand
              do 3 j=1,irand
                fx=randn(k1)
                fx=randn(k2)
                fx=randn(k3)
                fx=randn(k4)
                fx=randn(k5)
                fx=randn(k6)
                fx=randn(k7)
                fx=randn(k8)
                fx=randn(k9)
 3            continue
            endif
c
c         Simulation Type:
          else if(argv(2:2) .eq. 't') then
            read(argv(3:),*) iopt
c
c         Beginning Year:
          else if(argv(2:2) .eq. 'b') then
            read(argv(3:),*) ibyear
c
c         Duration in Years:
          else if(argv(2:2) .eq. 'y') then
            read(argv(3:),*) numyr
c
c         Input File:
          else if(argv(2:2) .eq. 'i') then
            infile = argv(3:)
c
c         Output File:
          else if(argv(2:2) .eq. 'o') then
            outfil = argv(3:)
c
c         Force overwrite of output file:
          else if(argv(2:2) .eq. 'F') then
            force = 1
c
c         Header in Output File:
          else if(argv(2:2) .eq. 'H') then
            dohedr=.false.
c
          else if((argv(2:2) .eq. 'v') .or. (argv(2:2) .eq. 'V')) then
            write(*,*)
c           write(*,*) 'CLIGEN - Climate Generator V-4.2c August 2000'
c           write(*,*) 'CLIGEN - Climate Generator V-5.1 August 2000'
c           write(*,*) 'CLIGEN - Climate Generator V-5.101 Feb. 2001'
c           write(*,*) 'CLIGEN - Climate Generator V-5.102 Mar. 2001'
c           write(*,*) 'CLIGEN - Climate Generator V-5.103 Apr. 2001'
            write(*,"('CLIGEN - Climate Generator V-', f7.5,
     1                ' Jan. 2013')") version

            write(*,*) 'Modified to support Command Line Options.'
            write(*,*)
c
          else if(argv(2:2).eq.'?' .or. argv(2:2).eq.'h') then
            write(*,*)
            write(*,"(' CLIGEN V-', f7.5, ' - Climate Generator w/ QC-',
     1                'SNDG')") version
            write(*,*) '   Usage:'
            write(*,*) '    cligen -S<state number> -s<station ID number
     1>'
            write(*,*) '           -i<input file name> -o<output file na
     1me>'
            write(*,*) '           -b<beginning year> -y<duration in yea
     1rs>'
            write(*,*) '           -f (old WEPS record format)'
            write(*,*) '           -F (overwrite output file if it exist
     1s)'
            write(*,*) '           -H (omit header output) -r<random see
     1d>'
            write(*,*) '           -t<Sim Type (WEPP: 4=SglStm, 5=Contin
     1)>'
            write(*,*) '           -I0 <no interpolation (default)>'
            write(*,*) '           -I1 <linear interpolation>'
            write(*,*) '           -I2 <Fourier interpolation>'
            write(*,*) '           -I3 <interpolation to preserve avgs>'
            write(*,*) '           -v, -V<verbose> -h, -?, -\\? <help>'
            write(*,*) '           -O <option 6 -- observed data filenam
     1e>'
            write(*,*) 'Make sure there are no spaces between each flag'
            write(*,*) 'and its parameter.'
            write(*,*) 'If command line options are omitted, CLIGEN will
     1'
            write(*,*) 'interactively request the required information.'
            write(*,*)
            stop
          else if(argv(2:2) .eq. 'I') then
c
c-----------no interpolation
            if(argv(3:3) .eq. '0') then
              interp = 0
c-----------linear interpolation
            else if(argv(3:3) .eq. '1') then 
              interp = 1
c-----------fourier interpolation
            else if(argv(3:3) .eq. '2') then 
              interp = 2
c-----------ryf interpolation
            else if(argv(3:3) .eq. '3') then 
              interp = 3
            endif
c
c---------Observed Data File (option 6):
          else if(argv(2:2) .eq. 'O') then
            odfile = argv(3:)
c
          else
            write(*, '("Unknown option: ", a," ignored.") ') argv 
          endif
 09     continue
c
CC    write(*,*) " State:", istate, " Station:", index
CC    write(*,*) " Beg_Year:", ibyear, " Num_Yr:", numyr
CC    write(*,*) " Interpolation:", interp
c
c ----- *** L1 ENDIF ***
      endif
c ************* END PARSING COMMAND LINE ARGUMENTS ************
c
c
      timpkd(0)=0.0
      moveto = 0
c     Version number set here for option 5 output header
c     version=5.103
c
c ***************************************************************************
c ---- Determine the desired station and return its climate generation parms.
 10   continue
      moveto = 0
c     call sta_dat(ylt,yll,years,elev,itype,tp6,wgt,moveto,
      call sta_dat(ylt,yll,years,elev,itype,tp6,wgt,version,moveto,
     1            stidd,timpkd,igcode)
      if(moveto.eq.10) goto 10
c ***************************************************************************
c
c     Begin Climate Generation
c
c **** L1 IF ****
      if(moveto.ne.230) then
        sml=0.0
        r5max=0.0
        do 120 i=1,12
          if(wi(i).ge.r5max) r5max=wi(i)
 120    continue
c
c -- XXX -- Huh??? -- CRM -- 9/14/99
C       do 125 i=1,12
C         wi(i)=wi(i)
C125    continue
c
c ------ Done Once per Run:
C       call r5mon(tp6)
        call r5monb
        ab=0.02083
        ab1=1.0-ab
        nt=0
        clt=57.296
        pit=58.13
        pi2=6.283185
        xx=ylt/clt
        yls=sin(xx)
        ylc=cos(xx)
        vv=randn(k1)
        l=2
        if(vv.gt.prw(1,1)) l=1
        rn1=randn(k7)
c
c ------ Initialize the 1st of 2 constants for std norm deviate generator.
        v1=randn(k2)
        v3=randn(k3)
        v5=randn(k4)
        v7=randn(k5)
        v9=randn(k8)
        v11=randn(k9)
c
        msim=1
        nsim=1
c
c ---- Get Options from User
      call usr_opt(moveto,ioyr)
c **** L1 ENDIF ****
      endif
c
c **** M1 IF ****
      if(moveto.ne.230) then
c
        call sing_stm(ioyr,moveto,jd,iyear,damt,usdur,ustpr,uxmav)
        call wxr_gen(version,igcode,stidd,ylt,yll,years,elev,
     1            jd,itype,clt,tymax,timpkd,usdur,damt,ustpr,uxmav,
CC   2            iyear,numyr,xm,smy,tmpcmx,tmpcmn,ntd1,moveto,
     2            iyear,xm,smy,tmpcmx,tmpcmn,ntd1,moveto,
     3            sumpp,sumptx,sumptm,sumprd,sumpdr)
c **** M1 ENDIF ****
      endif
c
c **** N1 IF ****
      if(moveto.ne.10 .and. moveto.ne.230 .and. moveto.ne.225) then
        if(iopt.eq.2) then
          write(*,2000)numyr
          write(*,2010)
          write(*,2020)(sumpp(i),i=1,12)
          write(*,2030)(sumptx(i),i=1,12)
          write(*,2040)(sumptm(i),i=1,12)
          write(*,2050)(sumprd(i),i=1,12)
          write(*,2060)(sumpdr(i),i=1,12)
          write(*,2070)numyr,sumpp(13),stidd
        endif
c **** N1 ENDIF ****
      endif
c
c      End - MAIN LOOP
c
c    Check for Another Run or End
c
c **** P1 IF ****
      if(moveto.ne.10 .and. moveto.ne.230) then
c ------ "Pure" Interactive Mode (No Command Line Arguments).
c       if((istate.le.0 .or. index.le.0) .and. infile.eq."XXX") then
        if(numarg.eq.0) then
          write(*,2080)
          read(*,'(a1)')yc
        else
          yc = 'N'
          moveto = 230
        endif
        if(yc.eq.'y'.or.yc.eq.'Y') then
	  moveto = 10
          istate = -1
          index  = -1
          ibyear = -1
          numyr  = -1
          iopt   = -1
          force  = 0
          outfil = 'XXX'
          infile = 'XXX'
          odfile = 'XXX'
          dohedr =.true.
          interp = 0
        else
          if(iopt.ge.4) then
            write(7,*) ' '
            close (7)
CC          close (72)
C           close (73)
          else if(iopt.eq.3) then
            write(8,*)' '
            close (8)
          endif
        endif
c **** P1 ENDIF ****
      endif
c
      if(moveto.eq.10) goto 10
      stop 'Normal program termination' 
      end
c
c
c
      subroutine alph
c
c     + + + PURPOSE + + +
c     Computes alpha, a dimensionless parameter that expresses the fraction 
c     of total rainfall that occurs during 0.5 ho.
c
c     + + + COMMON BLOCKS + + +
      include 'cbk3.inc'
c      read: ida
c     ida         - Julian Day of Year.  Used as a subscript to R.
c
      include 'cbk4.inc'
c      read: mo
c     mo          - The current month (1=Jan, 2=Feb...).
c
      include 'cbk5.inc'
c      read: r
c     r           - Daily Precipitation amount (inches of water)
c
      include 'cbk7.inc'
c      read: k7
c     k7          - Seed for random number generation.
c
      include 'cbk9.inc'
c      read: wi,ab,ab1,rn1
c      write: r1
c     wi          - Average Maximum .5 Hour Precip. Intensity (by month)
c     ab          - Set to 0.02083, and used to calculate AI
c     ab1         - Set to 1.0-ab, and used to calculate AI
c     rn1         - ??? -- used for precip gamma dsn
c
c     + + + LOCAL VARIABLES + + +
c
c     + + + FUNCTION DECLARATIONS + + +
      real dstg
c
c     + + + END SPECIFICATIONS + + +
c
      ei=r(ida)-sml
      ai=ab1/(wi(mo)-ab)
      if (ei.lt.1.0) then
        ajp=1.0
      else
        ajp=1.0-exp(-5.0/ei)
      endif
      r1=dstg(ai,k7)
      r1=(ei*(ab+r1*(ajp-ab))+sml*ab)/r(ida)
      return
      end
c
c
c
      block data
c
c     + + + PURPOSE + + +
c     Initialize variables in the Common Blocks.
c     Contains generator seeds for the weather generator.
c
c     + + + COMMON BLOCKS + + +
      include 'cbk4.inc'
      include 'cbk7.inc'
c -- XXX -- CRM Added 4/3/2000:
      include 'crandom3.inc'
c -- XXX -- CRM Added 7/17/2000:
      include 'command6.inc'
c -- XXX -- CRM Added 8/22/2000:
      include 'cinterp.inc'
c
c     + + + DATA INITIALIZATIONS + + +
      data k1/9,98,915,92/
      data k2/135,28,203,85/
      data k3/43,54,619,33/
      data k4/645,9,948,65/
      data k5/885,41,696,62/
      data k6/51,78,648,0/
      data k7/227,57,929,37/
      data k8/205,90,215,31/
      data k9/320,73,631,49/
      data k10/22,103,82,4/
      data nc/0,31,59,90,120,151,181,212,243,273,304,334,365/
      data dtp/.4,.32,.5,.5/
      data dmxi/18.24,5.76,32.88,20.16/
c -- XXX -- CRM Added 4/3/2000 for checking quality of Random Numbers:
      data mox/0/
      data dax/0/
      data ranary/279*0.0/
      data g_dimi/12*0/
      data g_dimp/12*0/
      data g_dsum/108*0.d0/
      data g_ssum/108*0.d0/
C     data chicnt/2160*0/
      data chicnt/2400*0/
      data thresh/50.0,50.0,50.0,50.0,50.0,50.0,50.0,50.0,50.0/
      data thres2/50.0,50.0,50.0,50.0,50.0,50.0,50.0,50.0,50.0/
      data istate /-1/
      data index /-1/
      data ibyear /-1/
      data numyr /-1/
      data iopt /-1/
      data force /0/
      data outfil /"XXX"/
      data infile /"XXX"/
      data odfile /"XXX"/
      data dohedr /.true./
      data interp /0/
      end
c
c
c
      subroutine clgen(ntd,iyear)
c
c     + + + PURPOSE + + +
c     Simulates daily solar radiation, simulates daily precipitation
c     and/or maximum and minimum air temperature at the users option,
c     and calls functions RANDN and DSTN1.
c
c      + + + ARGUMENT DECLARATIONS + + +
      integer ntd
c
c      + + + ARGUMENT DEFINITIONS + + +
c     ntd    - days in this year (365 or 366)
c
c     + + + COMMON BLOCKS + + +
      include 'cbk1.inc'
c      read: rh
c      modify: tdp
c     rh          - Avg Monthly Dew Point Temperature.  Used to calculate TDP.
c     tdp         - Generated dewpoint temperature (C).
c
      include 'cbk3.inc'
c      read: ida
c     ida         - Julian Day of Year.  Used as a subscript to R.
c
      include 'cbk4.inc'
c      read: mo, nt
c     mo          - The current month (1=Jan, 2=Feb...).
c     nt          - Set to 1 if IYEAR is not a leap year: otherwise, zero.
c
      include 'cbk7.inc'
c      read: prw,obmx,obmn,obsl,k1,k2,k3,k4,k5,k9,yls,ylc,pit,nsim,msim,
c            stdtx,stdtm
c      modify: rst,v1,v3,v5,v7,v11,ra,tmxg,tmng,rmx,l
c     prw(12,1)   - monthly probability of wet day after wet day
c     prw(12,2)   - monthly probability of wet day after dry day
c     obmx        - Mean Observed Maximum Temperature for the month.
c     obmn        - Mean Observed Minimum Temperature for the month.
c     k1 ... k9   - Seeds for random number generation.
c     yls         - ??? -- Used to compute CH and YS. sin(ylt/clt) sin(latitude)
c     ylc         - ??? -- Used to compute CH and YC. cos(ylt/clt) cos(latitude)
c     pit         - ??? -- Used to compute SD.  Defined as pit=58.13
c     nsim        - Set to one under option-6 if precip data is missing.
c     msim        - Set to one under option-6 if temperature data is missing.
c     stdtx       - Standard deviation of daily max. temp. for the month.
c     stdtm       - Standard deviation of daily min. temp. for the month.
c     rst(i,j)    - Array Of Monthly precipitation stats.
c                       dim1: month 1..12
c                       dim2: 1=mean of daily rainfall
c                               mean liquid equivalent precipitation
c                               depth (inches) for a day precipitation
c                               occurs (by month) [=avg total precip
c                               for month / # wet days in month]
c                             2=std deviation of daily rainfall
c                               standard deviation of the daily precip
c                               value (inches) (by month)
c                             3=skew coefficient of daily rainfall
c
c     v1 ... v11  - Random numbers used to generate various daily values.
c     ra          - Generated radiation?  RADG receives RA's value for output.
c     tmxg        - Generated daily max temp.
c     tmng        - Generated daily min temp.
c     rmx         - Maximum possible solar radiation.
c     l           - Set to either 1 or 2; linked to nsim 0 or 1; selects PRW.
c
      include 'cbk5.inc'
c      modify: r
c     r           - Daily Precipitation amount (inches of water)
c
      include 'cinterp.inc'
c      read: lf, rf, o_mo
c     lf     - weighting factor for the midpoint value on this month's end
c              of the time interval.
c     rf     - weighting factor for the midpoint value on the "other" end.
c     o_mo   - month (on the "other" end) whose average value should be used.
c
CC    include 'ctap2.inc'
c       write: tap1, tap2, tap3, tap4, tap5, tap6
c
      include 'crandom3.inc'
c        read: vvx,v2x,v4x,v6x,v8x,v12x
c      modify: mox,dax
c
c     + + + FUNCTION DECLARATIONS + + +
      real dstn1
c
c     + + + END SPECIFICATIONS + + +
c
cC    integer tdprob
cC    data tdprob/0/
cC    save tdprob
c
      xi=ida
      sd=0.4102*sin((xi-80.25)/pit)
      ch=-yls*tan(sd)/ylc
c
      if (ch.ge.1.0) then
        h=0.0
      else if (ch.le.-1.0) then
        h=3.1416
      else
        h=acos(ch)
      endif
c
      ys=yls*sin(sd)
C -- XXX -- Note that YC is also used to store a Y/N user response! 
C           CRM -- 10/21/99
      yc=ylc*cos(sd)
c ---- max possible solar radiation for this day of the year.
      rmx=711.0*(h*ys+yc*sin(h))
c
c ---- Generate a month's worth of Consecutive Random Numbers for
c       each parameter.  (All at once -- not interleaved! -- CRM)
      if(mo .ne. mox) then
        mox = mo
        dax = 1
        call ranset(ntd,iyear)
      else
        dax = dax + 1
      endif
c 
      if (nsim.ne.0) then
        vv=vvx(dax)
CC      tap1=vv
        if ((prw(mo,l).le.0.0).or.(vv.gt.prw(mo,l))) then
          r(ida)=0.0
          l=2
CC        tap3=0.0
	else
c -------- A Mutated variant of Equation 2.1.5
c -------- Generated Daily Precip
c          According to Larry M. Younkin, the amount of precip is
c          Assumed to follow a Pearson type III
c          distribution: 
c 
c                     2s  / /  g  /      g  \     \3     \
c          R = Rbar + --- | | --- | x - --- | + 1 |  - 1 |   
c                      g  \ \  6  \      6  /     /      / 
c 
          v8=v8x(dax)
CC    tap3=v8
C         if(rst(mo,3).eq.0.0) rst(mo,3)=0.01
C         r6=rst(mo,3)/6.0
C -- XXX -- Range Check for Skewness Coefficient -- CRM -- 3/6/03
          if(rst(mo,3) .gt. 4.5) rst(mo,3) = 4.5
          if(rst(mo,3) .lt. -4.5) rst(mo,3) = -4.5
c ---------(interpolate precip skewness)
          if(interp.eq.0) then
            tmpvr3 = rst(mo,3)
          else if(interp .eq. 1) then
            tmpvr3 = rst(mo,3)*lf + rst(o_mo,3)*rf
          else if(interp .eq. 2) then
            tmpvr3 = fouri2(3)
          else if(interp .eq. 3) then
            tmpvr3 = ryf2(mo,dax,ntd,3)
          endif
          if(tmpvr3.eq.0.0) tmpvr3=0.01
          r6=tmpvr3/6.0
c
c -- XXX -- Bandaid: -- CRM 4/26/2000
          if(v7 .eq. 0.0) v7 = randn(k5)
          xlv=(dstn1(v7,v8)-r6)*r6+1.0
          xlv=(xlv**3-1.0)*2.0/tmpvr3
C         tap3=xlv
          v7=v8
c ---------(interpolate precip std. dev. & mean)
          if(interp.eq.0) then
            tmpvr1 = rst(mo,1)
            tmpvr2 = rst(mo,2)
          else if(interp .eq. 1) then
            tmpvr1 = rst(mo,1)*lf + rst(o_mo,1)*rf
            tmpvr2 = rst(mo,2)*lf + rst(o_mo,2)*rf
          else if(interp .eq. 2) then
            tmpvr1 = fouri2(1)
            tmpvr2 = fouri2(2)
          else if(interp .eq. 3) then
            tmpvr1 = ryf2(mo,dax,ntd,1)
            tmpvr2 = ryf2(mo,dax,ntd,2)
          endif
c         r(ida)=xlv*rst(mo,2)+rst(mo,1)
          r(ida)=xlv*tmpvr2 + tmpvr1
          if (r(ida).lt.0.01) r(ida)=0.01
          l=1
        endif
      endif
c
c------Option-6, and Observed Temperature Data Value is present.
      if(msim.eq.0) then
        xx=1.
        v12=v12x(dax)
        tdp=xx*dstn1(v11,v12)
        v11=v12
C ------(interpolate max & min temp means & SD's)
        if(interp.eq.0) then
          tmpvr6 = obmx(mo)
          tmpvr8 = stdtx(mo)
          tmpvr7 = obmn(mo)
          tmpvr9 = stdtm(mo)
        else if(interp .eq. 1) then
          tmpvr6 = obmx(mo)*lf + obmx(o_mo)*rf
          tmpvr8 = stdtx(mo)*lf + stdtx(o_mo)*rf
          tmpvr7 = obmn(mo)*lf + obmn(o_mo)*rf
          tmpvr9 = stdtm(mo)*lf + stdtm(o_mo)*rf
        else if(interp .eq. 2) then
          tmpvr6 = fouri2(6)
          tmpvr8 = fouri2(8)
          tmpvr7 = fouri2(7)
          tmpvr9 = fouri2(9)
        else if(interp .eq. 3) then
          tmpvr6 = ryf2(mo,dax,ntd,6)
          tmpvr8 = ryf2(mo,dax,ntd,8)
          tmpvr7 = ryf2(mo,dax,ntd,7)
          tmpvr9 = ryf2(mo,dax,ntd,9)
        endif
c
c  following by jrf 1/14/2008, dew point was not being
c  generated for a type 6 run. 
c  
c ---------(interpolate Dew Point Temp)
          if(interp.eq.0) then
            tmpv13 = rh(mo)
          else if(interp .eq. 1) then
            tmpv13 = rh(mo)*lf + rh(o_mo)*rf
          else if(interp .eq. 2) then
            tmpv13 = fouri2(13)
          else if(interp .eq. 3) then
            tmpv13 = ryf2(mo,dax,ntd,13)
          endif
          
          if(tmpvr8 .ge. tmpvr9) then
c          SD_Delta:
          twiddle = sqrt(tmpvr8**2 - tmpvr9**2)
c          Delta_Mean:
          twiddld = tmpvr6 - tmpvr7
c          Tdew:
          twiddle = sqrt(abs(((tmpvr8+tmpvr9)/2)**2 - tmpvr9**2))
          twiddld = tmpv13 - tmpvr7
          tdp = tmng + twiddld + tdp*twiddle
c
cC ------Generate Tdew based on Tmax-Delta:
        else
c          SD_Delta:
          twiddle = sqrt(tmpvr9**2 - tmpvr8**2)
c          Delta_Mean:
          twiddld = tmpvr6 - tmpvr7
c          Tdew:
          twiddle = sqrt(abs(((tmpvr8+tmpvr9)/2)**2 - tmpvr8**2))
          twiddld = tmpvr6 - tmpv13
          tdp = tmxg - twiddld + tdp*twiddle
        endif
          
c -- end insert by jrf 1/14/2008
          
      else
        xx=1.
        v2=v2x(dax)
C     tap1=v2
        tmxg=xx*dstn1(v1,v2)
C       tap1=tmxg
c ------ Shift 2nd constant to 1st position for "reuse".
        v1=v2
        v4=v4x(dax)
C     tap2=v4
        tmng=xx*dstn1(v3,v4)
C       tap2=tmng
        v12=v12x(dax)
C     tap4=v12
        tdp =xx*dstn1(v11,v12)
C       tap4=tdp
c ------ Shift 2nd constant to 1st position for "reuse".
        v3=v4
        v11=v12
c
c -------Equations 2.1.10 & 2.1.11
C ------(interpolate max & min temp means & SD's)
        if(interp.eq.0) then
          tmpvr6 = obmx(mo)
          tmpvr8 = stdtx(mo)
          tmpvr7 = obmn(mo)
          tmpvr9 = stdtm(mo)
        else if(interp .eq. 1) then
          tmpvr6 = obmx(mo)*lf + obmx(o_mo)*rf
          tmpvr8 = stdtx(mo)*lf + stdtx(o_mo)*rf
          tmpvr7 = obmn(mo)*lf + obmn(o_mo)*rf
          tmpvr9 = stdtm(mo)*lf + stdtm(o_mo)*rf
        else if(interp .eq. 2) then
          tmpvr6 = fouri2(6)
          tmpvr8 = fouri2(8)
          tmpvr7 = fouri2(7)
          tmpvr9 = fouri2(9)
        else if(interp .eq. 3) then
          tmpvr6 = ryf2(mo,dax,ntd,6)
          tmpvr8 = ryf2(mo,dax,ntd,8)
          tmpvr7 = ryf2(mo,dax,ntd,7)
          tmpvr9 = ryf2(mo,dax,ntd,9)
        endif
c
c ---------(interpolate Dep Point Temp)
          if(interp.eq.0) then
            tmpv13 = rh(mo)
          else if(interp .eq. 1) then
            tmpv13 = rh(mo)*lf + rh(o_mo)*rf
          else if(interp .eq. 2) then
            tmpv13 = fouri2(13)
          else if(interp .eq. 3) then
            tmpv13 = ryf2(mo,dax,ntd,13)
          endif
c
c ------ Generated Max & Min Daily Temps.
C       tmxg=obmx(mo)+tmxg*stdtx(mo)
C       tmng=obmn(mo)+tmng*stdtm(mo)
cC      tmng=tmpvr7 + tmng*tmpvr9
cC      tmxg=tmpvr6 + tmxg*tmpvr8
c
cC ------Tmin, Tmax, & Tdew are generated for each day so they are related.
cC       The other two parameters are based on whichever of Tmin & Tmax has
cC       the smaller SD for the day (after any interpolations).
cC       CRM -- 1/6/04.
c
c - XXX -- Here:
c -------Generate Tmax & Tdew based on Tmin+Delta (Today, SD_Tmin <= SD_Tmax):
        if(tmpvr8 .ge. tmpvr9) then
c          Tmin:
          tmng=tmpvr7 + tmng*tmpvr9
c          SD_Delta:
          twiddle = sqrt(tmpvr8**2 - tmpvr9**2)
c          Delta_Mean:
          twiddld = tmpvr6 - tmpvr7
c          Tmax:
          tmxg=tmng + twiddld + tmxg*twiddle
c          Tdew:
          twiddle = sqrt(abs(((tmpvr8+tmpvr9)/2)**2 - tmpvr9**2))
          twiddld = tmpv13 - tmpvr7
          tdp = tmng + twiddld + tdp*twiddle
c
cC ------Generate Tmin & Tdew based on Tmax-Delta:
        else
c          Tmax:
          tmxg=tmpvr6 + tmxg*tmpvr8
c          SD_Delta:
          twiddle = sqrt(tmpvr9**2 - tmpvr8**2)
c          Delta_Mean:
          twiddld = tmpvr6 - tmpvr7
c          Tmin:
          tmng=tmxg - twiddld - tmng*twiddle
c          Tdew:
          twiddle = sqrt(abs(((tmpvr8+tmpvr9)/2)**2 - tmpvr8**2))
          twiddld = tmpvr6 - tmpv13
          tdp = tmxg - twiddld + tdp*twiddle
        endif
c --- RANGE CHECK on MIN TEMP:
cC      if (tmng.gt.tmxg) write(*,*) 'Tmin adjusted below Tmax.'
        if (tmng.gt.tmxg) tmng=tmxg-0.2*abs(tmxg)
      endif
c
c       TDP now calculated using standard dev. instead of CV. 3/95
c ---- Generated Daily Dew Point Temperature.
c -----A mutated version of Equation 2.1.14:
c -- XXX -- CRM -- 10/21/99 -- I think there is a mis-placed paren here.
c     tdp =rh(mo)+(tdp*(stdtx(mo)+stdtm(mo)/2.))
cc       I believe it should be as follows:
cc    tdp =rh(mo)+(tdp*(stdtx(mo)+stdtm(mo))/2.)
cC    tdp = tmpv13+tdp*(tmpvr8+tmpvr9)/2.0
c
c ----Range check on Tdew.  It must be less than avg. daily temp.
Cc    if (tdp.gt.((tmxg+tmng)/2.)) write(*,*)'Tdew rangecheck executed.'
cC    if (tdp.gt.((tmxg+tmng)/2.)) then
cC        tdprob = tdprob + 1
cC        write(*,*)'Tdew Problem #', tdprob
cC    endif
c     if (tdp.gt.((tmxg+tmng)/2.)) tdp=((tmxg+tmng)/2.)*0.99
      if (tdp .gt. .99*(tmxg+tmng)/2.) tdp=((tmxg+tmng)/2.)*0.99
c     Change to limit - dew point in cold months ADNe
      if(tdp.lt.-10.) write(*,*)'Tdew -10 rangecheck executed.'
      if(tdp.lt.-10.) tdp=1.1*tmng
c
      v6=v6x(dax)
CC    tap5=v6
c ---- Compute Daily Radiation.
      ra=xx*dstn1(v5,v6)
C     tap5=ra
c ---------(interpolate radiation)
c   Changed 3/3/2010 - to use the standard deviation from the par file
c   when coming up with daily solar radiation values. Similar to how 
c   tmax and tmin are done. jrf
      if(interp.eq.0) then
        tmpv10 = obsl(mo)
        tmpv11 = stdsl(mo)
      else if(interp .eq. 1) then
        tmpv10 = obsl(mo)*lf + obsl(o_mo)*rf
        tmpv11 = stdsl(mo)*lf + stdsl(o_mo)*rf
      else if(interp .eq. 2) then
        tmpv10 = fouri2(10)
        tmpv11 = fouri2(11)
      else if(interp .eq. 3) then
        tmpv10 = ryf2(mo,dax,ntd,10)
        tmpv11 = ryf2(mo,dax,ntd,11)
      endif
c --- include standard deviation in value          
      ra = tmpv10 + ra*tmpv11
c ---- end changes for interpolation      
c -----A mutation of Equations 2.1.12 & 2.1.13: (Mis-placed parens,
c      this time in WEPP User Doc ?)
c     rx=rmx-obsl(mo)
c     if (obsl(mo).gt.rx) rx=obsl(mo)
c     ra=obsl(mo)+ra*rx/4.0
c      rx=rmx-tmpv10
c      if (tmpv10.gt.rx) rx=tmpv10
c      ra=tmpv10+ra*rx/4.0
c      if(ra.ge.rmx) ra=0.90*rmx
c      if (ra.le.0.0) ra=0.05*rmx      
c  check that solar radiation is within bounds
c  updated 1/31/2013 to match wepp documentation
      if(ra.gt.rmx) ra=rmx
      if (ra.lt.(rmx*0.05)) ra=0.05*rmx
c ------ Shift 2nd constant to 1st position for "reuse".
      v5=v6
c
      return
      end
c
c
c
      subroutine clmout
     i           (iview)
c
c     + + + PURPOSE + + +
c     Calculates daily and monthly values for options 1 and 2.  Writes
c     output to screen for option 1.
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer iview
c
c     + + + ARGUMENT DEFINITIONS + + +
c     iview       -
c
c     + + + COMMON BLOCKS + + +
      include 'cbk4.inc'
c      read: nc,iopt
c     nc          - Number of days in the (non-leap) year preceeding each month.
c     iopt        - Weather Generator Options:
c                    1 - Single Year Simulation - Screen Output
c                    2 - Multiple Year - Screen Output',/,
c                    3 - Multiple Year Simulation - CREAMS - GLEAMS Output File
c                    4 - Selected Single Storm WEPP - Output File
c                    5 - Multiple Year - WEPP Output File
c                    6 - Read Observed P and Temp and Generate Missing Data
c                    7 - Single Design Storm - TR 55 Storm Type WEPP Output Filec                    8 - Exit Weather Generator Program
c
      include 'ccl1.inc'
c      read: prcip,tgmx,tgmn,radg,dur
c     prcip       - ??? Avg. Daily Precip?  Set to R.  Divided by NUMYR and
c                    added to SUMP.
c     tgmx        - ??? Avg. Max Daily Temp?  Set to TMXG.  Divided by NUMYR 
c                    and added to SUMTX.
c     tgmn        - ??? Avg. Min Daily Temp?  Set to TMNG.  Divided by NUMYR 
c                    and added to SUMTM.
c     radg        - Daily Solar Radiation (Langleys/Day)
c     dur         - Storm Duration in Hours for Single Storm.
c
      include 'csumr.inc'
c      modify: sump,sumtx,sumtm,sumrd,sumdr
c     sump        - [Avg Annual] Sum of Daily Precip?
c     sumtx       - [Avg Annual] Sum of Daily Max Temps?
c     sumtm       - [Avg Annual] Sum of Daily Min Temps?
c     sumrd       - [Avg Annual] Sum of Daily Radiation?
c     sumdr       - Sum of Storm Durations?
c
c     + + + OUTPUT FORMATS + + +
 2000 format(/1x,'Climate Data Viewing Options',/,1x,7('-'),
     11x,4('-'),1x,7('-'),1x,7('-'),/,1x,'1 - Precipitation',/,1x,
     1'2 - Maximum Air Temperature',/,1x,'3 - Minimum Air',
     1' Temperature',/,1x,'4 - Solar Radiation',/,1x,'5 - Storm',
     1' Duration',/,1x,'6 - End',//,1x,'Enter viewing option')
 2010 format(20x,'Daily Precipitation Amounts (Inches)'/)
 2020 format(1x,'     J     F     M     A     M     J     J     A',
     1'     S     O     N     D')
 2030 format(1x,i2,12f6.2)
 2040 format(/1x,'tot',f5.2,11f6.2)
 2050 format(/1x,'Annual Precipitation ',f6.2/)
 2060 format(/20x,'Daily  Maximum Air Temperature (F)'/)
 2070 format(1x,i2,12f6.0)
 2080 format(/1x,'ave',f5.0,11f6.0/)
 2090 format(/20x,'Daily Minimum Air Temperature (F)'/)
 2100 format(1x,i2,12f6.0)
 2110 format(/20x,'Daily Solar Radiation (Langleys) '/)
 2120 format(1x,i2,12f6.0)
 2130 format(/20x,'Storm Duration (Hours)'/)
 2140 format(1x,i2,12f6.2)
c
c     + + + END SPECIFICATIONS + + +
c
      do 10 i =1,12
         sump(i) = 0.0
         sumtx(i)= 0.0
         sumtm(i)= 0.0
         sumrd(i)= 0.0
         sumdr(i)= 0.0
 10   continue
      sump(13)= 0.0
      do 20 i =1,12
         an = nc(i+1)-nc(i)
         do 20 j =1,31
            sump(i)=sump(i)+prcip(i,j)
            sump(13)=sump(13)+prcip(i,j)
            sumtx(i)=sumtx(i)+tgmx(i,j)/an
            sumtm(i)=sumtm(i)+tgmn(i,j)/an
            sumrd(i)=sumrd(i)+radg(i,j)/an
            sumdr(i)=sumdr(i)+dur(i,j)
 20   continue
c
      if(iopt.ne.2 .and. iview.ne.0) then
 30     continue
          write(*,2000)
          read(*,*)i
          if(i.eq.1) then
            write(*,2010)
            write(*,2020)
            write(*,2030)(j,(prcip(i,j),i=1,12),j=1,31)
            write(*,2020)
            write(*,2040)(sump(i),i=1,12)
            write(*,2050)sump(13)
          elseif(i.eq.2) then
            write(*,2060)
            write(*,2020)
            write(*,2070)(j,(tgmx(i,j),i=1,12),j=1,31)
            write(*,2020)
            write(*,2080)(sumtx(i),i=1,12)
          elseif(i.eq.3) then
            write(*,2090)
            write(*,2020)
            write(*,2100)(j,(tgmn(i,j),i=1,12),j=1,31)
            write(*,2020)
            write(*,2080)(sumtm(i),i=1,12)
          elseif(i.eq.4) then
            write(*,2110)
            write(*,2020)
            write(*,2120)(j,(radg(i,j),i=1,12),j=1,31)
            write(*,2020)
            write(*,2080)(sumrd(i),i=1,12)
          elseif(i.eq.5) then
            write(*,2130)
            write(*,2020)
            write(*,2140)(j,(dur(i,j),i=1,12),j=1,31)
            write(*,2020)
            write(*,2040)(sumdr(i),i=1,12)
          endif
c -- XXX -- How does one EXIT this loop? -- CRM -- 10/18/99
c           Replaced statement below:       CRM -- 11/10/99
c       goto 30
        if(i.gt.0 .and. i.lt.6) goto 30
      endif
c
 90   continue
      return
      end
c
c
      real function dstg
     i         (ai,k7)
c
c     + + + PURPOSE + + +
c     Provides numbers from a gamma PDF f(x) given two random numbers.
c     This is used in Cligen to calculate alpha_0.5, the ratio of the
c     maximum 30-min rainfall to the total rainfall; ie, intensity for
c     individual events.
c
c     The result is a gamma distribution normalized in the y-direction,
c     and normalized in the x-direction from zero to 'ai', the maximum
c     x-value for the current month.  (Note: 5.795 was substituted for 
c     ai for reasons explained in comments at the top of this code.)
c
c     This is a general approach that can be used to generate random
c     deviates for *ANY* PDF.  For explanation of method, see "Handbook
c     of Mathematical Functions with Formulas, Graphs, and Mathematical 
c     Tables"; Milton Abramowitz and Irene A. Stegun, Eds.; 1965; Dover
c     Publications, Inc., NY, NY; pp. 925-996.  Of course the procedure
c     does require a properly functioning uniform random number generator.
c
c     Note that N. T. Kottegoda gives an alternate algorithm on p. 103ff
c     of "Stochastic Water Resources Technology", Wiley & Son NY, 1980.
c
c     + + + ARGUMENT DECLARATIONS + + +
      real ai,rn1
      integer k7(4)
c
c     + + + ARGUMENT DEFINITIONS + + +
c     ai          - Maximum value used in the "x" direction
c     k7          - Seed for random number generation.
c     rn1         - Random Gamma deviate returned from DSTG.
c
      include 'crandom3.inc'
c
c     + + + DATA INITIALIZATIONS + + +
c     data xn1/10.0/
c
c     Modified by Bofu Yu on July 4, 1999
c                        based on break-point data, B.YU
      data xn1/6.28/
c
c     + + + FUNCTION DECLARATIONS + + +
      real randn
c
      double precision fu,xx
c
c -- Added for QC 10/2003 -- CRM:
      integer level0,level1,itryct,itryc2,iarrct
      integer flg_10,flg_21
      real array(30)
c     level0 -- pass/fail (0/1) on chi-square test with up to 20 bins.
c     level1 -- pass/fail (0/1) on K-S test with up to 20 bins.
c     itryct -- count number of tries to generate Uniform set of 20, this call.
c     itryc2 -- count number of tries to generate Gamma deviate, this call.
c     flg_10 -- flag, return to '10' if set.
c     flg_21 -- flag, return to '21' if set.
c     iarrct -- index of current element in "array"
c      array -- array of next 20 available uniform deviates.
      data iarrct /30/
      save array,iarrct
c
c     + + + END SPECIFICATIONS + + +
c
      itryct = 0
      itryc2 = 0
c
 10   continue
        flg_10 = 0
c
c------Generate 20 uniform random deviates, and check with chi-square test.
        if(iarrct .eq. 30) then
 21       continue
            flg_21 = 0
            itryct = itryct + 1
            do 25, i=1,30
              array(i) = randn(k7)
              ichi = int(array(i)*20.0) + 1
              chicnt(10,mox,ichi) = chicnt(10,mox,ichi) + 1
 25         continue
C           call chitst(10,level0)
            call ks_tst(10,level1)
c
c----------passed chitst
C           if(level0 .eq. 0) then
c----------passed ks_tst
            if(level1 .eq. 0) then
              iarrct = 0
c----------failed chitst
            else
              if(itryct .eq. 10000) then
c -------------- Write exactly once.
                write(*,*) 'Uniform could not succeed in 10,000 tries.'
                write(*,*) 'Precipitation Intensities are suspect.'
                iarrct = 0
                flg_21 = 0
              else
                do 26, i=1,20
                  ichi = int(array(i)*20.0) + 1
                  chicnt(10,mox,ichi) = chicnt(10,mox,ichi) - 1
 26             continue
                flg_21 = 1
              endif
            endif
          if(flg_21 .ne. 0) goto 21
        endif
c
c -------- Grab next two uniform random deviates.
        iarrct = iarrct + 1
        rn1=array(iarrct)
        iarrct = iarrct + 1
        rn=array(iarrct)
        itryc2 = itryc2 + 1
c
        dstg=rn1
c
c------- Random "x" Deviate normalized to the max-x value:
        xx=rn1*ai
CC      xx=rn1*5.795
c------- f(x) value computed from "x" deviate (above):
        fu=xx**xn1*exp(xn1*(1.0-xx))
c------- If Gamma Deviate gets Rejected:
        if(fu.lt.rn) then
          flg_10 = 1
          itryc2 = itryc2 + 1
          if(itryc2 .eq. 10000) then
            write(*,*) 'Gamma could not succeed in 10,000 tries.'
            write(*,*) 'Precipitation Intensities are suspect.'
            flg_10 = 0
            dstg=rn1
          endif
        endif
      if(flg_10 .ne.0) goto 10
c
      return
      end
c
c
      real function dstn1 
     i         (rn1,rn2)
c
c     + + + PURPOSE + + +
c     Computes the distance from the mean of a normal distribution,
c     with mean = 0 and standard deviation = 1, given two random numbers.
c
c     + + + ARGUMENT DECLARATIONS + + +
      real rn1,rn2
c
c     + + + ARGUMENT DEFINITIONS + + +
c     rn1         - ??? -- used for precip gamma dsn
c     rn2         -
c
c     + + + END SPECIFICATIONS + + +
c
c     Note: This algorithm is found on p. 103ff of N. T. Kottegoda's
c           "Stochastic Water Resources Technology", Wiley & Sons NY, 1980.
      dstn1=sqrt(-2.0*alog(rn1))*cos(6.283185*rn2)
c ---- range check:
      if(dstn1 .lt. -10.0) dstn1 = -10.0
      if(dstn1 .gt. 10.0) dstn1 = 10.0
c
      return
      end
c
c
c
      integer function jdt 
     i         (nc,i,m,nt)
c
c     + + + PURPOSE + + +
c     Computes the (Julian) day of the year, given the month and the day
c     of the month.
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer nc(13),i,m,nt
c
c     + + + ARGUMENT DEFINITIONS + + +
c     nc          - Number of days in the (non-leap) year preceeding each month.
c     i           - Day of the Storm ("jd").
c     m           - The current month (1=Jan, 2=Feb...)("mo").
c     nt          - Set to 1 if IYEAR is not a leap year: otherwise, zero.
c
c     + + + END SPECIFICATIONS + + +
c
      if (m.gt.2) then
        jdt=nc(m)+nt+i
      else
        jdt=nc(m)+i
      endif
c
      return
      end
c
c
c
      subroutine jlt 
     i           (ntd,
     m            jday,
     o            mo,nday)
c
c     + + + PURPOSE + + +
c     Given the day of the year (Julian date) determine the month
c     and day of the month.
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer ntd,jday,mo,nday
c
c     + + + ARGUMENT DEFINITIONS + + +
c     ntd         - number of days in year (365; 366 if leap year and
c                    iopt=3, 5, 6)
c     jday        - Julian date; ie, day of the year.
c     mo          - The current month (1=Jan, 2=Feb...).
c     nday        - The day of the current month.
c
c     + + + LOCAL VARIABLES + + +
      integer nn(12)
c
c     + + + LOCAL DEFINITIONS + + +
c     nn          - Number of days in each month (non-leap year).
c
c     + + + DATA INITIALIZATIONS + + +
      data nn/31,28,31,30,31,30,31,31,30,31,30,31/
c
c     + + + END SPECIFICATIONS + + +
c
c ---- Adjust for Leap Year.
      if(ntd.eq.366) then
        nn(2) = 29
      else
        nn(2) = 28
      endif
      kday = jday
c
c ---- Find the month.
      j = 0
 10   continue
        j = j + 1
        jday = jday -nn(j)
        ndflag = 0
        if (jday.le.0) then
          nday = jday +nn(j)
          ndflag = 50
        endif
      if(ndflag.eq.0 .and. j.lt.12) goto 10
c
      jday = kday
      mo = j
c
      return
      end
c
c
c
      subroutine r5mon
     i           (tp6)
c
c     + + + PURPOSE + + +
c     Smoothes and corrects .r hour monthly rainfall intensity data.
c
c     + + + ARGUMENT DECLARATIONS + + +
      real tp6
c
c     + + + ARGUMENT DEFINITIONS + + +
c     tp6         - maximum 6 hour precipitation depth (inches).
c
c     + + + COMMON BLOCKS + + +
      include 'cbk9.inc'
c      modify: wi
c     wi          - Average Maximum .5 Hour Precip. Intensity (by month)
c
      include 'cbk7.inc'
c      modify: rst,prw 
c     rst(i,j)    - Array Of Monthly precipitation stats.
c                       dim1: month 1..12
c                       dim2: 1=mean of daily rainfall
c                               mean liquid equivalent precipitation
c                               depth (inches) for a day precipitation
c                               occurs (by month) [=avg total precip
c                               for month / # wet days in month]
c                             2=std deviation of daily rainfall
c                               standard deviation of the daily precip
c                               value (inches) (by month)
c                             3=skew coefficient of daily rainfall
c
c     prw(12,1)   - monthly probability of wet day after wet day
c     prw(12,2)   - monthly probability of wet day after dry day
c
      include 'cbk4.inc'
c      read: nc
c     nc          - Number of days in the (non-leap) year preceeding each month.
c
c     + + + END SPECIFICATIONS + + +
c
      real sm(12),smm(12)
c
      xy2=0.5/tp6
c ---- convert WI in inches to WI in mm.
      do 10 i=1,12
         wi(i)=wi(i)*25.4
 10   continue
c
c ---- average each month's WI with the month on either side.
      sm(1)=(wi(12)+wi(1)+wi(2))/3.0
      do 20 i=2,11
         sm(i)=(wi(i-1)+wi(i)+wi(i+1))/3.0
 20   continue
      sm(12)=(wi(11)+wi(12)+wi(1))/3.0
c
      do 30 i=1,12
         if(prw(i,1).eq.0.0) prw(i,1)=0.01
         if(prw(i,2).eq.0.0) prw(i,2)=0.01
         if(rst(i,1).eq.0.0) rst(i,1)=0.01
         xm=nc(i+1)-nc(i)
c -- XXX -- The equation below is backwards from the one to compute
c           Historic Monthly Precip amount -- CRM -- 7/30/00.
         smm(i)=xm*prw(i,1)/(1.0-prw(i,2)+prw(i,1))
         r25=rst(i,1)
         f=xy2/smm(i)
         wi(i)=-sm(i)/alog(f)
         wi(i)=1.0-exp(-wi(i)/r25)
         if (wi(i).lt.0.1) wi(i)=0.1
         if (wi(i).gt.0.95) wi(i)=0.95
 30   continue
c
      return
      end
c
c
c
      real function randn
     m         (k)
c
c     + + + PURPOSE + + +
c     Provides random numbers ranging from 0.0 to 1.0
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer k(4)
c
c     + + + ARGUMENT DEFINITIONS + + +
c     k           - Seed for random number generation.
c
c     + + + END SPECIFICATIONS + + +
c
 10   continue
      k(4)=3*k(4)+k(2)
      k(3)=3*k(3)+k(1)
      k(2)=3*k(2)
      k(1)=3*k(1)
      i=k(1)/1000
      k(1)=k(1)-i*1000
      k(2)=k(2)+i
      i=k(2)/100
      k(2)=k(2)-100*i
      k(3)=k(3)+i
      i=k(3)/1000
      k(3)=k(3)-i*1000
      k(4)=k(4)+i
      i=k(4)/100
      k(4)=k(4)-100*i
      randn=(((float(k(1))*.001+float(k(2)))*.01+float(k(3)))*.001+
     1      float(k(4)))*.01
c ---- range check:
      if(randn .le. 0.0 .or. randn .ge. 1.0) goto 10
c
      return
      end
c
c
c
      subroutine windg
c
c     + + + PURPOSE + + +
c     Simulates daily average wind velocity using mean, standard deviation,
c     and skew coefficient of wind speed.  Wind direction is selected from
c     a uniform distribution.
c
c     + + + COMMON BLOCKS + + +
      include 'cbk1.inc'
c      read: dir,pi2
c      modify: wvl,wv,th
c     dir(i,j)    - Cumulative % time (fraction) from dir1, dir1+dir2, ...
c                          dim1: month
c                          dim2: compass direction
c     pi2         - Pi * 2; ie, a full rotation.
c     wvl(i,j,k)  - array of wind parameters where:
c                     i - ith direction (1 - north  - 16 nnw)
c                     j - parameters (1 - 4)
c                          1 - % time from direction i
c                          2 - mean speed from direction i
c                          3 - standard deviation of speed from direction i
c                          4 - skew coeficient of speed from direction i
c                     k - month (1=Jan, 2=Feb...)
c     wv          - Wind Velocity (m/sec)
c     th          - Wind Direction (radians from North)
c
      include 'cbk3.inc'
c -- XXX -- Note that 'j' is a counter in a loop! -- CRM 10/18/99
c      modify: j
c
      include 'cbk4.inc'
c      read: mo
c     mo          - The current month (1=Jan, 2=Feb...).
c
      include 'cbk7.inc'
c      read: k6,k8
c      modify: v9
c     k1 ... k9   - Seeds for random number generation.
c     v1 ... v11  - Random numbers used to generate various daily values.
c
CC    include 'ctap2.inc'
c       write: tap6
c
      include 'crandom3.inc'
c        read: fxx
c
c     + + + FUNCTION DECLARATIONS + + +
      real dstn1
c
c     + + + END SPECIFICATIONS + + +
c
      fx=fxx(dax)
CC    tap2=fx
      j = 0
 1    continue
        j = j + 1
        j1=j-1
        ndflag = 0
        if (dir(mo,j).gt.fx) ndflag = 2
      if(ndflag.eq.0 .and. j.lt.16) goto 1
c
c       calm condition found
c
c
c **** L1 IF ****
      if(ndflag.eq.0) then
        wv=0.0
        th=0.0
CC      tap6=0.0
c **** L1 ELSE ****
      else 
c
c       Wind direction calculations
c
        if (j.eq.1) then
          g=fx/dir(mo,j)
        else
          g=(fx-dir(mo,j1))/(dir(mo,j)-dir(mo,j1))
        endif
        xj1=j1
        th=pi2*(g+xj1-.5)/16.
        if (th.lt.0.) th=pi2+th
c
c         Wind speed calculations
c
        v10=v10x(dax)
CC      tap6=v10
        if(wvl(j,4,mo).eq.0.0) wvl(j,4,mo)=0.01
        r6=wvl(j,4,mo)/6.0
        xlv=(dstn1(v9,v10)-r6)*r6+1.0
        xlv=(xlv**3-1.0)*2.0/wvl(j,4,mo)
        v9=v10
C	tap6=xlv
        wv=xlv*wvl(j,3,mo)+wvl(j,2,mo)
        if (wv.lt.0.) wv=.1
c **** L1 ENDIF ****
      endif
c
      return
      end
c
c
c
      subroutine nrmd
     m           (r1)
c
c     + + + PURPOSE + + +
c     Returns the standard normal deviate for a given probability value
c     (e.g. r1=.99 returns r1=2.328)
c
c     + + + ARGUMENT DECLARATIONS + + +
      real r1
c
c     + + + ARGUMENT DEFINITIONS + + +
c     r1          - initially, the probability; finally the std. norm. deviate.
c
c     + + + END SPECIFICATIONS + + +
c
      if(r1 .ge. 0.5) then
        sgn = 1.0
        hp = 1.0 - r1
      else
        sgn = -1.0
        hp = r1
      endif 
      t = sqrt(alog(1./(hp*hp)))
      r1 = sgn*(t-(2.30753+.27061*t)/(1.+.99229*t+.04481*t*t))
      return
      end
c
c
c
c     subroutine header
      subroutine header(version)
c
c     + + + PURPOSE + + +
c     Writes header information to screen.
c
c     + + + OUTPUT FORMATS + + +
 2000 format(//,2x,68('*'),/,2x,'*',66x,'*',
     1       /,2x,'*',14x,'USDA - WATER EROSION PREDICTION',
     2       ' PROJECT',13x,'*',/,2x,'*',16x,
     3       ' WEPP CLIMATE INPUT DATA GENERATOR',16x,'*'/2x,'*',66x,
     4       '*',/,2x,'*',20x,'CONTINUOUS SIMULATION AND',21x,'*',/,
     5       2x,'*',23x,'SINGLE STORM OPTIONS',23x,'*',/,
     6       2x,'*',20x,'with Command Line Options,',20x,'*',/,
     7       2x,'*',24x,'and Corrections to',24x,'*',/,
     8       2x,'*',18x,'Rainfall Intensity Calculations',17x,'*',/,
     9       2x,'*',19x,'and Random Number Generation.',18x,'*')
 2001 format(
c    1       2x,'*',66x,'*',/,2x,'*',27x,'VERSION ',f5.3,26x,'*',/,
c    1       2x,'*',66x,'*',/,2x,'*',27x,'VERSION ',f6.4,25x,'*',/,
     1       2x,'*',66x,'*',/,2x,'*',26x,'VERSION ',f7.5,25x,'*',/,
     2       2x,'*',21x,'Revised from VERSION 4.2',21x,'*',/,
c    3       2x,'*',26x,' January 2013 ',26x,'*',/,2x,'*',66x,'*',/,
     4       2x,'*',11x,'(Use -h or /h to list command line options.)',
     5       11x,'*',/,2x,'*',66x,'*',/,2x,68('*'),//)
c
c     + + + END SPECIFICATIONS + + +
c
      write(*,2000)
      write(*,2001) version
      return
      end
c
c
c
      real function timepk
     i         (timpkd,k10)
c
c     + + + PURPOSE + + +
c     Calculates the time to peak from the 12 interval time to peak
c     accummulated distribution parameters TIMPKD input for each station
c     location.
c
c     + + + ARGUMENT DECLARATIONS + + +
      real timpkd(0:12)
      integer k10(4)
c
c     + + + ARGUMENT DEFINITIONS + + +
c     timpkd      - The 12 interval time to peak accummulated distribution
c                   parameters for the station.  Cumulative distribution of 
c                   computed time to peak rainfall intensity values based on
c                   NWS 15-minute rainfall data (section 2.1.4 WEPP tech 1995)
c     k10         - Seed for random number generation.
c
      include 'crandom3.inc'
c        read: zx, dax
c       write: z
      include 'cbk4.inc'
c         read: iopt 
c      
CC    include 'ctap2.inc'
c
c     + + + END SPECIFICATIONS + + +
c
      if (iopt.eq.6) then
         z = randn(k10)
      else
         z=zx(dax)
      endif
      
CC    tap4=z
C     write(*,*) tap4
      i=0
 10   continue
        i=i+1
      if(timpkd(i).lt.z .and. i.lt.12) goto 10
c
      diff1= timpkd(i)-z
      diff2=timpkd(i) - timpkd(i-1)
      ratio= diff1/diff2
      timepk=0.08333*i - ratio*0.08333
c
      return
      end
c
c
c
      subroutine sta_dat
c    i           (ylt,yll,years,elev,itype,tp6,wgt,
     i           (ylt,yll,years,elev,itype,tp6,wgt,version,
     m            moveto,
     o            stidd,timpkd,igcode)
c     + + + PURPOSE + + +
c     To determine desired station and load the parameters required to
c     generate its weather.
c
c ----- Split out from the CLIGEN main module 9/22-27/99 by C. R. Meyer.
c
c     + + + ARGUMENT DECLARATIONS + + +
      real ylt,yll,wgt(3),timpkd(0:12)
c--- XXX -- Huh?  ELEV is declared to be an *integer*, but in the
c           data file it is a floating-point!!!  --- CRM -- 9/27/99
      integer years,elev,moveto
      character*41 stidd
c
c
c     + + + ARGUMENT DEFINITIONS + + +
c     ylt         - Station Degrees Latitude (+ is N, - is S).
c     yll         - Station Degrees Longitude (+ is E, - is W).
c     years       - Years of Record at Station.
c     elev        - Station Elevation above Sea Level (whole number of meters)
c     itype       - integer value [1..4] to set single storm parameters.
c     tp6         - maximum 6 hour precipitation depth (inches).
c     wgt(3)      - 3 wind station weights used for triangulation -- weighting
c                    factor for wind stations used for interpolation
c     istate      - Numeric Climate Code of Desired State.
c     index       - 4-digit numeric station index.
c     moveto      - A global flag.  If set to 'XX' it means "goto XX".
c     stidd       - 41-character alphanumeric station name.
c     timpkd      - The 12 interval time to peak accummulated distribution
c                   parameters for the station.  Cumulative distribution of 
c                   computed time to peak rainfall intensity values based on
c                   NWS 15-minute rainfall data (section 2.1.4 WEPP tech 1995)
c     igcode      - wind information/ET equation flag
c                      0 -- wind information exists: use Penman ET equation
c                      1 -- no wind information exists: use Priestly-Taylor
c                           ET equation
c
c     + + + COMMON BLOCKS + + +
      include 'command6.inc'
c
c     + + + LOCAL VARIABLES + + +
      integer iscnt,nst,nstat,ndflag
      character*41 stid
      character*2 dr(55)
      character*1 yc
      character*80 buffer
c
c     + + + LOCAL DEFINITIONS + + +
c      Variables Passed to other Modules:
c     istate      - Numeric Climate Code of Desired State.
c     iscnt       - Number of Stations in Selected State.
c     stid        - 41-character ASCII Station Name.
c     nstat       - 4-digit Numeric Station Code.
c
c     nst         - Numeric State Code
c     index       - 4-digit numeric station index.
c     ndflag      - "Set" if an "End-of-File" is read.
c     dr          - Array of 2-character state (postal) abbreviations.
c     yc          - 1-character user response (y/n).
c     dr          - Array of 2-character state (postal) abbreviations.
c                    File by that name contains parameters for all the
c                    weather stations in that state for which we can
c                    generate weather.
c     version     - CLIGEN version (ie, 5.103)
c     buffer      - Stores reads from Input File for parsing.
c
c     + + + SUBROUTINES CALLED + + +
c     header
c     sta_name
c     sta_parms
c
c     + + + DATA INITIALIZATIONS + + +
      data dr/'al','az','ar','ca','co','ct','de','fl','ga','id',
     1        'il','in','ia','ks','ky','la','me','md','ma','mi',
     1        'mn','ms','mo','mt','ne','nv','nh','nj','nm','ny',
     1        'nc','nd','oh','ok','or','pa','ri','sc','sd','tn',
     1        'tx','ut','vt','va','wa','wv','wi','wy','dc','ak',
     1        'hi','pr','pi','if','  '/
c
c     + + + INPUT FORMATS + + +
 1000 format(i4)
 1010 format(a41,i2,i4,i2)
 1020 format(//////////////////////////////////////////////////
     1       ///////////////////////////////)
c
c     + + + OUTPUT FORMATS + + +
 2000 format(/1x,'No stations available - do you want to',
     1' continue (y/n)? ')
 2010 format(/1x,'Enter the station index: ')
c
c
c     + + + END SPECIFICATIONS + + +
c
      if(moveto.ne.10) then
        timpkd(0)=0.0
c ------ Version number set here for option 5 output header
C       version=5.103
c
c       call header
        call header(version)
c ------ "Pure" Interactive Mode (No command line arguments):
C       if((istate.le.0 .or. index.le.0) .and. infile.eq."XXX") then
        if(numarg.eq.0) then
          write(*,*)' Press Enter to Continue'
          read(*,'(a1)')yc 
        endif 
      endif
c
c ***************************************************************************
c ---- Determine Desired State, and display Available Stations.
c10     continue
c     if(istate.le.0 .and. infile.eq."XXX") then 
      if(istate.le.0 .and. (infile.eq."XXX" .or. index.ge.0)) then 
        call sta_name(iscnt,stid,nstat,moveto)
      endif
c ***************************************************************************
c -- Move this section (through #50) into STA_NAME? -- CRM 7/13/00.
 40   continue
C     if(index.le.0 .and. infile.eq."XXX") then
      if(index.le.0 .and. (infile.eq."XXX" .or. istate.ge.0)) then
        if(moveto.eq.40 .or. (iscnt.le.0 .and. moveto .ne. 50)) then
c -------- No Stations Found
          write(*,2000)
          read(*,'(a1)')yc
          if(yc.eq.'y'.or.yc.eq.'Y') then
            moveto = 10
          else
            moveto = 230
          endif
        endif
      endif
c
 50     continue
c ****** L1 IF ****
      ndflag = 0
      if(moveto.ne.10 .and. moveto.ne.230) then
        if(index.le.0 .and. infile.eq."XXX") then 
          write(*,2010)
        endif
        iname = istate
        if(iname.eq.66) iname=52
        if(iname.eq.91) iname=53
        if(iname.eq.99) iname=54
        if(index.le.0 .and. infile.eq."XXX") then 
c -------- Choose the Desired Station, from the State Selected.
          read(*,1000)index
        endif
        ndflag = 55
        if(infile.eq."XXX") then
          open(10,file=dr(iname),status='old',err=55)
        else
          open(10,file=infile,status='old',err=55)
      write(*,*) infile
        endif
        rewind (10)
        ndflag = 0
 55     continue
        if((istate.le.0 .or. index.le.0) .and. infile.eq."XXX") then
          if(ndflag .ne. 0) then
C           write(*,*)'  Mother Nature does not want to run state ',
C    1                   dr(iname)
C           write(*,*)'  Parameters for this state are not loaded on '
C           write(*,*)'  this computer.  Check directory for file ',
C    1                   dr(iname)
            write(*,*)'  Parameters for this state must be loaded in'
            write(*,*)'  the same directory as the Cligen program.'
            write(*,*)'  Make sure you have downloaded the file: ',
     1                   dr(iname)
            write(*,*)'  -- Enter q to quit or c to Continue'
            read(*,'(a1)')yc
            if(yc.eq.'q'.or.yc.eq.'Q') then
              moveto = 230
            else
              moveto = 10
            endif
          endif
        endif
c
c ****** L2 IF ****
        if(moveto.ne.10 .and. moveto.ne.230) then
c          Gettin Station Parameter Data from File
C       if(infile.eq."XXX") then
c ------ State & Station known.
        if(istate.gt.0 .and. index.gt.0) then
          ndflag = 61
 60       continue
c         read(10,1010,end=61)stidd,nst,nstat,igcode
          read(10,'(a)',end=61) buffer
          read(buffer,1010,err=60,end=61)stidd,nst,nstat,igcode
c -------- Match 2-digit state code & 4-digit station code.
          if((nst.ne.istate).or.(nstat.ne.index)) goto 60
c         write(*,*) "State, Station:", istate, index
          ndflag = 0
c
 61       continue
c
c ---------- Requested Station Not Found -- Try Again?
          if(ndflag.eq.61) then
            if(istate.gt.0 .and. index.gt.0) then
              write(*,*)
              write(*,*)'  Parameters for this state must be loaded in'
              write(*,*)'  the same directory as the Cligen program.'
              write(*,*)'  Make sure you have downloaded the file \"',
     1                     dr(iname), '\".'
              write(*,*)'  http://horizon.nserl.purdue.edu/Cligen/ .'
              write(*,*)
C             stop(" Requested Station Not Found.")
              stop " Requested Station Not Found."
            else
              ndflag = 40
            endif
          endif
c         if(ndflag.eq.60) goto 60
c ------ For Single Station File use first station found.
        else
          read(10,1010)stidd,nst,nstat,igcode
          ndflag = 1
        endif
c
CC    write(*,*) "NDFLAG", ndflag
CC    write(*,*)"St_id: ",stidd, "NST: ",nst
CC    write(*,*)"Station_ID: ",stidd
CC    write(*,*)" State:",istate," Station Number:",index
          if(ndflag.ne.40)
c    1    call sta_parms(stidd,nstat,tp6,wgt,ylt,yll,years,elev,itype,
c    2                   timpkd)
     1    call sta_parms(stidd,index,tp6,wgt,ylt,yll,years,elev,itype,
     2                   timpkd)
c
c ****** L2 ENDIF ****
        endif
c **** L1 ENDIF ****
      endif
c
c ---- Try Again.
      if(ndflag.eq.40) goto 40
c
c230  continue
      return
      end
c
c
      subroutine sta_name
     m           (iscnt,stid,nstat,moveto)
c
c     + + + PURPOSE + + +
c     Choose a State (istate) and display the available climate stations
c     for the State selected.
c
c ----- Split out from the STA_DAT module 9/27/99 by C. R. Meyer.
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer iscnt,nstat,moveto
      character*41 stid
c
c     + + + ARGUMENT DEFINITIONS + + +
c     istate      - Numeric Climate Code of Desired State.
c     index       - 4-digit numeric station index.
c     iscnt       - Number of Stations in Selected State.
c     stid        - ASCII Station Name.
c     nstat       - 4-digit Numeric Station Code.
c     moveto      - A global flag.  If set to 'XX' it means "goto XX".
c
c     + + + COMMON BLOCKS + + +
      include 'command6.inc'
c
c     + + + LOCAL VARIABLES + + +
      integer ndflag
c -- XXX -- COUNTY is supposed to be read & written, but space is never
c           allocated to _store_ it!  Added stmt below.  CRM -- 9/27/99.
      character*20 county
      character*1 yc
c
c     + + + LOCAL DEFINITIONS + + +
c     yc          - 1-character user response (y/n).
c
c     + + + INPUT FORMATS + + +
 1000 format(i2)
 1010 format(a41,i2)
 1020 format(a41,i2,i4,i3,i4,a20)
c
c     + + + OUTPUT FORMATS + + +
 2000 format(1x,'State Climate Code - Available Stations'/)
 2010 format(1x,'01 Alabama       20 Michigan       39 S. Dakota',
     1/1x,'02 Arizona       21 Minnesota      40 Tennessee',
     2/1x,'03 Arkansas      22 Mississippi    41 Texas',
     3/1x,'04 California    23 Missouri       42 Utah',
     4/1x,'05 Colorado      24 Montana        43 Vermont',
     5/1x,'06 Connecticut   25 Nebraska       44 Virginia',
     6/1x,'07 Delaware      26 Nevada         45 Washington',
     7/1x,'08 Florida       27 New Hampshire  46 West Virginia',
     8/1x,'09 Georgia       28 New Jersey     47 Wisconsin',
     9/1x,'10 Idaho         29 New Mexico     48 Wyoming')
 2020 format(1x,'11 Illinois      30 New York       49 Washington DC',
     1/1x,'12 Indiana       31 North Carolina 50 Alaska',
     2/1x,'13 Iowa          32 North Dakota   51 Hawaii',
     3/1x,'14 Kansas        33 Ohio           66 Puerto Rico',
     4/1x,'15 Kentucky      34 Oklahoma       91 Pacific Islands',
     5/1x,'16 Louisiana     35 Oregon         99 Interpolated File',
     6/1x,'17 Maine         36 Pennsylvania   ',
     7/1x,'18 Maryland      37 Rhode Island    ',
     8/1x,'19 Massachusetts 38 South Carolina  ')
 2030 format(/1x,'Enter state climate code (ex. 01 for Alabama): ')
c2040 format(/1x,'Stations Available',19x,' Station No. ',
 2040 format(/1x,'Stations Available',18x,' Station No. ',
     1       'Lat.  Long County'/,
c    11x,8('-'),1x,9('-'),19x,17('-'),2x,4('-'),1x,7('-')/)
     11x,8('-'),1x,9('-'),18x,17('-'),2x,4('-'),1x,7('-')/)
c2050 format(1x,a41,2x,i4,3x,i3,2x,i4,1x,a19)
 2050 format(1x,a41,1x,i4,3x,i3,2x,i4,1x,a19)
c
c     + + + END SPECIFICATIONS + + +
c
c ---- Determine Desired State
c --- (skip, if state is a command line arg.) 
cc    if(istate.le.0) then 
 10     continue
        write(*,2000)
        write(*,2010)
        write(*,2020)
        write(*,2030)
        ndflag = 15 
        read(*,1000,err=15)istate
        ndflag = 0
 15     continue 
        if(ndflag .eq. 15) then 
          write(*,*)' Error Entering State Code '
          write(*,*)
          write(*,*)' Press Enter to Continue'
          read(*,'(a1)')yc 
        endif 
        if(ndflag .ne. 0) goto 10 
cc    endif 
c
c ---- Interpolated File
      moveto = 0
      if(istate.eq.99) then
        iscnt = 1
c       iyr = 0
        moveto = 50
      end if
c
      if(moveto.ne.50 .and. index.le.0) then
c
c ------ Read List of Available Stations
c -- istate -- Numeric Climate Code of Desired State
c -- kknt   -- Lines Displayed on Screen so far
c -- nstate -- Numeric State Code
c -- nst    -- Numeric State Code (same)
c
        open(11,file='stations',status='old',err=16)
        goto 17
c-------Stations File Not Found (not opened).
 16     continue
        write(*,*) 
        write(*,*) 'The \"stations\" file must be in the same directory'
        write(*,*) 'as the Cligen executable.  It is available on the'
        write(*,*) 'website http://horizon.nserl.purdue.edu/Cligen/ .'
        write(*,*) 
        stop
 17     continue
        write(*,*) 
        write(*,*) 'While this approach is fine for exploring use of'
        write(*,*) 'the Cligen program, for the best quality results'
        write(*,*) 'we recommend you use the new Forest Service data'
        write(*,*) 'instead.'
        write(*,*) 
        write(*,*) 'Because development of Cligen was funded by the'
        write(*,*) 'U. S. taxpayers, the available station datasets'
        write(*,*) 'are primarily for the continental United States.'
        write(*,*) 'Users outside the U. S. are welcome, but will'
        write(*,*) 'probably need to build their own station parameter'
        write(*,*) 'files.  \"Survey of Climatology\", 1982, by Griffith
     1s'
        write(*,*) 'and Driscoll will prove helpful in finding a U. S.'
cLEW    write(*,*) 'station with a climate similar to yours. Cligen\'s'
        write(*,*) 'station with a climate similar to yours. Cligen''s'
        write(*,*) '\"option-6\" may also prove useful.'
        write(*,*) 
        write(*,*) 'Press "ENTER" to continue.'
        read(*,'(a1)') yc
        rewind (11)
        iscnt=0
c       iyr=0
        write(*,2040)
        kknt=0
 20     continue
        ndflag =29 
        read(11,1010,end=29)stid,nstate
        if(nstate.eq.istate) then
          kknt=kknt+1
          iscnt=iscnt+1
          backspace(11)
          read(11,1020)stid,nst,nstat,lat,long,county
          if(kknt.gt.20) then
            kknt=0
            write(*,*) 'Press "ENTER" to continue.'
            read(*,'(a1)') yc
          endif
          write(*,2050)stid,nstat,lat,long,county
        endif
        ndflag = 0
 29     continue
        if(ndflag.eq.0 .and. nstate.le.istate) goto 20
      endif
c
      close (11)
      return
      end
c
c
c
      subroutine sta_parms
     i           (stidd,nstat,
     m            tp6,wgt,
     o            ylt,yll,years,elev,itype,timpkd)
c     + + + PURPOSE + + +
c     Derive parameters for the desired climate station.
c
c ----- Split out from the CLIGEN main module 9/28/99 by C. R. Meyer.
c
c     + + + ARGUMENT DECLARATIONS + + +
      character*41 stidd
      integer nstat,elev,years
      real ylt,yll,tp6,wgt(3),timpkd(0:12)
c
c     + + + ARGUMENT DEFINITIONS + + +
c     stidd       - 41-character alphanumeric station name.
c     nstat       - 4-digit Numeric Station Code
c     tp6         - maximum 6 hour precipitation depth (inches).
c     wgt(3)      - 3 wind station weights used for triangulation -- weighting
c                    factor for wind stations used for interpolation
c     ylt         - Station Latitude.
c     yll         - Station Longitude.
c     years       - Years of Record.
c     elev        - Station Elevation above Sea Level (whole number of meters)
c     itype       - integer value [1..4] to set single storm parameters.
c     timpkd      - The 12 interval time to peak accummulated distribution
c                   parameters for the station.  Cumulative distribution of 
c                   computed time to peak rainfall intensity values based on
c                   NWS 15-minute rainfall data (section 2.1.4 WEPP tech 1995)
c
c     + + + COMMON BLOCKS + + +
      include 'cbk1.inc'
c      modify: wvl,dir,rh,calm
c     wvl(i,j,k)  - array of wind paramters where:
c                     i - ith direction (1 - north  - 16 nnw)
c                     j - parameters (1 - 4)
c                          1 - % time from direction i
c                          2 - mean speed from direction i
c                          3 - standard deviation of speed from direction i
c                          4 - skew coeficient of speed from direction i
c                     k - month (1=Jan, 2=Feb...)
c
c     dir(i,j)    - Cumulative % time (fraction) from dir1, dir1+dir2, ...
c                          derived from wvl()
c                          dim1: month
c                          dim2: compass direction
c     rh          - Dew Point Temp.
c     calm        - % time air is calm (by month).
c                   Calm is treated separately [from WVL] as direction 0,
c                   speed 0.  Only a % time value is need for calm generation.
c
      include 'cbk7.inc'
c      modify: rst,prw,obmx,obmn,obsl,cvs,cvtx,cvtm,stdtx,stdtm,stdsl
c     rst(i,j)    - Array Of Monthly precipitation stats.
c                       dim1: month 1..12
c                       dim2: 1=mean of daily rainfall
c                               mean liquid equivalent precipitation
c                               depth (inches) for a day precipitation
c                               occurs (by month) [=avg total precip
c                               for month / # wet days in month]
c                             2=std deviation of daily rainfall
c                               standard deviation of the daily precip
c                               value (inches) (by month)
c                             3=skew coefficient of daily rainfall
c
c     prw(12,1)   - monthly probability of wet day after wet day
c     prw(12,2)   - monthly probability of wet day after dry day
c     obmx        - Maximum Temperature.
c     obmn        - Minimum Temperature.
c     obsl        - Observed mean daily solar radiation (Langleys) (by month)
c     cvs         - Coefficient of Variation of Solar Radiation (by month)
c     cvtx        - Coefficient of Variation of Maximum Temperature (by month)
c     cvtm        - Coefficient of Variation of Minimum Temperature (by month)
c     stdmx       - Standard Dev. Tmax.
c     stdmn       - Standard Dev. Tmin.
c     stdsl       - Standard Dev. Sol.
c
      include 'cbk9.inc'
c      modify: wi
c     wi          - Average Maximum .5 Hour Precip.
c
      include 'cinterp.inc'
c
      include 'command6.inc'
c
c     + + + LOCAL VARIABLES + + +
      character*1 yc
      character*19 site(3)
      real rst1(12),rst2(12),rst3(12),prw1(12),prw2(12)
c
c     + + + LOCAL DEFINITIONS + + +
c     yc          - 1-character user response (y/n).
c     site(3)     - 3 wind station names used for triangulation.  Stations
c                   from which wind data were interpolated.  (Not used in
c                   CLIGEN computations, but reported in the CLIGEN output.)
c
c     + + + INPUT FORMATS + + +
 1000 format(6x,f7.2,6x,f7.2,7x,i3,7x,i2/12x,i5,17x,f5.2)
 1010 format(8x,12f6.2)
 1020 format(8x,12f6.3)
 1030 format(a19,f6.3,2(2x,a19,f6.3))
c
c     + + + OUTPUT FORMATS + + +
 2000 format(/1x,'Do you want to view data found for station',//,
     1 a41,2x,i4,' (y/n)?: ')
 2010 format(1x,'Observed monthly ave max temperature (C)',/,
     1       1x,12(f5.1,1x),/,
     1       1x,'Observed monthly ave min temperature (C)',/,
     1       1x,12(f5.1,1x))
 2020 format(1x,'Observed monthly ave solar radiation (Langleys/day)',/,
     1       12(1x,f5.1))
 2030 format(/1x,'wet-dry state probabilities'/)
 2040 format(1x,2f10.5)
 2050 format(/1x,'mean,st.dev.,and skew coef. of daily rainfall'/)
 2060 format(1x,3f10.5)
 2070 format(/1x,'standard deviation for max and min temp,',
     1          ' and solar radiation'/)
 2080 format(1x,12f6.2)
 2090 format(1x,12f6.1)
 2100 format(/1x,'coefficient of variation for max, min temp,',
     1          ' solar radiation, and max .5 hr rain'/)
 2110 format(1x,'Average Monthly Dew Point Temperature',/1x,12f6.2)
 2120 format(1x,'Wind Data Interpolated from',
     1           /1x,a19,f6.3,2(2x,a19,f6.3))
 2130 format(1x,12f6.2)
c
c     + + + EQUIVALENCES + + +
      equivalence(rst1,rst(1,1))
      equivalence(rst2,rst(1,2))
      equivalence(rst3,rst(1,3))
      equivalence(prw1,prw(1,1))
      equivalence(prw2,prw(1,2))
c
c     + + + END SPECIFICATIONS + + +
c
c     Read Precipitation, Temperature, Radiation, etc.
c
      read(10,1000)ylt,yll,years,itype,elev,tp6
      read(10,1010)(rst(i,1),i=1,12),(rst(i,2),i=1,12),(rst(i,3),
     1             i=1,12)
      read(10,1010)(prw(i,1),i=1,12),(prw(i,2),i=1,12)
      read(10,1010)(obmx(i),i=1,12)
      read(10,1010)(obmn(i),i=1,12)
      read(10,1010)(stdtx(i),i=1,12),(stdtm(i),i=1,12)
      read(10,1010)(obsl(i),i=1,12)
      read(10,1010)(stdsl(i),i=1,12)
      read(10,1010)(wi(i),i=1,12)
c
c --- Bofu Yu's code discovered & added 7/19/2000 -- CRM:

c                  wi is input as max 30-min rainfall intenisty
c                  now wi is converted into depth as it should be.
c                  B.YU, 7/7/1999
c
      do 65 i=1,12
        wi(i) = 0.5*wi(i)
65    continue
c
      read(10,1010)(rh(i),i=1,12)
      read(10,1020)(timpkd(i),i=1,12)
c
c ----- Output the Max & Min Temp, Rad, and Precip values for testing CLIGEN.
CC     open(71,file='pop_nrs',status='unknown')
CC     write(71,1010)(obmx(i),i=1,12)
CC     write(71,1010)(stdtx(i),i=1,12)
CC     write(71,1010)(obmn(i),i=1,12)
CC     write(71,1010)(stdtm(i),i=1,12)
CC     write(71,1010)(obsl(i),i=1,12)
CC     write(71,1010)(stdsl(i),i=1,12)
CC     write(71,1010)(rst(i,1),i=1,12)
CC     write(71,1010)(rst(i,2),i=1,12)
CC     close (71)
c
c     Compute Fourier Coefficients, if Needed.
      if(interp .eq. 2) then
        call fouri1(rst1,1)
        call fouri1(rst2,2)
        call fouri1(rst3,3)
        call fouri1(prw1,4)
        call fouri1(prw2,5)
        call fouri1(obmx,6)
        call fouri1(obmn,7)
        call fouri1(stdtx,8)
        call fouri1(stdtm,9)
        call fouri1(obsl,10)
        call fouri1(stdsl,11)
        call fouri1(wi,12)
        call fouri1(rh,13)
        call fouri1(timpkd,14)
      else if(interp .eq. 3) then
        call ryf1(rst1,1)
        call ryf1(rst2,2)
        call ryf1(rst3,3)
        call ryf1(prw1,4)
        call ryf1(prw2,5)
        call ryf1(obmx,6)
        call ryf1(obmn,7)
        call ryf1(stdtx,8)
        call ryf1(stdtm,9)
        call ryf1(obsl,10)
        call ryf1(stdsl,11)
        call ryf1(wi,12)
        call ryf1(rh,13)
        call ryf1(timpkd,14)
C     write(*,*) "EMV:", emv
C     write(*,*)
C     write(*,*) "PMT:", pmt
C     write(*,*)
C     write(*,*) "PMV:", pmv
C     write(*,*)
C     write(*,*) "XES:", xes
C     write(*,*)
      endif
c
c     Wind Data read in here.
c     wvl(i,j,k) - array of wind paramters where
c     i - ith direction (1 - north  - 16 nnw)
c     j - parameters (1 - 4)
c       1 - % time from direction i
c       2 - mean speed from direction i
c       3 - standard deviation of speed from direction i
c       4 - skew coeficient of speed from direction i
c     Calm is treated seperately as direction 0, speed 0
c     Only a % time values is need for calm generation.
c
      read(10,1010)(((wvl(i,j,k),k=1,12),j=1,4),i=1,16)
      read(10,1010)(calm(i),i=1,12)
      read(10,1030)site(1),wgt(1),site(2),wgt(2),site(3),wgt(3)
c--- XXX -- Huh?  ELEV is declared to be an *integer*, but in the
c           data file it is a floating-point!!!  --- CRM -- 9/27/99
      elev=elev*.3048
c
c
      do 70 i=1,12
c -- XXX -- Huh??? -- CRM -- 9/14/99
c       wi(i)=wi(i)
        cvtm(i)=stdtm(i)/obmn(i)
        cvtx(i)=stdtx(i)/obmx(i) 
        if(obsl(i).le.0.0) then
          cvs(i)=0.0
        else
          cvs(i)=stdsl(i)/obsl(i)
        endif
 70   continue
c
c -- XXX -- Huh??? -- CRM -- 9/14/99
c     do 80 i=1,12
c       wi(i)=wi(i)
c
      do 6000 i=1,12
6000  dir(i,1)=wvl(1,1,i)
c
      do 7010 i=1,12
        do 7000 j=2,16
          dir(i,j) = dir(i,j-1)+wvl(j,1,i)
7000    continue
        j=17
        dir(i,j)=100.0
7010  continue
c
      do 7050 i=1,12
        do 7049 j=1,17
          dir(i,j)=dir(i,j)*.01
7049    continue
7050  continue
c
      close (10)
c       Close the Parameter File and Write the Parameter if He Wants.
c
c     if((istate.le.0 .or. index.le.0) .and. infile.eq."XXX") then
      if(numarg.eq.0) then
        write(*,2000)stidd,nstat
        read(*,'(a1)')yc
      else
        yc = 'N'
      endif
c
c **** L1 IF ****
      if(yc.ne.'n'.and.yc.ne.'N') then
        write(*,2010)obmx,obmn
        write(*,2020)obsl
        write(*,2030)
        do 90 i=1,12
          write(*,2040) prw(i,1),prw(i,2)
 90     continue
        write(*,2050)
        do 100 i=1,12
          write(*,2060)(rst(i,j),j=1,3)
 100    continue
        write(*,2070)
        write(*,2080)(stdtx(i),i=1,12)
        write(*,2080)(stdtm(i),i=1,12)
        write(*,2090)(stdsl(i),i=1,12)
        write(*,2100)
        write(*,2080)(cvtx(i),i=1,12)
        write(*,2080)(cvtm(i),i=1,12)
        write(*,2080)(cvs(i),i=1,12)
        write(*,2080)(wi(i),i=1,12)
        write(*,2110)rh
        write(*,2080)tp6
        write(*,2120)site(1),wgt(1),site(2),wgt(2),site(3),wgt(3)
        write(*,2130)(((wvl(i,j,k),k=1,12),j=1,4),i=1,16)
        write(*,2130)(calm(i),i=1,12)
c **** L1 ENDIF ****
      endif
      return
      end
c
c
c
      subroutine day_gen
     i           (nbt,jd,iyear,clt,tymax,timpkd,usdur,damt,ustpr,uxmav,
     i            itype,ntd1,
     m            ntd,moveto)
c     + + + PURPOSE + + +
c     To generate the Daily Outputs for CLIGEN.
c
c ----- Split out from the CLIGEN main module 10/1/99 by C. R. Meyer.
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer nbt,jd,iyear,itype,ntd1,ntd,moveto
      real tymax(4),timpkd(0:12),usdur,damt,ustpr,uxmav
c
c     + + + ARGUMENT DEFINITIONS + + +
c     nbt         - Julian day of the year.
c     jd          - Day of the Storm.
c     iyear       - Beginning Simulation Year.
c     clt         - 57.296 180/pi: deg -> radians convert; deg/clt -> radian
c     tymax(4)    - upper limit of r5p (based on itype)
c     timpkd      - The 12 interval time to peak accummulated distribution
c                   parameters for the station.  Cumulative distribution of 
c                   computed time to peak rainfall intensity values based on
c                   NWS 15-minute rainfall data (section 2.1.4 WEPP tech 1995)
c     usdur       - Storm Duration in Hours for Single Storm.
c     damt        - Design Storm Amount in Inches for Single Storm.
c     ustpr       - Time to Peak Intensity (% Duration e.g. .4).
c     uxmav       - Maximum Intensity Inches/Hour for Single Storm.
c     itype       - integer value [1..4] to set single storm parameters.
c     ntd         - Days in this year; ie, 365 or 366.
c     moveto      - A global flag.  If set to 'XX' it means "goto XX".
c
c     + + + COMMON BLOCKS + + +
      include 'cbk1.inc'
c      read: wv
c      modify: th,tdp
c
      include 'cbk3.inc'
c      modify: ida
c
      include 'cbk4.inc'
c      read: mo,iopt,dtp
c
      include 'cbk7.inc'
c      read: k10,ra
c      write: nsim,msim
c      modify: tmxg,tmng
c
      include 'cbk5.inc'
c      modify: r
c
      include 'cbk9.inc'
c      read: r1 
c
      include 'ccl1.inc'
c      write: prcip,tgmx,tgmn
c      modify: radg,dur
c
CC    include 'ctap2.inc'
c        read: tap1, tap2, tap3, tap4, tap5, tap6
c
      include 'cinterp.inc'
c
c     + + + LOCAL VARIABLES + + +
c     tpr         - ratio of (time to rainfall peak)/(rainfall duration)
c     xmav        - ratio of (max rainfall intensity)/(avg rainfall intensity)
c     r5p         - apparently: max rainfall intensity; peak rainfall rate 
c                    (mm/h).  (Yu's r_p): instantaneous peak intensity (mm/h)
c
c
c     + + + SUBROUTINES CALLED + + +
c     jlt
c     clgen
c     windg
c     alph
c     timepk
c
c     + + + INPUT FORMATS + + +
 1000 format(15x,3i5)
c
c     + + + OUTPUT FORMATS + + +
 2000 format(2i3,1x,i5,1x,f5.1,1x,f5.2,1x,f4.2,1x,f6.2,2(1x,f5.1),
     1       1x,f4.0,1x,f4.1,2x,f4.0,1x,f5.1)
c2001 format(i2,1x,6g17.9)
 2001 format(i2,1x,6f17.13)
c
c     + + + END SPECIFICATIONS + + +
c
c
c      Start of the Daily Generation Loop  with nbt and ntd
      if(iopt.eq.4.or.iopt.eq.7) ntd=ntd1
      ida=nbt
 180  continue
        if(iopt.eq.6) then
          msim=0
          nsim=0
          moveto = 225
          read(9,1000,end=199)irida,itmxg,itmng
          moveto = 0
 199      continue
          if(moveto .eq. 0) then
            if(irida.eq.9999) nsim=1
            if(itmxg.eq.9999) msim=1
            if(itmng.eq.9999) msim=1
            r(ida)=irida*.01
            tmxg=itmxg
            tmng=itmng
          endif
        endif
c ****** L1 IF ****
        if(moveto .eq. 0) then
          idr=ida
          call jlt(ntd,idr,mo,jd)
c -------- Interpolation Code inserted here.
          if(interp .eq. 1) then
c-----------(compute lf, rf, & o_mo for current day)
            call lintrp(mo,jd,ntd)
          endif
          call clgen(ntd,iyear)
          call windg
c
c ----------- Output Standard Normal deviates for testing CLIGEN.
c           write(73,*) mo, tap1, tap2, tap3, tap4, tap5, tap6
C           write(73,2001) mo, tap1, tap2, tap3, tap4, tap5, tap6
c ----------- Output date and generated temps, rad & RF for testing CLIGEN.
CC          write(72,2223) mo,jd,iyear,tmxg,tmng,ra,r(ida)
 2223 format(2i3,1x,i4,2(1x,f5.1),1x,f4.0,1x,f5.2)
c
          th=th*clt
          prcip(mo,jd)=r(ida)
c -------- generated max & min daily temp (F)
          tgmx(mo,jd)=tmxg
          tgmn(mo,jd)=tmng
c -------- convert generated max & min daily temp from F to C.
          tmxg=(tmxg-32.0)*(5.0/9.0)
          tmng=(tmng-32.0)*(5.0/9.0)
          tdp = (tdp-32.0)*(5.0/9.0)
          radg(mo,jd)=ra
          if(r(ida).le.0.0) then
            r(ida)=0.0
            dur(mo,jd)=0.0
          else
c           call alph
            call alphb
c ---------- Equation 2.1.6
c           dur(mo,jd)=9.210/(-2.0*alog(1.0-r1))
c ---- This is the new cofficient 3.99 developed for the validation sites.
c      B.YU -- 6/99.
            dur(mo,jd) = 3.99/(-2.0*alog(1.0-r1)) 
C     write(*,*) "R1:",r1," Dur(mo,jd):",dur(mo,jd)
c 
            if(dur(mo,jd).gt.24.0) dur(mo,jd)=24.0
          endif
c
c        Set duration if a single storm is selected
c
c -- XXX -- Where is "RDUR" set? -- CRM -- 9/22/99
c -- Added initialization of RDUR to Zero:
c         rdur = 0.0
c         if(iopt.eq.4.or.iopt.eq.7) dur(mo,jd)=rdur
          if(iopt.eq.4.or.iopt.eq.7) dur(mo,jd)=0.0
c ******** L2 IF ****
          if(iopt.ge.4) then
            if(r(ida).gt.0.) then
c             call alph
              call alphb
              xr=r(ida)*25.4
              tpr=timepk(timpkd,k10)
C     tap4=z
              if(tpr.gt.0.99) tpr=0.99
c ------------ Equation 2.1.7
              r5p=-2.0*xr*alog(1.0-r1)
              if(r5p.gt.tymax(itype)) r5p = tymax(itype)
              xmav=r5p/(xr/dur(mo,jd))
              if((tmxg+tmng)/2.0.le.0.0) xmav = 1.01
              if(xmav.lt.1.01) xmav =1.01
            else
      tap4=0.0
              xr=r(ida)*25.4
              xmav=0.0
              tpr=0.0
            endif
c -------- Change for new option 4 and 7
            if(iopt.eq.4) then
              dur(mo,jd)=usdur
              xr=damt*25.4
              tpr=ustpr
              xmav=(uxmav*25.4)/(xr/dur(mo,jd))
              if(xmav.lt.1.01) xmav=1.01
            else if(iopt.eq.7) then
              dur(mo,jd)=24.
              xr = damt*25.4
              xmav=tymax(itype)/(xr/dur(mo,jd))
              if(xmav.lt.1.01) xmav=1.01
              tpr=dtp(itype)
            endif
c
c            Write WEPP Continuous Storm File
c            Writes WEPP Single Storm when nbt=ndt
            write(7,2000) jd,mo,iyear,xr,dur(mo,jd),tpr,
     1                 xmav,tmxg,tmng,radg(mo,jd),wv,th,tdp
c ******** L2 ENDIF ****
          endif
c ****** L1 ENDIF ****
        endif
c180  continue
      ida=ida+1
      if(ida .le. ntd) goto 180
c
c      End of Daily Loop
c
c
      moveto = 0
c     
      return
      end
c
c
c
      subroutine opt_calc
CC   i           (iyear,stidd,numyr,nstat,ii,
     i           (iyear,stidd,nstat,ii,
     m            sumpp,sumptx,sumptm,sumprd,sumpdr,
     o            moveto)
c     + + + PURPOSE + + +
c     To handle Options 1-3.
c
c ----- Split out from the CLIGEN main module 10/1/99 by C. R. Meyer.
c
c     iyear       - Beginning Simulation Year.
c     stidd       - 41-character alphanumeric station name.
c     numyr       - Number of years to simulate.
c     nstat       - 4-digit Numeric Station Code.
c     ii          -
c     sumpp(13)   - "prcp" (average monthly values for numyr years)
c                     (13: average annual precipitation)
c     sumptx(12)  - "tmax" (average monthly values for numyr years)
c     sumptm(12)  - "tmin" (average monthly values for numyr years)
c     sumprd(12)  - "rad" (average monthly values for numyr years)
c     sumpdr(12)  - "dur" (average monthly values for numyr years)
c     moveto      - A global flag.  If set to 'XX' it means "goto XX".
c
      character*41 stidd
c
      character*1 yc
      real sumpp(13),sumptx(12),sumptm(12),sumprd(12),sumpdr(12)
c
c     + + + COMMON BLOCKS + + +
      include 'cbk4.inc'
c      read: iopt
c
      include 'csumr.inc'
c      read: sump,sumtx,sumtm,sumrd,sumdr
c
      include 'cbk5.inc'
c      modify: r
c
      include 'command6.inc'
c       read: numyr
c
c     + + + SUBROUTINES CALLED + + +
c     clmout
c
c     + + + OUTPUT FORMATS + + +
 2000 format(/1x,'Do you want to view generated data (y/n)? ')
 2010 format(/1x,'Do you want to simulate another year (y/n)? ')
 2020 format(/1x,'Do you want another Station (y/n)? ')
 2030 format(/15x,'Summary of Elements Generated - Year ',i4)
 2040 format(1x,'elem',' yr','   J     F     M     A     M     J',
     1                      '     J     A     S     O     N     D'/)
 2050 format(1x,'prcp',i3,12f6.2)
 2060 format(1x,'tmax',i3,12f6.2)
 2070 format(1x,'tmin',i3,12f6.2)
 2080 format(1x,'rad ',i3,12f6.1)
 2090 format(1x,'dur ',i3,12f6.2)
 2100 format(/1x,'Annual Precipitation =',f6.2,a30/)
 2110 format(i5,i5,10f5.2,8x,i2)
 2120 format(i5,i5,6f5.2,28x,i2)
c
c     + + + END SPECIFICATIONS + + +
c
c      Option 1 Stuff
      if(iopt.le.1) then
        write(*,2000)
        read(*,'(a1)')yc
        if(yc.eq.'y'.or.yc.eq.'Y') then
          iview=1
          call clmout(iview)
        end if
        write(*,2010)
        read(*,'(a1)')yc
        if(yc.eq.'y'.or.yc.eq.'Y') then
          moveto = 160
        else
          write(*,2020)
          read(*,'(a1)')yc
          if(yc.eq.'y'.or.yc.eq.'Y') then
            moveto = 10
          else
            moveto = 230
          endif
        endif
c
c      Option 2 Stuff
      elseif(iopt.eq.2) then
        jj=ii
        call clmout(0)
        write(*,2030) iyear
        write(*,*)' '
        write(*,2040)
        write(*,2050)jj,(sump(i),i=1,12)
        write(*,2060)jj,(sumtx(i),i=1,12)
        write(*,2070)jj,(sumtm(i),i=1,12)
        write(*,2080)jj,(sumrd(i),i=1,12)
        write(*,2090)jj,(sumdr(i),i=1,12)
        write(*,2100)sump(13),stidd
        an=numyr
        do 200 i=1,12
          sumpp(i)=sumpp(i)+sump(i)/an
          sumptx(i)=sumptx(i)+sumtx(i)/an
          sumptm(i)=sumptm(i)+sumtm(i)/an
          sumprd(i)=sumprd(i)+sumrd(i)/an
          sumpdr(i)=sumpdr(i)+sumdr(i)/an
 200    continue
        sumpp(13)=sumpp(13)+sump(13)/an
c
c      Option 3 Stuff
      elseif(iopt.eq.3) then
        nb=1
        ne=10
        do 210  j=1, 36
          write(8,2110)nstat,iyear,(r(i),i=nb,ne),j
          nb=ne + 1
          ne=nb + 9
 210    continue
        nb=361
        ne=366
        if((iyear-iyear/400*400.eq.0).or.((iyear-iyear/4*4.eq.0)
     1     .and..not.(iyear-iyear/100*100.eq.0))) r(366)=0.0
        j=37
        write(8,2120)nstat,iyear,(r(i),i=nb,ne),j
      endif
c
      return
      end
c
c
c
      subroutine sing_stm
     i           (ioyr,
     m            moveto,
     o            jd,iyear,damt,usdur,ustpr,uxmav)
c     + + + PURPOSE + + +
c     Generate Single Storm Data.
c
c ----- Split out from the CLIGEN main module 10/1/99 by C. R. Meyer.
c
c     + + + ARGUMENT DECLARATIONS + + +
CC    integer ioyr,moveto,jd,iyear,index,numyr
      integer ioyr,moveto,jd,iyear
      character*1 yc
      real damt,usdur,ustpr,uxmav
c
c     + + + ARGUMENT DEFINITIONS + + +
c     ioyr        - Years of Record for Observed Data File (?)
c     moveto      - A global flag.  If set to 'XX' it means "goto XX".
c     jd          - Day of the Storm.
c     iyear       - Beginning Simulation Year.
c     damt        - Design Storm Amount in Inches for Single Storm.
c     usdur       - Storm Duration in Hours for Single Storm.
c     ustpr       - Time to Peak Intensity (% Duration e.g. .4).
c     uxmav       - Maximum Intensity Inches/Hour for Single Storm.
c     numyr       - Number of years to simulate.
c     index       - 4-digit numeric station index.
c     outfil      - File which will contain generated climate data.
c
c     + + + COMMON BLOCKS + + +
      include 'cbk4.inc'
c       read: iopt
c      write: mo
c
      include 'command6.inc'
c     modify: numyr
c
c     + + + LOCAL VARIABLES + + +
c     ibyear      - year of storm (iopt 4 or 7); beginning simulation year
c
c     + + + INPUT FORMATS + + +
 1000 format(a51)
c
c     + + + OUTPUT FORMATS + + +
 2000 format(/1x,'Enter beginning simulation year',
     1' (positive integer value; e.g. 1 ): ')
 2010 format(/1x,'Enter number of years to simulate: ')
 2020 format(/1x,'Enter output file name (ex. Indy.cli): ',
     1       'Station No. ',i4)
c
c     + + + END SPECIFICATIONS + + +
c
c
c **** L1 IF ****
      if (iopt.ne.1) then
c ****** K2 IF ****
        if(moveto.ne.135) then
c
c       Single storm input data section
c
          if (iopt.eq.4.or.iopt.eq.7) then
            write(*,*)
            write(*,*)' Enter Month Day and Year of Storm (mo da yr)'
            read(*,*)mo,jd,ibyear
            write(*,*)' Enter Design Storm Amount in Inches (e.g. 6.30)'
            read(*,*)damt
            if(iopt.eq.4) then
              write(*,*)' Enter Storm Duration in Hours (e.g. 6)'
              read(*,*)usdur
              write(*,*)
     1              ' Enter Time to Peak Intensity (% Duration e.g. .4)'
              read(*,*)ustpr
              write(*,*)' Enter Maximum Intensity Inches/Hour (e.g. 3.0)
     1'
              read(*,*)uxmav
            endif
          else
            if(iopt.eq.6) then
C             ibyear=ioyr
C             numyr=100
              if(ibyear .eq. -1) ibyear=ioyr
              if(numyr .eq. -1) numyr=100
            else
              if(ibyear.le.0) then
C     write(*,*) "Ibyear:", ibyear
                write(*,2000)
                read(*,*) ibyear
              endif
              if(numyr.le.0) then
                write(*,2010)
                read(*,*) numyr
              endif
            endif
          endif
c
          iyear=ibyear
c ****** K2 ENDIF ****
        endif
c
c ****** L2 IF ****
c       if (moveto.eq.135 .or. iopt.ne.2) then
        if (iopt.ne.2) then
 135      continue
          if(outfil(1:3).eq."XXX") then
            write(*,2020)index
            read(*,1000)outfil
          endif
c
c         Output Options
c          3 - CREAMS - GLEAMS
c          4 - Single Storm WEPP
c          5 - Continuous Storms WEPP
c          6 - Observed Data WEPP
c          7 - Design Storm WEPP
c
c -------- Output Numbers to Analyze Confidence Limits:
CC        open(72,file='dist_nrs',status='unknown')
C         open(73,file='rand_nrs',status='unknown')
c
          if(iopt.eq.3) then
             ndflag = 136
             open(8,file=outfil,status='new',err=136,iostat=ndfalt)
             ndflag = 0
             moveto = 0
             rewind (8)
 136         continue
          else
             ndflag = 137
             open(7,file=outfil,status='new',err=137,iostat=ndfalt)
             ndflag = 0
             moveto = 0
             rewind (7)
 137         continue
          endif
          if(ndflag.ne.0) then
c ----------- Ask about file over-write.
            if(force .eq. 0) then
              write(*,*)' '
              write(*,*)'     **** File Already Exists *****'
c             write(*,*)' '
              write(*,*)'           ',outfil
c             write(*,*)' '
c             write(*,*)'          Enter New File Name '
              write(*,*)'              Overwrite?'
              write(*,*)' '
              read(*,'(a1)')yc
c ----------- Set flag to Force file over-write.
            else
              yc = 'y'
            endif
            if(yc.eq.'y' .or. yc.eq.'Y') then
              if(ndflag .eq. 136) then
                open(8,file=outfil,status='unknown')
              else if(ndflag .eq. 137) then
                open(7,file=outfil,status='unknown')
              endif
              moveto = 0
            else
              write(outfil(1:3), '("XXX")')
              moveto = 135
            endif
          endif
          if(moveto.eq.135) goto 135
c ****** L2 ENDIF ****
        endif
c **** L1 ENDIF ****
      endif
c
      return
      end
c
c
c
      subroutine usr_opt
     m           (moveto,
     o            ioyr)
c     + + + PURPOSE + + +
c     Get Options from User.
c
c ----- Split out from the CLIGEN main module 10/6/99 by C. R. Meyer.
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer moveto,ioyr
c
c     + + + ARGUMENT DEFINITIONS + + +
c     moveto      - A global flag.  If set to 'XX' it means "goto XX".
c     ioyr        - Years of Record for Observed Data File (?)
c
c     + + + COMMON BLOCKS + + +
      include 'cbk4.inc'
c
      include 'command6.inc'
c
c     + + + LOCAL VARIABLES + + +
      character*1 yc
c
c     + + + LOCAL DEFINITIONS + + +
c     odfile      - File contains observed climate data.
c     ndflag      - A flag for loop control, local to this module.
c
c     + + + INPUT FORMATS + + +
 1000 format(a51)
C1010 format(13x,i2)
 1010 format(10x,i5)
c
c     + + + OUTPUT FORMATS + + +
 2000 format(/1x,'Weather Generator Options',/,1x,7('-'),1x,
     19('-'),1x,7('-'),//,1x,'1 - Single Year Simulation - Screen',/,
     11x,'2 - Multiple Year - Screen Output',/,
     11x,'3 - Multiple Year Simulation - CREAMS - GLEAMS Output File',/,
     11x,'4 - Selected Single Storm WEPP - Output File',/,
     11x,'5 - Multiple Year - WEPP Output File',/,
     11x,'6 - Read Observed P and Temp and Generate Missing Data',/,
     11x,'7 - Single Design Storm - TR 55 Storm Type WEPP Output File',/
     11x,'8 - Exit Weather Generator Program',//,
     11x,'Enter generator option (1-8): ')
c
c     + + + END SPECIFICATIONS + + +
c
c
 56   continue
        if(iopt.lt.0) then
          write(*,2000)
          read(*,*)iopt
        endif
        if (iopt.eq.6) then
          if(odfile.eq."XXX") then
            write(*,*)'Enter Observed Data Input File Name'
            read(*,1000)odfile
          else
            write(*,*) odfile
          endif
          ndflag = 57
          open(9,file=odfile,status='old',err=57)
          ndflag = 0
 57       continue
          if(ndflag.eq.57) then
            write(*,*)' Error - File not Found '
            write(*,*)' Check Directory for Observed Data File or'
            write(*,*)' Re-enter File Name'
            write(*,*)' Enter c to continue or q to quit'
            read(*,'(a1)')yc
            if(yc.ne.'q'.and.yc.ne.'Q') then
              ndflag = 56
            else
              moveto = 230
            endif
          else
            rewind(9)
            read(9,1010)ioyr
            backspace (9)
          endif
        elseif (iopt.eq.8) then
          ndflag = 0
          moveto = 230
        else
          ndflag = 0
        endif
      if(ndflag.eq.56) goto 56
c     if(moveto.ne.230) numyr=1
      return
      end
c
c
c
      subroutine wxr_gen
     i           (version,igcode,stidd,ylt,yll,years,elev,
     i            jd,itype,clt,tymax,timpkd,usdur,damt,ustpr,uxmav,
CC   m            iyear,numyr,xm,smy,tmpcmx,tmpcmn,ntd1,moveto,
     m            iyear,xm,smy,tmpcmx,tmpcmn,ntd1,moveto,
     o            sumpp,sumptx,sumptm,sumprd,sumpdr)
c
c ----- Split out from the CLIGEN main module 10/8/99 by C. R. Meyer.
c
c     + + + PURPOSE + + +
c     The "guts" of the weather generating code.
c
c     + + + ARGUMENT DECLARATIONS + + +
      real version,ylt,yll,xm,clt,damt,ustpr,uxmav
      real smy(12),tymax(4),timpkd(0:12),tmpcmx(12),tmpcmn(12)
      real sumpp(13),sumptx(12),sumptm(12),sumprd(12),sumpdr(12)
C     integer igcode,years,elev,iyear,numyr,ntd1,jd,itype,moveto
      integer igcode,years,elev,iyear,ntd1,jd,itype,moveto
      character*41 stidd
c
c     + + + ARGUMENT DEFINITIONS + + +
c     version     - CLIGEN version (ie, 5.103)
c     igcode      - wind information/ET equation flag
c                      0 -- wind information exists: use Penman ET equation
c                      1 -- no wind information exists: use Priestly-Taylor
c                           ET equation
c     stidd       - 41-character alphanumeric station name.
c     ylt         - Station Latitude.
c     yll         - Station Longitude.
c     years       - Years of Record.
c     elev        - Station Elevation above Sea Level (whole number of meters)
c     jd          - Day of the Storm.
c     itype       - integer value [1..4] to set single storm parameters.
c     clt         - 57.296 180/pi: deg -> radians convert; deg/clt -> radian
c     tymax(4)    - upper limit of r5p (based on itype)
c     timpkd      - The 12 interval time to peak accummulated distribution
c     usdur       - Storm Duration in Hours for Single Storm.
c     damt        - Design Storm Amount in Inches for Single Storm.
c     ustpr       - Time to Peak Intensity (% Duration e.g. .4).
c     uxmav       - Maximum Intensity Inches/Hour for Single Storm.
c     iyear       - Beginning Simulation Year.
c     numyr       - Number of years to simulate.
c     xm          - number of days in the month of interest
c     smy         - Observed Monthly Average Precipitation (mm)
c     tmpcmx      - Observed Monthly Average Max Temperature (C)
c     tmpcmn      - Observed Monthly Average Min Temperature (C)
c     ntd1        - julian date of jd, mo (iopt = 4, 7)
c     moveto      - A global flag.  If set to 'XX' it means "goto XX".
c     sumpp(13)   - "prcp" (average monthly values for numyr years)
c                     (13: average annual precipitation)
c     sumptx(12)  - "tmax" (average monthly values for numyr years)
c     sumptm(12)  - "tmin" (average monthly values for numyr years)
c     sumprd(12)  - "rad" (average monthly values for numyr years)
c     sumpdr(12)  - "dur" (average monthly values for numyr years)
c
c     + + + COMMON BLOCKS + + +
      include 'cbk4.inc'
c      read: nc,mo,iopt
c      modify: nt
c
      include 'cbk7.inc'
c      read: rst,prw,obmx,obmn,obsl
c
      include 'ccl1.inc'
c      write: prcip,tgmx,tgmn,radg,dur
c
      include 'command6.inc'
c       read: numyr, irand
c
      include 'cinterp.inc'
c       read: interp
c
c     + + + LOCAL VARIABLES + + +
c     isim        - simulation mode
c                     1 -- continuous storm (iopt = 5, 6)
c                     2 -- single storm     (iopt = 4, 7)
c     itemp       - breakpoint data flag
c                     0 -- no breakpoint data used
c                     1 -- breakpoint data used
c     xr          - daily precipitation amount (mm of water)
c                    simulated rainfall amount (mm)  [Yu's P]
c
c     + + + SUBROUTINES CALLED + + +
c     day_gen
c     opt_calc
c
c     + + + FUNCTION DECLARATIONS + + +
      integer jdt
c
c     + + + OUTPUT FORMATS + + +
 500  format(1x,'Observed monthly ave max temperature (C)',/,
     1       1x,12(f5.1,1x),/,
     1       1x,'Observed monthly ave min temperature (C)',/,
     1       1x,12(f5.1,1x))
 520  format(1x,'Observed monthly ave solar radiation (Langleys/day)',/,
     1       12(1x,f5.1))
 555  format(1x,'Observed monthly ave precipitation (mm)',/,
     1       12(1x,f5.1))
 642  format(f7.5)
c644  format('  Station: ',a41,6x,' CLIGEN VER. 5.22564 -r:',i5,' -I:'
 644  format('  Station: ',a41,6x,' CLIGEN VER.',f8.5,' -r:',i5,' -I:'
     1       ,i2,/ ' Latitude Longitude Elevation (m) Obs. Years ',
     1       '  Beginning year  Years simulated ', 'Command Line:'/
     1       2f9.2,i12,2i12,i16,
     1       '          ',a)
 648  format(' da mo year  prcp  dur   tp     ip  tmax',
     1       '  tmin  rad  w-vl w-dir  tdew',
     2         /,13x,'(mm)  (h)',15x,'(C)   (C)',
     3       ' (l/d) (m/s)(Deg)   (C)')
 778  format(3i4)
c
c     + + + END SPECIFICATIONS + + +
c
c
c     Get Everything Ready to Start Generation by Options Selected
c
 140    continue
        do 150 i=1,12
          sumpp(i)=0.0
          sumptx(i)=0.0
          sumptm(i)=0.0
          sumprd(i)=0.0
          sumpdr(i)=0.0
 150    continue
        sumpp(13)=0.0
c
c     See What Option was Selected and Set Paths
c
        nbt=1
        if(iopt .ge. 4)then
          isim=1
          if(iopt.eq.4.or.iopt.eq.7) isim =2
          itemp=0
          write(7,642)version
          write(7,778)isim,itemp,igcode
          if(iopt.ge.4) then
C           write(7,644)stidd,ylt,yll,elev,years,iyear,numyr
c           write(7,644)stidd,irand,interp,ylt,yll,elev,years,iyear,
c    1                   numyr,arg_v(:av_len)
            write(7,644)stidd,version,irand,interp,ylt,yll,elev,years,
     1                   iyear,numyr,arg_v(:av_len)
c           write(7,646) iyear,numyr
c
c  **************************************************************
c  *   P(A,B) = P(A|B) * P(B), so:                              *
c  *                                                            *
c  *   P(W) = P(W,D)+P(W,W) = P(W|D) / [1 - P(W|W) + P(W|D)]    *
c  *                                                            *
c  *   C. R. Meyer -- 7/30/2000                                 *
c  **************************************************************
c
c  CALCULATE MONTHLY RAINFALL AMOUNTS
            do 111 kkk = 1,12
              xm = nc(kkk+1)-nc(kkk)
c ---------- calculate number of days of rainfall in month
              smy(kkk) = xm*prw(kkk,2)/(1.-prw(kkk,1) + prw(kkk,2))
c ---------- monthly rainfall in mm
              smy(kkk) = smy(kkk) * rst(kkk,1) * 25.4
c ---------- Convert Observed Temps to degrees-C
              tmpcmx(kkk)=(obmx(kkk)-32.0)*(5.0/9.0)
              tmpcmn(kkk)=(obmn(kkk)-32.0)*(5.0/9.0)
 111        continue
            write(7,500)tmpcmx,tmpcmn
            write(7,520)obsl
            write(7,555)smy
            write(7,648)
          endif
        endif
  
        if(iopt.eq.4.or.iopt.eq.7) then
          nt=0
          if((iyear-iyear/400*400.eq.0).or.((iyear-iyear/4*4.eq.0)
     1       .and.(iyear-iyear/100*100.eq.0)))  nt=1
          ntd1 = jdt(nc,jd,mo,nt)
          nbt = ntd1
          numyr=1
        endif
c
c      Generate Data by Number of Years and Option - MAIN LOOP
c
        ii = 1
 160    continue
          moveto = 0
          ntd=365
          if((iopt.le.3.or.iopt.eq.5.or.iopt.eq.6) .and.
     1       ((iyear-iyear/400*400.eq.0).or.((iyear-iyear/4*4.eq.0)
     1        .and..not.(iyear-iyear/100*100.eq.0)))) ntd=366
          do 170 i=1,12
            do 169 jk=1,31
              prcip(i,jk)=0.0
              tgmx(i,jk)=0.0
              tgmn(i,jk)=0.0
              radg(i,jk)=0.0
              dur(i,jk)=0.0
 169        continue
 170      continue
c
          call day_gen(nbt,jd,iyear,clt,tymax,timpkd,usdur,damt,ustpr,
     1                 uxmav,itype,ntd1,ntd,moveto)
c
          if(moveto .ne. 225) then
CC          call opt_calc(iyear,stidd,numyr,nstat,ii,sumpp,sumptx,
            call opt_calc(iyear,stidd,nstat,ii,sumpp,sumptx,
     1                    sumptm,sumprd,sumpdr,moveto)
c
            if(moveto.eq.0) then
              iyear=iyear+1
              ii = ii + 1
            endif
          endif
        if((moveto.eq.0 .and. ii.le.numyr).or.(moveto.eq.160)) goto 160
c
      return
      end
c
c
c ----------------------< Begin Bofu Yu's Corrections: >---------------------
c
c
      subroutine alphb
c
c     This subroutine computes alpha, a dimensionless parameter that
c     expresses the ratio of the max 30-min rain to total rain.
c
c     This subroutine was re-written by B. YU on June 30, 1999.
c
      include 'cbk3.inc'
c      read: ida
c     ida         - Julian Day of Year.  Used as a subscript to R.
c
      include 'cbk4.inc'
c      read: mo
c     mo          - The current month (1=Jan, 2=Feb...).
c
      include 'cbk5.inc'
c      read: r
c     r           - Daily Precipitation amount (inches of water)
c
      include 'cbk7.inc'
c      read: k7
c     k7          - Seed for random number generation.
c
      include 'cbk9.inc'
c      read: wi,ab,ab1,rn1
c      write: r1
c
c      dimension k7(4)
c
      ei=r(ida)-sml

c                                           ei is the rainfall for the
c                                           day in inches since sml = 0, B.YU

      ai=ab1/(wi(mo)-ab)

c                                     ai is basically 1/alpha
c                                     ab is the lower bound =0.02083
c                                     ab1 = 1 - ab, B.YU

      if (ei .lt. 1.0) then
        ajp = 1.
c
c                                  if rain < 1 inch, the upper bound is 1, by
c
      else

c               in CLIGEN 4.2, if rain > 1 inch, the upper bound is reduced
c               after Arnold and Williams (1989).  This may be all right for
c               middle latitude without high intensity rain.  Note that
c               the observed maximum 30-min rain ever recorded can be as high as
c               299 mm (Linsley et al 1982). This max. 30-min rain in mm was
c               estimated by Rmm = 417.*t(hr)**0.48.  B.YU.  125 mm is used
c               here to be consistent with Arnold and Williams (1989).
c               Probably more work is required here.

        tmax = 125./25.4
c
        ajp=1.0-exp(-tmax/ei)
      endif

c                              generate random number using parameter ai
c                              and seed k7, by

CRM   r1=dstg(ai,k7,rn1)
      r1=dstg(ai,k7)
C     write(*,*) "R1:", r1

c                              This basically re-scales r1 which is in the
c                              range from 0 to 1 into the range from
c                              ab to ajp, where ajp is the upper bound. by
CCC -- XXX -- Added to output values of Gamma dist for testing -- CRM 5/16/03:
C     write(73,2001) mo, r1
C2001 format(i2,1x,6f17.13)
c
      r1=(ei*(ab+r1*(ajp-ab))+sml*ab)/r(ida)
c
      return
      end
c
c
      subroutine r5monb
c
c  This subroutine was full of errors,
c  what follows is a complete re-write. B.YU
c
c  This subroutine smoothes max 30-min rainfall data and computes
c  the R30/R ratio for each month.  On return the ratio is stored in the
c  array wi(i), i=1,12.  This subroutine will be called only once in the main
c  program to generate parameters for storm simulation

c  The subroutine was re-written by B. YU on June 30, 1999
c
c
      include 'cbk3.inc'
c      read: ida
c     ida         - Julian Day of Year.  Used as a subscript to R.
c
      include 'cbk4.inc'
c      read: mo
c     mo          - The current month (1=Jan, 2=Feb...).
c
      include 'cbk5.inc'
c      read: r
c     r           - Daily Precipitation amount (inches of water)
c
      include 'cbk7.inc'
c      read: k7
c     k7          - Seed for random number generation.
c
      include 'cbk9.inc'
c      read: wi,ab,ab1,rn1
c      write: r1

      dimension sm(12),smm(12)

c                                      Smoothing 30-min rainfall data
c                                      3-month running average

      sm(1)=(wi(12)+wi(1)+wi(2))/3.0
      do 20 i=2,11
         sm(i)=(wi(i-1)+wi(i)+wi(i+1))/3.0
 20   continue
      sm(12)=(wi(11)+wi(12)+wi(1))/3.0

      do 30 i=1,12

         if(prw(i,2).eq.0.0) then
           smm(i) = 0.0006944
c                               ! = 1 min
         else

c                     xm - no. of calender days for the month,
c                     smm - no. of rain days

           xm=nc(i+1)-nc(i)
           smm(i)=xm*prw(i,2)/(1.0-prw(i,1)+prw(i,2))
         endif

         if(rst(i,1).eq.0.0) then
           r25 = 0.001
c                    so that it won't overflow = 1/1000 of an inch
         else
           r25 = rst(i,1)
c                    rain per rain day for the month
         endif

c                          See the paper for details.  The idea was that
c                          if there are n rain days, the max 30-min rain is
c                          Rmax30, then the mean of the max 30-min rain for
c                          the month will be R30 = - Rmax30/ln(F), where
c                          F is the exceedance frequency for Rmax, which
c                          is approximately 1/(n + 0.5).  The original Hazen
c                          formula or Weibull's formula were not used, although
c                          what is used here is similar to Weibull's when
c                          the number of rain days is large.  B.YU

         f = 1./(smm(i)+0.5)
         f = -1./alog(f)
CRM      if (f .gt. 1.) then
         if (f .gt. 1.0 .or. f .le. 0.0) then

c                         If no. raindays/month < 2.218, no adjustment to
c                         max 30-min rain is made.

           wi(i) = sm(i)
         else
           wi(i) = f*sm(i)
         endif

c                        sm(i) is the smoothed max 30-min rain for the month
c                        wi(i)  is the mean of the max 30-min rain

         wi(i) = wi(i)/r25
c                        alpha = R30/R


30    continue
      return
      end
c
c
c ----------------------< End Bofu Yu's Corrections. >---------------------
c
c
      subroutine ranset(ntd,iyear)
c ---- Purpose: Generate Array of Random Numbers, an entire month at a
c               time, for each parameter.  Ensure that for the run to 
c               date, the numbers will produce standard normal deviates
c               with a mean at the specified confidence level ("thresh")
c               and a varience at the specified confidence level ("thres2").
c               C. R. Meyer -- 4/6-24/2000 and 5/16-17/2000.
c
c    Modified: Was using NT to determine whether the current year is a
c              Leap Year; however, NT only gets set for options 4 & 7,
c              and only for the *initial year*.  Modified to use NTD.
c              C. R. Meyer -- 3/7/2001
c
c      + + + ARGUMENT DECLARATIONS + + +
      integer ntd
c
c      + + + ARGUMENT DEFINITIONS + + +
c     ntd    - days in this year (365 or 366)
c
c   RanAry elements:
c       ranary(1,1) -- precip prob
c       ranary(1,2) -- max temp
c       ranary(1,3) -- min temp
c       ranary(1,4) -- radiation
c       ranary(1,5) -- precip amount
c       ranary(1,6) -- wind dir
c       ranary(1,7) -- wind vel
c       ranary(1,8) -- TDP
c       ranary(1,9) -- TP
c
C -- XXX -- NT is no longer used.  Probably can eliminate this include:
C       CRM -- 3/20/01.
      include 'cbk4.inc'
c        read: nt
c     nt          - Set to 1 if IYEAR is not a leap year: otherwise, zero
c
      include 'cbk7.inc'
c      modify: k1-k6, k8-k10, v1-v12
c
      include 'crandom3.inc'
c        read: mox, dax, thresh, thres2, vv
c      modify: chicnt
c       write: ranary,g_dimi,g_dimp,g_dsum,g_ssum,chicnt
c
c ---- dim -- Days in each Month.
      integer dim(12), dimi, ldimp, ell, ellx
      character*15 params(9)
      real last_r(9), lst_rx(9)
      data dim/31,28,31,30,31,30,31,31,30,31,30,31/
c ---- ell -- ell=1 ==> Precip; ell=2 ==> No Precip.
      data ell/2/
      data last_r/9* -1.0/
      data params/"Prob. of Precip", "Max. Temp.", "Min. Temp.",
     & "Radiation", "Precip. Amt.", "Wind Dir.", "Wind Vel.",
     & "Temp. Dew Pt.", "Time to Peak"/
      save ell, last_r
CC    save chisum
CC    save chicnt
c
c     real ransum,ranavg,level
      integer level0,level1,chisum(20),ichi
      real ransum,level,level2
c     ransum -- sum of this month's random std norm deviates
c     x2sum  -- sum of S^2 (ie, Xi^2) for this month's random std norm deviates
c     chisum -- count of the chi-square bins for this month only
c     ranavg -- avg of this month's random std norm deviates
c     level0 -- pass/fail (0/1) on chi-square test with up to 20 bins. 
c     level1 -- pass/fail (0/1) on K-S test with up to 20 bins. 
c     level  -- level at which we're confident of a difference in the means.
c     level2 -- level at which we're confident of a difference in the S^2's.
c     g_dsum -- Sum of std norm deviates, for this parameter, for this month,
c                from the beginning of the simulation.
c     g_ssum -- Sum of S^2 of std norm deviates, for this parameter, for this
c                month, from the beginning of the simulation.
c     chi_n  -- number of observations
c     chicnt -- Sum of bins for chi-square test of RNG.
c     sumchi -- V-statistic
c     g_dimi -- Total days for this month of the year, from the beginning
c                of simulation to end of current month.
c     g_dimp -- Total days with precip for this month of the year, from the
c                beginning of simulation to end of current month.
c     g_davg -- avg to date for this parm's random std norm deviates, for
c                this month of the year.
c     ldimp  -- Local variable for days in the month with precip.
c     last_r -- Value of last random deviate generated for this parm.
c     ellx   -- Saves the value of "ell" in case we re-do preip amts.
c     lst_rx -- Saves the value of "last_r(j)" in case of a re-do
c
c ---- update the number of days to end of this month.
      if(mox.ne.2 .or. ntd.eq.365) then
        dimi = dim(mox)
      else
        dimi = 29
      endif
      g_dimi(mox) = g_dimi(mox) + dimi
c
c ---- If RANSET has not executed before, load initial RNG values:
      if(last_r(1) .eq. -1.0) then
        vv = randn(k1)
        last_r(1) = vv
        v1 = randn(k2)
        last_r(2) = v1
        v3 = randn(k3)
        last_r(3) = v3
        v5 = randn(k4)
        last_r(4) = v5
        v7 = randn(k5)
        last_r(5) = v7
        fx = randn(k6)
        last_r(6) = fx
        v9 = randn(k8)
        last_r(7) = v9
        v11 = randn(k9)
        last_r(8) = v11
        z = randn(k10)
        last_r(9) = z
      endif
c
c ------ prepare to count number of re-do's.
      iredo = 0
c ------ save the value of "ell" in case we re-do precip amount.
      ellx = ell
c
      do 999 j=1, 9
c ------ save "last_r(j)" in case we have a re-do.
        lst_rx(j) = last_r(j)
c
c ------ Sum up the standard normal deviates for this month.
 10     continue
          if(j .eq.5) ldimp = 0
          ransum=0.0
          x2sum =0.0
          do 112 ichi=1, 20
            chisum(ichi) = 0
  112     continue
c
c ------ BEGIN random value generation for the current parm.
c        (Each parm uses its own instance of the random number generator.)
          do 998 i=1, dimi
            if(j .eq. 1) then
              ranary(i,j) = randn(k1)
            else if(j .eq. 2) then
              ranary(i,j) = randn(k2)
            else if(j .eq. 3) then
              ranary(i,j) = randn(k3)
            else if(j .eq. 4) then
              ranary(i,j) = randn(k4)
            else if(j .eq. 5) then
c ---------- we had precip today.
              if(ranary(i,1) .le. prw(mox,ell)) then
                ranary(i,j) = randn(k5)
                ell = 1
                ldimp = ldimp + 1
c ---------- we didnt have precip today.
              else
                ranary(i,j) = 0.0
                ell = 2
              endif
            else if(j .eq. 6) then
              ranary(i,j) = randn(k6)
            else if(j .eq. 7) then
              ranary(i,j) = randn(k8)
            else if(j .eq. 8) then
              ranary(i,j) = randn(k9)
            else if(j .eq. 9) then
              if (iopt.eq.6) then
                 ranary(i,j) = 0.0
              else
c------------   if there was precip today, generate a Tpeak:
                if(ranary(i,5) .gt. 0.0) then
C     write(*,*) "yes precip"
                  ranary(i,j) = randn(k10)
                 ell = 1
                else
C     write(*,*) " no precip"
                  ranary(i,j) = 0.0
                  ell = 2
                endif
              endif
C     write(*,*) ranary(i,j)
            endif
c ------ END random value generation for the current parm.
c
c -------- Sum the matching Chi-square bin.
            if(ranary(i,j) .gt. 0.0) then
              ichi = int(ranary(i,j)*20.0) + 1
              if(ichi.gt.20) ichi = 20
              chisum(ichi) = chisum(ichi) + 1
            endif
c -------- Sum Random Standard Normal Deviates for the current parm. & month
CC          if(j.ne.1 .and. j.ne.6 .and. j.ne.7 .and. j.ne.9) then
            if(j.ne.1 .and. j.ne.6 .and. j.ne.7 .and. j.ne.9 
     &                                         .and. j.ne.10) then
              if(j .ne. 5 .or. ell .eq. 1) then
                ransum = ransum + dstn1(last_r(j),ranary(i,j))
                x2sum = x2sum + dstn1(last_r(j),ranary(i,j))**2
                last_r(j) = ranary(i,j)
              endif
            endif
c
 998      continue
 
c        do stat's for everything but observed and tpeak 
         if (iopt.ne.6 .or. j.ne.9) then
c
c ------ Update the Grand Count for the chi-square deviates for this month.
C         if(iredo .eq. 9996)
C    &       write(*,*) " CHICNT IN:",(chicnt(j,mox,ichi),ichi=1,20)
          do 113 ichi=1, 20
            chicnt(j,mox,ichi) = chicnt(j,mox,ichi) + chisum(ichi)
  113     continue
c
c ------ Perform SN tests -- This is a SN distribution.
          if(j.ne.1 .and. j.ne.6 .and. j.ne.7 .and. j.ne.9 
     &                                        .and. j.ne.10) then
c -------- Update the Grand Sum of the standard normal deviates for this month.
            g_dsum(j,mox) = g_dsum(j,mox) + ransum
            g_ssum(j,mox) = g_ssum(j,mox) + x2sum
c -------- Compute the Grand Average of the Std Norm Dev's for this month.
c -------- There is an Observation each day.
            if(j .ne. 5) then
              g_davg = g_dsum(j,mox) / float(g_dimi(mox))
CC    write(*,*) "Orig:", (chicnt(j,mox,ichi),ichi=1,20)
C             call chitst(j,level0)
C             if(level0.eq.0) then
              call ks_tst(j,level1)
              if(level1.eq.0) then
                call conflm(g_davg,g_dimi(mox),0.0,1.0,level)
                call confls(real(g_ssum(j,mox)),g_dimi(mox),level2)
              endif
c -------- There is not an Observation every day (Pcp Amt).
            else
              g_dimp(mox) = g_dimp(mox) + ldimp
              if(g_dimp(mox) .gt. 0) then
                g_davg = g_dsum(j,mox) / float(g_dimp(mox))
              else
                g_davg = 0.0
              endif
CC    write(*,*) "Orig:", (chicnt(j,mox,ichi),ichi=1,20)
C             call chitst(j,level0)
C             if(level0.eq.0) then
              call ks_tst(j,level1)
              if(level1.eq.0) then
                call conflm(g_davg,g_dimp(mox),0.0,1.0,level)
                call confls(real(g_ssum(j,mox)),g_dimp(mox),level2)
C               if((level0.gt.0) .or. (level.gt.thresh(j)))
C    &             write(*,*) "Passed Chi-Square, But *FAILED* SN."
              endif
            endif
c
c ------ Don't Perform SN tests -- uniform dist (P(pcp), Wdir, Wvel, & Tpeak).
          else
CC      write(*,*) "Orig:", (chicnt(j,mox,ichi),ichi=1,20)
C           call chitst(j,level0)
            call ks_tst(j,level1)
            level=-1.0
            level2=-1.0
          endif
c
CC    if(j.eq.5 .and. mox.eq.9) then
CC      write(*,*) " "
CC      write(*,*) "Parm:",params(j), " Mon:",mox," Year:",iyear," N:",
CC   &             chi_n," V:",sumchi
CC      write(*,*) "Chicnt:",(chicnt(j,mox,ichi),ichi=1,20)
CC      write(*,*) " "
CC    endif
c
c ------ If the result throws us outside the confidence limits, RE-DO.
C         if((level0.gt.0) .or. (level.gt.thresh(j)) .or. 
          if((level1.gt.0) .or. (level.gt.thresh(j)) .or. 
     &       (level2.gt.thres2(j))) then
CC          write(*,*) " ********** REDO ***********"
c----------- Count the number of "re-do's" to exit after the prescribed number.
            iredo = iredo + 1
            if(iredo .gt. 9995) then
c ---------- Print error messages:
C             if(level0.gt.0) then
              if(level1.gt.0) then
CC              write(*,*) "Failed Chi-square test."
CC              write(*,*) "Chicnt:",(chicnt(j,mox,ichi),ichi=1,20)
C               write(*,*) "FAILED Chi-Sq -- Parm:",params(j), " Mon:",
C    &                     mox," Year:",iyear," N:",chi_n," V:",sumchi
                write(*,*) "FAILED K-S -- Parm:",params(j), " Mon:",
     &                     mox," Year:",iyear," N:",chi_n
              endif
c ---------- SN parameters:
              if(level.gt.thresh(j) .or. level2.gt.thres2(j)) then
                if(level.gt.thresh(j)) write(*,*) "Failed SN Mean test."
                if(level2.gt.thres2(j)) write(*,*) "Failed SN SD test."
                if(j.ne.5) then
                  write(*,*) "Parm:",params(j)," Month:",mox," Year:",
     &              iyear," Mlevel=",level," SDlevel=",level2," Days:",
     &              g_dimi(mox)
                else
                  write(*,*) "Parm:",params(j)," Month:",mox," Year:",
     &              iyear," Mlevel=",level," SDlevel=",level2," Days:",
     &              g_dimp(mox)
                endif
              endif
            endif
            if(iredo.ne.10000) then
c ---------- Subtract this iteration from QC stats:
CC            if(j.ne.1 .and. j.ne.6 .and. j.ne.7 .and. j.ne.9) then
              if(j.ne.1 .and. j.ne.6 .and. j.ne.7 .and. j.ne.9 
     &                                           .and. j.ne.10) then
                g_dsum(j,mox) = g_dsum(j,mox) - ransum
                g_ssum(j,mox) = g_ssum(j,mox) - x2sum
              endif
              if(j .eq. 5) then
                g_dimp(mox) = g_dimp(mox) - ldimp
                ell = ellx
              endif
              do 114 ichi=1, 20
                chicnt(j,mox,ichi) = chicnt(j,mox,ichi) - chisum(ichi)
  114         continue
c
CC            if(iredo .eq. 9996) then
CC              write(*,*) "    CHISUM:",(chisum(ichi),ichi=1,20)
CC              write(*,*) "CHICNT OUT:",(chicnt(j,mox,ichi),ichi=1,20)
CC            endif
              last_r(j) = lst_rx(j)
            else
c ---------- Give up and Bail Out:
              write(*, '("*** ERROR *** Could not produce desired",
     &              " level of quality in",/20x, "<< ", a, " >>",
     &              " random deviates.")') params(j)
            endif
          endif
C       if((level0.gt.0.or.level.gt.thresh(j).or.level2.gt.thres2(j))
        if((level1.gt.0.or.level.gt.thresh(j).or.level2.gt.thres2(j))
     &     .and. iredo.lt.10000) goto 10
c     
c      don't do stats for observed and tpeak     
       endif
c
 999  continue
c
      return
      end
c
      subroutine chitst(n,level0)
      integer n,level0
c
c     n      -- parameter number
c     level0 -- pass/fail = 0/1
c
      include 'crandom3.inc'
c        read: mox, chicnt
c       write: chi_n, sumchi
c     chicnt -- count of this month's chi-square bins
c     chi_n  -- number of observations 
c     sumchi -- V-statistic
c
      real e_chi
CC    write(*,*) 'ChiCnt:', (chicnt(n,mox,ichi),ichi=1,20)
c
c ---- Determine number of bins to use for chi-square test.
c      (Count number of observations and divide by 5.)
CC    write(*,*) 'N: ', n, ' Level0:', level0
      chi_n = 0
      do 10, i=1, 20
        chi_n = chi_n + chicnt(n,mox,i)
 10   continue
CC    write(*,*) (chicnt(10,mox,i),i=1,20)
CC    write(*,*) 'Chi_N: ', chi_n, ' Mox: ', mox
c
      level0 = 0
      sumchi = 0.0
c ---- Use 20 bins.
      if(chi_n.ge.100) then
        e_chi = 0.05*chi_n
        do 20, i=1, 20
          sumchi = sumchi + (e_chi - chicnt(n,mox,i))**2 / e_chi
 20     continue
        if(sumchi.gt.18.3376) level0 = 1
C       if(sumchi.gt.15.3517) level0 = 1
c
c ---- Use 10 bins.
      else if(chi_n.ge.50) then
        e_chi = 0.1*chi_n
        do 30, i=1, 20, 2
          sumchi = sumchi + (e_chi - chicnt(n,mox,i)
     1                             - chicnt(n,mox,i+1))**2 / e_chi
 30     continue
        if(sumchi.gt.8.34283) level0 = 1
C       if(sumchi.gt.6.3933) level0 = 1
c 
c ---- Use 5 bins.
      else if(chi_n.ge.25) then
        e_chi = 0.2*chi_n
        do 40, i=1, 20, 4
          sumchi = sumchi + (e_chi - chicnt(n,mox,i)
     1                             - chicnt(n,mox,i+1)
     2                             - chicnt(n,mox,i+2)
     3                             - chicnt(n,mox,i+3))**2 / e_chi
 40     continue
        if(sumchi.gt.3.35669) level0 = 1
C       if(sumchi.gt.2.1947) level0 = 1
c 
c ---- Use 4 bins.
      else if(chi_n.ge.20) then
        e_chi = 0.25*chi_n
        do 50, i=1, 20, 5
          sumchi = sumchi + (e_chi - chicnt(n,mox,i)
     1                             - chicnt(n,mox,i+1)
     2                             - chicnt(n,mox,i+2)
     3                             - chicnt(n,mox,i+3)
     4                             - chicnt(n,mox,i+4))**2 / e_chi
 50     continue
        if(sumchi.gt.2.36597) level0 = 1
C       if(sumchi.gt.1.4237) level0 = 1
c
c ---- Use 2 bins.
c      Note: Not valid for chi_n < 10, but keeps things from going 
c            totally out of control before chi_n reaches 10....
Cc    else if(chi_n.ge.10) then
      else if(chi_n.ge.2) then
        e_chi = 0.5*chi_n
        do 60, i=1, 20, 10
          sumchi = sumchi + (e_chi - chicnt(n,mox,i)
     1                             - chicnt(n,mox,i+1)
     2                             - chicnt(n,mox,i+2)
     3                             - chicnt(n,mox,i+3)
     4                             - chicnt(n,mox,i+4)
     5                             - chicnt(n,mox,i+5)
     6                             - chicnt(n,mox,i+6)
     7                             - chicnt(n,mox,i+7)
     8                             - chicnt(n,mox,i+8)
     9                             - chicnt(n,mox,i+9))**2 / e_chi
 60     continue
        if(sumchi.gt.0.45494) level0 = 1
C       if(sumchi.gt.0.14847) level0 = 1
c
c ---- Skip Chi-square test because it's not valid.
      else
        level0 = 0
        e_chi = 0.0
        sumchi = 0.0
      endif
c
CC    if(level0.eq.1) then
CC      write(*,*) 'Chi Reject'
CC      write(*,*) 'Parm:',n,' Month:',mox,' N:',chi_n,' V:',sumchi
CC    else
CC      write(*,*) 'Chi Pass'
CC    endif
c
      return
      end
c
c
      subroutine ks_tst(n,level1)
      integer n,level1
c
c    Purpose: Calculate whether parameters are uniformly distributed, using
c             the Kolmogorov-Smirnov goodness of fit test.
c    Comment: Based on the Chi-Square routine.
c    Written 5/5/2004 -- C. R. Meyer
c
c     n      -- parameter number
c     level1 -- pass/fail = 0/1
c
      include 'crandom3.inc'
c        read: mox, chicnt
c       write: chi_n
c     chicnt -- count of this month's chi-square bins
c     chi_n  -- number of observations 
c
      real e_chi
      integer ks_cnt(20)
      real ks_dif(20), maxdif
c
c      e_chi -- number of obs expected in each Chi-Sq bin
c     ks_cnt -- cumulative count of chi-square bins
c     ks_dif -- cum difference between expected and count of bins
c
CC    write(*,*) 'ChiCnt:', (chicnt(n,mox,ichi),ichi=1,20)
c
c ---- Determine number of bins to use for chi-square test.
c      (Ie, count number of observations and divide by 5.)
      level1 = 0
      chi_n = 0
      do 10, i=1, 20
        chi_n = chi_n + chicnt(n,mox,i)
        ks_cnt(i) = 0
        ks_dif(i) = 0.0
 10   continue
c
c ---- Use 20 bins.
      if(chi_n.ge.100) then
        e_chi = 0.05*chi_n
        ks_cnt(1) = chicnt(n,mox,1)
        ks_dif(1) = abs(ks_cnt(1) - e_chi)
        maxdif = ks_dif(1)
        do 20, i=2, 20
          e_chi = i*0.05*chi_n
          ks_cnt(i) = ks_cnt(i-1) + chicnt(n,mox,i)
          ks_dif(i) = abs(ks_cnt(i) - e_chi)
          if(maxdif .lt. ks_dif(i)) maxdif = ks_dif(i)
 20     continue
        if(maxdif/sqrt(float(chi_n)) .gt. 0.8276) level1 = 1
c
Cc ---- Use 10 bins.
C      else if(chi_n.ge.50) then
C        e_chi = 0.1*chi_n
C        ks_cnt(1) = chicnt(n,mox,1) + chicnt(n,mox,2)
C        ks_dif(1) = abs(ks_cnt(1) - e_chi)
C        maxdif = ks_dif(1)
C        do 30, i=3, 20, 2
C          e_chi = i*0.1*chi_n
C          ks_cnt(i) = ks_cnt(i-2) + chicnt(n,mox,i) + chicnt(n,mox,i+1)
C          ks_dif(i) = abs(ks_cnt(i) - e_chi)
C          if(maxdif .lt. ks_dif(i)) maxdif = ks_dif(i)
C 30     continue
C        if(maxdif/sqrt(float(chi_n)) .gt. 0.8276) level1 = 1
Cc 
Cc ---- Use 5 bins.
C      else if(chi_n.ge.25) then
C        e_chi = 0.2*chi_n
C        ks_cnt(1) = chicnt(n,mox,1) + chicnt(n,mox,2) + chicnt(n,mox,3)
C     1              + chicnt(n,mox,4)
C        ks_dif(1) = abs(ks_cnt(1) - e_chi)
C        maxdif = ks_dif(1)
C        do 40, i=5, 20, 4
C          e_chi = i*0.2*chi_n
C          ks_cnt(i) = ks_cnt(i-4) + chicnt(n,mox,i) + chicnt(n,mox,i+1)
C     1                + chicnt(n,mox,i+2) + chicnt(n,mox,i+3)
C          ks_dif(i) = abs(ks_cnt(i) - e_chi)
C          if(maxdif .lt. ks_dif(i)) maxdif = ks_dif(i)
C 40     continue
C        if(maxdif/sqrt(float(chi_n)) .gt. 0.8276) level1 = 1
Cc 
Cc ---- Use 4 bins.
C      else if(chi_n.ge.20) then
C        e_chi = 0.25*chi_n
C        ks_cnt(1) = chicnt(n,mox,1) + chicnt(n,mox,2) + chicnt(n,mox,3)
C     1              + chicnt(n,mox,4) + chicnt(n,mox,5)
C        ks_dif(1) = abs(ks_cnt(1) - e_chi)
C        maxdif = ks_dif(1)
C        do 50, i=6, 20, 5
C          e_chi = i*0.25*chi_n
C          ks_cnt(i) = ks_cnt(i-4) + chicnt(n,mox,i) + chicnt(n,mox,i+1)
C     1                + chicnt(n,mox,i+2) + chicnt(n,mox,i+3)
C     2                + chicnt(n,mox,i+4)
C          ks_dif(i) = abs(ks_cnt(i) - e_chi)
C          if(maxdif .lt. ks_dif(i)) maxdif = ks_dif(i)
C 50     continue
C        if(maxdif/sqrt(float(chi_n)) .gt. 0.8276) level1 = 1
Cc
Cc ---- Use 2 bins.
Cc      Note: Not valid for chi_n < 10, but keeps things from going 
Cc            totally out of control before chi_n reaches 10....
CCc    else if(chi_n.ge.10) then
C      else if(chi_n.ge.2) then
C        e_chi = 0.5*chi_n
C        ks_cnt(1) = chicnt(n,mox,1) + chicnt(n,mox,2)
C     1              + chicnt(n,mox,3) + chicnt(n,mox,4)
C     2              + chicnt(n,mox,5) + chicnt(n,mox,6)
C     3              + chicnt(n,mox,7) + chicnt(n,mox,8)
C     4              + chicnt(n,mox,9) + chicnt(n,mox,10)
C        ks_dif(1) = abs(ks_cnt(1) - e_chi)
C        maxdif = ks_dif(1)
C        e_chi = chi_n
C        ks_cnt(11) = chicnt(n,mox,11) + chicnt(n,mox,12)
C     1              + chicnt(n,mox,13) + chicnt(n,mox,14)
C     2              + chicnt(n,mox,15) + chicnt(n,mox,16)
C     3              + chicnt(n,mox,17) + chicnt(n,mox,18)
C     4              + chicnt(n,mox,19) + chicnt(n,mox,20)
C        ks_dif(11) = abs(ks_cnt(11) - e_chi)
C        if(maxdif .lt. ks_dif(11)) maxdif = ks_dif(11)
C        if(maxdif/sqrt(float(chi_n)) .gt. 0.8276) level1 = 1
c
c ---- Skip Chi-square (and K-S) test because it's not valid.
      else
        level1 = 0
      endif
c
C     if(level1.eq.1) then
C       write(*,*) 'KS Reject -- Parm:',n,' Month:',mox,' N:',chi_n
C     else
C       write(*,*) 'KS Pass'
C     endif
c
      return
      end
c
c
      subroutine conflm(xbar,n,mu,sigma,level)
      real xbar,mu,sigma,level
      integer n
c
c    A confidence interval on the sample mean.
c    Returns the "level" (percent) at which one can be confident
c    that the sample of 'N' measurements which produced Xbar, came
c    from a population DIFFERENT FROM a population with mean 'Mu'
c    and variance 'Sigma'.  Note that this is a test of the sample
c    mean only -- it does not involve or test the sample varience.
c
c    Generally:
c               xbar - mu
c            ---------------  ~  N(0,1); ie, Std. Normal
c            sigma / sqrt(n)
c
c    This was originally a recursive routine.  F-77 doesn't support
c    recursion.
c
c    Written 12/28/99 -- C. R. Meyer
c
      parameter (nz=15)
      real z(nz),prob(nz)
      real up_lim,lowlim,margin
      integer bkthru
c
c ------ Standard Normal Z-values:
      data z/2.807,2.576,1.96,1.645,1.282,1.036,0.8416,0.6745,
     1       0.5244,0.3853,0.2533,0.1257,0.06271,0.01253,0.006267/
c ------ Probabilities (percent) that populations are DIFFERENT.
      data prob/99.5,99.0,95.0,90.0,80.0,70.0,60.0,50.0,40.0,
     1       30.0,20.0,10.0,5.0,1.0,0.5/
c
      if(n. gt. 0) then
        index = 0
 10     continue
        bkthru = 0
        if(index .lt. nz) then
          index = index + 1
          margin = z(index)*sigma/sqrt(float(n))
          up_lim = xbar + margin
          lowlim = xbar - margin
c
          if((mu .gt. up_lim).or.(mu .lt.lowlim)) then
            level = prob(index)
          else
            bkthru = 1
c           call conflm(xbar,n,mu,sigma,level,index)
          endif
        else
          level = 0.0
        endif
        if(bkthru .ne. 0) goto 10
      else
        level = 0.0
      endif
c
      return
      end
c
c
      subroutine confls(x2sum,n,level)
c ---- x2sum  -- sum of the squares of the standard normal deviates.
c ---- n      -- number of deviates (degrees of freedom)
c ---- level  -- level at which one can be sure the sample of deviates
c                 is DIFFERENT FROM the parent population.
c
      real x2sum,level
      integer n
c ---- For ACM routines:
      double precision bound,df,p,q,x
      integer status,which
      data which /1/
      x = dble(x2sum)
      df = dble(n)
c
c    A confidence interval on the *Standard Normal* sample varience.
c    Returns the "level" (percent) at which one can be confident
c    that the sample of 'N' measurements which produced X2sum, came
c    from a population DIFFERENT FROM a population with mean=0.0,
c    and variance=1.0 .  Note that this is a test of the sample
c    varience only -- it does not involve or test the sample mean.
c
c    Generally:
c             / Xi - mu \ 2       2
c        Sum  | ------- |     ~  X (N); ie, Chi-Square(N)
c             \  sigma  /
c
c    But for our purposes, since mu=0 and sigma=1, this reduces to:
c
c            /  2\      2
c        Sum \Xi /  ~  X (N); ie, Chi-Square(N)
c
c    Written 05/02/00 -- C. R. Meyer
c    Rewritten 5/16/00 to utilize Chi-Square routines from Numerical Recipes
c    --- CRM
c
      if(n .gt. 0) then
c ------- LEVEL is a decimal fraction here:
c ------- To use binary "chi-sq.o" from copyrighted Numerical Recipes:
c       level = gammp(float(n)/2.0,x2sum/2.0)
c ------- To use public domain ACM code (below):
        call cdfchi(which,p,q,x,df,status,bound)
        level = p
c ------- LEVEL is a percent here:
        level = 100.0 * 2.0*abs(0.5 - level)
      else
        level = 0.0
      endif
c
      return
      end
c
c
c -------< ACM Chi-square code from Anderson Cancer Center in Texas: >-------
c
      SUBROUTINE cdfchi(which,p,q,x,df,status,bound)
c**********************************************************************
c
c      SUBROUTINE CDFCHI( WHICH, P, Q, X, DF, STATUS, BOUND )
c               Cumulative Distribution Function
c               CHI-Square distribution
c
c
c                              Function
c
c
c     Calculates any one parameter of the chi-square
c     distribution given values for the others.
c
c
c                              Arguments
c
c
c     WHICH --> Integer indicating which of the next three argument
c               values is to be calculated from the others.
c               Legal range: 1..3
c               iwhich = 1 : Calculate P and Q from X and DF
c               iwhich = 2 : Calculate X from P,Q and DF
c               iwhich = 3 : Calculate DF from P,Q and X
c                    INTEGER WHICH
c
c     P <--> The integral from 0 to X of the chi-square
c            distribution.
c            Input range: [0, 1].
c                    DOUBLE PRECISION P
c
c     Q <--> 1-P.
c            Input range: (0, 1].
c            P + Q = 1.0.
c                    DOUBLE PRECISION Q
c
c     X <--> Upper limit of integration of the non-central
c            chi-square distribution.
c            Input range: [0, +infinity).
c            Search range: [0,1E100]
c                    DOUBLE PRECISION X
c
c     DF <--> Degrees of freedom of the
c             chi-square distribution.
c             Input range: (0, +infinity).
c             Search range: [ 1E-100, 1E100]
c                    DOUBLE PRECISION DF
c
c     STATUS <-- 0 if calculation completed correctly
c               -I if input parameter number I is out of range
c                1 if answer appears to be lower than lowest
c                  search bound
c                2 if answer appears to be higher than greatest
c                  search bound
c                3 if P + Q .ne. 1
c               10 indicates error returned from cumgam.  See
c                  references in cdfgam
c                    INTEGER STATUS
c
c     BOUND <-- Undefined if STATUS is 0
c
c               Bound exceeded by parameter number I if STATUS
c               is negative.
c
c               Lower search bound if STATUS is 1.
c
c               Upper search bound if STATUS is 2.
c
c
c                              Method
c
c
c     Formula    26.4.19   of Abramowitz  and     Stegun, Handbook  of
c     Mathematical Functions   (1966) is used   to reduce the chisqure
c     distribution to the incomplete distribution.
c
c     Computation of other parameters involve a seach for a value that
c     produces  the desired  value  of P.   The search relies  on  the
c     monotinicity of P with the other parameter.
c
c**********************************************************************
c     .. Parameters ..
      DOUBLE PRECISION tol
      PARAMETER (tol=1.0D-8)
      DOUBLE PRECISION atol
      PARAMETER (atol=1.0D-50)
      DOUBLE PRECISION zero,inf
      PARAMETER (zero=1.0D-100,inf=1.0D100)
c     ..
c     .. Scalar Arguments ..
      DOUBLE PRECISION bound,df,p,q,x
      INTEGER status,which
c     ..
c     .. Local Scalars ..
      DOUBLE PRECISION ccum,cum,fx,porq,pq
      LOGICAL qhi,qleft,qporq
c     ..
c     .. External Functions ..
      DOUBLE PRECISION spmpar
      EXTERNAL spmpar
c     ..
c     .. External Subroutines ..
      EXTERNAL cumchi,dinvr,dstinv
c     ..
c     .. Intrinsic Functions ..
      INTRINSIC abs
c     ..
c
c -- XXX -- Added 4/13/01 -- CRM:
      data porq/0.d0/
      IF (.NOT. ((which.LT.1).OR. (which.GT.3))) GO TO 30
      IF (.NOT. (which.LT.1)) GO TO 10
      bound = 1.0D0
      GO TO 20

   10 bound = 3.0D0
   20 status = -1
      RETURN

   30 IF (which.EQ.1) GO TO 70
      IF (.NOT. ((p.LT.0.0D0).OR. (p.GT.1.0D0))) GO TO 60
      IF (.NOT. (p.LT.0.0D0)) GO TO 40
      bound = 0.0D0
      GO TO 50

   40 bound = 1.0D0
   50 status = -2
      RETURN

   60 CONTINUE
   70 IF (which.EQ.1) GO TO 110
      IF (.NOT. ((q.LE.0.0D0).OR. (q.GT.1.0D0))) GO TO 100
      IF (.NOT. (q.LE.0.0D0)) GO TO 80
      bound = 0.0D0
      GO TO 90

   80 bound = 1.0D0
   90 status = -3
      RETURN

  100 CONTINUE
  110 IF (which.EQ.2) GO TO 130
      IF (.NOT. (x.LT.0.0D0)) GO TO 120
      bound = 0.0D0
      status = -4
      RETURN

  120 CONTINUE
  130 IF (which.EQ.3) GO TO 150
      IF (.NOT. (df.LE.0.0D0)) GO TO 140
      bound = 0.0D0
      status = -5
      RETURN

  140 CONTINUE
  150 IF (which.EQ.1) GO TO 190
      pq = p + q
      IF (.NOT. (abs(((pq)-0.5D0)-0.5D0).GT.
     +    (3.0D0*spmpar(1)))) GO TO 180
      IF (.NOT. (pq.LT.0.0D0)) GO TO 160
      bound = 0.0D0
      GO TO 170

  160 bound = 1.0D0
  170 status = 3
      RETURN

  180 CONTINUE
  190 IF (which.EQ.1) GO TO 220
      qporq = p .LE. q
      IF (.NOT. (qporq)) GO TO 200
      porq = p
      GO TO 210

  200 porq = q
  210 CONTINUE
  220 IF ((1).EQ. (which)) THEN
          status = 0
          CALL cumchi(x,df,p,q)
          IF (porq.GT.1.5D0) THEN
              status = 10
              RETURN

          END IF

      ELSE IF ((2).EQ. (which)) THEN
          x = 5.0D0
          CALL dstinv(0.0D0,inf,0.5D0,0.5D0,5.0D0,atol,tol)
          status = 0
          CALL dinvr(status,x,fx,qleft,qhi)
  230     IF (.NOT. (status.EQ.1)) GO TO 270
          CALL cumchi(x,df,cum,ccum)
          IF (.NOT. (qporq)) GO TO 240
          fx = cum - p
          GO TO 250

  240     fx = ccum - q
  250     IF (.NOT. ((fx+porq).GT.1.5D0)) GO TO 260
          status = 10
          RETURN

  260     CALL dinvr(status,x,fx,qleft,qhi)
          GO TO 230

  270     IF (.NOT. (status.EQ.-1)) GO TO 300
          IF (.NOT. (qleft)) GO TO 280
          status = 1
          bound = 0.0D0
          GO TO 290

  280     status = 2
          bound = inf
  290     CONTINUE
  300     CONTINUE

      ELSE IF ((3).EQ. (which)) THEN
          df = 5.0D0
          CALL dstinv(zero,inf,0.5D0,0.5D0,5.0D0,atol,tol)
          status = 0
          CALL dinvr(status,df,fx,qleft,qhi)
  310     IF (.NOT. (status.EQ.1)) GO TO 350
          CALL cumchi(x,df,cum,ccum)
          IF (.NOT. (qporq)) GO TO 320
          fx = cum - p
          GO TO 330

  320     fx = ccum - q
  330     IF (.NOT. ((fx+porq).GT.1.5D0)) GO TO 340
          status = 10
          RETURN

  340     CALL dinvr(status,df,fx,qleft,qhi)
          GO TO 310

  350     IF (.NOT. (status.EQ.-1)) GO TO 380
          IF (.NOT. (qleft)) GO TO 360
          status = 1
          bound = zero
          GO TO 370

  360     status = 2
          bound = inf
  370     CONTINUE
  380 END IF

      RETURN
      END
c
c
      SUBROUTINE cumchi(x,df,cum,ccum)
c**********************************************************************
c
c     SUBROUTINE FUNCTION CUMCHI(X,DF,CUM,CCUM)
c             CUMulative of the CHi-square distribution
c
c
c                              Function
c
c
c     Calculates the cumulative chi-square distribution.
c
c
c                              Arguments
c
c
c     X       --> Upper limit of integration of the
c                 chi-square distribution.
c                                                 X is DOUBLE PRECISION
c
c     DF      --> Degrees of freedom of the
c                 chi-square distribution.
c                                                 DF is DOUBLE PRECISION
c
c     CUM <-- Cumulative chi-square distribution.
c                                                 CUM is DOUBLE PRECISIO
c
c     CCUM <-- Compliment of Cumulative chi-square distribution.
c                                                 CCUM is DOUBLE PRECISI
c
c
c                              Method
c
c
c     Calls incomplete gamma function (CUMGAM)
c
c**********************************************************************
c     .. Scalar Arguments ..
      DOUBLE PRECISION df,x,cum,ccum
c     ..
c     .. Local Scalars ..
      DOUBLE PRECISION a,xx
c     ..
c     .. External Subroutines ..
      EXTERNAL cumgam
c     ..
c     .. Executable Statements ..
      a = df*0.5D0
      xx = x*0.5D0
      CALL cumgam(xx,a,cum,ccum)
      RETURN
      END
c
c
      SUBROUTINE cumgam(x,a,cum,ccum)
c**********************************************************************
c
c     SUBROUTINE CUMGAM(X,A,CUM,CCUM)
c           Double precision cUMulative incomplete GAMma distribution
c
c
c                              Function
c
c
c     Computes   the  cumulative        of    the     incomplete   gamma
c     distribution, i.e., the integral from 0 to X of
c          (1/GAM(A))*EXP(-T)*T**(A-1) DT
c     where GAM(A) is the complete gamma function of A, i.e.,
c          GAM(A) = integral from 0 to infinity of
c                    EXP(-T)*T**(A-1) DT
c
c
c                              Arguments
c
c
c     X --> The upper limit of integration of the incomplete gamma.
c                                                X is DOUBLE PRECISION
c
c     A --> The shape parameter of the incomplete gamma.
c                                                A is DOUBLE PRECISION
c
c     CUM <-- Cumulative incomplete gamma distribution.
c                                        CUM is DOUBLE PRECISION
c
c     CCUM <-- Compliment of Cumulative incomplete gamma distribution.
c                                                CCUM is DOUBLE PRECISIO
c
c
c                              Method
c
c
c     Calls the routine GRATIO.
c
c**********************************************************************
c
c     ..
c     .. Scalar Arguments ..
      DOUBLE PRECISION a,x,cum,ccum
c     ..
c     .. External Routines ..
      EXTERNAL gratio
c     ..
c     .. Executable Statements ..
      IF (.NOT. (x.LE.0.0D0)) GO TO 10
      cum = 0.0D0
      ccum = 1.0D0
      RETURN

   10 CALL gratio(a,x,cum,ccum,0)

c     Call gratio routine

      RETURN
      END
c
c
      SUBROUTINE dinvr(status,x,fx,qleft,qhi)
c**********************************************************************
c
c     SUBROUTINE DINVR(STATUS, X, FX, QLEFT, QHI)
c          Double precision
c          bounds the zero of the function and invokes zror
c                    Reverse Communication
c
c
c                              Function
c
c
c     Bounds the    function  and  invokes  ZROR   to perform the   zero
c     finding.  STINVR  must  have   been  called  before this   routine
c     in order to set its parameters.
c
c
c                              Arguments
c
c
c     STATUS <--> At the beginning of a zero finding problem, STATUS
c                 should be set to 0 and INVR invoked.  (The value
c                 of parameters other than X will be ignored on this cal
c
c                 When INVR needs the function evaluated, it will set
c                 STATUS to 1 and return.  The value of the function
c                 should be set in FX and INVR again called without
c                 changing any of its other parameters.
c
c                 When INVR has finished without error, it will return
c                 with STATUS 0.  In that case X is approximately a root
c                 of F(X).
c
c                 If INVR cannot bound the function, it returns status
c                 -1 and sets QLEFT and QHI.
c                         INTEGER STATUS
c
c     X <-- The value of X at which F(X) is to be evaluated.
c                         DOUBLE PRECISION X
c
c     FX --> The value of F(X) calculated when INVR returns with
c            STATUS = 1.
c                         DOUBLE PRECISION FX
c
c     QLEFT <-- Defined only if QMFINV returns .FALSE.  In that
c          case it is .TRUE. If the stepping search terminated
c          unsucessfully at SMALL.  If it is .FALSE. the search
c          terminated unsucessfully at BIG.
c                    QLEFT is LOGICAL
c
c     QHI <-- Defined only if QMFINV returns .FALSE.  In that
c          case it is .TRUE. if F(X) .GT. Y at the termination
c          of the search and .FALSE. if F(X) .LT. Y at the
c          termination of the search.
c                    QHI is LOGICAL

c
c**********************************************************************
c     .. Scalar Arguments ..
      DOUBLE PRECISION fx,x,zabsst,zabsto,zbig,zrelst,zrelto,zsmall,
     +                 zstpmu
      INTEGER status
      LOGICAL qhi,qleft
c     ..
c     .. Local Scalars ..
      DOUBLE PRECISION absstp,abstol,big,fbig,fsmall,relstp,reltol,
     +                 small,step,stpmul,xhi,xlb,xlo,xsave,xub,yy,zx,zy,
     +                 zz
      INTEGER i99999
      LOGICAL qbdd,qcond,qdum1,qdum2,qincr,qlim,qok,qup
c     ..
c     .. External Subroutines ..
      EXTERNAL dstzr,dzror
c     ..
c     .. Intrinsic Functions ..
      INTRINSIC abs,max,min
c     ..
c     .. Statement Functions ..
      LOGICAL qxmon
c     ..
c     .. Save statement ..
      SAVE
c     ..
c     .. Statement Function definitions ..
      qxmon(zx,zy,zz) = zx .LE. zy .AND. zy .LE. zz
c     ..
c     .. Executable Statements ..

      IF (status.GT.0) GO TO 310

      qcond = .NOT. qxmon(small,x,big)
      IF (qcond) STOP ' SMALL, X, BIG not monotone in INVR'
      xsave = x
c
c     See that SMALL and BIG bound the zero and set QINCR
c
      x = small
c     GET-FUNCTION-VALUE
      ASSIGN 10 TO i99999
      GO TO 300

   10 fsmall = fx
      x = big
c     GET-FUNCTION-VALUE
      ASSIGN 20 TO i99999
      GO TO 300

   20 fbig = fx
      qincr = fbig .GT. fsmall
      IF (.NOT. (qincr)) GO TO 50
      IF (fsmall.LE.0.0D0) GO TO 30
      status = -1
      qleft = .TRUE.
      qhi = .TRUE.
      RETURN

   30 IF (fbig.GE.0.0D0) GO TO 40
      status = -1
      qleft = .FALSE.
      qhi = .FALSE.
      RETURN

   40 GO TO 80

   50 IF (fsmall.GE.0.0D0) GO TO 60
      status = -1
      qleft = .TRUE.
      qhi = .FALSE.
      RETURN

   60 IF (fbig.LE.0.0D0) GO TO 70
      status = -1
      qleft = .FALSE.
      qhi = .TRUE.
      RETURN

   70 CONTINUE
   80 x = xsave
      step = max(absstp,relstp*abs(x))
c      YY = F(X) - Y
c     GET-FUNCTION-VALUE
      ASSIGN 90 TO i99999
      GO TO 300

   90 yy = fx
      IF (.NOT. (yy.EQ.0.0D0)) GO TO 100
      status = 0
      qok = .TRUE.
      RETURN

  100 qup = (qincr .AND. (yy.LT.0.0D0)) .OR.
     +      (.NOT.qincr .AND. (yy.GT.0.0D0))
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c
c     HANDLE CASE IN WHICH WE MUST STEP HIGHER
c
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      IF (.NOT. (qup)) GO TO 170
      xlb = xsave
      xub = min(xlb+step,big)
      GO TO 120

  110 IF (qcond) GO TO 150
c      YY = F(XUB) - Y
  120 x = xub
c     GET-FUNCTION-VALUE
      ASSIGN 130 TO i99999
      GO TO 300

  130 yy = fx
      qbdd = (qincr .AND. (yy.GE.0.0D0)) .OR.
     +       (.NOT.qincr .AND. (yy.LE.0.0D0))
      qlim = xub .GE. big
      qcond = qbdd .OR. qlim
      IF (qcond) GO TO 140
      step = stpmul*step
      xlb = xub
      xub = min(xlb+step,big)
  140 GO TO 110

  150 IF (.NOT. (qlim.AND..NOT.qbdd)) GO TO 160
      status = -1
      qleft = .FALSE.
      qhi = .NOT. qincr
      x = big
      RETURN

  160 GO TO 240
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c
c     HANDLE CASE IN WHICH WE MUST STEP LOWER
c
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  170 xub = xsave
      xlb = max(xub-step,small)
      GO TO 190

  180 IF (qcond) GO TO 220
c      YY = F(XLB) - Y
  190 x = xlb
c     GET-FUNCTION-VALUE
      ASSIGN 200 TO i99999
      GO TO 300

  200 yy = fx
      qbdd = (qincr .AND. (yy.LE.0.0D0)) .OR.
     +       (.NOT.qincr .AND. (yy.GE.0.0D0))
      qlim = xlb .LE. small
      qcond = qbdd .OR. qlim
      IF (qcond) GO TO 210
      step = stpmul*step
      xub = xlb
      xlb = max(xub-step,small)
  210 GO TO 180

  220 IF (.NOT. (qlim.AND..NOT.qbdd)) GO TO 230
      status = -1
      qleft = .TRUE.
      qhi = qincr
      x = small
      RETURN

  230 CONTINUE
  240 CALL dstzr(xlb,xub,abstol,reltol)
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c
c     IF WE REACH HERE, XLB AND XUB BOUND THE ZERO OF F.
c
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      status = 0
      GO TO 260

  250 IF (.NOT. (status.EQ.1)) GO TO 290
  260 CALL dzror(status,x,fx,xlo,xhi,qdum1,qdum2)
      IF (.NOT. (status.EQ.1)) GO TO 280
c     GET-FUNCTION-VALUE
      ASSIGN 270 TO i99999
      GO TO 300

  270 CONTINUE
  280 GO TO 250

  290 x = xlo
      status = 0
      RETURN

      ENTRY dstinv(zsmall,zbig,zabsst,zrelst,zstpmu,zabsto,zrelto)
c**********************************************************************
c
c      SUBROUTINE DSTINV( SMALL, BIG, ABSSTP, RELSTP, STPMUL,
c     +                   ABSTOL, RELTOL )
c      Double Precision - SeT INverse finder - Reverse Communication
c
c
c                              Function
c
c
c     Concise Description - Given a monotone function F finds X
c     such that F(X) = Y.  Uses Reverse communication -- see invr.
c     This routine sets quantities needed by INVR.
c
c          More Precise Description of INVR -
c
c     F must be a monotone function, the results of QMFINV are
c     otherwise undefined.  QINCR must be .TRUE. if F is non-
c     decreasing and .FALSE. if F is non-increasing.
c
c     QMFINV will return .TRUE. if and only if F(SMALL) and
c     F(BIG) bracket Y, i. e.,
c          QINCR is .TRUE. and F(SMALL).LE.Y.LE.F(BIG) or
c          QINCR is .FALSE. and F(BIG).LE.Y.LE.F(SMALL)
c
c     if QMFINV returns .TRUE., then the X returned satisfies
c     the following condition.  let
c               TOL(X) = MAX(ABSTOL,RELTOL*ABS(X))
c     then if QINCR is .TRUE.,
c          F(X-TOL(X)) .LE. Y .LE. F(X+TOL(X))
c     and if QINCR is .FALSE.
c          F(X-TOL(X)) .GE. Y .GE. F(X+TOL(X))
c
c
c                              Arguments
c
c
c     SMALL --> The left endpoint of the interval to be
c          searched for a solution.
c                    SMALL is DOUBLE PRECISION
c
c     BIG --> The right endpoint of the interval to be
c          searched for a solution.
c                    BIG is DOUBLE PRECISION
c
c     ABSSTP, RELSTP --> The initial step size in the search
c          is MAX(ABSSTP,RELSTP*ABS(X)). See algorithm.
c                    ABSSTP is DOUBLE PRECISION
c                    RELSTP is DOUBLE PRECISION
c
c     STPMUL --> When a step doesn't bound the zero, the step
c                size is multiplied by STPMUL and another step
c                taken.  A popular value is 2.0
c                    DOUBLE PRECISION STPMUL
c
c     ABSTOL, RELTOL --> Two numbers that determine the accuracy
c          of the solution.  See function for a precise definition.
c                    ABSTOL is DOUBLE PRECISION
c                    RELTOL is DOUBLE PRECISION
c
c
c                              Method
c
c
c     Compares F(X) with Y for the input value of X then uses QINCR
c     to determine whether to step left or right to bound the
c     desired x.  the initial step size is
c          MAX(ABSSTP,RELSTP*ABS(S)) for the input value of X.
c     Iteratively steps right or left until it bounds X.
c     At each step which doesn't bound X, the step size is doubled.
c     The routine is careful never to step beyond SMALL or BIG.  If
c     it hasn't bounded X at SMALL or BIG, QMFINV returns .FALSE.
c     after setting QLEFT and QHI.
c
c     If X is successfully bounded then Algorithm R of the paper
c     'Two Efficient Algorithms with Guaranteed Convergence for
c     Finding a Zero of a Function' by J. C. P. Bus and
c     T. J. Dekker in ACM Transactions on Mathematical
c     Software, Volume 1, No. 4 page 330 (DEC. '75) is employed
c     to find the zero of the function F(X)-Y. This is routine
c     QRZERO.
c
c**********************************************************************
      small = zsmall
      big = zbig
      absstp = zabsst
      relstp = zrelst
      stpmul = zstpmu
      abstol = zabsto
      reltol = zrelto
      RETURN

cc    STOP '*** EXECUTION FLOWING INTO FLECS PROCEDURES ***'
c     TO GET-FUNCTION-VALUE
  300 status = 1
      RETURN

  310 CONTINUE
      GO TO i99999
      END
c
c
      SUBROUTINE dzror(status,x,fx,xlo,xhi,qleft,qhi)
c**********************************************************************
c
c     SUBROUTINE DZROR(STATUS, X, FX, XLO, XHI, QLEFT, QHI)
c     Double precision ZeRo of a function -- Reverse Communication
c
c
c                              Function
c
c
c     Performs the zero finding.  STZROR must have been called before
c     this routine in order to set its parameters.
c
c
c                              Arguments
c
c
c     STATUS <--> At the beginning of a zero finding problem, STATUS
c                 should be set to 0 and ZROR invoked.  (The value
c                 of other parameters will be ignored on this call.)
c
c                 When ZROR needs the function evaluated, it will set
c                 STATUS to 1 and return.  The value of the function
c                 should be set in FX and ZROR again called without
c                 changing any of its other parameters.
c
c                 When ZROR has finished without error, it will return
c                 with STATUS 0.  In that case (XLO,XHI) bound the answe
c
c                 If ZROR finds an error (which implies that F(XLO)-Y an
c                 F(XHI)-Y have the same sign, it returns STATUS -1.  In
c                 this case, XLO and XHI are undefined.
c                         INTEGER STATUS
c
c     X <-- The value of X at which F(X) is to be evaluated.
c                         DOUBLE PRECISION X
c
c     FX --> The value of F(X) calculated when ZROR returns with
c            STATUS = 1.
c                         DOUBLE PRECISION FX
c
c     XLO <-- When ZROR returns with STATUS = 0, XLO bounds the
c             inverval in X containing the solution below.
c                         DOUBLE PRECISION XLO
c
c     XHI <-- When ZROR returns with STATUS = 0, XHI bounds the
c             inverval in X containing the solution above.
c                         DOUBLE PRECISION XHI
c
c     QLEFT <-- .TRUE. if the stepping search terminated unsucessfully
c                at XLO.  If it is .FALSE. the search terminated
c                unsucessfully at XHI.
c                    QLEFT is LOGICAL
c
c     QHI <-- .TRUE. if F(X) .GT. Y at the termination of the
c              search and .FALSE. if F(X) .LT. Y at the
c              termination of the search.
c                    QHI is LOGICAL
c
c**********************************************************************
c     .. Scalar Arguments ..
      DOUBLE PRECISION fx,x,xhi,xlo,zabstl,zreltl,zxhi,zxlo
      INTEGER status
      LOGICAL qhi,qleft
c     ..
c     .. Save statement ..
      SAVE
c     ..
c     .. Local Scalars ..
      DOUBLE PRECISION a,abstol,b,c,d,fa,fb,fc,fd,fda,fdb,m,mb,p,q,
     +                 reltol,tol,w,xxhi,xxlo,zx
      INTEGER ext,i99999
      LOGICAL first,qrzero
c     ..
c     .. Intrinsic Functions ..
      INTRINSIC abs,max,sign
c     ..
c     .. Statement Functions ..
      DOUBLE PRECISION ftol
c     ..
c     .. Statement Function definitions ..
      ftol(zx) = 0.5D0*max(abstol,reltol*abs(zx))
c     ..
c     .. Executable Statements ..

      IF (status.GT.0) GO TO 280
      xlo = xxlo
      xhi = xxhi
      b = xlo
      x = xlo
c     GET-FUNCTION-VALUE
      ASSIGN 10 TO i99999
      GO TO 270

   10 fb = fx
      xlo = xhi
      a = xlo
      x = xlo
c     GET-FUNCTION-VALUE
      ASSIGN 20 TO i99999
      GO TO 270
c
c     Check that F(ZXLO) < 0 < F(ZXHI)  or
c                F(ZXLO) > 0 > F(ZXHI)
c
   20 IF (.NOT. (fb.LT.0.0D0)) GO TO 40
      IF (.NOT. (fx.LT.0.0D0)) GO TO 30
      status = -1
      qleft = fx .LT. fb
      qhi = .FALSE.
      RETURN

   30 CONTINUE
   40 IF (.NOT. (fb.GT.0.0D0)) GO TO 60
      IF (.NOT. (fx.GT.0.0D0)) GO TO 50
      status = -1
      qleft = fx .GT. fb
      qhi = .TRUE.
      RETURN

   50 CONTINUE
   60 fa = fx
c
      first = .TRUE.
   70 c = a
      fc = fa
      ext = 0
   80 IF (.NOT. (abs(fc).LT.abs(fb))) GO TO 100
      IF (.NOT. (c.NE.a)) GO TO 90
      d = a
      fd = fa
   90 a = b
      fa = fb
      xlo = c
      b = xlo
      fb = fc
      c = a
      fc = fa
  100 tol = ftol(xlo)
      m = (c+b)*.5D0
      mb = m - b
      IF (.NOT. (abs(mb).GT.tol)) GO TO 240
      IF (.NOT. (ext.GT.3)) GO TO 110
      w = mb
      GO TO 190

  110 tol = sign(tol,mb)
      p = (b-a)*fb
      IF (.NOT. (first)) GO TO 120
      q = fa - fb
      first = .FALSE.
      GO TO 130

  120 fdb = (fd-fb)/ (d-b)
      fda = (fd-fa)/ (d-a)
      p = fda*p
      q = fdb*fa - fda*fb
  130 IF (.NOT. (p.LT.0.0D0)) GO TO 140
      p = -p
      q = -q
  140 IF (ext.EQ.3) p = p*2.0D0
      IF (.NOT. ((p*1.0D0).EQ.0.0D0.OR.p.LE. (q*tol))) GO TO 150
      w = tol
      GO TO 180

  150 IF (.NOT. (p.LT. (mb*q))) GO TO 160
      w = p/q
      GO TO 170

  160 w = mb
  170 CONTINUE
  180 CONTINUE
  190 d = a
      fd = fa
      a = b
      fa = fb
      b = b + w
      xlo = b
      x = xlo
c     GET-FUNCTION-VALUE
      ASSIGN 200 TO i99999
      GO TO 270

  200 fb = fx
      IF (.NOT. ((fc*fb).GE.0.0D0)) GO TO 210
      GO TO 70

  210 IF (.NOT. (w.EQ.mb)) GO TO 220
      ext = 0
      GO TO 230

  220 ext = ext + 1
  230 GO TO 80

  240 xhi = c
      qrzero = (fc.GE.0.0D0 .AND. fb.LE.0.0D0) .OR.
     +         (fc.LT.0.0D0 .AND. fb.GE.0.0D0)
      IF (.NOT. (qrzero)) GO TO 250
      status = 0
      GO TO 260

  250 status = -1
  260 RETURN

      ENTRY dstzr(zxlo,zxhi,zabstl,zreltl)
c**********************************************************************
c
c     SUBROUTINE DSTZR( XLO, XHI, ABSTOL, RELTOL )
c     Double precision SeT ZeRo finder - Reverse communication version
c
c
c                              Function
c
c
c
c     Sets quantities needed by ZROR.  The function of ZROR
c     and the quantities set is given here.
c
c     Concise Description - Given a function F
c     find XLO such that F(XLO) = 0.
c
c          More Precise Description -
c
c     Input condition. F is a double precision function of a single
c     double precision argument and XLO and XHI are such that
c          F(XLO)*F(XHI)  .LE.  0.0
c
c     If the input condition is met, QRZERO returns .TRUE.
c     and output values of XLO and XHI satisfy the following
c          F(XLO)*F(XHI)  .LE. 0.
c          ABS(F(XLO)  .LE. ABS(F(XHI)
c          ABS(XLO-XHI)  .LE. TOL(X)
c     where
c          TOL(X) = MAX(ABSTOL,RELTOL*ABS(X))
c
c     If this algorithm does not find XLO and XHI satisfying
c     these conditions then QRZERO returns .FALSE.  This
c     implies that the input condition was not met.
c
c
c                              Arguments
c
c
c     XLO --> The left endpoint of the interval to be
c           searched for a solution.
c                    XLO is DOUBLE PRECISION
c
c     XHI --> The right endpoint of the interval to be
c           for a solution.
c                    XHI is DOUBLE PRECISION
c
c     ABSTOL, RELTOL --> Two numbers that determine the accuracy
c                      of the solution.  See function for a
c                      precise definition.
c                    ABSTOL is DOUBLE PRECISION
c                    RELTOL is DOUBLE PRECISION
c
c
c                              Method
c
c
c     Algorithm R of the paper 'Two Efficient Algorithms with
c     Guaranteed Convergence for Finding a Zero of a Function'
c     by J. C. P. Bus and T. J. Dekker in ACM Transactions on
c     Mathematical Software, Volume 1, no. 4 page 330
c     (Dec. '75) is employed to find the zero of F(X)-Y.
c
c**********************************************************************
      xxlo = zxlo
      xxhi = zxhi
      abstol = zabstl
      reltol = zreltl
      RETURN

cc    STOP '*** EXECUTION FLOWING INTO FLECS PROCEDURES ***'
c     TO GET-FUNCTION-VALUE
  270 status = 1
      RETURN

  280 CONTINUE
      GO TO i99999
      END
c
c
      DOUBLE PRECISION FUNCTION erf(x)
c-----------------------------------------------------------------------
c             EVALUATION OF THE REAL ERROR FUNCTION
c-----------------------------------------------------------------------
c     .. Scalar Arguments ..
      DOUBLE PRECISION x
c     ..
c     .. Local Scalars ..
      DOUBLE PRECISION ax,bot,c,t,top,x2
c     ..
c     .. Local Arrays ..
      DOUBLE PRECISION a(5),b(3),p(8),q(8),r(5),s(4)
c     ..
c     .. Intrinsic Functions ..
      INTRINSIC abs,exp,sign
c     ..
c     .. Data statements ..
c-------------------------
c-------------------------
c-------------------------
c-------------------------
      DATA c/.564189583547756D0/
      DATA a(1)/.771058495001320D-04/,a(2)/-.133733772997339D-02/,
     +     a(3)/.323076579225834D-01/,a(4)/.479137145607681D-01/,
     +     a(5)/.128379167095513D+00/
      DATA b(1)/.301048631703895D-02/,b(2)/.538971687740286D-01/,
     +     b(3)/.375795757275549D+00/
      DATA p(1)/-1.36864857382717D-07/,p(2)/5.64195517478974D-01/,
     +     p(3)/7.21175825088309D+00/,p(4)/4.31622272220567D+01/,
     +     p(5)/1.52989285046940D+02/,p(6)/3.39320816734344D+02/,
     +     p(7)/4.51918953711873D+02/,p(8)/3.00459261020162D+02/
      DATA q(1)/1.00000000000000D+00/,q(2)/1.27827273196294D+01/,
     +     q(3)/7.70001529352295D+01/,q(4)/2.77585444743988D+02/,
     +     q(5)/6.38980264465631D+02/,q(6)/9.31354094850610D+02/,
     +     q(7)/7.90950925327898D+02/,q(8)/3.00459260956983D+02/
      DATA r(1)/2.10144126479064D+00/,r(2)/2.62370141675169D+01/,
     +     r(3)/2.13688200555087D+01/,r(4)/4.65807828718470D+00/,
     +     r(5)/2.82094791773523D-01/
      DATA s(1)/9.41537750555460D+01/,s(2)/1.87114811799590D+02/,
     +     s(3)/9.90191814623914D+01/,s(4)/1.80124575948747D+01/
c     ..
c     .. Executable Statements ..
c-------------------------
      ax = abs(x)
      IF (ax.GT.0.5D0) GO TO 10
      t = x*x
      top = ((((a(1)*t+a(2))*t+a(3))*t+a(4))*t+a(5)) + 1.0D0
      bot = ((b(1)*t+b(2))*t+b(3))*t + 1.0D0
      erf = x* (top/bot)
      RETURN
c
   10 IF (ax.GT.4.0D0) GO TO 20
      top = ((((((p(1)*ax+p(2))*ax+p(3))*ax+p(4))*ax+p(5))*ax+p(6))*ax+
     +      p(7))*ax + p(8)
      bot = ((((((q(1)*ax+q(2))*ax+q(3))*ax+q(4))*ax+q(5))*ax+q(6))*ax+
     +      q(7))*ax + q(8)
      erf = 0.5D0 + (0.5D0-exp(-x*x)*top/bot)
      IF (x.LT.0.0D0) erf = -erf
      RETURN
c
   20 IF (ax.GE.5.8D0) GO TO 30
      x2 = x*x
      t = 1.0D0/x2
      top = (((r(1)*t+r(2))*t+r(3))*t+r(4))*t + r(5)
      bot = (((s(1)*t+s(2))*t+s(3))*t+s(4))*t + 1.0D0
      erf = (c-top/ (x2*bot))/ax
      erf = 0.5D0 + (0.5D0-exp(-x2)*erf)
      IF (x.LT.0.0D0) erf = -erf
      RETURN
c
   30 erf = sign(1.0D0,x)
      RETURN
      END
c
c
      DOUBLE PRECISION FUNCTION erfc1(ind,x)
c-----------------------------------------------------------------------
c         EVALUATION OF THE COMPLEMENTARY ERROR FUNCTION
c
c          ERFC1(IND,X) = ERFC(X)            IF IND = 0
c          ERFC1(IND,X) = EXP(X*X)*ERFC(X)   OTHERWISE
c-----------------------------------------------------------------------
c     .. Scalar Arguments ..
      DOUBLE PRECISION x
      INTEGER ind
c     ..
c     .. Local Scalars ..
      DOUBLE PRECISION ax,bot,c,e,t,top,w
c     ..
c     .. Local Arrays ..
      DOUBLE PRECISION a(5),b(3),p(8),q(8),r(5),s(4)
c     ..
c     .. External Functions ..
      DOUBLE PRECISION exparg
      EXTERNAL exparg
c     ..
c     .. Intrinsic Functions ..
      INTRINSIC abs,dble,exp
c     ..
c     .. Data statements ..
c-------------------------
c-------------------------
c-------------------------
c-------------------------
      DATA c/.564189583547756D0/
      DATA a(1)/.771058495001320D-04/,a(2)/-.133733772997339D-02/,
     +     a(3)/.323076579225834D-01/,a(4)/.479137145607681D-01/,
     +     a(5)/.128379167095513D+00/
      DATA b(1)/.301048631703895D-02/,b(2)/.538971687740286D-01/,
     +     b(3)/.375795757275549D+00/
      DATA p(1)/-1.36864857382717D-07/,p(2)/5.64195517478974D-01/,
     +     p(3)/7.21175825088309D+00/,p(4)/4.31622272220567D+01/,
     +     p(5)/1.52989285046940D+02/,p(6)/3.39320816734344D+02/,
     +     p(7)/4.51918953711873D+02/,p(8)/3.00459261020162D+02/
      DATA q(1)/1.00000000000000D+00/,q(2)/1.27827273196294D+01/,
     +     q(3)/7.70001529352295D+01/,q(4)/2.77585444743988D+02/,
     +     q(5)/6.38980264465631D+02/,q(6)/9.31354094850610D+02/,
     +     q(7)/7.90950925327898D+02/,q(8)/3.00459260956983D+02/
      DATA r(1)/2.10144126479064D+00/,r(2)/2.62370141675169D+01/,
     +     r(3)/2.13688200555087D+01/,r(4)/4.65807828718470D+00/,
     +     r(5)/2.82094791773523D-01/
      DATA s(1)/9.41537750555460D+01/,s(2)/1.87114811799590D+02/,
     +     s(3)/9.90191814623914D+01/,s(4)/1.80124575948747D+01/
c     ..
c     .. Executable Statements ..
c-------------------------
c
c                     ABS(X) .LE. 0.5
c
      ax = abs(x)
      IF (ax.GT.0.5D0) GO TO 10
      t = x*x
      top = ((((a(1)*t+a(2))*t+a(3))*t+a(4))*t+a(5)) + 1.0D0
      bot = ((b(1)*t+b(2))*t+b(3))*t + 1.0D0
      erfc1 = 0.5D0 + (0.5D0-x* (top/bot))
      IF (ind.NE.0) erfc1 = exp(t)*erfc1
      RETURN
c
c                  0.5 .LT. ABS(X) .LE. 4
c
   10 IF (ax.GT.4.0D0) GO TO 20
      top = ((((((p(1)*ax+p(2))*ax+p(3))*ax+p(4))*ax+p(5))*ax+p(6))*ax+
     +      p(7))*ax + p(8)
      bot = ((((((q(1)*ax+q(2))*ax+q(3))*ax+q(4))*ax+q(5))*ax+q(6))*ax+
     +      q(7))*ax + q(8)
      erfc1 = top/bot
      GO TO 40
c
c                      ABS(X) .GT. 4
c
   20 IF (x.LE.-5.6D0) GO TO 60
      IF (ind.NE.0) GO TO 30
      IF (x.GT.100.0D0) GO TO 70
      IF (x*x.GT.-exparg(1)) GO TO 70
c
   30 t = (1.0D0/x)**2
      top = (((r(1)*t+r(2))*t+r(3))*t+r(4))*t + r(5)
      bot = (((s(1)*t+s(2))*t+s(3))*t+s(4))*t + 1.0D0
      erfc1 = (c-t*top/bot)/ax
c
c                      FINAL ASSEMBLY
c
   40 IF (ind.EQ.0) GO TO 50
      IF (x.LT.0.0D0) erfc1 = 2.0D0*exp(x*x) - erfc1
      RETURN

   50 w = dble(x)*dble(x)
      t = w
      e = w - dble(t)
      erfc1 = ((0.5D0+ (0.5D0-e))*exp(-t))*erfc1
      IF (x.LT.0.0D0) erfc1 = 2.0D0 - erfc1
      RETURN
c
c             LIMIT VALUE FOR LARGE NEGATIVE X
c
   60 erfc1 = 2.0D0
      IF (ind.NE.0) erfc1 = 2.0D0*exp(x*x)
      RETURN
c
c             LIMIT VALUE FOR LARGE POSITIVE X
c                       WHEN IND = 0
c
   70 erfc1 = 0.0D0
      RETURN
      END
c
c
      DOUBLE PRECISION FUNCTION exparg(l)
c--------------------------------------------------------------------
c     IF L = 0 THEN  EXPARG(L) = THE LARGEST POSITIVE W FOR WHICH
c     EXP(W) CAN BE COMPUTED.
c
c     IF L IS NONZERO THEN  EXPARG(L) = THE LARGEST NEGATIVE W FOR
c     WHICH THE COMPUTED VALUE OF EXP(W) IS NONZERO.
c
c     NOTE... ONLY AN APPROXIMATE VALUE FOR EXPARG(L) IS NEEDED.
c--------------------------------------------------------------------
c     .. Scalar Arguments ..
      INTEGER l
c     ..
c     .. Local Scalars ..
      DOUBLE PRECISION lnb
      INTEGER b,m
c     ..
c     .. External Functions ..
      INTEGER ipmpar
      EXTERNAL ipmpar
c     ..
c     .. Intrinsic Functions ..
      INTRINSIC dble,dlog
c     ..
c     .. Executable Statements ..
c
      b = ipmpar(4)
      IF (b.NE.2) GO TO 10
      lnb = .69314718055995D0
      GO TO 40

   10 IF (b.NE.8) GO TO 20
      lnb = 2.0794415416798D0
      GO TO 40

   20 IF (b.NE.16) GO TO 30
      lnb = 2.7725887222398D0
      GO TO 40

   30 lnb = dlog(dble(b))
c
   40 IF (l.EQ.0) GO TO 50
      m = ipmpar(9) - 1
      exparg = 0.99999D0* (m*lnb)
      RETURN

   50 m = ipmpar(10)
      exparg = 0.99999D0* (m*lnb)
      RETURN
      END
c
c
      DOUBLE PRECISION FUNCTION gam1(a)
c     ------------------------------------------------------------------
c     COMPUTATION OF 1/GAMMA(A+1) - 1  FOR -0.5 .LE. A .LE. 1.5
c     ------------------------------------------------------------------
c     .. Scalar Arguments ..
      DOUBLE PRECISION a
c     ..
c     .. Local Scalars ..
      DOUBLE PRECISION bot,d,s1,s2,t,top,w
c     ..
c     .. Local Arrays ..
      DOUBLE PRECISION p(7),q(5),r(9)
c     ..
c     .. Data statements ..
c     -------------------
c     -------------------
c     -------------------
c     -------------------
      DATA p(1)/.577215664901533D+00/,p(2)/-.409078193005776D+00/,
     +     p(3)/-.230975380857675D+00/,p(4)/.597275330452234D-01/,
     +     p(5)/.766968181649490D-02/,p(6)/-.514889771323592D-02/,
     +     p(7)/.589597428611429D-03/
      DATA q(1)/.100000000000000D+01/,q(2)/.427569613095214D+00/,
     +     q(3)/.158451672430138D+00/,q(4)/.261132021441447D-01/,
     +     q(5)/.423244297896961D-02/
      DATA r(1)/-.422784335098468D+00/,r(2)/-.771330383816272D+00/,
     +     r(3)/-.244757765222226D+00/,r(4)/.118378989872749D+00/,
     +     r(5)/.930357293360349D-03/,r(6)/-.118290993445146D-01/,
     +     r(7)/.223047661158249D-02/,r(8)/.266505979058923D-03/,
     +     r(9)/-.132674909766242D-03/
      DATA s1/.273076135303957D+00/,s2/.559398236957378D-01/
c     ..
c     .. Executable Statements ..
c     -------------------
      t = a
      d = a - 0.5D0
      IF (d.GT.0.0D0) t = d - 0.5D0
      IF (t) 40,10,20
c
   10 gam1 = 0.0D0
      RETURN
c
   20 top = (((((p(7)*t+p(6))*t+p(5))*t+p(4))*t+p(3))*t+p(2))*t + p(1)
      bot = (((q(5)*t+q(4))*t+q(3))*t+q(2))*t + 1.0D0
      w = top/bot
      IF (d.GT.0.0D0) GO TO 30
      gam1 = a*w
      RETURN

   30 gam1 = (t/a)* ((w-0.5D0)-0.5D0)
      RETURN
c
   40 top = (((((((r(9)*t+r(8))*t+r(7))*t+r(6))*t+r(5))*t+r(4))*t+r(3))*
     +      t+r(2))*t + r(1)
      bot = (s2*t+s1)*t + 1.0D0
      w = top/bot
      IF (d.GT.0.0D0) GO TO 50
      gam1 = a* ((w+0.5D0)+0.5D0)
      RETURN

   50 gam1 = t*w/a
      RETURN
      END
c
c
      DOUBLE PRECISION FUNCTION gamma(a)
c-----------------------------------------------------------------------
c
c         EVALUATION OF THE GAMMA FUNCTION FOR REAL ARGUMENTS
c
c                           -----------
c
c     GAMMA(A) IS ASSIGNED THE VALUE 0 WHEN THE GAMMA FUNCTION CANNOT
c     BE COMPUTED.
c
c-----------------------------------------------------------------------
c     WRITTEN BY ALFRED H. MORRIS, JR.
c          NAVAL SURFACE WEAPONS CENTER
c          DAHLGREN, VIRGINIA
c-----------------------------------------------------------------------
c     .. Scalar Arguments ..
      DOUBLE PRECISION a
c     ..
c     .. Local Scalars ..
      DOUBLE PRECISION bot,d,g,lnx,pi,r1,r2,r3,r4,r5,s,t,top,w,x,z
      INTEGER i,j,m,n
c     ..
c     .. Local Arrays ..
      DOUBLE PRECISION p(7),q(7)
c     ..
c     .. External Functions ..
      DOUBLE PRECISION exparg,spmpar
      EXTERNAL exparg,spmpar
c     ..
c     .. Intrinsic Functions ..
      INTRINSIC abs,dble,dlog,exp,int,mod,sin
c     ..
c     .. Data statements ..
c--------------------------
c     D = 0.5*(LN(2*PI) - 1)
c--------------------------
c--------------------------
c--------------------------
      DATA pi/3.1415926535898D0/
      DATA d/.41893853320467274178D0/
      DATA p(1)/.539637273585445D-03/,p(2)/.261939260042690D-02/,
     +     p(3)/.204493667594920D-01/,p(4)/.730981088720487D-01/,
     +     p(5)/.279648642639792D+00/,p(6)/.553413866010467D+00/,
     +     p(7)/1.0D0/
      DATA q(1)/-.832979206704073D-03/,q(2)/.470059485860584D-02/,
     +     q(3)/.225211131035340D-01/,q(4)/-.170458969313360D+00/,
     +     q(5)/-.567902761974940D-01/,q(6)/.113062953091122D+01/,
     +     q(7)/1.0D0/
      DATA r1/.820756370353826D-03/,r2/-.595156336428591D-03/,
     +     r3/.793650663183693D-03/,r4/-.277777777770481D-02/,
     +     r5/.833333333333333D-01/
c     ..
c     .. Executable Statements ..
c--------------------------
      gamma = 0.0D0
      x = a
      IF (abs(a).GE.15.0D0) GO TO 110
c-----------------------------------------------------------------------
c            EVALUATION OF GAMMA(A) FOR ABS(A) .LT. 15
c-----------------------------------------------------------------------
      t = 1.0D0
      m = int(a) - 1
c
c     LET T BE THE PRODUCT OF A-J WHEN A .GE. 2
c
      IF (m) 40,30,10
   10 DO 20 j = 1,m
          x = x - 1.0D0
          t = x*t
   20 CONTINUE
   30 x = x - 1.0D0
      GO TO 80
c
c     LET T BE THE PRODUCT OF A+J WHEN A .LT. 1
c
   40 t = a
      IF (a.GT.0.0D0) GO TO 70
      m = -m - 1
      IF (m.EQ.0) GO TO 60
      DO 50 j = 1,m
          x = x + 1.0D0
          t = x*t
   50 CONTINUE
   60 x = (x+0.5D0) + 0.5D0
      t = x*t
      IF (t.EQ.0.0D0) RETURN
c
   70 CONTINUE
c
c     THE FOLLOWING CODE CHECKS IF 1/T CAN OVERFLOW. THIS
c     CODE MAY BE OMITTED IF DESIRED.
c
      IF (abs(t).GE.1.D-30) GO TO 80
      IF (abs(t)*spmpar(3).LE.1.0001D0) RETURN
      gamma = 1.0D0/t
      RETURN
c
c     COMPUTE GAMMA(1 + X) FOR  0 .LE. X .LT. 1
c
   80 top = p(1)
      bot = q(1)
      DO 90 i = 2,7
          top = p(i) + x*top
          bot = q(i) + x*bot
   90 CONTINUE
      gamma = top/bot
c
c     TERMINATION
c
      IF (a.LT.1.0D0) GO TO 100
      gamma = gamma*t
      RETURN

  100 gamma = gamma/t
      RETURN
c-----------------------------------------------------------------------
c            EVALUATION OF GAMMA(A) FOR ABS(A) .GE. 15
c-----------------------------------------------------------------------
  110 IF (abs(a).GE.1.D3) RETURN
      IF (a.GT.0.0D0) GO TO 120
      x = -a
      n = x
      t = x - n
      IF (t.GT.0.9D0) t = 1.0D0 - t
      s = sin(pi*t)/pi
      IF (mod(n,2).EQ.0) s = -s
      IF (s.EQ.0.0D0) RETURN
c
c     COMPUTE THE MODIFIED ASYMPTOTIC SUM
c
  120 t = 1.0D0/ (x*x)
      g = ((((r1*t+r2)*t+r3)*t+r4)*t+r5)/x
c
c     ONE MAY REPLACE THE NEXT STATEMENT WITH  LNX = ALOG(X)
c     BUT LESS ACCURACY WILL NORMALLY BE OBTAINED.
c
      lnx = dlog(x)
c
c     FINAL ASSEMBLY
c
      z = x
      g = (d+g) + (z-0.5D0)* (lnx-1.D0)
      w = g
      t = g - dble(w)
      IF (w.GT.0.99999D0*exparg(0)) RETURN
      gamma = exp(w)* (1.0D0+t)
      IF (a.LT.0.0D0) gamma = (1.0D0/ (gamma*s))/x
      RETURN
      END
c
c
      SUBROUTINE gratio(a,x,ans,qans,ind)
c ----------------------------------------------------------------------
c        EVALUATION OF THE INCOMPLETE GAMMA RATIO FUNCTIONS
c                      P(A,X) AND Q(A,X)
c
c                        ----------
c
c     IT IS ASSUMED THAT A AND X ARE NONNEGATIVE, WHERE A AND X
c     ARE NOT BOTH 0.
c
c     ANS AND QANS ARE VARIABLES. GRATIO ASSIGNS ANS THE VALUE
c     P(A,X) AND QANS THE VALUE Q(A,X). IND MAY BE ANY INTEGER.
c     IF IND = 0 THEN THE USER IS REQUESTING AS MUCH ACCURACY AS
c     POSSIBLE (UP TO 14 SIGNIFICANT DIGITS). OTHERWISE, IF
c     IND = 1 THEN ACCURACY IS REQUESTED TO WITHIN 1 UNIT OF THE
c     6-TH SIGNIFICANT DIGIT, AND IF IND .NE. 0,1 THEN ACCURACY
c     IS REQUESTED TO WITHIN 1 UNIT OF THE 3RD SIGNIFICANT DIGIT.
c
c     ERROR RETURN ...
c        ANS IS ASSIGNED THE VALUE 2 WHEN A OR X IS NEGATIVE,
c     WHEN A*X = 0, OR WHEN P(A,X) AND Q(A,X) ARE INDETERMINANT.
c     P(A,X) AND Q(A,X) ARE COMPUTATIONALLY INDETERMINANT WHEN
c     X IS EXCEEDINGLY CLOSE TO A AND A IS EXTREMELY LARGE.
c ----------------------------------------------------------------------
c     WRITTEN BY ALFRED H. MORRIS, JR.
c        NAVAL SURFACE WEAPONS CENTER
c        DAHLGREN, VIRGINIA
c     --------------------
c     .. Scalar Arguments ..
      DOUBLE PRECISION a,ans,qans,x
      INTEGER ind
c     ..
c     .. Local Scalars ..
      DOUBLE PRECISION a2n,a2nm1,acc,alog10,am0,amn,an,an0,apn,b2n,
     +                 b2nm1,c,c0,c1,c2,c3,c4,c5,c6,cma,d10,d20,d30,d40,
     +                 d50,d60,d70,e,e0,g,h,j,l,r,rt2pin,rta,rtpi,rtx,s,
     +                 sum,t,t1,third,tol,twoa,u,w,x0,y,z
      INTEGER i,iop,m,max,n
c     ..
c     .. Local Arrays ..
      DOUBLE PRECISION acc0(3),big(3),d0(13),d1(12),d2(10),d3(8),d4(6),
     +                 d5(4),d6(2),e00(3),wk(20),x00(3)
c     ..
c     .. External Functions ..
      DOUBLE PRECISION erf,erfc1,gam1,gamma,rexp,rlog,spmpar
      EXTERNAL erf,erfc1,gam1,gamma,rexp,rlog,spmpar
c     ..
c     .. Intrinsic Functions ..
      INTRINSIC abs,dble,dlog,dmax1,exp,int,sqrt
c     ..
c     .. Data statements ..
c     --------------------
c     --------------------
c     ALOG10 = LN(10)
c     RT2PIN = 1/SQRT(2*PI)
c     RTPI   = SQRT(PI)
c     --------------------
c     --------------------
c     --------------------
c     --------------------
c     --------------------
c     --------------------
c     --------------------
c     --------------------
c     --------------------
      DATA acc0(1)/5.D-15/,acc0(2)/5.D-7/,acc0(3)/5.D-4/
      DATA big(1)/20.0D0/,big(2)/14.0D0/,big(3)/10.0D0/
      DATA e00(1)/.25D-3/,e00(2)/.25D-1/,e00(3)/.14D0/
      DATA x00(1)/31.0D0/,x00(2)/17.0D0/,x00(3)/9.7D0/
      DATA alog10/2.30258509299405D0/
      DATA rt2pin/.398942280401433D0/
      DATA rtpi/1.77245385090552D0/
      DATA third/.333333333333333D0/
      DATA d0(1)/.833333333333333D-01/,d0(2)/-.148148148148148D-01/,
     +     d0(3)/.115740740740741D-02/,d0(4)/.352733686067019D-03/,
     +     d0(5)/-.178755144032922D-03/,d0(6)/.391926317852244D-04/,
     +     d0(7)/-.218544851067999D-05/,d0(8)/-.185406221071516D-05/,
     +     d0(9)/.829671134095309D-06/,d0(10)/-.176659527368261D-06/,
     +     d0(11)/.670785354340150D-08/,d0(12)/.102618097842403D-07/,
     +     d0(13)/-.438203601845335D-08/
      DATA d10/-.185185185185185D-02/,d1(1)/-.347222222222222D-02/,
     +     d1(2)/.264550264550265D-02/,d1(3)/-.990226337448560D-03/,
     +     d1(4)/.205761316872428D-03/,d1(5)/-.401877572016461D-06/,
     +     d1(6)/-.180985503344900D-04/,d1(7)/.764916091608111D-05/,
     +     d1(8)/-.161209008945634D-05/,d1(9)/.464712780280743D-08/,
     +     d1(10)/.137863344691572D-06/,d1(11)/-.575254560351770D-07/,
     +     d1(12)/.119516285997781D-07/
      DATA d20/.413359788359788D-02/,d2(1)/-.268132716049383D-02/,
     +     d2(2)/.771604938271605D-03/,d2(3)/.200938786008230D-05/,
     +     d2(4)/-.107366532263652D-03/,d2(5)/.529234488291201D-04/,
     +     d2(6)/-.127606351886187D-04/,d2(7)/.342357873409614D-07/,
     +     d2(8)/.137219573090629D-05/,d2(9)/-.629899213838006D-06/,
     +     d2(10)/.142806142060642D-06/
      DATA d30/.649434156378601D-03/,d3(1)/.229472093621399D-03/,
     +     d3(2)/-.469189494395256D-03/,d3(3)/.267720632062839D-03/,
     +     d3(4)/-.756180167188398D-04/,d3(5)/-.239650511386730D-06/,
     +     d3(6)/.110826541153473D-04/,d3(7)/-.567495282699160D-05/,
     +     d3(8)/.142309007324359D-05/
      DATA d40/-.861888290916712D-03/,d4(1)/.784039221720067D-03/,
     +     d4(2)/-.299072480303190D-03/,d4(3)/-.146384525788434D-05/,
     +     d4(4)/.664149821546512D-04/,d4(5)/-.396836504717943D-04/,
     +     d4(6)/.113757269706784D-04/
      DATA d50/-.336798553366358D-03/,d5(1)/-.697281375836586D-04/,
     +     d5(2)/.277275324495939D-03/,d5(3)/-.199325705161888D-03/,
     +     d5(4)/.679778047793721D-04/
      DATA d60/.531307936463992D-03/,d6(1)/-.592166437353694D-03/,
     +     d6(2)/.270878209671804D-03/
      DATA d70/.344367606892378D-03/
c     ..
c     .. Executable Statements ..
c     --------------------
c     ****** E IS A MACHINE DEPENDENT CONSTANT. E IS THE SMALLEST
c            FLOATING POINT NUMBER FOR WHICH 1.0 + E .GT. 1.0 .
c
      e = spmpar(1)
c
c     --------------------
      IF (a.LT.0.0D0 .OR. x.LT.0.0D0) GO TO 430
      IF (a.EQ.0.0D0 .AND. x.EQ.0.0D0) GO TO 430
      IF (a*x.EQ.0.0D0) GO TO 420
c
      iop = ind + 1
      IF (iop.NE.1 .AND. iop.NE.2) iop = 3
      acc = dmax1(acc0(iop),e)
      e0 = e00(iop)
      x0 = x00(iop)
c
c            SELECT THE APPROPRIATE ALGORITHM
c
      IF (a.GE.1.0D0) GO TO 10
      IF (a.EQ.0.5D0) GO TO 390
      IF (x.LT.1.1D0) GO TO 160
      t1 = a*dlog(x) - x
      u = a*exp(t1)
      IF (u.EQ.0.0D0) GO TO 380
      r = u* (1.0D0+gam1(a))
      GO TO 250
c
   10 IF (a.GE.big(iop)) GO TO 30
      IF (a.GT.x .OR. x.GE.x0) GO TO 20
      twoa = a + a
      m = int(twoa)
      IF (twoa.NE.dble(m)) GO TO 20
      i = m/2
      IF (a.EQ.dble(i)) GO TO 210
      GO TO 220

   20 t1 = a*dlog(x) - x
      r = exp(t1)/gamma(a)
      GO TO 40
c
   30 l = x/a
      IF (l.EQ.0.0D0) GO TO 370
      s = 0.5D0 + (0.5D0-l)
      z = rlog(l)
      IF (z.GE.700.0D0/a) GO TO 410
      y = a*z
      rta = sqrt(a)
      IF (abs(s).LE.e0/rta) GO TO 330
      IF (abs(s).LE.0.4D0) GO TO 270
c
      t = (1.0D0/a)**2
      t1 = (((0.75D0*t-1.0D0)*t+3.5D0)*t-105.0D0)/ (a*1260.0D0)
      t1 = t1 - y
      r = rt2pin*rta*exp(t1)
c
   40 IF (r.EQ.0.0D0) GO TO 420
      IF (x.LE.dmax1(a,alog10)) GO TO 50
      IF (x.LT.x0) GO TO 250
      GO TO 100
c
c                 TAYLOR SERIES FOR P/R
c
   50 apn = a + 1.0D0
      t = x/apn
      wk(1) = t
      DO 60 n = 2,20
          apn = apn + 1.0D0
          t = t* (x/apn)
          IF (t.LE.1.D-3) GO TO 70
          wk(n) = t
   60 CONTINUE
      n = 20
c
   70 sum = t
      tol = 0.5D0*acc
   80 apn = apn + 1.0D0
      t = t* (x/apn)
      sum = sum + t
      IF (t.GT.tol) GO TO 80
c
      max = n - 1
      DO 90 m = 1,max
          n = n - 1
          sum = sum + wk(n)
   90 CONTINUE
      ans = (r/a)* (1.0D0+sum)
      qans = 0.5D0 + (0.5D0-ans)
      RETURN
c
c                 ASYMPTOTIC EXPANSION
c
  100 amn = a - 1.0D0
      t = amn/x
      wk(1) = t
      DO 110 n = 2,20
          amn = amn - 1.0D0
          t = t* (amn/x)
          IF (abs(t).LE.1.D-3) GO TO 120
          wk(n) = t
  110 CONTINUE
      n = 20
c
  120 sum = t
  130 IF (abs(t).LE.acc) GO TO 140
      amn = amn - 1.0D0
      t = t* (amn/x)
      sum = sum + t
      GO TO 130
c
  140 max = n - 1
      DO 150 m = 1,max
          n = n - 1
          sum = sum + wk(n)
  150 CONTINUE
      qans = (r/x)* (1.0D0+sum)
      ans = 0.5D0 + (0.5D0-qans)
      RETURN
c
c             TAYLOR SERIES FOR P(A,X)/X**A
c
  160 an = 3.0D0
      c = x
      sum = x/ (a+3.0D0)
      tol = 3.0D0*acc/ (a+1.0D0)
  170 an = an + 1.0D0
      c = -c* (x/an)
      t = c/ (a+an)
      sum = sum + t
      IF (abs(t).GT.tol) GO TO 170
      j = a*x* ((sum/6.0D0-0.5D0/ (a+2.0D0))*x+1.0D0/ (a+1.0D0))
c
      z = a*dlog(x)
      h = gam1(a)
      g = 1.0D0 + h
      IF (x.LT.0.25D0) GO TO 180
      IF (a.LT.x/2.59D0) GO TO 200
      GO TO 190

  180 IF (z.GT.-.13394D0) GO TO 200
c
  190 w = exp(z)
      ans = w*g* (0.5D0+ (0.5D0-j))
      qans = 0.5D0 + (0.5D0-ans)
      RETURN
c
  200 l = rexp(z)
      w = 0.5D0 + (0.5D0+l)
      qans = (w*j-l)*g - h
      IF (qans.LT.0.0D0) GO TO 380
      ans = 0.5D0 + (0.5D0-qans)
      RETURN
c
c             FINITE SUMS FOR Q WHEN A .GE. 1
c                 AND 2*A IS AN INTEGER
c
  210 sum = exp(-x)
      t = sum
      n = 1
      c = 0.0D0
      GO TO 230
c
  220 rtx = sqrt(x)
      sum = erfc1(0,rtx)
      t = exp(-x)/ (rtpi*rtx)
      n = 0
      c = -0.5D0
c
  230 IF (n.EQ.i) GO TO 240
      n = n + 1
      c = c + 1.0D0
      t = (x*t)/c
      sum = sum + t
      GO TO 230

  240 qans = sum
      ans = 0.5D0 + (0.5D0-qans)
      RETURN
c
c              CONTINUED FRACTION EXPANSION
c
  250 tol = dmax1(5.0D0*e,acc)
      a2nm1 = 1.0D0
      a2n = 1.0D0
      b2nm1 = x
      b2n = x + (1.0D0-a)
      c = 1.0D0
  260 a2nm1 = x*a2n + c*a2nm1
      b2nm1 = x*b2n + c*b2nm1
      am0 = a2nm1/b2nm1
      c = c + 1.0D0
      cma = c - a
      a2n = a2nm1 + cma*a2n
      b2n = b2nm1 + cma*b2n
      an0 = a2n/b2n
      IF (abs(an0-am0).GE.tol*an0) GO TO 260
c
      qans = r*an0
      ans = 0.5D0 + (0.5D0-qans)
      RETURN
c
c                GENERAL TEMME EXPANSION
c
  270 IF (abs(s).LE.2.0D0*e .AND. a*e*e.GT.3.28D-3) GO TO 430
      c = exp(-y)
      w = 0.5D0*erfc1(1,sqrt(y))
      u = 1.0D0/a
      z = sqrt(z+z)
      IF (l.LT.1.0D0) z = -z
      IF (iop-2) 280,290,300
c
  280 IF (abs(s).LE.1.D-3) GO TO 340
      c0 = ((((((((((((d0(13)*z+d0(12))*z+d0(11))*z+d0(10))*z+d0(9))*z+
     +     d0(8))*z+d0(7))*z+d0(6))*z+d0(5))*z+d0(4))*z+d0(3))*z+d0(2))*
     +     z+d0(1))*z - third
      c1 = (((((((((((d1(12)*z+d1(11))*z+d1(10))*z+d1(9))*z+d1(8))*z+
     +     d1(7))*z+d1(6))*z+d1(5))*z+d1(4))*z+d1(3))*z+d1(2))*z+d1(1))*
     +     z + d10
      c2 = (((((((((d2(10)*z+d2(9))*z+d2(8))*z+d2(7))*z+d2(6))*z+
     +     d2(5))*z+d2(4))*z+d2(3))*z+d2(2))*z+d2(1))*z + d20
      c3 = (((((((d3(8)*z+d3(7))*z+d3(6))*z+d3(5))*z+d3(4))*z+d3(3))*z+
     +     d3(2))*z+d3(1))*z + d30
      c4 = (((((d4(6)*z+d4(5))*z+d4(4))*z+d4(3))*z+d4(2))*z+d4(1))*z +
     +     d40
      c5 = (((d5(4)*z+d5(3))*z+d5(2))*z+d5(1))*z + d50
      c6 = (d6(2)*z+d6(1))*z + d60
      t = ((((((d70*u+c6)*u+c5)*u+c4)*u+c3)*u+c2)*u+c1)*u + c0
      GO TO 310
c
  290 c0 = (((((d0(6)*z+d0(5))*z+d0(4))*z+d0(3))*z+d0(2))*z+d0(1))*z -
     +     third
      c1 = (((d1(4)*z+d1(3))*z+d1(2))*z+d1(1))*z + d10
      c2 = d2(1)*z + d20
      t = (c2*u+c1)*u + c0
      GO TO 310
c
  300 t = ((d0(3)*z+d0(2))*z+d0(1))*z - third
c
  310 IF (l.LT.1.0D0) GO TO 320
      qans = c* (w+rt2pin*t/rta)
      ans = 0.5D0 + (0.5D0-qans)
      RETURN

  320 ans = c* (w-rt2pin*t/rta)
      qans = 0.5D0 + (0.5D0-ans)
      RETURN
c
c               TEMME EXPANSION FOR L = 1
c
  330 IF (a*e*e.GT.3.28D-3) GO TO 430
      c = 0.5D0 + (0.5D0-y)
      w = (0.5D0-sqrt(y)* (0.5D0+ (0.5D0-y/3.0D0))/rtpi)/c
      u = 1.0D0/a
      z = sqrt(z+z)
      IF (l.LT.1.0D0) z = -z
      IF (iop-2) 340,350,360
c
  340 c0 = ((((((d0(7)*z+d0(6))*z+d0(5))*z+d0(4))*z+d0(3))*z+d0(2))*z+
     +     d0(1))*z - third
      c1 = (((((d1(6)*z+d1(5))*z+d1(4))*z+d1(3))*z+d1(2))*z+d1(1))*z +
     +     d10
      c2 = ((((d2(5)*z+d2(4))*z+d2(3))*z+d2(2))*z+d2(1))*z + d20
      c3 = (((d3(4)*z+d3(3))*z+d3(2))*z+d3(1))*z + d30
      c4 = (d4(2)*z+d4(1))*z + d40
      c5 = (d5(2)*z+d5(1))*z + d50
      c6 = d6(1)*z + d60
      t = ((((((d70*u+c6)*u+c5)*u+c4)*u+c3)*u+c2)*u+c1)*u + c0
      GO TO 310
c
  350 c0 = (d0(2)*z+d0(1))*z - third
      c1 = d1(1)*z + d10
      t = (d20*u+c1)*u + c0
      GO TO 310
c
  360 t = d0(1)*z - third
      GO TO 310
c
c                     SPECIAL CASES
c
  370 ans = 0.0D0
      qans = 1.0D0
      RETURN
c
  380 ans = 1.0D0
      qans = 0.0D0
      RETURN
c
  390 IF (x.GE.0.25D0) GO TO 400
      ans = erf(sqrt(x))
      qans = 0.5D0 + (0.5D0-ans)
      RETURN

  400 qans = erfc1(0,sqrt(x))
      ans = 0.5D0 + (0.5D0-qans)
      RETURN
c
  410 IF (abs(s).LE.2.0D0*e) GO TO 430
  420 IF (x.LE.a) GO TO 370
      GO TO 380
c
c                     ERROR RETURN
c
  430 ans = 2.0D0
      RETURN
      END
c
c
      INTEGER FUNCTION ipmpar(i)
c-----------------------------------------------------------------------
c
c     IPMPAR PROVIDES THE INTEGER MACHINE CONSTANTS FOR THE COMPUTER
c     THAT IS USED. IT IS ASSUMED THAT THE ARGUMENT I IS AN INTEGER
c     HAVING ONE OF THE VALUES 1-10. IPMPAR(I) HAS THE VALUE ...
c
c  INTEGERS.
c
c     ASSUME INTEGERS ARE REPRESENTED IN THE N-DIGIT, BASE-A FORM
c
c               SIGN ( X(N-1)*A**(N-1) + ... + X(1)*A + X(0) )
c
c               WHERE 0 .LE. X(I) .LT. A FOR I=0,...,N-1.
c
c     IPMPAR(1) = A, THE BASE.
c
c     IPMPAR(2) = N, THE NUMBER OF BASE-A DIGITS.
c
c     IPMPAR(3) = A**N - 1, THE LARGEST MAGNITUDE.
c
c  FLOATING-POINT NUMBERS.
c
c     IT IS ASSUMED THAT THE SINGLE AND DOUBLE PRECISION FLOATING
c     POINT ARITHMETICS HAVE THE SAME BASE, SAY B, AND THAT THE
c     NONZERO NUMBERS ARE REPRESENTED IN THE FORM
c
c               SIGN (B**E) * (X(1)/B + ... + X(M)/B**M)
c
c               WHERE X(I) = 0,1,...,B-1 FOR I=1,...,M,
c               X(1) .GE. 1, AND EMIN .LE. E .LE. EMAX.
c
c     IPMPAR(4) = B, THE BASE.
c
c  SINGLE-PRECISION
c
c     IPMPAR(5) = M, THE NUMBER OF BASE-B DIGITS.
c
c     IPMPAR(6) = EMIN, THE SMALLEST EXPONENT E.
c
c     IPMPAR(7) = EMAX, THE LARGEST EXPONENT E.
c
c  DOUBLE-PRECISION
c
c     IPMPAR(8) = M, THE NUMBER OF BASE-B DIGITS.
c
c     IPMPAR(9) = EMIN, THE SMALLEST EXPONENT E.
c
c     IPMPAR(10) = EMAX, THE LARGEST EXPONENT E.
c
c-----------------------------------------------------------------------
c
c     TO DEFINE THIS FUNCTION FOR THE COMPUTER BEING USED, ACTIVATE
c     THE DATA STATMENTS FOR THE COMPUTER BY REMOVING THE C FROM
c     COLUMN 1. (ALL THE OTHER DATA STATEMENTS SHOULD HAVE C IN
c     COLUMN 1.)
c
c-----------------------------------------------------------------------
c
c     IPMPAR IS AN ADAPTATION OF THE FUNCTION I1MACH, WRITTEN BY
c     P.A. FOX, A.D. HALL, AND N.L. SCHRYER (BELL LABORATORIES).
c     IPMPAR WAS FORMED BY A.H. MORRIS (NSWC). THE CONSTANTS ARE
c     FROM BELL LABORATORIES, NSWC, AND OTHER SOURCES.
c
c-----------------------------------------------------------------------
c     .. Scalar Arguments ..
      INTEGER i
c     ..
c     .. Local Arrays ..
      INTEGER imach(10)
c     ..
c     .. Data statements ..
c
c     MACHINE CONSTANTS FOR AMDAHL MACHINES.
c
c     DATA IMACH( 1) /   2 /
c     DATA IMACH( 2) /  31 /
c     DATA IMACH( 3) / 2147483647 /
c     DATA IMACH( 4) /  16 /
c     DATA IMACH( 5) /   6 /
c     DATA IMACH( 6) / -64 /
c     DATA IMACH( 7) /  63 /
c     DATA IMACH( 8) /  14 /
c     DATA IMACH( 9) / -64 /
c     DATA IMACH(10) /  63 /
c
c     MACHINE CONSTANTS FOR THE AT&T 3B SERIES, AT&T
c     PC 7300, AND AT&T 6300.
c
c     DATA IMACH( 1) /     2 /
c     DATA IMACH( 2) /    31 /
c     DATA IMACH( 3) / 2147483647 /
c     DATA IMACH( 4) /     2 /
c     DATA IMACH( 5) /    24 /
c     DATA IMACH( 6) /  -125 /
c     DATA IMACH( 7) /   128 /
c     DATA IMACH( 8) /    53 /
c     DATA IMACH( 9) / -1021 /
c     DATA IMACH(10) /  1024 /
c
c     MACHINE CONSTANTS FOR THE BURROUGHS 1700 SYSTEM.
c
c     DATA IMACH( 1) /    2 /
c     DATA IMACH( 2) /   33 /
c     DATA IMACH( 3) / 8589934591 /
c     DATA IMACH( 4) /    2 /
c     DATA IMACH( 5) /   24 /
c     DATA IMACH( 6) / -256 /
c     DATA IMACH( 7) /  255 /
c     DATA IMACH( 8) /   60 /
c     DATA IMACH( 9) / -256 /
c     DATA IMACH(10) /  255 /
c
c     MACHINE CONSTANTS FOR THE BURROUGHS 5700 SYSTEM.
c
c     DATA IMACH( 1) /    2 /
c     DATA IMACH( 2) /   39 /
c     DATA IMACH( 3) / 549755813887 /
c     DATA IMACH( 4) /    8 /
c     DATA IMACH( 5) /   13 /
c     DATA IMACH( 6) /  -50 /
c     DATA IMACH( 7) /   76 /
c     DATA IMACH( 8) /   26 /
c     DATA IMACH( 9) /  -50 /
c     DATA IMACH(10) /   76 /
c
c     MACHINE CONSTANTS FOR THE BURROUGHS 6700/7700 SYSTEMS.
c
c     DATA IMACH( 1) /      2 /
c     DATA IMACH( 2) /     39 /
c     DATA IMACH( 3) / 549755813887 /
c     DATA IMACH( 4) /      8 /
c     DATA IMACH( 5) /     13 /
c     DATA IMACH( 6) /    -50 /
c     DATA IMACH( 7) /     76 /
c     DATA IMACH( 8) /     26 /
c     DATA IMACH( 9) / -32754 /
c     DATA IMACH(10) /  32780 /
c
c     MACHINE CONSTANTS FOR THE CDC 6000/7000 SERIES
c     60 BIT ARITHMETIC, AND THE CDC CYBER 995 64 BIT
c     ARITHMETIC (NOS OPERATING SYSTEM).
c
c     DATA IMACH( 1) /    2 /
c     DATA IMACH( 2) /   48 /
c     DATA IMACH( 3) / 281474976710655 /
c     DATA IMACH( 4) /    2 /
c     DATA IMACH( 5) /   48 /
c     DATA IMACH( 6) / -974 /
c     DATA IMACH( 7) / 1070 /
c     DATA IMACH( 8) /   95 /
c     DATA IMACH( 9) / -926 /
c     DATA IMACH(10) / 1070 /
c
c     MACHINE CONSTANTS FOR THE CDC CYBER 995 64 BIT
c     ARITHMETIC (NOS/VE OPERATING SYSTEM).
c
c     DATA IMACH( 1) /     2 /
c     DATA IMACH( 2) /    63 /
c     DATA IMACH( 3) / 9223372036854775807 /
c     DATA IMACH( 4) /     2 /
c     DATA IMACH( 5) /    48 /
c     DATA IMACH( 6) / -4096 /
c     DATA IMACH( 7) /  4095 /
c     DATA IMACH( 8) /    96 /
c     DATA IMACH( 9) / -4096 /
c     DATA IMACH(10) /  4095 /
c
c     MACHINE CONSTANTS FOR THE CRAY 1, XMP, 2, AND 3.
c
c     DATA IMACH( 1) /     2 /
c     DATA IMACH( 2) /    63 /
c     DATA IMACH( 3) / 9223372036854775807 /
c     DATA IMACH( 4) /     2 /
c     DATA IMACH( 5) /    47 /
c     DATA IMACH( 6) / -8189 /
c     DATA IMACH( 7) /  8190 /
c     DATA IMACH( 8) /    94 /
c     DATA IMACH( 9) / -8099 /
c     DATA IMACH(10) /  8190 /
c
c     MACHINE CONSTANTS FOR THE DATA GENERAL ECLIPSE S/200.
c
c     DATA IMACH( 1) /    2 /
c     DATA IMACH( 2) /   15 /
c     DATA IMACH( 3) / 32767 /
c     DATA IMACH( 4) /   16 /
c     DATA IMACH( 5) /    6 /
c     DATA IMACH( 6) /  -64 /
c     DATA IMACH( 7) /   63 /
c     DATA IMACH( 8) /   14 /
c     DATA IMACH( 9) /  -64 /
c     DATA IMACH(10) /   63 /
c
c     MACHINE CONSTANTS FOR THE HARRIS 220.
c
c     DATA IMACH( 1) /    2 /
c     DATA IMACH( 2) /   23 /
c     DATA IMACH( 3) / 8388607 /
c     DATA IMACH( 4) /    2 /
c     DATA IMACH( 5) /   23 /
c     DATA IMACH( 6) / -127 /
c     DATA IMACH( 7) /  127 /
c     DATA IMACH( 8) /   38 /
c     DATA IMACH( 9) / -127 /
c     DATA IMACH(10) /  127 /
c
c     MACHINE CONSTANTS FOR THE HONEYWELL 600/6000
c     AND DPS 8/70 SERIES.
c
c     DATA IMACH( 1) /    2 /
c     DATA IMACH( 2) /   35 /
c     DATA IMACH( 3) / 34359738367 /
c     DATA IMACH( 4) /    2 /
c     DATA IMACH( 5) /   27 /
c     DATA IMACH( 6) / -127 /
c     DATA IMACH( 7) /  127 /
c     DATA IMACH( 8) /   63 /
c     DATA IMACH( 9) / -127 /
c     DATA IMACH(10) /  127 /
c
c     MACHINE CONSTANTS FOR THE HP 2100
c     3 WORD DOUBLE PRECISION OPTION WITH FTN4
c
c     DATA IMACH( 1) /    2 /
c     DATA IMACH( 2) /   15 /
c     DATA IMACH( 3) / 32767 /
c     DATA IMACH( 4) /    2 /
c     DATA IMACH( 5) /   23 /
c     DATA IMACH( 6) / -128 /
c     DATA IMACH( 7) /  127 /
c     DATA IMACH( 8) /   39 /
c     DATA IMACH( 9) / -128 /
c     DATA IMACH(10) /  127 /
c
c     MACHINE CONSTANTS FOR THE HP 2100
c     4 WORD DOUBLE PRECISION OPTION WITH FTN4
c
c     DATA IMACH( 1) /    2 /
c     DATA IMACH( 2) /   15 /
c     DATA IMACH( 3) / 32767 /
c     DATA IMACH( 4) /    2 /
c     DATA IMACH( 5) /   23 /
c     DATA IMACH( 6) / -128 /
c     DATA IMACH( 7) /  127 /
c     DATA IMACH( 8) /   55 /
c     DATA IMACH( 9) / -128 /
c     DATA IMACH(10) /  127 /
c
c     MACHINE CONSTANTS FOR THE HP 9000.
c
c     DATA IMACH( 1) /     2 /
c     DATA IMACH( 2) /    31 /
c     DATA IMACH( 3) / 2147483647 /
c     DATA IMACH( 4) /     2 /
c     DATA IMACH( 5) /    24 /
c     DATA IMACH( 6) /  -126 /
c     DATA IMACH( 7) /   128 /
c     DATA IMACH( 8) /    53 /
c     DATA IMACH( 9) / -1021 /
c     DATA IMACH(10) /  1024 /
c
c     MACHINE CONSTANTS FOR THE IBM 360/370 SERIES,
c     THE ICL 2900, THE ITEL AS/6, THE XEROX SIGMA
c     5/7/9 AND THE SEL SYSTEMS 85/86.
c
c     DATA IMACH( 1) /    2 /
c     DATA IMACH( 2) /   31 /
c     DATA IMACH( 3) / 2147483647 /
c     DATA IMACH( 4) /   16 /
c     DATA IMACH( 5) /    6 /
c     DATA IMACH( 6) /  -64 /
c     DATA IMACH( 7) /   63 /
c     DATA IMACH( 8) /   14 /
c     DATA IMACH( 9) /  -64 /
c     DATA IMACH(10) /   63 /
c
c     MACHINE CONSTANTS FOR THE IBM PC.
c
c      DATA imach(1)/2/
c      DATA imach(2)/31/
c      DATA imach(3)/2147483647/
c      DATA imach(4)/2/
c      DATA imach(5)/24/
c      DATA imach(6)/-125/
c      DATA imach(7)/128/
c      DATA imach(8)/53/
c      DATA imach(9)/-1021/
c      DATA imach(10)/1024/
c
c     MACHINE CONSTANTS FOR THE MACINTOSH II - ABSOFT
c     MACFORTRAN II.
c
c     DATA IMACH( 1) /     2 /
c     DATA IMACH( 2) /    31 /
c     DATA IMACH( 3) / 2147483647 /
c     DATA IMACH( 4) /     2 /
c     DATA IMACH( 5) /    24 /
c     DATA IMACH( 6) /  -125 /
c     DATA IMACH( 7) /   128 /
c     DATA IMACH( 8) /    53 /
c     DATA IMACH( 9) / -1021 /
c     DATA IMACH(10) /  1024 /
c
c     MACHINE CONSTANTS FOR THE MICROVAX - S FORTRAN.
c
c     DATA IMACH( 1) /    2 /
c     DATA IMACH( 2) /   31 /
c     DATA IMACH( 3) / 2147483647 /
c     DATA IMACH( 4) /    2 /
c     DATA IMACH( 5) /   24 /
c     DATA IMACH( 6) / -127 /
c     DATA IMACH( 7) /  127 /
c     DATA IMACH( 8) /   56 /
c     DATA IMACH( 9) / -127 /
c     DATA IMACH(10) /  127 /
c
c     MACHINE CONSTANTS FOR THE PDP-10 (KA PROCESSOR).
c
c     DATA IMACH( 1) /    2 /
c     DATA IMACH( 2) /   35 /
c     DATA IMACH( 3) / 34359738367 /
c     DATA IMACH( 4) /    2 /
c     DATA IMACH( 5) /   27 /
c     DATA IMACH( 6) / -128 /
c     DATA IMACH( 7) /  127 /
c     DATA IMACH( 8) /   54 /
c     DATA IMACH( 9) / -101 /
c     DATA IMACH(10) /  127 /
c
c     MACHINE CONSTANTS FOR THE PDP-10 (KI PROCESSOR).
c
c     DATA IMACH( 1) /    2 /
c     DATA IMACH( 2) /   35 /
c     DATA IMACH( 3) / 34359738367 /
c     DATA IMACH( 4) /    2 /
c     DATA IMACH( 5) /   27 /
c     DATA IMACH( 6) / -128 /
c     DATA IMACH( 7) /  127 /
c     DATA IMACH( 8) /   62 /
c     DATA IMACH( 9) / -128 /
c     DATA IMACH(10) /  127 /
c
c     MACHINE CONSTANTS FOR THE PDP-11 FORTRAN SUPPORTING
c     32-BIT INTEGER ARITHMETIC.
c
c     DATA IMACH( 1) /    2 /
c     DATA IMACH( 2) /   31 /
c     DATA IMACH( 3) / 2147483647 /
c     DATA IMACH( 4) /    2 /
c     DATA IMACH( 5) /   24 /
c     DATA IMACH( 6) / -127 /
c     DATA IMACH( 7) /  127 /
c     DATA IMACH( 8) /   56 /
c     DATA IMACH( 9) / -127 /
c     DATA IMACH(10) /  127 /
c
c     MACHINE CONSTANTS FOR THE SEQUENT BALANCE 8000.
c
c     DATA IMACH( 1) /     2 /
c     DATA IMACH( 2) /    31 /
c     DATA IMACH( 3) / 2147483647 /
c     DATA IMACH( 4) /     2 /
c     DATA IMACH( 5) /    24 /
c     DATA IMACH( 6) /  -125 /
c     DATA IMACH( 7) /   128 /
c     DATA IMACH( 8) /    53 /
c     DATA IMACH( 9) / -1021 /
c     DATA IMACH(10) /  1024 /
c
c     MACHINE CONSTANTS FOR THE SILICON GRAPHICS IRIS-4D
c     SERIES (MIPS R3000 PROCESSOR).
c
c     DATA IMACH( 1) /     2 /
c     DATA IMACH( 2) /    31 /
c     DATA IMACH( 3) / 2147483647 /
c     DATA IMACH( 4) /     2 /
c     DATA IMACH( 5) /    24 /
c     DATA IMACH( 6) /  -125 /
c     DATA IMACH( 7) /   128 /
c     DATA IMACH( 8) /    53 /
c     DATA IMACH( 9) / -1021 /
c     DATA IMACH(10) /  1024 /
c
c     MACHINE CONSTANTS FOR IEEE ARITHMETIC MACHINES, SUCH AS THE AT&T
c     3B SERIES, MOTOROLA 68000 BASED MACHINES (E.G. SUN 3 AND AT&T
c     PC 7300), AND 8087 BASED MICROS (E.G. IBM PC AND AT&T 6300).
c
      DATA IMACH( 1) /     2 /
      DATA IMACH( 2) /    31 /
      DATA IMACH( 3) / 2147483647 /
      DATA IMACH( 4) /     2 /
      DATA IMACH( 5) /    24 /
      DATA IMACH( 6) /  -125 /
      DATA IMACH( 7) /   128 /
      DATA IMACH( 8) /    53 /
      DATA IMACH( 9) / -1021 /
      DATA IMACH(10) /  1024 /
c
c     MACHINE CONSTANTS FOR THE UNIVAC 1100 SERIES.
c
c     DATA IMACH( 1) /    2 /
c     DATA IMACH( 2) /   35 /
c     DATA IMACH( 3) / 34359738367 /
c     DATA IMACH( 4) /    2 /
c     DATA IMACH( 5) /   27 /
c     DATA IMACH( 6) / -128 /
c     DATA IMACH( 7) /  127 /
c     DATA IMACH( 8) /   60 /
c     DATA IMACH( 9) /-1024 /
c     DATA IMACH(10) / 1023 /
c
c     MACHINE CONSTANTS FOR THE VAX 11/780.
c
c     DATA IMACH( 1) /    2 /
c     DATA IMACH( 2) /   31 /
c     DATA IMACH( 3) / 2147483647 /
c     DATA IMACH( 4) /    2 /
c     DATA IMACH( 5) /   24 /
c     DATA IMACH( 6) / -127 /
c     DATA IMACH( 7) /  127 /
c     DATA IMACH( 8) /   56 /
c     DATA IMACH( 9) / -127 /
c     DATA IMACH(10) /  127 /
c
      ipmpar = imach(i)
      RETURN
      END
c
c
      DOUBLE PRECISION FUNCTION rexp(x)
c-----------------------------------------------------------------------
c            EVALUATION OF THE FUNCTION EXP(X) - 1
c-----------------------------------------------------------------------
c     .. Scalar Arguments ..
      DOUBLE PRECISION x
c     ..
c     .. Local Scalars ..
      DOUBLE PRECISION p1,p2,q1,q2,q3,q4,w
c     ..
c     .. Intrinsic Functions ..
      INTRINSIC abs,exp
c     ..
c     .. Data statements ..
      DATA p1/.914041914819518D-09/,p2/.238082361044469D-01/,
     +     q1/-.499999999085958D+00/,q2/.107141568980644D+00/,
     +     q3/-.119041179760821D-01/,q4/.595130811860248D-03/
c     ..
c     .. Executable Statements ..
c-----------------------
      IF (abs(x).GT.0.15D0) GO TO 10
      rexp = x* (((p2*x+p1)*x+1.0D0)/ ((((q4*x+q3)*x+q2)*x+q1)*x+1.0D0))
      RETURN
c
   10 w = exp(x)
      IF (x.GT.0.0D0) GO TO 20
      rexp = (w-0.5D0) - 0.5D0
      RETURN

   20 rexp = w* (0.5D0+ (0.5D0-1.0D0/w))
      RETURN
      END
c
c
      DOUBLE PRECISION FUNCTION rlog(x)
c     -------------------
c     COMPUTATION OF  X - 1 - LN(X)
c     -------------------
c     .. Scalar Arguments ..
      DOUBLE PRECISION x
c     ..
c     .. Local Scalars ..
      DOUBLE PRECISION a,b,p0,p1,p2,q1,q2,r,t,u,w,w1
c     ..
c     .. Intrinsic Functions ..
      INTRINSIC dble,dlog
c     ..
c     .. Data statements ..
c     -------------------
      DATA a/.566749439387324D-01/
      DATA b/.456512608815524D-01/
      DATA p0/.333333333333333D+00/,p1/-.224696413112536D+00/,
     +     p2/.620886815375787D-02/
      DATA q1/-.127408923933623D+01/,q2/.354508718369557D+00/
c     ..
c     .. Executable Statements ..
c     -------------------
      IF (x.LT.0.61D0 .OR. x.GT.1.57D0) GO TO 40
      IF (x.LT.0.82D0) GO TO 10
      IF (x.GT.1.18D0) GO TO 20
c
c              ARGUMENT REDUCTION
c
      u = (x-0.5D0) - 0.5D0
      w1 = 0.0D0
      GO TO 30
c
   10 u = dble(x) - 0.7D0
      u = u/0.7D0
      w1 = a - u*0.3D0
      GO TO 30
c
   20 u = 0.75D0*dble(x) - 1.D0
      w1 = b + u/3.0D0
c
c               SERIES EXPANSION
c
   30 r = u/ (u+2.0D0)
      t = r*r
      w = ((p2*t+p1)*t+p0)/ ((q2*t+q1)*t+1.0D0)
      rlog = 2.0D0*t* (1.0D0/ (1.0D0-r)-r*w) + w1
      RETURN
c
c
   40 r = (x-0.5D0) - 0.5D0
      rlog = r - dlog(x)
      RETURN
      END
c
c
      DOUBLE PRECISION FUNCTION spmpar(i)
c-----------------------------------------------------------------------
c
c     SPMPAR PROVIDES THE SINGLE PRECISION MACHINE CONSTANTS FOR
c     THE COMPUTER BEING USED. IT IS ASSUMED THAT THE ARGUMENT
c     I IS AN INTEGER HAVING ONE OF THE VALUES 1, 2, OR 3. IF THE
c     SINGLE PRECISION ARITHMETIC BEING USED HAS M BASE B DIGITS AND
c     ITS SMALLEST AND LARGEST EXPONENTS ARE EMIN AND EMAX, THEN
c
c        SPMPAR(1) = B**(1 - M), THE MACHINE PRECISION,
c
c        SPMPAR(2) = B**(EMIN - 1), THE SMALLEST MAGNITUDE,
c
c        SPMPAR(3) = B**EMAX*(1 - B**(-M)), THE LARGEST MAGNITUDE.
c
c-----------------------------------------------------------------------
c     WRITTEN BY
c        ALFRED H. MORRIS, JR.
c        NAVAL SURFACE WARFARE CENTER
c        DAHLGREN VIRGINIA
c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
c     MODIFIED BY BARRY W. BROWN TO RETURN DOUBLE PRECISION MACHINE
c     CONSTANTS FOR THE COMPUTER BEING USED.  THIS MODIFICATION WAS
c     MADE AS PART OF CONVERTING BRATIO TO DOUBLE PRECISION
c-----------------------------------------------------------------------
c     .. Scalar Arguments ..
      INTEGER i
c     ..
c     .. Local Scalars ..
      DOUBLE PRECISION b,binv,bm1,one,w,z
      INTEGER emax,emin,ibeta,m
c     ..
c     .. External Functions ..
      INTEGER ipmpar
      EXTERNAL ipmpar
c     ..
c     .. Intrinsic Functions ..
      INTRINSIC dble
c     ..
c     .. Executable Statements ..
c
      IF (i.GT.1) GO TO 10
      b = ipmpar(4)
      m = ipmpar(8)
      spmpar = b** (1-m)
      RETURN
c
   10 IF (i.GT.2) GO TO 20
      b = ipmpar(4)
      emin = ipmpar(9)
      one = dble(1)
      binv = one/b
      w = b** (emin+2)
      spmpar = ((w*binv)*binv)*binv
      RETURN
c
   20 ibeta = ipmpar(4)
      m = ipmpar(8)
      emax = ipmpar(10)
c
      b = ibeta
      bm1 = ibeta - 1
      one = dble(1)
      z = b** (m-1)
      w = ((z-one)*b+bm1)/ (b*z)
c
      z = b** (emax-2)
      spmpar = ((w*z)*b)*b
      RETURN
      END
c
c 
c =============== BEGIN LAHEY-SPECIFIC CODE: =====================
c
c -- Don't use these two functions except with the Lahey compiler:
cc----< Modules written by WERU to support command line args with Lahey: >----
cc      Modified 7/11/2000 by C. R. Meyer to be more generic.
cc
cc This function returns the number of args/options on the commandline
cc It is Lahey lf95 PC compiler specific because it uses the getcl()
cc subroutine call - LEW
c 
c      function narg()
cc
c      integer narg
c      character*128 cmdline
c      integer i,j
cc       
c      narg = 0
cc------Lahey compiler specific
c      call getcl(cmdline)
c      j = 0
cc------find next '-'
c 10   continue
c        i = index(cmdline(j:), ' -')
cc       write(6,*) 'i,j,narg is: ', i,j,narg
c        if (i .ne. 0) then
c          narg = narg + 1
c          j = j+ i + 1
c        endif
c      if(i .ne. 0) goto 10
cc
c      return
c      end
cc
cc
cc This subroutine returns the argument/option specified by argcnt
cc from the commandline
cc cannot return argcnt = 1 (filename), therefore, we return ""
cc This is Lahey lf95 PC compiler specific because it uses the getcl()
cc subroutine call - LEW
cc      Modified 7/11/2000 by C. R. Meyer to be more generic.
cc
c      subroutine argopt(argcnt, option)
cc
c      integer argcnt
c      character*128 option
cc
c      integer narg
c      character*128 cmdline
c      integer i,j
cc
cc ---- If there are no parameters after the command name...
c      if (argcnt .eq. 0) then
c        option = ' '
cc ---- There are Parameters after the Command Name
c      else
c        narg = 0
c        j = 0
cc ---- Lahey compiler specific
c        call getcl(cmdline)
cc------Find Start of Desired Argument.
c 10     continue
cc----------find next '-'
c          i = index(cmdline(j:), ' -')
c          if (i .ne. 0) then
c            narg = narg + 1
c            j = j+ i + 1
c          endif
c        if (narg .ne. argcnt) goto 10
cc ------ Find End of Desired Argument.
c        i = index(cmdline(j:), ' -')
c        if (i .ne. 0) then
c          option = cmdline(j:j+i-1)
c        else
c          option = cmdline(j:)
c        endif
cc
c      endif
cc
c      return
c      end
c
c ================= END LAHEY-SPECIFIC CODE: =====================
c
c
      subroutine lintrp
     i           (mo,jd,ntd)
c
c      + + + PURPOSE + + +
c     Used for smoothing between average monthly values (like max & min
c     temp), from which daily values are stochastically generated (using
c     random numbers) by CLIGEN.  Provides factors "lf" & "rf" to perform
c     a linear interpolation for the input date, between the middle of
c     the current month, and the middle of an adjacent month.  Depending
c     on the day of the month, values will be needed for either the
c     previous month, or the following month.  The value "o_mo" is the
c     number of the other month involved in the interpolation.  The
c     interpolated value (Vi) is simply computed as follows:
c       Vi = V(mo)*lf + V(o_mo)*rf .
c
c     Uses linear interpolation.  Outputs "weighting factors" for the
c     average monthly parameters associated with the midpoints of the
c     months on either side of the date that is input.
c
c     Written by Charles R. Meyer 12/02/99 in consultation with Roel Vining.
c
c     Note that for a linear interpolation "lf" is simply the fraction
c     of the distance from the _upper_ time midpoint, and "rf" is the
c     converse at the other end of the interval.  Also lf + rf = 1.0 .
c
c      + + + ARGUMENT DECLARATIONS + + +
      integer mo, jd, ntd
c
c      + + + ARGUMENT DEFINITIONS + + +
c     mo     - month input.
c     jd     - day of the month input.
c     ntd    - days in this year (365 or 366)
c
c      + + + COMMON BLOCKS + + +
      include 'cinterp.inc'
c      write: lf, rf, o_mo
c     lf     - weighting factor for the midpoint value on this month's end
c              of the time interval.
c     rf     - weighting factor for the midpoint value on the "other" end.
c     o_mo   - month (on the "other" end) whose average value should be used.
c
c      + + + LOCAL VARIABLES + + +
      real mp(12),ni(12)
c
c      + + + LOCAL DEFINITIONS + + +
c     mp      - Number of days from beginning to midpoint of each month
c               (non-leap year).
c     ni      - Number of days from midpoint of previous month to midpoint
c               of current month; ie, number of days in the interval.
c
c     + + + DATA INITIALIZATIONS + + +
      data mp/15.5,14.0,15.5,15.0,15.5,15.0,15.5,15.5,15.0,15.5,15.0,
     1        15.5/
      data ni/31.0,29.5,30.0,30.5,30.5,30.5,30.5,31.0,30.5,30.5,30.5,
     1        30.5/
c
c      + + + END SPECIFICATIONS + + +
c
c ---- Adjust February's midpoint (ni) for Leap Year.
      if(ntd .eq. 366) then
        ni(2) = 30.0
      else
        ni(2) = 29.5
      endif
c
c      There are two intervals related to each month.  The appropriate
c      one to use depends upon the day of the month.  Determine whether
c      it is the previous month, or the following month that is of interest.
c ---- Compare to the next month.
      if(float(jd) .gt. mp(mo)) then
        o_mo = mod(mo+1,12)
        if(o_mo .eq. 0) o_mo = 12
        rf = (jd - mp(mo)) / ni(o_mo)
c ---- Compare to the previous month.
      else
        o_mo = mod(mo-1,12)
        if(o_mo .eq. 0) o_mo = 12
        rf = (mp(mo) - jd) / ni(mo)
      endif
c
      lf = 1.0 - rf
c
      return
      end
c
c
      subroutine fouri1
     i                 (x, indpar)
c     Subroutine to calculate Fourier Series coefficients.
c     Code received 01/21/99 from Clarence Richardson.
c     Modified by C. R. Meyer and verified to give identical results.
c
      real x(12)
      integer indpar
c
c     x      - 12 measured monthly average values (means, sd, skew).
c     indpar - index of the current parameter.
c
c     s      - sum of the x's.
c     x_bar   - average (mean) of the x's.
c
      include 'cinterp.inc'
c     write: c, t
c     c      - fourier series coefficient.
c     t      - fourier series coefficient.
c
c ---- sum the x's:
      s=0.0
      do 5 i=1,12
        s=s+x(i)
  5   continue
c
c ---- compute t & c:
      x_bar(indpar)=s/12.
      do 20 j=1,6
        suma = 0.0
        sumb=0.0
c
        do 10 i=1,12
          d=x(i)-x_bar(indpar)
          v=6.2832*float(i)*float(j)/12.
          suma=suma+(d*cos(v))
          sumb=sumb+(d*sin(v))
 10     continue
c
        a=suma*(2./12.)
        b=sumb*(2./12.)
        t(j,indpar)=atan(-b/a)
        c(j,indpar)=a/cos(t(j,indpar))
 20   continue
c
      return
      end
c
c
      function fouri2
     i               (indpar)
c     Subroutine to calculate Fourier Series representation of seasonal
c     variables.
c     Code received 01/21/99 from Clarence Richardson.
c     Modified by C. R. Meyer and verified to give identical results.
c
      integer indpar
c
c     indpar - index of the current parameter.
c
c     + + + COMMON BLOCKS + + +
      include 'cinterp.inc'
c      read: c, t
c     c      - fourier series coefficient.
c     t      - fourier series coefficient.
c
      include 'cbk3.inc'
c      read: ida
c     ida         - Julian Day of Year.
c
c + + + END SPECIFICATIONS + + +
c
c ---- compute the daily values from c & t:
      dd=(float(ida)+15.5)/366.
      fouri2 = x_bar(indpar)
     1       + c(1,indpar)*cos(6.2832*float(1)*dd+t(1,indpar))
     2       + c(2,indpar)*cos(6.2832*float(2)*dd+t(2,indpar))
     3       + c(3,indpar)*cos(6.2832*float(3)*dd+t(3,indpar))
     4       + c(4,indpar)*cos(6.2832*float(4)*dd+t(4,indpar))
     5       + c(5,indpar)*cos(6.2832*float(5)*dd+t(5,indpar))
     6       + c(6,indpar)*cos(6.2832*float(6)*dd+t(6,indpar))
c
      return
      end
c
c
      subroutine ryf1
     i               (x, indpar)
c
c     Subroutine 1 of 2 to interpolate preserving the monthly means.
c     This is accomplished in most months by shifting the time that
c     the mean occurs ("pseudo-midpoint").  In months whose mean is
c     a local minimum or maximum, the mid-month value is shifted to
c     a pseudo-value and the mean is assumed to occur at two times,
c     one before the midpoint, and one after.
c
c     Written 8/4/2000 by C. R. Meyer to implement an algorithm
c     from Daniel Yoder, and modified by George Foster in response
c     to C. R. Meyer's criticism of lack of a solution for minima
c     or maxima.  The essential idea is to make the area (under the
c     parameter curve) above the monthly mean the same as the area
c     below it.  This shifts the time of the mean (PMT) from the
c     midpoint of the month.  However, for maxima and minima, it
c     leaves the time at the midpoint, but shifts the value (PMV).
c     The values at the end (and beginning) of each month (EMV)
c     resulting from a simple linear interpolation are utilized in
c     the calculations.
c
c     From the mean value and the length of each month, calculate
c     the interpolated values at the month boundaries.  Also calculate
c     the time(s) of the pseudo-midpoint, of the value.
c
      real x(12)
      integer indpar
c
c     x      - 12 measured monthly average values (means, sd, skew).
c     indpar - index of the current parameter.
c
      include 'cinterp.inc'
c       write: emv, pmt, pmv, xes
c     emv    - End-of-the-Month Value for the monthly mean of each parameter.
c     pmt    - Pseudo-Midpoint Time for each month, for each parameter.
c     pmv    - Pseudo-Midpoint Value for each month, for each parameter.
c     xes    - the monthly values (mean, SD, skew) for the 14 values.
c              (Set to -9999.0 if this is not a Max or Min month.)
c
c      + + + LOCAL VARIABLES + + +
      integer dim(12)
c
c      + + + LOCAL DEFINITIONS + + +
c ---- dim -- Days in each Month.
c
c     + + + DATA INITIALIZATIONS + + +
      data dim/31,28,31,30,31,30,31,31,30,31,30,31/
c
c      + + + END SPECIFICATIONS + + +
c
c ---- End of Month Values (EMV):
      do 100 i=1,11
        tte = dim(i)/2.0
        tfs = dim(i+1)/2.0
c--------(fraction of time between middles, from middle to end of this month)
        ratio = tte/(tte+tfs)
        emv(i,indpar) = x(i) + (x(i+1)-x(i))*ratio
 100  continue
c--------(December)
        emv(12,indpar) = (x(12)+x(1))*0.5
c--------(End of January Value, Leap Years)
        emv(13,indpar) = x(1) + (x(2)-x(1))*0.516667
c--------(End of February Value, Leap Years)
        emv(14,indpar) = x(2) + (x(3)-x(2))*0.483333
c
c --- Pseudo-Midpoint Times & Values (PMT & PMV):
      do 200 i=2,12
c-------(3 consecutive identical monthly values)
        if(emv(i-1,indpar).eq.x(i).and.emv(i,indpar).eq.x(i)) then
           pmt(i,indpar) = dim(i)/2.0
           pmv(i,indpar) = x(i)
           xes(i,indpar) = -9999
c-------(not a max or min)
        else if(emv(i-1,indpar).lt.x(i).and.emv(i,indpar).gt.x(i) .or.
     1     emv(i-1,indpar).gt.x(i).and.emv(i,indpar).lt.x(i)) then
           pmt(i,indpar) = dim(i) *
     1            (emv(i,indpar)-x(i))/(emv(i,indpar)-emv(i-1,indpar))
           pmv(i,indpar) = x(i)
           xes(i,indpar) = -9999.0
c-------(max, min, or 1 EOM value identical to x(i))
        else
           pmv(i,indpar) = 2.0*x(i)-(emv(i,indpar)+emv(i-1,indpar))/2.0
           pmt(i,indpar) = dim(i)/2.0
           xes(i,indpar) = x(i)
        endif
 200  continue
c--------(January)
        if(emv(12,indpar).eq.x(1).and.emv(1,indpar).eq.x(1)) then
           pmt(1,indpar) = dim(1)/2.0
           pmv(1,indpar) = x(1)
           xes(1,indpar) = -9999
        else if(emv(12,indpar).lt.x(1).and.emv(1,indpar).gt.x(1) .or.
     1     emv(12,indpar).gt.x(1).and.emv(1,indpar).lt.x(1)) then
           pmt(1,indpar) = 31.0 *
     1            (emv(1,indpar)-x(1))/(emv(1,indpar)-emv(12,indpar))
           pmv(1,indpar) = x(1)
           xes(1,indpar) = -9999.0
        else
           pmv(1,indpar) = 2.0*x(1)-(emv(1,indpar)+emv(12,indpar))/2.0
           pmt(1,indpar) = 15.5
           xes(1,indpar) = x(1)
        endif
c--------(February, Leap Years)
        if(emv(1,indpar).eq.x(2).and.emv(2,indpar).eq.x(2)) then
           pmt(13,indpar) = 14.5
           pmv(13,indpar) = x(2)
        else if(emv(1,indpar).lt.x(2).and.emv(2,indpar).gt.x(2) .or.
     1     emv(1,indpar).gt.x(2).and.emv(2,indpar).lt.x(2)) then
           pmt(13,indpar) = 29.0 *
     1            (emv(2,indpar)-x(2))/(emv(2,indpar)-emv(1,indpar))
           pmv(13,indpar) = x(2)
        else
           pmv(13,indpar) = 2.0*x(2)-(emv(2,indpar)+emv(1,indpar))/2.0
           pmt(13,indpar) = 14.5
        endif
c
      return
      end
c
c
      function ryf2
     i               (mo,jd,ntd,indpar)
c     Function 2 of 2 to interpolate preserving the monthly means.
c     From the values of the month endpoints and the time and value
c     of the pseudo-midpoint, calculate a daily value for the X-parameter
c     with index "indpar", for the current day.
c
c      + + + ARGUMENT DECLARATIONS + + +
      integer mo, jd
      integer indpar
c
c      + + + ARGUMENT DEFINITIONS + + +
c     mo     - month input.
c     jd     - day of the month input.
c     indpar - index of the current parameter.
c     ntd    - days in this year (365 or 366)
c
      include 'cinterp.inc'
c        read: emv, pmt, pmv, xes
c     emv    - End-of-the-Month Value for the monthly mean of each parameter.
c     pmt    - Pseudo-Midpoint Time for each month, for each parameter.
c     pmv    - Pseudo-Midpoint Value for each month, for each parameter.
c
c      + + + LOCAL VARIABLES + + +
      integer dim(12)
      real idim,ipmt,ipmv,emv0,emv1,ratio,mjd
c
c      + + + LOCAL DEFINITIONS + + +
c     dim     - Days in each Month.
c     idim    - Number of days in the current month.
c     ipmt    - Pseudo-Midpoint Time of the current month.
c     ipmv    - Pseudo-Midpoint Value of the current month.
c     emv0    - End of Month Value at Start of current month.
c     emv1    - End of Month Value at End of current month.
c     mjd     - (middle of) day of the month input (ie, noon).
c
c
c     + + + DATA INITIALIZATIONS + + +
      data dim/31,28,31,30,31,30,31,31,30,31,30,31/
c
c      + + + END SPECIFICATIONS + + +
c
c      The intercepts at the end of the months are based on Midnight.
c      However, the appropriate time to represent a day is Noon (the
c      middle of the day, or the expected value).  Therefore it is
c      necessary to adjust from Midnight on day-X to Noon on day-X; ie,
c      to subtract half a day.
c
c      There are two intervals related to each month, before the pseudo-
c      midpoint, and after it.  The appropriate one to use depends upon
c      the current day of the month (JD).  Determine which to use, and
c      interpolate a daily value for the current parameter.
c
c-----(not February in a LY)
      if(mo.ne.2 .or. ntd.ne.366) then
        idim = dim(mo)
        ipmt = pmt(mo,indpar)
        ipmv = pmv(mo,indpar)
        if(mo.gt.1) then
          emv0 = emv(mo-1,indpar)
        else
          emv0 = emv(12,indpar)
        endif
        emv1 = emv(mo,indpar)
c-----(it is February in a LY)
      else
        idim = 29
        ipmt = pmt(13,indpar)
        ipmv = pmv(13,indpar)
        emv0 = emv(13,indpar)
        emv1 = emv(14,indpar)
      endif
c
c ---- Compare current date (MJD) to the pseudo-midpoint of month (IPMT).
      mjd = float(jd) - 0.5
c-----(last half of month)
      if(mjd .gt. ipmt) then
c-------(not a max or min month)
	if(xes(mo,indpar) .eq. -9999.0) then
          ratio = (idim - mjd) / (idim - ipmt)
          ryf2 = ratio*ipmv + (1-ratio)*emv1
c-------(it is a max or min month)
	else
c---------(last quarter of the month)
          if(mjd .gt. 0.75*idim) then
            ratio = (idim - mjd) / (0.25*idim)
            ryf2 = ratio*xes(mo,indpar) + (1-ratio)*emv1
c---------(third quarter)
          else
            ratio = (mjd - 0.5*idim) / (0.25*idim)
            ryf2 = ratio*xes(mo,indpar) + (1-ratio)*ipmv
          endif
        endif
c ---- Compare to the previous month.
      else
	if(xes(mo,indpar) .eq. -9999.0) then
          ratio = mjd / ipmt
          ryf2 = ratio*ipmv + (1-ratio)*emv0
	else
c---------(first quarter of the month)
          if(mjd .lt. 0.25*idim) then
            ratio = mjd / (0.25*idim)
            ryf2 = ratio*xes(mo,indpar) + (1-ratio)*emv0
c---------(second quarter)
          else
            ratio = (0.5*idim - mjd) / (0.25*idim)
            ryf2 = ratio*xes(mo,indpar) + (1-ratio)*ipmv
          endif
        endif
      endif
c
      return
      end
