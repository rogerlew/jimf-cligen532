# CLIGEN 5.32.1

## February 28, 2020

Source and Executables provided by:

Jim Frankenberger<br>
Computer Engineer<br>
USDA-ARS National Soil Erosion Research Laboratory


> Warning: No effort is made to ensure these resources are up to date. They are
> provided here solely for Roger's convenience. Please check the following url
> for updates:
> http://fargo.nserl.purdue.edu/cligen532/


This is the source code along with executables for Windows and Linux for CLIGEN version 5.32. This version has a few updates from the CLIGEN 5.3:

- Cligen version 5.32.1 04/06/2022 - fix to to not consider years where year % 100 == 0 as a leap year. (Did not rebuild windows)
- Cligen version 5.32 03/14/2013 - using observed option (type=6) the tpeak variable was being generated based only the cligen predicted days with precip so most days with observed data had tpeak=0. Updated to make sure type 6 input has the same distribution of tpeak as generated precip.
- Cligen version 5.31 01/31/2013 - Corrected solar radiation, it was not using standard deviations from input par file. Increased year field to 5 digits for 10000 year+ runs.
- Cligen version 5.30002. 09/14/09 - Extended character string length for reading command line arguments to allow longer output path length on linux. Should now allow paths as long as WEPS. Path length on Windows was already longer than maximum allowed.
- Cligen version 5.30001. 06/30/09 - Changed order of reading in wet/wet & wet/dry Equivalence statement had interleaved the values making them incorrect Affected only Yoder-Foster & Fourier interpolation

There are also a couple updates to CLIGEN from the Wind Erosion Research (WEPS) group at Fort Collins that have not been merged into this version. When these updates are combined the program will be available from the CLIGEN website.

Repo contains:

- Prebuilt Windows Executable cligen532.exe
- Prebuilt Linux Executable cligen532
- Source Code cligen532.tar.gz
