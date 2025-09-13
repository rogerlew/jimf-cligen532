# CLIGEN 5.32.3

## September 13, 2025

Source and Executables provided by:

Roger Lew
University of Idaho

Jim Frankenberger  
Computer Engineer  
USDA-ARS National Soil Erosion Research Laboratory

> Warning: No effort is made to ensure these resources are up to date. They are
> provided here solely for Roger's convenience. Please check the following url
> for updates:  
> http://fargo.nserl.purdue.edu/cligen532/
> https://github.com/jfrankenberger/cligen5

This is the source code along with executables for Windows, Linux, and Apple Silicon macOS for CLIGEN version 5.32.x.  
This version series has a few updates since CLIGEN 5.3:

- **Cligen version 5.323** – 09/13/2025 (Roger Lew / Gemini AI)  
  Corrected a bug in observed climate file generation (`-O` option) that caused an extra day to be appended to the output.  
  This occurred when the input `.prn` file contained a partial final year.  
  The `day_gen` subroutine now properly handles the end-of-file condition.

- **Cligen version 5.322** – 09/10/2024 (Fred Fox)  
  Corrected calculation of Coefficient of Variation so a zero monthly average temperature does not cause a divide by zero.  
  Merged change from WEPS version 5.3004 (12/13/2018).

- **Cligen version 5.32.1** – 04/06/2022  
  Fixed leap year bug so that years where `year % 100 == 0` are not considered leap years.  
  (Did not rebuild Windows executable.)

- **Cligen version 5.32** – 03/14/2013  
  Using observed option (`type=6`), the `tpeak` variable was being generated based only on CLIGEN-predicted days with precip.  
  Updated to ensure type 6 input has the same distribution of `tpeak` as generated precip.

- **Cligen version 5.31** – 01/31/2013  
  Corrected solar radiation (was not using standard deviations from input `.par` file).  
  Increased year field to 5 digits for 10,000+ year runs.

- **Cligen version 5.30002** – 09/14/2009  
  Extended character string length for reading command line arguments to allow longer output path length on Linux.  
  Paths should now allow as long as WEPS. Windows path length was already longer than maximum allowed.

- **Cligen version 5.30001** – 06/30/2009  
  Changed order of reading in wet/wet & wet/dry equivalence. Interleaving had made values incorrect.  
  Affected only Yoder-Foster & Fourier interpolation.

There are also a couple updates to CLIGEN from the Wind Erosion Research (WEPS) group at Fort Collins that have not been merged into this version.  
When these updates are combined, the program will be available from the CLIGEN website.

## Repo contains:

- Prebuilt Windows Executable `cligen532.exe` (v 5.321)
- Prebuilt Linux Executable `cligen532` (v 5.323)
- Prebuilt Apple Silicon Executable `cligen532.arm64.mac` (v 5.321)