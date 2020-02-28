      common /command/ arg_v,av_len,numarg,istate,index,ibyear,numyr,
     1                 irand,dohedr,fs_flag,force,infile,odfile,outfil
c
c --- Command Line control variables:
c
      integer numarg,istate, index, ibyear, numyr, irand, force
      integer av_len
      logical dohedr, fs_flag
      character*1024 arg_v
      character*256 infile
      character*256 odfile
      character*256 outfil
c
c arg_v   - string concatenation of the command line options.
c av_len  - length of arg_v + 1.
c numarg  - number of arguments passed on the command line.
c istate  - Number of State.
c index   - Number of Station.
c ibyear  - Number of Beginning Year for Output.
c numyr   - Number of Years to Generate Output.
c irand   - Number of times to "cycle" the RNG before using random numbers.
c dohedr  - (Logical) Whether to write WEPS header to output file.
c fs_flag - (Logical) Whether station parameter file is in FS formant or not.
c force   - Force overwrite of existing output file.
c infile  - Input file, to be used in lieu of default station parameter files.
c odfile  - Observed data file, used with "option 6".
c outfil  - File to receive CLIGEN output.
