c    File: ccl1.inc
c
      common /cl1/ prcip(12,31),tgmx(12,31),tgmn(12,31),radg(12,31),
     1             dur(12,31)
c     prcip       - ??? Avg. Daily Precip?  Set to R.  Divided by NUMYR and
c                    added to SUMP.
c     tgmx        - ??? Avg. Max Daily Temp?  Set to TMXG.  Divided by NUMYR 
c                    and added to SUMTX.
c     tgmn        - ??? Avg. Min Daily Temp?  Set to TMNG.  Divided by NUMYR 
c                    and added to SUMTM.
c     radg        - Daily Solar Radiation (Langleys/Day)
c     dur         - Storm Duration in Hours for Single Storm.
c
