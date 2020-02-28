c    File: ctap2.inc
c
c    Order of Parameters changed to be consistent with v's & k's.
c    Note this requires a corresponding change in CONRAN.
c    3/05/2001 -- C. R. Meyer
c
c    Parameters used to run confidence interval tests on random
c    Standard Normal deviates produced by CLIGEN.  These are 
c    the values before any range checks, or scaling are done.
c    They represent a distribution with mean=0, SD=1.
c    C. R. Meyer -- 4/7/2000.
c
      common /ctap/tap1,tap2,tap3,tap4,tap5,tap6
c
      real tap1,tap2,tap3,tap4,tap5,tap6
c
c     tap1 -- Std. Norm. deviate for Max Temp.
c     tap2 -- Std. Norm. deviate for Min Temp.
c     tap3 -- Std. Norm. deviate for Radiation
c     tap4 -- Std. Norm. deviate for Precip. (has skewness)
c     tap5 -- Std. Norm. deviate for Wind Velocity
c     tap6 -- Std. Norm. deviate for TDP.
