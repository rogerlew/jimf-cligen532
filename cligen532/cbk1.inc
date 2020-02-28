c    FILE: cbk1.inc
c
      common /bk1/ wvl(16,4,12),dir(12,17),wv,th,pi2,ang,rh(12),
     1             tdp,calm(12)
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
c     wv          - Wind Velocity (m/sec)
c     th          - Wind Direction (radians from North)
c     pi2         - Pi * 2; ie, a full rotation.
c     ang         - Never seems to get used -- CRM -- 10/20/99
c     rh          - Avg Monthly Dew Point Temperature.  Used to calculate TDP.
c     tdp         - Generated dewpoint temperature (C).
c     calm        - % time air is calm (by month).
c                   Calm is treated separately [from WVL] as direction 0,
c                   speed 0.  Only a % time value is need for calm generation.
c
