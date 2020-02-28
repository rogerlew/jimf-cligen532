c    File: cbk7
c
C     common /bk7/ rst(12,3),t0(5),prw(2,12),obmx(12),obmn(12),
C    1             obsl(12),wft(12),subp(5),sno(5),snoev,k1(4),
C    1             k2(4),cvs(12),k3(4),k4(4),k5(4),k6(4),k7(4),
C    1             k8(4),k9(4),k10(4),cvtx(12),cvtm(12),v1,v3,v5,v7,v9,
C    1             v11,ra,tmxg,tmng,rmx,yls,ylc,pit,st0,amp,avt,ts,
C    1             nsim,msim,l,tmx(5),tmn(5),tx(5),tb(5),to(5),
C    1             dmm(12,5),dm1(5),stdtx(12),stdtm(12),stdsl(12)
      common /bk7/ rst(12,3), prw(12,2),obmx(12),obmn(12),obsl(12),
     1             k1(4),k2(4),cvs(12),k3(4),k4(4),k5(4),k6(4),k7(4),
     1             k8(4),k9(4),k10(4),cvtx(12),cvtm(12),v1,v3,v5,v7,v9,
     1             v11,ra,tmxg,tmng,rmx,yls,ylc,pit,nsim,msim,l,
     1             stdtx(12),stdtm(12),stdsl(12)
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
c     t0          - Never seems to get used -- CRM -- 10/20/99
c     prw(1,12)   - monthly probability of wet day after wet day
c     prw(2,12)   - monthly probability of wet day after dry day
c     obmx        - Observed monthly average Maximum Temperature
c     obmn        - Observed monthly average Minimum Temperature
c     obsl        - Observed mean daily solar radiation (Langleys) (by month)
c     wft         - Never seems to get used -- CRM -- 10/20/99
c     subp        - Never seems to get used -- CRM -- 10/20/99
c     sno         - Never seems to get used -- CRM -- 10/20/99
c     snoev       - Never seems to get used -- CRM -- 10/20/99
c     k1          - Seed for random number generation.
c     k2          - Seed for random number generation.
c     cvs         - Coefficient of Variation of Solar Radiation (by month)
c     k3          - Seed for random number generation.
c     k4          - Seed for random number generation.
c     k5          - Seed for random number generation.
c     k6          - Seed for random number generation.
c     k7          - Seed for random number generation.
c     k8          - Seed for random number generation.
c     k9          - Seed for random number generation.
c     k10         - Seed for random number generation.
c     cvtx        - Coefficient of Variation of Maximum Temperature (by month)
c     cvtm        - Coefficient of Variation of Minimum Temperature (by month)
c     v1          - A random number (?) used to generate TDP & XLV.
c     v3          - A random number (?) used to generate TMNG.
c     v5          - A random number (?) used to generate RA.
c     v7          - A random number (?) used to generate XLV.
c     v9          - Another random number (?) used to generate XLV.
c     v11         - Another random number (?) used to generate TDP.
c     ra          - Generated radiation?  RADG receives RA's value, and is output.
c     tmxg        - Generated daily max temp.
c     tmng        - Generated daily min temp.
c     rmx         - Maximum possible solar radiation.
c     yls         - ??? -- Used to compute CH and YS. sin(ylt/clt) sin(latitude)
c     ylc         - ??? -- Used to compute CH and YC. cos(ylt/clt) cos(latitude)
c     pit         - ??? -- Used to compute SD.  Defined as pit=58.13
c     st0         - Never seems to get used -- CRM -- 10/20/99
c     amp         - Never seems to get used -- CRM -- 10/21/99
c     avt         - Never seems to get used -- CRM -- 10/21/99
c     ts          - Never seems to get used -- CRM -- 10/21/99
c     nsim        - ??? Has value zero or one.  Used as a Switch.
c     msim        - ??? Has value zero or one.  Used as a Switch.
c     l           - Set to either 1 or 2; linked to nsim 0 or 1; selects PRW.
c     tmx         - Never seems to get used -- CRM -- 10/21/99
c     tmn         - Never seems to get used -- CRM -- 10/21/99
c     tx          - Never seems to get used -- CRM -- 10/21/99
c     tb          - Never seems to get used -- CRM -- 10/21/99
c     to          - Never seems to get used -- CRM -- 10/21/99
c     dmm         - Never seems to get used -- CRM -- 10/21/99
c     dm1         - Never seems to get used -- CRM -- 10/21/99
c     stdtx       - Standard deviation of daily max. temp. for the month.
c     stdtm       - Standard deviation of daily min. temp. for the month.
c     stdsl       - Std. deviation of daily solar radiation for the month.
c
