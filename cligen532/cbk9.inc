c    File: cbk9.inc
c
C     common /bk9/ hyd(250,5),xi(2,5),wi(12),tc(6),al(5),ab,ab1,rn1,r1,
C    1             pr,lu1,vo,nmb,tp,tr,ddt,tt
      common /bk9/ xi(2,5),wi(12),ab,ab1,rn1,r1
c     hyd         - Never seems to get used -- CRM -- 10/21/99
c     xi          - Used to calculate SD.
c     wi          - Average Maximum .5 Hour Precip. Intensity (by month)
c     tc          - Never seems to get used -- CRM -- 10/21/99
c     al          - Never seems to get used -- CRM -- 10/21/99
c     ab          - Set to 0.02083, and used to calculate AI
c     ab1         - Set to 1.0-ab, and used to calculate AI
c     rn1         - ??? -- used for precip gamma dsn
c     r1          - alpha_0.5 -- "the ratio of the maximum 30-min rainfall 
c                     to total rainfall for individual events" [Yu's A];
c                     a dimensionless parameter that expresses the fraction 
c                     of total rainfall that occurs during 0.5 hr.
c                     Nrmd(r1) returns the standard normal deviate for a
c                     given probability value (e.g. r1=.99 returns r1=2.328)
c     pr          - Never seems to get used -- CRM -- 10/21/99
c     lu1         - Never seems to get used -- CRM -- 10/21/99
c     vo          - Never seems to get used -- CRM -- 10/21/99
c     nmb         - Never seems to get used -- CRM -- 10/21/99
c     tp          - Never seems to get used -- CRM -- 10/21/99
c     tr          - Never seems to get used -- CRM -- 10/21/99
c     ddt         - Never seems to get used -- CRM -- 10/21/99
c     tt          - Never seems to get used -- CRM -- 10/21/99
c
