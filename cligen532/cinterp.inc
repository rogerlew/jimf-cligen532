c    File: cinterp.inc
c    Common Block for interpolations.
c
      common /interp/ interp,o_mo,lf,rf,x_bar,c,t,emv,pmt,pmv,xes
      integer interp,o_mo
      real lf,rf,x_bar(14),c(6,14),t(6,14),emv(14,14),pmt(13,14),
     1     pmv(13,14),xes(12,14)
c
c     + + + VARIABLE DEFINITIONS + + +
c     interp - type: 0-none, 1-linear, 2-fourier, 3-yfm
c     xes    - the monthly values (mean, SD, skew) for the 14 values.
c              (Gets set to -9999.0 if this is not a Max or Min month.)
c               1 - Mean P
c               2 - SD P
c               3 - Skew P
c               4 - P(W|W)
c               5 - P(W|D)
c               6 - Mean Tmax
c               7 - Mean Tmin
c               8 - SD Tmax
c               9 - SD Tmin
c              10 - Mean Rad
c              11 - SD Rad
c              12 - Max 0.5 P
c              13 - Dew Pt
c              14 - Time to Peak
c
c  Linear Interpolation:
c     o_mo   - month (on the "other" end) whose average value should be used.
c     lf     - weighting factor for the midpoint value on this month's end
c              of the time interval.
c     rf     - weighting factor for the midpoint value on the "other" end.
c
c  Fourier Series:
c     x_bar  - Mean of seasonal data (X) values.
c     c      - 6 Fourier factors for each of 14 parameters.
c     t      - 6 Fourier factors for each of 14 parameters.
c               1 - Mean P
c               2 - SD P
c               3 - Skew P
c               4 - P(W|W)
c               5 - P(W|D)
c               6 - Mean Tmax
c               7 - Mean Tmin
c               8 - SD Tmax
c               9 - SD Tmin
c              10 - Mean Rad
c              11 - SD Rad
c              12 - Max 0.5 P
c              13 - Dew Pt
c              14 - Time to Peak
c
c  Revised Yoder/Foster Interpolation:
c          PARM(MON,PARM,MIDPT)
c     emv    - End-of-the-Month Value for the monthly mean of each parameter.
c     pmt    - Pseudo-Midpoint Time(s) for each month, for each parameter.
c     pmv    - Pseudo-Midpoint Value(s) for each month, for each parameter.
c               1 - Mean P
c               2 - SD P
c               3 - Skew P
c               4 - P(W|W)
c               5 - P(W|D)
c               6 - Mean Tmax
c               7 - Mean Tmin
c               8 - SD Tmax
c               9 - SD Tmin
c              10 - Mean Rad
c              11 - SD Rad
c              12 - Max 0.5 P
c              13 - Dew Pt
c              14 - Time to Peak
