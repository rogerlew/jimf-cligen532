c    File: cbk4.inc
c
      common /bk4/ nc(13),iyr,nt,mo,px,iopt,dtp(4),dmxi(4)
c
c     nc          - Number of days in the (non-leap) year preceeding each month.
c     iyr         - Set to zero, but not used.
c     nt          - Set to 1 if IYEAR is not a leap year: otherwise, zero.
c                   NOTE: Only set for options 4 and 7, and only for the
c                         *beginning year*!
c     mo          - The current month (1=Jan, 2=Feb...).
c     px          - Never seems to get used -- CRM -- 10/20/99
c     iopt        - Weather Generator Options:
c                    1 - Single Year Simulation - Screen Output
c                    2 - Multiple Year - Screen Output',/,
c                    3 - Multiple Year Simulation - CREAMS - GLEAMS Output File
c                    4 - Selected Single Storm WEPP - Output File
c                    5 - Multiple Year - WEPP Output File
c                    6 - Read Observed P and Temp and Generate Missing Data
c                    7 - Single Design Storm - TR 55 Storm Type WEPP Output File
c                    8 - Exit Weather Generator Program
c
c     dtp         - ??? of each storm type at this station???
c     dmxi        - Values are set -- Never used.
c
