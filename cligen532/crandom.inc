c    File: crandom.inc
c     Parameters used to check the quality of the Random Numbers generated.
c
      common /random/g_dsum,g_ssum,ranary,g_dimi,g_dimp,mox,dax,
     1        thresh,thres2,vv,fx,z
c
      parameter  (nrparm=9)
      parameter  (nrelem=31)
c ---- nrparm -- number of parameters for which random numbers are generated.
c ---- nrelem -- quantity of random numbers generated together per parameter.
c
      real ranary(nrelem,nrparm),thresh(nrparm),thres2(nrparm),vv,fx,z
      integer mox, dax, g_dimi(12),g_dimp(12)
      double precision g_dsum(nrparm,12)
      double precision g_ssum(nrparm,12)
c      ranary -- array to simplify storing random numbers.
c      mox    -- month last time "ranary" was "loaded".
c      dax    -- current day.
c      thresh -- Threshold on means for re-doing a month's random numbers.
c      thres2 -- Threshold on varinece for re-doing a month's random numbers.
c      g_dimi -- Total days from the beginning of simulation to date.
c      g_dimp -- Total days with precip from the beg. of simulation to date.
c      g_dsum -- Sum of std norm devs, for this month, for this parameter,
c                 from the beginning of the run.
c      g_ssum -- Sum of S^2 of std norm deviates, for this parameter, for this
c                month, from the beginning of the simulation.
c      vv     -- (Random) Probability of precip today.
c      fx     -- (Random) Wind direction.
c      z      -- (Random) Time to Peak.
c
      real vvx(nrelem),fxx(nrelem),zx(nrelem),
     1     v2x(nrelem),v4x(nrelem),v6x(nrelem),v8x(nrelem),
     2     v10x(nrelem),v12x(nrelem)
c
c ---- Equivalence to simplify retrieving random numbers from "ranary".
c     __x(nrelem) -- vector of up to "nrelem" random daily values, 
c                    to be generated one after the other.
c ---- prob of precip today
      equivalence (vvx,ranary(1,1))
c ----- max temp
      equivalence (v2x,ranary(1,2))
c ----- min temp
      equivalence (v4x,ranary(1,3))
c ----- radiation
      equivalence (v6x,ranary(1,4))
c ----- amount, if there is precip
      equivalence (v8x,ranary(1,5))
c ----- wind dir
      equivalence (fxx,ranary(1,6))
c ----- wind velocity
      equivalence (v10x,ranary(1,7))
c ----- temp dew point
      equivalence (v12x,ranary(1,8))
c ----- time to peak
      equivalence (zx,ranary(1,9))
