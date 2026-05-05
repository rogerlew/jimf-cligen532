# CLIGEN Source Contributors

This file is a source-level contributor record for CLIGEN as it exists in this
repository (CLIGEN 5.32.3). It is reconstructed from `Author(s)`,
`Recoded by`, `Modified by`, `Written by`, `Re-written by`, and dated
annotation lines in the FORTRAN source (`cligen532/cligen.f`, ~7,657 lines)
and its include files (`cligen532/*.inc`), the version-history changelog at
the top of `cligen.f`, the supporting `db/README.md` station-database notes,
and Charles R. Meyer's "General Description of the CLIGEN Model and its
History" published by USDA-ARS-NSERL.

The intent is **completeness** and **correctness**. Contributors who appeared
in the source as initials or as bare surnames have been expanded to full names
where the expansion is confidently sourced; expansions still requiring
verification are flagged at the bottom of this file.

CLIGEN is the stochastic weather generator that supplies WEPP and WEPS with
synthetic climate input. It originated at USDA-ARS Durant, Oklahoma in the
1980s and has been maintained since by contributors at USDA-ARS NSERL (West
Lafayette), USDA-ARS WERU (Fort Collins), USDA Forest Service (Moscow ID),
Griffith University (Australia), Washington State University, the University
of Idaho, and others.

This file is in the public domain along with the rest of the source.

## Contributors

Listed alphabetically by surname. Roles are summarized from source
attribution and from the published CLIGEN history note.

| Name | Role / scope of contribution |
| --- | --- |
| **Barry W. Brown** | Modified `IPMPAR` to return double-precision machine constants as part of converting `BRATIO` to double precision (statistical/numerical helper code, originally from Naval Surface Warfare Center heritage). |
| **William J. Elliot** | Listed indirectly via WEPP/CLIGEN cross-pollination; not directly attributed in source. *(verification needed; see below)* |
| **George Foster** | Modified Daniel Yoder's pseudo-midpoint interpolation algorithm in response to C. R. Meyer's request for a solution covering local maxima and minima (August 2000). |
| **Fred Fox** (USDA-ARS WERU, Fort Collins) | Author of CLIGEN 5.30002 (09/14/2009): extended command-line argument string length for longer Linux output paths; author of CLIGEN 5.322 (09/10/2024): corrected Coefficient of Variation calculation to avoid divide-by-zero with zero monthly average temperature, merged from WEPS 5.3004 (12/13/2018); cited by C. R. Meyer for spotting Lahey-compiler subscript-out-of-range error in 5.22561 (with Larry Wagner) and uninitialized `NDFLAG` in 5.22563. |
| **Jim Frankenberger** (USDA-ARS NSERL) | Author of CLIGEN 5.3 (01/15/2008): corrected dew-point calculation for type-6 runs; CLIGEN 5.31 (01/31/2013): corrected solar radiation to use std-dev from input `.par` file, extended year field to 5 digits for 10,000+ year runs; CLIGEN 5.32 (03/14/2013): made type-6 input produce same `tpeak` distribution as generated precip; inline `jrf` annotations in `cligen.f` (e.g., `1/14/2008` dew-point insert). Listed as author of source/executable provided to Roger Lew at University of Idaho. |
| **Gemini AI** (Google) | Co-author of CLIGEN 5.323 (09/13/2025) with Roger Lew: corrected the `day_gen` subroutine's end-of-file handling so observed climate file generation (`-O` option) does not append an extra day when the input `.prn` contains a partial final year. |
| **Gene Gander** (USDA-ARS, Durant, Oklahoma) | Co-original author of CLIGEN with Arlin D. Nicks. Last significant changes to the model in the mid-1990s before Dr. Nicks' retirement and death in July 1997. *(per Meyer 1999 CLIGEN history note)* |
| **David Hall** (USDA Forest Service, Moscow, Idaho) | Provided "MANY variable definitions" during Charles R. Meyer's August–November 1999 recoding; co-acquired CLIGEN code from Dr. Nicks' computer after Nicks' retirement and death, mechanically cleaned the existing station data files, and added a large number of new U.S. station files. |
| **Roger Lew** (University of Idaho) | Author of CLIGEN 5.32.1 (04/06/2022): leap-year fix for years divisible by 100 but not by 400; co-author of CLIGEN 5.323 (09/13/2025) with Gemini AI; current source/executable maintainer for this vendored copy; commit history holder of the repository as published. |
| **Charles R. Meyer** ("Chuck Meyer", USDA-ARS NSERL, West Lafayette) | **Principal recoder of CLIGEN, August–November 1999.** Recoded the source from prior form into the WEPP F-77 coding conventions; produced CLIGEN V-4.2 (verified to give bit-identical results to the original) and CLIGEN 5.x series. Discovered that CLIGEN's uniform random number generator and standard normal generator were not operating correctly; introduced a quality-control framework borrowed from industrial engineering to filter generator outputs against acceptance distributions. Author of CLIGEN versions 5.101 (02/06/2001) through 5.22564 (10/26/2004) — 27 documented version increments over four years addressing storm-duration math, peak-intensity Gamma deviates, K-S quality control, Chi-square testing, Pearson Type-III precipitation skew limits, command-line argument handling, file-I/O robustness on multiple compilers, and dozens of smaller fixes. Author of `lintrp` (linear interpolation routine, 12/02/1999, in consultation with Roel Vining). Author of `e_chi` Chi-square test code (05/05/2004). Author of monthly-mean-preserving interpolation implementation (08/04/2000, implementing Daniel Yoder's algorithm). Author of "General Description of the CLIGEN Model and its History" (USDA-ARS-NSERL, signed "Chuck Meyer"). |
| **Alfred H. Morris, Jr.** (Naval Surface Weapons Center / Naval Surface Warfare Center, Dahlgren, Virginia) | Original author of `BRATIO`, `IPMPAR`, and related statistical/numerical helper routines used in CLIGEN's distribution-handling code. |
| **Arlin D. Nicks** (USDA-ARS, Durant, Oklahoma; deceased July 1997) | Co-original author of CLIGEN with Gene Gander. Last significant changes to the model in the mid-1990s. |
| **Clarence Richardson** | Original author of the Fourier-series subroutines `fouri1` and `fouri2` used for seasonal-variable interpolation (code received by C. R. Meyer 01/21/1999, then modified by Meyer with verified-identical results). |
| **Bill Rust** | Author of CLIGEN 5.30001 (06/30/2009): corrected the order of reading wet/wet and wet/dry equivalence values, where an `equivalence` statement had interleaved them; affected Yoder-Foster and Fourier interpolation. |
| **Dayna Scheele** (USDA Forest Service, Moscow, Idaho) | Post-Nicks station-file maintenance and expansion, alongside David Hall. *(per Meyer 1999 CLIGEN history note)* |
| **Anurag Srivastava** (USDA-ARS NSERL) | Updated U.S. station database files (2015) per `db/README.md`. *(Same person as the WEPP source `CAS` annotator.)* |
| **Roel Vining** | Consulted on the linear interpolation routine `lintrp` written by C. R. Meyer (12/02/1999). |
| **Larry Wagner** (USDA-ARS WERU, Fort Collins) | Cited by C. R. Meyer for spotting the Lahey-compiler subscript-out-of-range error in `SR RYF1` that produced CLIGEN 5.22561 (09/30/2004), and the uninitialized `NDFLAG` issue addressed in 5.22563 (10/21/2004). |
| **Daniel Yoder** | Original developer of the pseudo-midpoint interpolation algorithm for monthly-mean-preserving interpolation (implemented by C. R. Meyer 08/04/2000; modified by George Foster). |
| **Bofu Yu** (Griffith University, Australia) | Re-wrote two subroutines (06/30/1999 and 07/04/1999) to correct CLIGEN's rainfall-intensity unit-conversion error and to make rainfall intensity responsive to latitude (replacing routines `ALPH` and `R5MON` and altering constants `DUR` in `DAY_GEN` and `XN1` in `DSTG`). One of his corrections was carried forward in C. R. Meyer's 11/08/2000 release. |

## Annotation tag legend

The legacy source uses author initials and short tags to attribute
modifications. Resolved attributions:

| Tag | Person |
| --- | --- |
| `CRM` | Charles R. Meyer |
| `jrf` | Jim Frankenberger |
| `B. YU` / `B.YU` | Bofu Yu |
| `XXX` (CRM era) | Marker C. R. Meyer used to flag questionable lines during recoding (per the explicit note: *"Note that questionable lines contain the string 'XXX'"*) |

## CLIGEN version history (by attribution)

The version-history block at the top of `cligen.f` is exhaustively dated and
attributed. This table summarizes the lineage:

| Version | Date | Author(s) | Summary |
| --- | --- | --- | --- |
| original through ~mid-1990s | — | Arlin D. Nicks; Gene Gander | CLIGEN at USDA-ARS Durant, OK |
| post-Nicks station files | 1997+ | David Hall; Dayna Scheele | Station-data acquisition, cleanup, U.S. station expansion (USDA Forest Service Moscow, ID) |
| rainfall-intensity / latitude corrections | 06/30/1999, 07/04/1999 | Bofu Yu | Subroutine re-writes (`ALPH`, `R5MON`, etc.) |
| **V-4.2 recoded** | Aug–Nov 1999 | Charles R. Meyer (with David Hall variable definitions) | Radical recoding to WEPP F-77 conventions; verified bit-identical to original |
| 5.1, 5.101 – 5.22564 | 2001–2004 | Charles R. Meyer | Quality-control framework, RNG fixes, K-S testing, Chi-square testing, storm duration / peak intensity, Pearson skew limits, file I/O, command-line, compiler portability |
| 5.3 | 01/15/2008 | Jim Frankenberger | Dew-point calculation for type-6 runs |
| 5.30001 | 06/30/2009 | Bill Rust | wet/wet and wet/dry read-order correction |
| 5.30002 | 09/14/2009 | Fred Fox | Extended command-line argument length |
| 5.31 | 01/31/2013 | Jim Frankenberger | Solar radiation std-dev correction; 5-digit year field |
| 5.32 | 03/14/2013 | Jim Frankenberger | type-6 `tpeak` distribution fix |
| 5.32.1 | 04/06/2022 | Roger Lew | Leap-year fix (years divisible by 400 but not 100) |
| 5.322 | 09/10/2024 | Fred Fox | Coefficient of Variation divide-by-zero fix (merged from WEPS 5.3004) |
| 5.323 | 09/13/2025 | Roger Lew; Gemini AI | observed climate `-O` extra-day fix in `day_gen` EOF handling |

## External code components

CLIGEN includes statistical/numerical helper code with non-CLIGEN authorship:

- `BRATIO`, `IPMPAR`, and related routines — **Alfred H. Morris, Jr.** (Naval Surface Weapons Center, Dahlgren, VA), with **Barry W. Brown** modifications for double precision.
- WERU command-line argument support modules (for Lahey compiler) — Modified `7/11/2000 by C. R. Meyer to be more generic`.

Numerical Recipes equivalents are referenced for the Chi-square test code but
**not used in source** in this version due to licensing; the public-domain ACM
code is appended to the end of `cligen.f` instead.

## Cited science (NOT CLIGEN code contributors)

The source cites external publications and equations from non-CLIGEN
authors. They are recorded here for reference traceability only and are
**not** contributors to this codebase.

- Wischmeier (USLE, cited via WEPP heritage)
- Norman J. Rosenberg et al. (1983)
- Mein and Larson (Green-Ampt infiltration form)
- Griffiths and Driscoll, *Survey of Climatology* (1982; cited as a recommended reference for international users)
- Zhang and Garbrecht — *"Evaluation of CLIGEN precipitation parameters and their implication on WEPP runoff and erosion prediction"*, Trans. ASAE Vol. 46(2):311-320 (cited as feedback that motivated C. R. Meyer's 5.212/5.213 storm-duration and peak-intensity work)
- Johnson et al. — paper referenced by C. R. Meyer in his 1999 history note as reporting effects consistent with the random-number-generator defect he later discovered

## Related WEPP source contributor record

Many CLIGEN contributors also appear in the WEPP source attribution record.
The companion record for WEPP (in the `wepp-forest` repository) is the
reference document for WEPP-side attribution. Contributors who span both
codebases:

- Charles R. Meyer (principal recoder of WEPP and of CLIGEN)
- Jim Frankenberger
- Anurag Srivastava
- Roger Lew

## Names requiring verification

The following items need verification by someone with direct project
knowledge before this file is treated as authoritative:

- **William J. Elliot** — Listed as a possible CLIGEN contributor by association with WEPP/CLIGEN cross-pollination, but no direct attribution appears in `cligen.f`. Should likely be removed unless a specific CLIGEN contribution is identified.
- **Bill Rust** — Affiliation not stated in source. Likely USDA-ARS WERU given the timing and the overlap with Fox's WERU work, but unconfirmed.
- **Daniel Yoder** — Affiliation not stated in source (likely University of Tennessee, given his published WEPP-related work, but unconfirmed in this codebase).
- **Roel Vining** — Affiliation not stated in source.
- **Clarence Richardson** — Affiliation not stated in source. Almost certainly the same Clarence W. Richardson who developed the original WGEN weather generator at USDA-ARS Temple, TX (CLIGEN's structural ancestor), but the explicit attribution in this codebase does not state that.
- **George Foster** — Almost certainly G. R. Foster (also a WEPP contributor), but the CLIGEN-side attribution does not include his initials.

## Notes on record completeness

This contributor record is reconstructed from source-code attribution and
from Charles R. Meyer's published 1999 CLIGEN history note. It does not
capture institutional affiliations beyond what the sources explicitly record.
It does not capture contributions to station-file generation or to the
international station library beyond what is named in `db/README.md`. It
does not capture funding-agency contributions, peer review, or validation
work performed outside the code itself.

## Maintenance

When adding new contributors, update the table above, the annotation tag
legend (if a new tag is introduced), the version history, and the
names-requiring-verification section if any uncertainty remains.

The CLIGEN documentation home is currently:
http://fargo.nserl.purdue.edu/cligen532/
