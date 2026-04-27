# CLIGEN532 Observed-Mode EOF Contract Error

- `package_id`: `20260427_observed-mode-eof-contract-error`
- `status`: `completed`
- `scope`: `cligen532`, observed input mode (`-O`, `-t6`, `-I2`)
- `created`: `2026-04-27`
- `completed`: `2026-04-27`
- `trigger`: WEPPcloud run `intriguing-kingmaker`, hillslope `p980`

## Problem

CLIGEN532 observed-mode generation can leave the caller with an invalid contract:
it may emit a CLI file and then terminate with a Fortran EOF runtime error, and
the downstream caller may accept a non-empty but truncated CLI as successful
output.

The `intriguing-kingmaker:p980` production failure exposed the contract break:

- source PRN covers `1980-1991` and ends on `31 Dec 1991`
- source CLI has only 523 lines and ends on `22 May 1981`
- WEPP later fails in `stmget` when simulation year 2 reads past EOF
- production log contains
  `cligen run_observed timed out; attempting to terminate/kill.`

The WEPP failure is downstream. The CLIGEN contract error is that observed-mode
generation does not provide a clean, machine-verifiable success/failure boundary
for callers.

## Local Evidence

At package start, the CLIGEN binary vendored by WEPPpy matched this repository's
Linux binary:

```text
3eda6d5d327977bcf677d6b399b6083487391c9c8637cc4ce31bb8bb435ce211  /workdir/jimf-cligen532/cligen532/cligen532
3eda6d5d327977bcf677d6b399b6083487391c9c8637cc4ce31bb8bb435ce211  /workdir/wepppy/wepppy/climates/cligen/bin/cligen532
```

Direct replay against the captured full PRN/par produced a complete 4398-line
CLI ending `31 Dec 1991`, but CLIGEN exited with return code `2`:

```text
At line 3070 of file cligen.f (unit = 9, file = 'gridmet_observed_4182_1980-1991.prn')
Fortran runtime error: Sequential READ or WRITE not allowed after EOF marker, possibly use REWIND or BACKSPACE
```

Relevant source location:

- `cligen532/cligen.f`: observed daily read in `day_gen`, currently at line
  `3070`
- `cligen532/cligen.f`: yearly generation loop calls `day_gen` and stops when
  `moveto == 225`

## Contract To Restore

For observed-mode generation (`-O <prn> -t6 -I2`), CLIGEN532 must satisfy one of
these outcomes:

1. Success:
   - output CLI is complete for the intended observed input span
   - process exits cleanly with status `0`
   - no Fortran runtime error is written

2. Failure:
   - process exits non-zero with an explicit diagnostic
   - any partial CLI is absent, removed, or clearly marked invalid by the caller
   - the caller cannot mistake a non-empty partial CLI for a valid product

Normal EOF after consuming the observed input must not trigger an additional
sequential read after the EOF marker.

## Candidate Fix Direction

The observed read currently sets `moveto = 225` before the read and uses
`end=199`. On EOF, control falls through label `199` with `moveto` still set,
but the daily loop can continue and attempt another read on the same unit after
the EOF marker.

The fix should make EOF a terminal condition for the current observed generation
path before another `read(9,...)` can occur. Candidate approaches:

- branch out of the daily loop immediately on EOF
- set loop state so `ida` cannot continue to another observed read
- preserve the existing partial-final-year behavior fixed in CLIGEN `5.323`

The final patch should keep the scope in `day_gen` unless evidence shows the
yearly loop also needs a contract adjustment.

## Acceptance Criteria

- Replaying the captured `gridmet_observed_4182_1980-1991.prn` and `or354811.par`
  produces a complete CLI ending `31 Dec 1991`
- CLIGEN exits `0` for the complete observed input case
- stderr contains no Fortran runtime error
- a deliberately truncated PRN fails explicitly or produces a clearly bounded
  shorter CLI without a runtime EOF read-after-EOF
- WEPPpy callers can distinguish complete success from partial output

## Outcome

Completed on `2026-04-27`.

- `day_gen` now exits the daily loop immediately when observed-mode EOF leaves
  `moveto == 225`, preventing any follow-up sequential read from unit 9 after
  the EOF marker.
- The main end-of-run block treats `moveto == 225` as terminal and closes unit 7
  without appending the normal trailing blank record.
- `make clean && make all` rebuilt `cligen532` and `cligen532_backtrace`.
- The captured `1980-1991` PRN replays to a 4398-line CLI ending
  `31 Dec 1991` with exit code `0` for both binaries and no Fortran runtime EOF
  error.
- A deliberately shortened PRN produces bounded shorter output ending at the
  last observed row without a read-after-EOF runtime error.

Detailed command evidence and replay results are recorded in `validation.md`.

## Evidence References

- WEPP ablation package:
  `/workdir/wepp-forest/docs/ablation/20260427_intriguing-kingmaker_p980_hillslope_eof-stmget/`
- Production source climate log captured there:
  `artifacts/logs/source_wepp1/climate/cligen_gridmet_observed_4182_1980-1991.log`
- Direct CLIGEN replay stderr captured there:
  `artifacts/logs/C030.cligen.stderr.txt`
- Passing WEPP repair-control replay:
  `artifacts/logs/C041.stdout.txt`
