# Validation

Date: 2026-04-27

## Source Changes

Changed `cligen532/cligen.f`:

- line 941: treat `moveto=225` as a terminal condition in the main end-of-run block.
- line 978: close unit 7 directly for `moveto=225` without appending the normal trailing blank record.
- line 3074: after observed-mode EOF leaves `moveto=225`, branch out of the daily loop before another read on unit 9.
- line 3184: added the `181` loop-exit label before the existing daily-loop epilogue.

## Baseline Reproduction

Command source: `reproduction.md`, run in `/tmp/cligen532_observed_eof_repro_before`.

Result before patch:

- binary: `/workdir/jimf-cligen532/cligen532/cligen532`
- exit code: `2`
- CLI line count: `4398`
- final CLI date: `31 Dec 1991`
- stderr contained:
  `Fortran runtime error: Sequential READ or WRITE not allowed after EOF marker`

## Build

Commands run:

```bash
cd /workdir/jimf-cligen532/cligen532
make clean
make all
```

Result: build exited `0` and produced both `cligen532` and
`cligen532_backtrace`. The compiler emitted existing legacy Fortran feature
warnings.

## Full Observed Replay

Input files:

```text
/workdir/wepp-forest/docs/ablation/20260427_intriguing-kingmaker_p980_hillslope_eof-stmget/artifacts/repro/source_wepp1/climate/gridmet_observed_4182_1980-1991.prn
/workdir/wepp-forest/docs/ablation/20260427_intriguing-kingmaker_p980_hillslope_eof-stmget/artifacts/repro/source_wepp1/climate/or354811.par
```

Replay command shape:

```bash
timeout 180 <binary> \
  -ior354811.par \
  -Ogridmet_observed_4182_1980-1991.prn \
  -ogridmet_observed_4182_1980-1991.cli \
  -t6 \
  -I2 \
  > stdout.txt \
  2> stderr.txt
```

Results:

| Binary | Temp workdir | Exit code | CLI lines | Final CLI date | Runtime EOF error |
| --- | --- | ---: | ---: | --- | --- |
| `/workdir/jimf-cligen532/cligen532/cligen532` | `/tmp/cligen532_observed_eof_repro_after_opt` | `0` | `4398` | `31 Dec 1991` | no |
| `/workdir/jimf-cligen532/cligen532/cligen532_backtrace` | `/tmp/cligen532_observed_eof_repro_after_backtrace` | `0` | `4398` | `31 Dec 1991` | no |

Stderr contains `STOP Normal program termination`. The optimized binary also
reports `IEEE_DENORMAL`; neither stderr contains the Fortran runtime EOF error,
`Sequential READ`, or `Error termination`.

The optimized output matches the pre-patch CLI content exactly; the only
pre-patch failure was the process contract after the final generated day.

## Shortened PRN Check

Created a deliberately shortened PRN by keeping the first `4373` rows, removing
the final 10 observed days. The shortened input ends on `12 21 1991`.

Results:

| Binary | Temp workdir | Exit code | PRN rows | CLI lines | Final CLI date | Runtime EOF error |
| --- | --- | ---: | ---: | ---: | --- | --- |
| `/workdir/jimf-cligen532/cligen532/cligen532` | `/tmp/cligen532_observed_eof_repro_short_opt` | `0` | `4373` | `4388` | `21 Dec 1991` | no |
| `/workdir/jimf-cligen532/cligen532/cligen532_backtrace` | `/tmp/cligen532_observed_eof_repro_short_backtrace` | `0` | `4373` | `4388` | `21 Dec 1991` | no |

This preserves the CLIGEN 5.323 partial-final-year behavior: EOF bounds the
observed output at the last available observed row and does not append an extra
generated day.

## Remaining Compatibility Risk

Observed-mode EOF is still treated as a clean terminal condition, including for
partial final years. Callers that require a specific requested span must validate
the produced CLI line count or final date themselves; CLIGEN now provides a
clean process contract and no longer fails with a read-after-EOF runtime error.
