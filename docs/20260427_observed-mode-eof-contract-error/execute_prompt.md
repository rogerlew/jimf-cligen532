# Execution Prompt

You are working in `/workdir/jimf-cligen532`.

Resolve the CLIGEN532 observed-mode EOF contract error documented in:

- `docs/20260427_observed-mode-eof-contract-error/README.md`
- `docs/20260427_observed-mode-eof-contract-error/reproduction.md`

## Goal

Fix CLIGEN observed input mode (`-O <prn> -t6 -I2`) so EOF at the end of a valid
observed PRN is handled cleanly. The binary must not perform another sequential
read after the EOF marker, and callers must get a clear success/failure contract.

The triggering case is WEPPcloud run `intriguing-kingmaker`, hillslope `p980`.
WEPP failed only because the generated climate file was truncated. The upstream
CLIGEN contract error is at `cligen532/cligen.f`, currently around the observed
daily read in `day_gen`:

```fortran
moveto = 225
read(9,1000,end=199)irida,itmxg,itmng
```

Direct replay with the captured full PRN/par currently produces a complete
4398-line CLI ending `31 Dec 1991`, but CLIGEN exits rc `2` with:

```text
At line 3070 of file cligen.f (unit = 9, file = 'gridmet_observed_4182_1980-1991.prn')
Fortran runtime error: Sequential READ or WRITE not allowed after EOF marker
```

## Constraints

- Keep the fix tightly scoped. Prefer a minimal `day_gen` EOF-control fix unless
  evidence proves the yearly loop also needs adjustment.
- Preserve the CLIGEN `5.323` behavior for observed inputs with a partial final
  year; do not reintroduce the extra-day bug described in `README.md`.
- Do not edit WEPPpy in this task. WEPPpy hardening is a follow-up after CLIGEN
  has a clean binary contract.
- Do not mutate production run data under `/geodata` or `/wc1`.
- Do not hide errors by suppressing stderr or ignoring return codes. Make EOF a
  normal terminal condition only when it is the expected end of observed input.

## Required Work

1. Reproduce the current failure using the commands in
   `docs/20260427_observed-mode-eof-contract-error/reproduction.md`.
2. Inspect `cligen532/cligen.f` around `day_gen` and the yearly generation loop.
3. Patch the EOF handling so a valid observed-mode PRN exits cleanly after the
   final generated day.
4. Rebuild both binaries:

   ```bash
   cd /workdir/jimf-cligen532/cligen532
   make clean
   make all
   ```

5. Re-run the direct replay with both:
   - `/workdir/jimf-cligen532/cligen532/cligen532`
   - `/workdir/jimf-cligen532/cligen532/cligen532_backtrace`

6. Record validation results in this package, preferably in a new
   `validation.md`.

## Acceptance Criteria

- Captured `gridmet_observed_4182_1980-1991.prn` plus `or354811.par` produces a
  complete CLI ending `31 Dec 1991`.
- `cligen532` exits `0` for the complete observed input case.
- `cligen532_backtrace` exits `0` for the same case.
- stderr contains no Fortran runtime error.
- The output line count remains 4398 for the triggering case unless you can
  justify a different count from the CLI header/data contract.
- A deliberately shortened PRN does not trigger the read-after-EOF runtime
  error; it either exits cleanly with a bounded shorter observed output or fails
  explicitly with a diagnostic.

## Evidence Paths

Use these read-only inputs:

```text
/workdir/wepp-forest/docs/ablation/20260427_intriguing-kingmaker_p980_hillslope_eof-stmget/artifacts/repro/source_wepp1/climate/gridmet_observed_4182_1980-1991.prn
/workdir/wepp-forest/docs/ablation/20260427_intriguing-kingmaker_p980_hillslope_eof-stmget/artifacts/repro/source_wepp1/climate/or354811.par
```

Relevant prior evidence:

```text
/workdir/wepp-forest/docs/ablation/20260427_intriguing-kingmaker_p980_hillslope_eof-stmget/artifacts/logs/C030.cligen.stderr.txt
/workdir/wepp-forest/docs/ablation/20260427_intriguing-kingmaker_p980_hillslope_eof-stmget/artifacts/logs/C030.result.txt
/workdir/wepp-forest/docs/ablation/20260427_intriguing-kingmaker_p980_hillslope_eof-stmget/artifacts/logs/C041.stdout.txt
```

## Handoff

In the final handoff, report:

- exact source lines changed
- build commands run
- replay commands run
- exit codes for optimized and backtrace binaries
- output line count and final CLI date
- whether the shortened-PRN check passed
- any remaining compatibility risk for observed-mode partial-final-year inputs
