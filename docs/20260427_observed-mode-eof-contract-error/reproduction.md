# Reproduction Notes

These commands use the evidence captured from the WEPP `intriguing-kingmaker`
ablation package. They are intended to reproduce the CLIGEN contract error
without mutating production run data.

## Inputs

```text
/workdir/wepp-forest/docs/ablation/20260427_intriguing-kingmaker_p980_hillslope_eof-stmget/artifacts/repro/source_wepp1/climate/gridmet_observed_4182_1980-1991.prn
/workdir/wepp-forest/docs/ablation/20260427_intriguing-kingmaker_p980_hillslope_eof-stmget/artifacts/repro/source_wepp1/climate/or354811.par
```

The PRN has 4383 lines and ends on `31 Dec 1991`.

## Direct CLIGEN Replay

```bash
work=/tmp/cligen532_observed_eof_repro
rm -rf "$work"
mkdir -p "$work"
cp /workdir/wepp-forest/docs/ablation/20260427_intriguing-kingmaker_p980_hillslope_eof-stmget/artifacts/repro/source_wepp1/climate/gridmet_observed_4182_1980-1991.prn "$work"/
cp /workdir/wepp-forest/docs/ablation/20260427_intriguing-kingmaker_p980_hillslope_eof-stmget/artifacts/repro/source_wepp1/climate/or354811.par "$work"/
cd "$work"

timeout 180 /workdir/jimf-cligen532/cligen532/cligen532 \
  -ior354811.par \
  -Ogridmet_observed_4182_1980-1991.prn \
  -ogridmet_observed_4182_1980-1991.cli \
  -t6 \
  -I2 \
  > stdout.txt \
  2> stderr.txt
echo "rc=$?"
wc -l gridmet_observed_4182_1980-1991.cli
tail -n 8 gridmet_observed_4182_1980-1991.cli
tail -n 40 stderr.txt
```

## Current Result

Observed during the `20260427` incident campaign:

- CLI output line count: `4398`
- last output date: `31 Dec 1991`
- process exit code: `2`
- stderr signature:

```text
At line 3070 of file cligen.f (unit = 9, file = 'gridmet_observed_4182_1980-1991.prn')
Fortran runtime error: Sequential READ or WRITE not allowed after EOF marker, possibly use REWIND or BACKSPACE
```

## Expected Result After Fix

- CLI output remains complete for `1980-1991`
- process exit code is `0`
- stderr has no Fortran runtime error
- EOF at the end of observed input terminates generation cleanly
