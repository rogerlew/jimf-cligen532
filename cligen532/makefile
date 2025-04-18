# makefile for cligen532 with gfortran
# Compiler
FC_GNU = gfortran
FC     = $(FC_GNU)
LINKER = $(FC_GNU)

# Sources
SRCS   = cligen.f

# Objects
OBJS_OPT       = $(SRCS:.f=.o)
OBJS_BACKTRACE = $(SRCS:.f=.debug.o)

# Flags
FFLAGS_OPT       = -c -O3 -march=x86-64 -funroll-loops -fno-math-errno -fno-trapping-math -fno-signaling-nans -ffinite-math-only -fno-protect-parens -fno-stack-protector
#   -O3: Aggressive optimization
#   -march=x86-64: Don't use CPU-specific instructions
#   -funroll-loops: Speed-up tight loops
#   -fno-math-errno, -fno-trapping-math: Avoid unnecessary libc calls for math
#   -ffinite-math-only: Skip checks for NaNs/Infs if you know inputs are sane
#   -fno-signaling-nans: Avoid edge-case traps
#   -fno-protect-parens: Allow expression reordering
#   -fno-stack-protector: Reduces overhead if you’re not worried about buffer overflows

FFLAGS_BACKTRACE = -c -g -O0
#   -g Debuggable, accurate stacktraces
#   -O0: No optimiation

# Targets
all: cligen532 cligen532_backtrace

cligen532: $(OBJS_OPT)
	$(LINKER) $(OBJS_OPT) -o cligen532

cligen532_backtrace: $(OBJS_BACKTRACE)
	$(LINKER) -g -fbacktrace $(OBJS_BACKTRACE) -o cligen532_backtrace

# Compile rules
%.o: %.f
	$(FC) $(FFLAGS_OPT) $< -o $@

%.debug.o: %.f
	$(FC) $(FFLAGS_BACKTRACE) $< -o $@

# Clean up
clean:
	rm -f $(OBJS_OPT) $(OBJS_BACKTRACE)
	rm -f cligen532 cligen532_backtrace

