# Makefile for building cligen532 using gfortran with debug and checks

FC_GNU       = gfortran
FFLAGS_GNU = -c -O3 -march=x86-64 -funroll-loops -fno-math-errno -fno-trapping-math -fno-signaling-nans -ffinite-math-only -fno-protect-parens -fno-stack-protector
#   -O3: Aggressive optimization
#   -march=x86-64: Don't use CPU-specific instructions
#   -funroll-loops: Speed-up tight loops
#   -fno-math-errno, -fno-trapping-math: Avoid unnecessary libc calls for math
#   -ffinite-math-only: Skip checks for NaNs/Infs if you know inputs are sane
#   -fno-signaling-nans: Avoid edge-case traps
#   -fno-protect-parens: Allow expression reordering
#   -fno-stack-protector: Reduces overhead if you’re not worried about buffer overflows

LINK_GNU     = gfortran
LDFLAGS      = -g -fbacktrace

PROGRAM      = cligen532
FFLAGS       = $(FFLAGS_GNU)
FC           = $(FC_GNU)
LINKER       = $(LINK_GNU)

DEST         = .
.SUFFIXES: .for .o
MAKEFILE     = Makefile
OBJS         = cligen.o
SRCS         = cligen.f

.for.o:
	$(FC) $(FFLAGS) $<

$(PROGRAM): $(OBJS) $(LIBS)
	$(LINKER) $(LDFLAGS) $(OBJS) -o $(PROGRAM)

all: $(PROGRAM)

clean:
	rm -f $(OBJS)
	rm -f $(PROGRAM)