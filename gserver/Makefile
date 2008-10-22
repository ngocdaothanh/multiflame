MODULES   = foo bar baz

BEAMS     = $(MODULES:%=%.beam)

BINDIR    = ebin
SRCDIR    = src
INCDIR    = include
VPATH     = $(BINDIR):$(SRCDIR):$(INCDIR)

ERL       = erl
ERLC      = erlc
ERLCFLAGS = -W -smp

all: $(BEAMS)

%.beam : %.erl %.hrl
  $(ERLC) -b beam $(ERLCFLAGS) -I $(INCDIR) -o $(BINDIR) $< .PHONY: clean

clean:
  rm -rf $(BINDIR)/*.beam $(INCDIR)/*~ $(SRCDIR)/*~ *~
