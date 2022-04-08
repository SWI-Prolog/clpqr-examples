# CLP(QR) examples

This          repo          complements            [clp(qr)          for
SWI-Prolog](https://github.com/SWI-Prolog/packages-clpqr).   It    holds
examples by the original author  of   clp(QR),  Christian Holzbauer from
FOAI, Vienna that have been copied  from   a  SICStus Prolog version and
adapted for SWI-Prolog. The changes are minimal:

  - Include a `:- use_module(library(clpq))` or `:- use_module(library(clpr))`
  - Removed some dead code
  - In `root.pl` we have deleted the printing because printing rational
    numbers as decimal with a specified number of digits is provided by
    SWI-Prolog's.
  - Added test_clpr.pl and test_clpq.pl.  These are SWI-Prolog unit
    test files that test most of the examples.

## Legal

The legal status of these files is   rather  unclear. They belong to the
original clp(QR) system, copyright 1992-1995 Austrian Research Institute
for  Artificial  Intelligence  (OFAI).  They    appear  in  the  SICStus
distribution and the (publically accessible) "Prolog Commons" initiative
files. The original clp(QR) system was ported to SWI-Prolog by Leslie De
Koninck  (K.U.Leuven).  For  this  port    we   obtained  permission  to
redistribute  it  under  the  previous   SWI-Prolog  license  conditions
(GPL+clause that allows use in proprietary   code). These files were not
part of this port.

Please contact us (for example using the   issues of this repository) if
publishing this is not permitted or (better)  if you have information or
contacts that can help resolving this.  Ideally   we  would like to have
access to this code under a license  that is compatible with open source
such that we can bundle this code  as   part  of SWI-Prolog for demo and
testing purposes.

