---
draft: false
toc: true
type: docs

title: Library requirements for the EPOCH codes
linktitle: Libraries
menu:
  documentation:
    parent: Basic usage
---

The EPOCH codes are written using MPI for parallelism, but have no other
libraries or dependencies. Currently, the codes are written to only
require MPI1.2 compatible libraries, although this may change to require
full MPI2 compliance in the future. Current versions of both MPICH and
OpenMPI implement the MPI2 standard and are known to work with this
code. The SCALI MPI implementation is only compliant with the MPI1.2
specification and may loose support soon. There are no plans to write a
version of EPOCH which does not require the MPI libraries.

The code is supplied with a standard GNU make Makefile, which is also
compatible with most other forms of the *`*make*`* utility.
In theory it is possible to compile the code without a
*`*make*`* utility, but it is much easier to compile the code
using the supplied makefile.
