          ----------------------------------------------------
          * * * A NOTE OF CAUTION CONCERNING PHYSMEM.SYS * * *
          ----------------------------------------------------

The device driver PHYSMEM.SYS provided with this package is, in one sense,
a "gateway" by which the normal memory protection mechanisms used by OS/2
can be bypassed. The selectors provided by version 1.11 of PHYSMEM correspond
to descriptors in the calling program's LDT marked as "readable" and
"executable" but not "writable". The access types for the selectors created
with the PhysToUVirt call (see PHYSMEM.ASM) must be one of two possible
combinations: read/write or read/execute. This is determined by the value
passed to PhysToUVirt in the DH register (0=r/w, 1=r/e). Read/execute is
the one used here, since it is the safer of the two (RamScope does not write
to the segment thus obtained).

There is no method provided for verifying the identity of the program which
calls the PHYSMEM device driver; it could be called by any program which
"knows" of its existence. With the current version, a "badly behaved" program
could not write anywhere in memory, but it would be possible for it to perform
a far call or jump to anywhere, thus precipitating a probable system crash.

It is of course possible to modify PHYSMEM.ASM and re-assemble it to produce
a version which returns selectors to read/write segments. A driver thus
created would, as before, be accessible to all programs in the system,
giving "badly behaved" programs a means to trash anything in memory,
including modifying other programs' code segments, system descriptor tables,
etc.

This is not to say that PHYSMEM will cause problems, since it is rather
unlikely that any programs will "accidentally" open a device by this name
and then make the specific IOCtl calls required to use it. However, a
deliberately malicious program, OR a legitimate one under development which
uses PHYSMEM, could wreak havoc.

All this is nothing new, since any program running in real mode in the DOS
compatibility box could do this also, but it is best that users of PHYSMEM
be aware of this different type of "loophole" in the memory protection.
