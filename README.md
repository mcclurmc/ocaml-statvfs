Ctypes statvfs bindings for OCaml
=================================

This library provides a simple wrapper to the POSIX statvfs syscall,
and a few higher-level functions to make it easier to find the total,
free, and used bytes on a filesystem.

## TODO

- [ ] Implement mount flags
- [ ] Better ctypes casting from PosixTypes (get rid of Obj.magic!)
