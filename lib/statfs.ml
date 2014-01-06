module Array_std = Array

open Ctypes
open PosixTypes
open Foreign

module type STATFS =
  sig
    type fsid
    type statfs
    val statfs : string -> statfs
    val fstatfs : Unix.file_descr -> statfs
  end

module type STATVFS =
  sig
    type mount_flag = ST_RDONLY | ST_NOSUID
    type statvfs = {
      f_bsize   : int64;
      f_frsize  : int64;
      f_blocks  : int64;
      f_bfree   : int64;
      f_bavail  : int64;
      f_files   : int64;
      f_ffree   : int64;
      f_favail  : int64;
      f_flag    : mount_flag list;
      f_namemax : int64;
    }
    val statvfs  : string -> statvfs
    val fstatvfs : Unix.file_descr -> statvfs
  end

module Posix = struct

  type mount_flag = ST_RDONLY | ST_NOSUID

  type statvfs = {
    f_bsize   : int64;
    f_frsize  : int64;
    f_blocks  : int64;
    f_bfree   : int64;
    f_bavail  : int64;
    f_files   : int64;
    f_ffree   : int64;
    f_favail  : int64;
    f_flag    : mount_flag list;
    f_namemax : int64;
  }

  type statvfs_t
  let statvfs_t : statvfs structure typ = structure "statvfs"

  let f_bsize   = field statvfs_t "f_bsize" uint64_t
  let f_frsize  = field statvfs_t "f_frsize" uint64_t
  let f_blocks  = field statvfs_t "f_blocks" fsblkcnt_t
  let f_bfree   = field statvfs_t "f_bfree" fsblkcnt_t
  let f_bavail  = field statvfs_t "f_bavail" fsblkcnt_t
  let f_files   = field statvfs_t "f_files" fsfilcnt_t
  let f_ffree   = field statvfs_t "f_ffree" fsfilcnt_t
  let f_favail  = field statvfs_t "f_favail" fsblkcnt_t
  let f_fsid    = field statvfs_t "f_fsid" uint64_t
  let f_flag    = field statvfs_t "f_flag" uint64_t
  let f_namemax = field statvfs_t "f_namemax" uint64_t

  let () = seal statvfs_t

  (* TODO *)
  let flags_of_uint64 _ = []

  (* XXX why isn't this a view in Ctypes? *)
  let int64_of_t ty n =
    match sizeof ty with
    | 4 ->
      (Obj.magic n : Unsigned.UInt32.t) |> Unsigned.UInt32.to_int32 |> Int64.of_int32
    | 8 ->
      (Obj.magic n : Unsigned.UInt64.t) |> Unsigned.UInt64.to_int64
    | _ -> failwith "invalid conversion size"

  let from_statvfs_t statvfs = {
    f_bsize   = getf statvfs f_bsize   |> Unsigned.UInt64.to_int64;
    f_frsize  = getf statvfs f_frsize  |> Unsigned.UInt64.to_int64;
    f_blocks  = getf statvfs f_blocks  |> int64_of_t fsblkcnt_t;
    f_bfree   = getf statvfs f_bfree   |> int64_of_t fsblkcnt_t;
    f_bavail  = getf statvfs f_bavail  |> int64_of_t fsblkcnt_t;
    f_files   = getf statvfs f_files   |> int64_of_t fsfilcnt_t;
    f_ffree   = getf statvfs f_ffree   |> int64_of_t fsfilcnt_t;
    f_favail  = getf statvfs f_favail  |> int64_of_t fsfilcnt_t;
    f_flag    = getf statvfs f_flag    |> flags_of_uint64;
    f_namemax = getf statvfs f_namemax |> Unsigned.UInt64.to_int64;
  }

  let statvfs =
    foreign ~check_errno:true "statvfs" (string @-> ptr statvfs_t @-> returning int)
  let statvfs f =
    let s = make statvfs_t in
    statvfs f (addr s) |> ignore;
    from_statvfs_t s

  let fstatvfs =
    foreign ~check_errno:true "fstatvfs" (int @-> ptr statvfs_t @-> returning int)
  let fstatvfs fd =
    let s = make statvfs_t in
    fstatvfs (Obj.magic fd) (addr s) |> ignore;
    from_statvfs_t s

  let ( ** ) = Int64.mul
  and ( // ) = Int64.div

  let free_blocks ?bs path =
    let s = statvfs path in
    match bs with
    | None -> s.f_bavail
    | Some bs ->
       (s.f_bavail ** s.f_frsize) // (Int64.of_int bs)

  let free_bytes path = free_blocks ~bs:1 path

  let size_blocks ?bs path =
    let s = statvfs path in
    match bs with
    | None -> s.f_blocks
    | Some bs ->
       (s.f_blocks ** s.f_frsize) // (Int64.of_int bs)

  let size_bytes path = size_blocks ~bs:1 path

  let used_blocks ?(bs=1024) path =
    Int64.sub
      (size_blocks ~bs path)
      (free_blocks ~bs path)

  let used_bytes path = used_blocks ~bs:1 path

  let block_size path =
    let s = statvfs path in
    Int64.to_int s.f_frsize

end

module Darwin = struct

  type fsid = {
    f_val : int32 array
  }

  type statfs = {
    f_otype       : int;
    f_oflags      : int;
    f_bsize       : int64;
    f_iosize      : int64;
    f_blocks      : int64;
    f_bfree       : int64;
    f_bavail      : int64;
    f_files       : int64;
    f_ffree       : int64;
    f_fsid        : fsid;
    f_owner       : uid_t;
    f_type        : int;
    f_flags       : int64;
    f_fstypename  : string;
    f_mntonname   : string;
    f_mntfromname : string;
  }

  type fsid_t
  let fsid_t : fsid_t structure typ = structure "fsid_t"
  let f_val = field fsid_t "f_val" (array 2 int32_t)
  let () = seal fsid_t

  (* Magic numbers come from macros described in statfs man page *)
  let _MFSNAMELEN = 15
  and _MNAMELEN = 90

  type statfs_t
  let statfs_t : statfs_t structure typ = structure "statfs"

  let f_otype       = field statfs_t "f_otype" short
  let f_oflags      = field statfs_t "f_oflags" short
  let f_bsize       = field statfs_t "f_bsize" long
  let f_iosize      = field statfs_t "f_iosize" long
  let f_blocks      = field statfs_t "f_blocks" long
  let f_bfree       = field statfs_t "f_bfree" long
  let f_bavail      = field statfs_t "f_bavail" long
  let f_files       = field statfs_t "f_files" long
  let f_ffree       = field statfs_t "f_ffree" long
  let f_fsid        = field statfs_t "f_fsid" fsid_t
  let f_owner       = field statfs_t "f_owner" uid_t
  let f_reserved1   = field statfs_t "f_reserved1" short
  let f_type        = field statfs_t "f_type" short
  let f_flags       = field statfs_t "f_flags" long
  let f_reserved2   = field statfs_t "f_reserved2" (array 2 long)
  let f_fstypename  = field statfs_t "f_fstypename" (array _MFSNAMELEN char)
  let f_mntonname   = field statfs_t "f_mntonname" (array _MNAMELEN char)
  let f_mntfromname = field statfs_t "f_mntfromname" (array _MNAMELEN char)
  let f_reserved3   = field statfs_t "f_reserved3" char
  let f_reserved4   = field statfs_t "f_reserved4" (array 4 long)

  let () = seal statfs_t

  let from_fsid_t fsid = {
    f_val = getf fsid f_val;
  }

  let from_statfs_t statfs = {
    f_otype       = getf statfs f_otype;
    f_oflags      = getf statfs f_oflags;
    f_bsize       = getf statfs f_bsize       |> Signed.Long.to_int64;
    f_iosize      = getf statfs f_iosize      |> Signed.Long.to_int64;
    f_blocks      = getf statfs f_blocks      |> Signed.Long.to_int64;
    f_bfree       = getf statfs f_bfree       |> Signed.Long.to_int64;
    f_bavail      = getf statfs f_bavail      |> Signed.Long.to_int64;
    f_files       = getf statfs f_files       |> Signed.Long.to_int64;
    f_ffree       = getf statfs f_ffree       |> Signed.Long.to_int64;
    f_fsid        = getf statfs f_fsid        |> from_fsid_t;
    f_owner       = getf statfs f_owner;
    f_type        = getf statfs f_type;
    f_flags       = getf statfs f_flags       |> Signed.Long.to_int64;
    f_fstypename  = getf statfs f_fstypename  |> Array.start |> coerce (ptr char) string;
    f_mntonname   = getf statfs f_mntonname   |> Array.start |> coerce (ptr char) string;
    f_mntfromname = getf statfs f_mntfromname |> Array.start |> coerce (ptr char) string;
  }

  let statfs =
    foreign ~check_errno:true "statfs" (string @-> ptr statfs_t @-> returning int)
  let statfs f =
    let s = make statfs_t in
    statfs f (addr s) |> ignore;
    from_statfs_t s

  let fstatfs =
    foreign ~check_errno:true "fstatfs" (int @-> ptr statfs_t @-> returning int)
  let fstatfs fd =
    let s = make statfs_t in
    fstatfs (Obj.magic fd) (addr s) |> ignore;
    from_statfs_t s

end

module Linux64 = struct

  (* test if we're 32 or 64 bit. could we use nativeint? *)

  type statfs = {
    f_type    : int64;
    f_bsize   : int64;
    f_blocks  : int64;
    f_bfree   : int64;
    f_bavail  : int64;
    f_files   : int64;
    f_ffree   : int64;
    f_namelen : int64;
    f_frsize  : int64;
  }

  type fsid_t
  let fsid_t : fsid_t structure typ = structure "fsid_t"
  let f_val = field fsid_t "f_val" (array 2 int32_t)
  let () = seal fsid_t

  type statfs_t
  let statfs_t : statfs_t structure typ = structure "statfs"

  let fsblkcnt_t = long
  and fsfilcnt_t = long

  let f_type    = field statfs_t "f_type" long
  let f_bsize   = field statfs_t "f_bsize" long
  let f_blocks  = field statfs_t "f_blocks" fsblkcnt_t
  let f_bfree   = field statfs_t "f_bfree" fsblkcnt_t
  let f_bavail  = field statfs_t "f_bavail" fsblkcnt_t
  let f_files   = field statfs_t "f_files" fsfilcnt_t
  let f_ffree   = field statfs_t "f_ffree" fsfilcnt_t
  let f_fsid    = field statfs_t "f_fsid" fsid_t
  let f_namelen = field statfs_t "f_namelen" long
  let f_frsize  = field statfs_t "f_frsize" long
  let f_spare   = field statfs_t "f_spare" (array 5 long)

  let () = seal statfs_t

  let from_statfs_t statfs = {
    f_type    = getf statfs f_type    |> Signed.Long.to_int64;
    f_bsize   = getf statfs f_bsize   |> Signed.Long.to_int64;
    f_blocks  = getf statfs f_blocks  |> Signed.Long.to_int64;
    f_bfree   = getf statfs f_bfree   |> Signed.Long.to_int64;
    f_bavail  = getf statfs f_bavail  |> Signed.Long.to_int64;
    f_files   = getf statfs f_files   |> Signed.Long.to_int64;
    f_ffree   = getf statfs f_ffree   |> Signed.Long.to_int64;
    f_namelen = getf statfs f_namelen |> Signed.Long.to_int64;
    f_frsize  = getf statfs f_frsize  |> Signed.Long.to_int64;
  }

  let statfs =
    foreign ~check_errno:true "statfs" (string @-> ptr statfs_t @-> returning int)
  let statfs f =
    let s = make statfs_t in
    statfs f (addr s) |> ignore;
    from_statfs_t s

  let fstatfs =
    foreign ~check_errno:true "fstatfs" (int @-> ptr statfs_t @-> returning int)
  let fstatfs fd =
    let s = make statfs_t in
    fstatfs (Obj.magic fd) (addr s) |> ignore;
    from_statfs_t s

end

(* Local Variables: *)
(* indent-tabs-mode: nil *)
(* End: *)
