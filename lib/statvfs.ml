module Array_std = Array

open Ctypes
open PosixTypes
open Foreign

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

(* Local Variables: *)
(* indent-tabs-mode: nil *)
(* End: *)
