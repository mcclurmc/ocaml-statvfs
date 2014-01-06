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

val free_blocks : ?bs:int -> string -> int64
val free_bytes  : string -> int64

val size_blocks : ?bs:int -> string -> int64
val size_bytes  : string -> int64

val used_blocks : ?bs:int -> string -> int64
val used_bytes  : string -> int64

val block_size  : string -> int
