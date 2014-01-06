open Ctypes
open Statfs

let print_statfs_osx stats =
	let open Darwin in
	Printf.printf "f_otype: %d\n" stats.f_otype;
	Printf.printf "f_oflags: %d\n" stats.f_oflags;
	Printf.printf "f_bsize: %Ld\n" stats.f_bsize;
	Printf.printf "f_iosize: %Ld\n" stats.f_iosize;
	Printf.printf "f_blocks: %Ld\n" stats.f_blocks;
	Printf.printf "f_bfree: %Ld\n" stats.f_bfree;
	Printf.printf "f_bavail: %Ld\n" stats.f_bavail;
	Printf.printf "f_files: %Ld\n" stats.f_files;
	Printf.printf "f_ffree: %Ld\n" stats.f_ffree;
	Printf.printf "f_fsid[0]: %ld\n" stats.f_fsid.f_val.(0);
	Printf.printf "f_fsid[1]: %ld\n" stats.f_fsid.f_val.(1);
	Printf.printf "f_owner: %ld\n" (Obj.magic stats.f_owner);
	Printf.printf "f_type: %d\n" stats.f_type;
	Printf.printf "f_flags: %Ld\n" stats.f_flags;
	Printf.printf "f_fstypename: %s\n" stats.f_fstypename;
	Printf.printf "f_mntonname: %s\n" stats.f_mntonname;
	Printf.printf "f_mntfromname: %s\n" stats.f_mntfromname

let print_statfs_linux stats =
	let open Linux64 in
	Printf.printf "f_type:   %Lx\n" stats.f_type;
	Printf.printf "f_bsize:  %Ld\n" stats.f_bsize;
	Printf.printf "f_blocks: %Ld\n" stats.f_blocks;
	Printf.printf "f_bfree:  %Ld\n" stats.f_bfree;
	Printf.printf "f_bavail: %Ld\n" stats.f_bavail

let print_statfs_posix stats =
	let open Posix in
	Printf.printf "f_bsize:  %Lu\n" stats.f_bsize;
	Printf.printf "f_frsize: %Lu\n" stats.f_frsize;
	Printf.printf "f_blocks: %Lu\n" stats.f_blocks;
	Printf.printf "f_bfree:  %Lu\n" stats.f_bfree;
	Printf.printf "f_bavail: %Lu\n" stats.f_bavail;
	Printf.printf "f_ffree:  %Lu\n" stats.f_ffree;
	()

let test_darwin () =
	let open Darwin in
	let stats = statfs "/dev" in
	print_statfs_osx stats;
	print_newline ();

	let fd = Unix.openfile "/tmp/foo" [ Unix.O_CREAT; Unix.O_RDWR ] 0o644 in
	let stats = fstatfs fd in
	Unix.close fd;
	print_statfs_osx stats

let test_linux () =
	let open Linux64 in
	let stats = statfs "/tmp" in
	print_statfs_linux stats

let test_posix () =
	let open Posix in
	let ( // ) = Int64.div in
	let path = "/" in
	let stats = statvfs path in
	print_statfs_posix stats;
	Printf.printf "System block size: %d\n" (block_size path);
	Printf.printf "Available 1024k blocks: %Lu\n" (free_blocks ~bs:1024 path);
	Printf.printf "Available 512k blocks: %Lu\n" (free_blocks ~bs:512 path);
	Printf.printf "Free Kb: %Lu\n" (free_bytes path // 1024L);
	Printf.printf "Free Gb: %Lu\n" (free_bytes path // 1024L // 1024L // 1024L);
	Printf.printf "Total blocks: %Lu\n" (size_blocks ~bs:512 path);
	Printf.printf "Total Gb: %Lu\n" (size_bytes path // 1024L // 1024L // 1024L);
	Printf.printf "Used blocks: %Lu\n" (used_blocks ~bs:512 path);
	Printf.printf "Used Gb: %Lu\n" (used_bytes path // 1024L // 1024L // 1024L);
	()

let main : unit =
	test_posix ()
