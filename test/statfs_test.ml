open Ctypes
open Statfs

let print_statvfs stats =
	Printf.printf "f_bsize:  %Lu\n" stats.f_bsize;
	Printf.printf "f_frsize: %Lu\n" stats.f_frsize;
	Printf.printf "f_blocks: %Lu\n" stats.f_blocks;
	Printf.printf "f_bfree:  %Lu\n" stats.f_bfree;
	Printf.printf "f_bavail: %Lu\n" stats.f_bavail;
	Printf.printf "f_ffree:  %Lu\n" stats.f_ffree;
	()

let test_statvfs () =
	let ( // ) = Int64.div in
	let path = "/" in
	let stats = statvfs path in
	print_statvfs stats;
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
	test_statvfs ()
