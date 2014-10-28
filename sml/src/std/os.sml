
signature OS_FILE_SYS = sig
   include OS_FILE_SYS
   val fileExists : string -> bool

   (*Get all the files in a directory *)
   val ls : string -> string list

   (* Get the immediate subdirectories of a directory.
    * Same as
    * find . -maxdepth 1 -mindepth 1 -type d *)
   val dirs : string -> string list
end

structure OSFileSys : OS_FILE_SYS = struct
   structure F = OS.FileSys

   fun ls dir =
       let
          val stm = F.openDir dir
          val fs = ref []
          val f = ref (F.readDir stm)
       in
          let in
             while !f <> NONE do
                let in
                   fs := (valOf (!f)) :: (!fs)
                 ; f := F.readDir stm
                end
          end
        ; F.closeDir stm
        ; !fs
       end

   fun dirs dir =
       let
          val fs = ls dir
       in
          List.filter (fn x => F.isDir (dir ^ "/" ^ x)) fs
       end

   fun fileExists s = F.access (s, [])

   open F
end

structure OS = struct
  structure FileSys = OSFileSys
  structure Path = OS.Path
  structure Process = OS.Process
end
