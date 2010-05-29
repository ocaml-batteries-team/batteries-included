(* Interface to camomile that defaults to CamomileDefaultConfig but
   tries to use CAMOMILE_BASE from environment to set root *)

let camomile_base = "CAMOMILE_BASE"

let try_env suff default =
  try Filename.concat (Sys.getenv camomile_base) suff
  with Not_found -> default

module CDC = CamomileLibrary.CamomileDefaultConfig

module CamConfig = struct
  let datadir = try_env "database" CDC.datadir
  let localedir = try_env "locales" CDC.localedir
  let charmapdir = try_env "charmaps" CDC.charmapdir
  let unimapdir = try_env "mappings" CDC.unimapdir
end

include CamomileLibrary.Main.Make(CamConfig)
