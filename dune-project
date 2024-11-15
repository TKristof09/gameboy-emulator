(lang dune 3.16)

(name gameboy)

(generate_opam_files true)

(source
 (github TKristof09/gameboy-emulator))

(authors "Kristof Toth")

(maintainers "Kristof Toth")

(license MIT)

(package
 (name gameboy)
 (synopsis "A Gameboy emulator")
 (description "A Gameboy emulator")
 (depends ocaml dune)
 (tags
  (topics "gameboy" "emulator")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
