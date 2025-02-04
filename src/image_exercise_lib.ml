open! Core

let command =
  Command.group
    ~summary:"A tool to perform various image manipulations"
    [ "grayscale", Grayscale.command
    ; "bluescreen", Blue_screen.command
    ; "blur", Blur.command
    ; "dither", Dither.command
    ; "steganography", Steganography.command
    ; "edge", Edge.command
    ; "color-dither", Color_dither.command
    ; "solarize", Solarize.command
    ]
;;
