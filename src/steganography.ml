open Core

let transform image =
  Image.map image ~f: (fun pixel -> (
    (((Pixel.red pixel) % 4) lsl 6, ((Pixel.green pixel) % 4) lsl 6, ((Pixel.blue pixel) % 4) lsl 6)
  ))
;;


let command =
  Command.basic
    ~summary:"Steganography"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm
          image
          ~filename: "images/mystery.ppm"]
;;
