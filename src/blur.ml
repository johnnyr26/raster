open Core

(* You need to modify this function to blur the input image
   based on the provided radius instead of ignoring it. *)
let transform image ~radius = 
  Image.mapi image ~f: (fun ~x ~y pixel -> (
    if x - radius < 0 || x + radius >= Image.width image || y - radius < 0 || y + radius >= Image.height image then pixel else
    let sliced_image = Image.slice image ~x_start: (x - radius) ~x_end: (x + radius) ~y_start: (y - radius) ~y_end: (y + radius) 
    in Image.mean_pixel sliced_image
  ))

let command =
  Command.basic
    ~summary:"Blur an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and radius =
        flag
          "radius"
          (required Command.Param.int)
          ~doc:"N the radius to use when blurring (higher = more blurred)"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~radius in
        Image.save_ppm
          image'
          ~filename:(String.chop_suffix_exn filename ~suffix:".ppm" ^ "_blur.ppm")]
;;
