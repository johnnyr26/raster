open Core

let get_red_pixel ~(image : Image.t) ~(threshold : float) ~(red : int) =
  let pix_value = float_of_int red /. float_of_int (Image.max_val image) in
  printf "%f\n" pix_value;
  if Float.( >= ) pix_value threshold
  then
    Image.max_val image
    - red
  else red
;;

let get_green_pixel ~(image : Image.t) ~(threshold : float) ~(green : int) =
  let pix_value = float_of_int green /. float_of_int (Image.max_val image) in
  if Float.( >= ) pix_value threshold
  then
    Image.max_val image
    - green
  else green
;;

let get_blue_pixel ~(image : Image.t) ~(threshold : float) ~(blue : int) =
  let pix_value = float_of_int blue /. float_of_int (Image.max_val image) in
  if Float.( >= ) pix_value threshold
  then
    Image.max_val image
    - blue
  else blue

let transform image threshold =
  Image.map image ~f:(fun (red, green, blue) ->
    (* printf "red: %d green: %d blue: %d\n" red green blue; *)
    ( get_red_pixel ~image ~threshold ~red
    , get_green_pixel ~image ~threshold ~green
    , get_blue_pixel ~image ~threshold ~blue ))
;;

let command =
  Command.basic
    ~summary:"Convert an image to grayscale"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = transform (Image.load_ppm ~filename) 0.7 in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_solarize.ppm")]
;;
