open Core

type color =
  | Red
  | Green
  | Blue

let distribute_error
  ~(image : Image.t)
  ~(x : int)
  ~(y : int)
  ~(error : float)
  ~(color : color)
  =
  if x + 1 < Image.width image
  then (
    let pixel = Image.get image ~x:(x + 1) ~y in
    let hex_value =
      match color with
      | Red -> Pixel.red pixel
      | Green -> Pixel.green pixel
      | Blue -> Pixel.blue pixel
    in
    let pixel_value =
      Int.to_float hex_value /. Int.to_float (Image.max_val image)
    in
    let new_pixel_value = pixel_value +. (7.0 *. error /. 16.0) in
    let new_pixel_rgb =
      Float.to_int (Int.to_float (Image.max_val image) *. new_pixel_value)
    in
    Image.set
      image
      ~x:(x + 1)
      ~y
      (new_pixel_rgb, new_pixel_rgb, new_pixel_rgb));
  if x - 1 >= 0 && y + 1 < Image.height image
  then (
    let pixel = Image.get image ~x:(x - 1) ~y:(y + 1) in
    let hex_value =
      match color with
      | Red -> Pixel.red pixel
      | Green -> Pixel.green pixel
      | Blue -> Pixel.blue pixel
    in
    let pixel_value =
      Int.to_float hex_value /. Int.to_float (Image.max_val image)
    in
    let new_pixel_value = pixel_value +. (3.0 *. error /. 16.0) in
    let new_pixel_rgb =
      Float.to_int (Int.to_float (Image.max_val image) *. new_pixel_value)
    in
    Image.set
      image
      ~x:(x - 1)
      ~y:(y + 1)
      (new_pixel_rgb, new_pixel_rgb, new_pixel_rgb));
  if y + 1 < Image.height image
  then (
    let pixel = Image.get image ~x ~y:(y + 1) in
    let hex_value =
      match color with
      | Red -> Pixel.red pixel
      | Green -> Pixel.green pixel
      | Blue -> Pixel.blue pixel
    in
    let pixel_value =
      Int.to_float hex_value /. Int.to_float (Image.max_val image)
    in
    let new_pixel_value = pixel_value +. (5.0 *. error /. 16.0) in
    let new_pixel_rgb =
      Float.to_int (Int.to_float (Image.max_val image) *. new_pixel_value)
    in
    Image.set
      image
      ~x
      ~y:(y + 1)
      (new_pixel_rgb, new_pixel_rgb, new_pixel_rgb));
  if x + 1 < Image.width image && y + 1 < Image.height image
  then (
    let pixel = Image.get image ~x:(x + 1) ~y:(y + 1) in
    let hex_value =
      match color with
      | Red -> Pixel.red pixel
      | Green -> Pixel.green pixel
      | Blue -> Pixel.blue pixel
    in
    let pixel_value =
      Int.to_float hex_value /. Int.to_float (Image.max_val image)
    in
    let new_pixel_value = pixel_value +. (1.0 *. error /. 16.0) in
    let new_pixel_rgb =
      Float.to_int (Int.to_float (Image.max_val image) *. new_pixel_value)
    in
    Image.set
      image
      ~x:(x + 1)
      ~y:(y + 1)
      (new_pixel_rgb, new_pixel_rgb, new_pixel_rgb))
;;

(* n = 2 -> (0, max_val) n = 3 -> (0, max_val / 2, max_val) n = 4 -> (0,
   max_val / 3, 2 * max_val / 3, max_val) *)

let rec iterate_image
  ~(image : Image.t)
  ~(x : int)
  ~(y : int)
  ~(n : int)
  ~(color : color)
  : Image.t
  =
  if y = Image.height image
  then image
  else if x = Image.width image
  then iterate_image ~image ~x:0 ~y:(y + 1) ~n ~color
  else (
    let pixel = Image.get image ~x ~y in
    let hex_value =
      match color with
      | Red -> Pixel.red pixel
      | Green -> Pixel.green pixel
      | Blue -> Pixel.blue pixel
    in
    let pixel_value =
      Int.to_float hex_value /. Int.to_float (Image.max_val image)
    in
    if Float.compare pixel_value 0.5 > 0
    then (
      let new_color =
        match color with
        | Red -> Image.max_val image, min (Pixel.green pixel) (Image.max_val image), min (Image.max_val image) (Pixel.blue pixel)
        | Green -> min (Image.max_val image) (Pixel.red pixel), Image.max_val image, min (Image.max_val image) (Pixel.blue pixel)
        | Blue -> min (Image.max_val image) (Pixel.red pixel), min (Image.max_val image) (Pixel.green pixel), Image.max_val image
      in
      Image.set image ~x ~y new_color;
      let error = pixel_value -. 1.0 in
      distribute_error ~image ~x ~y ~error ~color)
    else (
      Image.set image ~x ~y Pixel.zero;
      let error = pixel_value in
      distribute_error ~image ~x ~y ~error ~color);
    iterate_image ~image ~x:(x + 1) ~y ~n ~color)
;;

let transform image n =
  let image = image in
  List.fold [ Red; Green; Blue ] ~init:image ~f:(fun image color ->
    iterate_image ~image ~x:0 ~y:0 ~n ~color)
;;

let command =
  Command.basic
    ~summary:"Dither an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = transform (Image.load_ppm ~filename) 2 in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;
