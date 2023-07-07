open Core

let distribute_error
  ~(image : Image.t)
  ~(x : int)
  ~(y : int)
  ~(error : float)
  =
  if x + 1 < Image.width image
  then (
    let pixel_value =
      Int.to_float (Pixel.red (Image.get image ~x: (x + 1) ~y))
      /. Int.to_float (Image.max_val image)
    in
    let new_pixel_value = pixel_value +. (7.0 *. error /. 16.0) in
    let new_pixel_rgb = Float.to_int (Int.to_float (Image.max_val image) *. new_pixel_value) in
    Image.set
      image
      ~x:(x + 1)
      ~y
      (new_pixel_rgb, new_pixel_rgb, new_pixel_rgb)
      );
  if x - 1 >= 0 && y + 1 < Image.height image
  then (
    let pixel_value =
      Int.to_float (Pixel.red (Image.get image ~x: (x - 1) ~y: (y + 1)))
      /. Int.to_float (Image.max_val image)
    in
    let new_pixel_value = pixel_value +. (3.0 *. error /. 16.0) in
    let new_pixel_rgb = Float.to_int (Int.to_float (Image.max_val image) *. new_pixel_value) in

    Image.set
      image
      ~x:(x - 1)
      ~y:(y + 1)
      (new_pixel_rgb, new_pixel_rgb, new_pixel_rgb));
  if y + 1 < Image.height image
  then (
    let pixel_value =
      Int.to_float (Pixel.red (Image.get image ~x ~y: (y + 1)))
      /. Int.to_float (Image.max_val image)
    in
    let new_pixel_value = pixel_value +. (5.0 *. error /. 16.0) in
    let new_pixel_rgb = Float.to_int (Int.to_float (Image.max_val image) *. new_pixel_value) in
    Image.set
      image
      ~x
      ~y: (y + 1)
      (new_pixel_rgb, new_pixel_rgb, new_pixel_rgb));
  if x + 1 < Image.width image && y + 1 < Image.height image
  then (
    let pixel_value =
      Int.to_float (Pixel.red (Image.get image ~x: (x + 1) ~y: (y + 1)))
      /. Int.to_float (Image.max_val image)
    in
    let new_pixel_value = pixel_value +. (1.0 *. error /. 16.0) in
    let new_pixel_rgb = Float.to_int (Int.to_float (Image.max_val image) *. new_pixel_value) in
    Image.set
      image
      ~x:(x + 1)
      ~y:(y + 1)
      (new_pixel_rgb, new_pixel_rgb, new_pixel_rgb));
;;

let rec iterate_image ~(image : Image.t) ~(x : int) ~(y : int) : Image.t =
  if y = Image.height image
  then image
  else if x = Image.width image
  then iterate_image ~image ~x:0 ~y:(y + 1)
  else (
    let pixel = Image.get image ~x ~y in
    let pixel_value =
      Int.to_float (Pixel.red pixel) /. Int.to_float (Image.max_val image)
    in
    if Float.compare pixel_value 0.5 > 0
    then (
      Image.set
        image
        ~x
        ~y
        (Image.max_val image, Image.max_val image, Image.max_val image);
      let error = pixel_value -. 1.0 in
      distribute_error ~image ~x ~y ~error)
    else (
      Image.set image ~x ~y Pixel.zero;
      let error = pixel_value in
      distribute_error ~image ~x ~y ~error);
    iterate_image ~image ~x:(x + 1) ~y)
;;

let transform image =
  let image = Grayscale.transform image in
  iterate_image ~image ~x:0 ~y:0
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
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;
