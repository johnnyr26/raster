open Core

let get_edge_values_x ~(image : Image.t) ~(x : int) ~(y : int) =
  let sum = ref 0 in
  if x - 1 >= 0 && y - 1 >= 0
  then sum := !sum + (Pixel.red (Image.get image ~x: (x - 1) ~y: (y - 1)) * -1);
  if x - 1 >= 0 then sum := !sum + (Pixel.red (Image.get image ~x: (x - 1) ~y) * -2);
  if x - 1 >= 0 && y + 1 < Image.height image
  then sum := !sum + (Pixel.red (Image.get image ~x: (x - 1) ~y: (y + 1)) * -1);
  if x + 1 < Image.width image && y - 1 >= 0
  then sum := !sum + Pixel.red (Image.get image ~x: (x + 1) ~y: (y - 1));
  if x + 1 < Image.width image then sum := !sum + (Pixel.red (Image.get image ~x: (x + 1) ~y) * 2);
  if x + 1 < Image.width image && y + 1 < Image.height image
  then sum := !sum + Pixel.red (Image.get image ~x: (x + 1) ~y: (y + 1));
  !sum
;;

let get_edge_values_y ~(image : Image.t) ~(x : int) ~(y : int) =
  let sum = ref 0 in
  if x - 1 >= 0 && y - 1 >= 0
  then sum := !sum + (Pixel.red (Image.get image ~x: (x - 1) ~y: (y - 1)) * -1);
  if y - 1 >= 0 then sum := !sum + (Pixel.red (Image.get image ~x ~y: (y - 1)) * -2);
  if x + 1 < Image.width image && y - 1 >= 0
  then sum := !sum + (Pixel.red (Image.get image ~x: (x + 1) ~y: (y - 1)) * -1);
  if x - 1 >= 0 && y + 1 < Image.height image
  then sum := !sum + Pixel.red (Image.get image ~x: (x - 1) ~y: (y + 1));
  if y + 1 < Image.height image then sum := !sum + (Pixel.red (Image.get image ~x ~y: (y + 1)) * 2);
  if x + 1 < Image.width image && y + 1 < Image.height image
  then sum := !sum + Pixel.red (Image.get image ~x: (x + 1) ~y: (y + 1));
  !sum
;;

let transform image threshold =
  let image = Blur.transform (Grayscale.transform image) ~radius: 2 in
  Image.mapi image ~f:(fun ~x ~y _ ->
    let sum_x_values = get_edge_values_x ~image ~x ~y in
    let sum_y_values = get_edge_values_y ~image ~x ~y in
    let convolution =
      sqrt
        (Int.to_float
           ((sum_x_values * sum_x_values) + (sum_y_values * sum_y_values)))
    in
    let threshold = Int.to_float (Image.max_val image) *. threshold in
    if Float.compare convolution threshold = -1
    then 0, 0, 0
    else Image.max_val image, Image.max_val image, Image.max_val image)
;;

let command =
  Command.basic
    ~summary:"Edge"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = transform (Image.load_ppm ~filename) 0.4 in
        Image.save_ppm image ~filename:"images/edge.ppm"]
;;
