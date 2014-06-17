(**** postscipt utilities *****************************************************)
let ps_normal_font = "Palatino-Roman10"
let ps_header_font = "Palatino-Bold10"
let ps_code_font = "Courier-New10"

(** [ps_set_font c f] Resets the font of text output to channel [c] *)
let ps_set_font (chn : out_channel) (font : string) : unit =
  output_string chn (Format.sprintf "\n\001font{%s}" font)

(** [ps_open_channel f t] opens a stream to file [f] with title [t]
 * that pipes its input into a postscript-formatted file *)
let ps_open_channel (fname : string) (title : string) : out_channel =
  Unix.open_process_out (Format.sprintf "enscript --quiet -p %s -b '%s' -M Letter --fancy-header --escapes=\001 --no-formfeed" fname title)
