#load "str.cma";;

module Inventory = struct

  type t = {
      elf: int;
      snacks: snack list;
    }

  type input_line =
    | Number of int
    | Empty

  let rules = [
      (Str.regexp "^[ \t]*$"), (fun _ -> Empty);
      (Str.regexp "^[0-9]+$"), (fun line -> Number(int_of_string line));
    ]
  
  let input_line line =
    let rec apply_rules = function
      | [] -> None
      | (rule, action) :: remaining_rules ->
         if Str.string_match rule line 0 then
           Some(action line)
         else
           apply_rules remaining_rules
    in
    match apply_rules rules with
    | Some(result) -> result
    | None -> failwith "Cannot decode line"

  let read_next_inventory elf lines =
    let maybe_inventory current =
      match current.snacks with
      | [] -> None
      | _ -> Some(current)
    in
    let rec reader current stream =
      try
        match Stream.next stream with
        | Empty -> maybe_inventory current
        | Number(calories) -> reader { current with snacks = calories :: current.snacks } stream
      with
      | Stream.Failure -> maybe_inventory current
    in
    reader { elf = elf ; snacks = [] } lines

  let stream_from_input_lines input_lines =
    Stream.from (fun n -> read_next_inventory n input_lines)

  let stream_from_channel channel =
    let maybe_next_line () =
      try Some(Pervasives.input_line channel)
      with _ -> None
    in
    let next _ =
      match maybe_next_line () with
      | Some(line) -> Some(input_line line)
      | None -> None
    in
    let input_lines = Stream.from next in
    stream_from_input_lines input_lines

  let stream_from_file pathname =
    stream_from_channel (open_in pathname)

  let calories inventory =
    List.fold_left ( + ) 0 inventory.snacks

  let group_calories group =
    List.fold_left ( + ) 0 (List.map calories group)

  let compare_descending i1 i2 =
    (calories i1) - (calories i2)

  let compare_ascending i1 i2 =
    (calories i1) - (calories i2)
end

let rec stream_fold transition initial_state stream =
  let current_state = ref initial_state in
  begin
    Stream.iter (fun elt -> current_state := transition !current_state elt) stream;
    !current_state
  end

let find_elves_with_most_calories n stream =
  let transition current next =
    if List.length current < n then
      next :: current
    else
      List.tl (List.sort Inventory.compare_ascending (next :: current))
  in
  stream_fold transition [] stream

let puzzle1 pathname =
  Inventory.stream_from_file pathname
  |> find_elves_with_most_calories 1
  |> Inventory.group_calories

let puzzle2 pathname =
  Inventory.stream_from_file pathname
  |> find_elves_with_most_calories 3
  |> Inventory.group_calories
