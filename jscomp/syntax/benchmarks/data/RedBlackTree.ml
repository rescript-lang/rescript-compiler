type nonrec node_color =
  | Red
  | Black
type 'value node =
  {
  mutable left: 'value node option ;
  mutable right: 'value node option ;
  mutable parent: 'value node option ;
  mutable sum: float ;
  mutable color: node_color ;
  mutable height: float ;
  mutable value: 'value }
type nonrec 'value t =
  {
  mutable size: int ;
  mutable root: 'value node option ;
  compare: (('value -> 'value -> int)[@bs ]) }
let create_node ~color  ~value  ~height  =
  { left = None; right = None; parent = None; sum = 0.; height; value; color
  }
external cast_not_option : 'a option -> 'a = "%identity"
let update_sum node =
  let left_sum = match node.left with | None -> 0. | Some left -> left.sum in
  let right_sum = match node.right with | None -> 0. | Some right -> right.sum in
  node.sum <- ((left_sum +. right_sum) +. node.height)
let rec update_sum_recursive rbt node =
  update_sum node;
  (match node.parent with
   | None -> ()
   | Some parent -> rbt |. (update_sum_recursive parent))
let grand_parent_of node =
  match node.parent with | None -> None | Some ref_ -> ref_.parent
let is_left node =
  match node.parent with
  | None -> false
  | Some parent -> (Some node) == parent.left
let left_or_right_set ~node  x value =
  ((if is_left node then x.left <- value else x.right <- value)[@res.ternary ])
let sibling_of node =
  if is_left node
  then (cast_not_option node.parent).right
  else (cast_not_option node.parent).left
let uncle_of node =
  match grand_parent_of node with
  | None -> None
  | Some grand_parent_of_node ->
      if is_left (cast_not_option node.parent)
      then grand_parent_of_node.right
      else grand_parent_of_node.left
let rec find_node rbt node value =
  match node with
  | None -> None
  | Some node ->
      let cmp = ((rbt.compare value node.value)[@bs ]) in
      if cmp == 0
      then Some node
      else
        if cmp < 0
        then find_node rbt node.left value
        else find_node rbt node.right value
let has rbt value = (find_node rbt rbt.root value) != None
let rec peek_min_node node =
  match node with
  | None -> None
  | Some node ->
      ((if node.left == None then Some node else node.left |. peek_min_node)
      [@res.ternary ])
let rec peek_max_node node =
  match node with
  | None -> None
  | Some node ->
      ((if node.right == None then Some node else node.right |. peek_max_node)
      [@res.ternary ])
let rotate_left rbt node =
  let parent = node.parent in
  let right = node.right in
  (match parent with
   | Some parent -> parent |. (left_or_right_set ~node right)
   | None -> rbt.root <- right);
  node.parent <- right;
  (let right = right |. cast_not_option in
   let right_left = right.left in
   node.right <- right_left;
   (match right_left with
    | Some right_left -> right_left.parent <- (Some node)
    | None -> ());
   right.parent <- parent;
   right.left <- (Some node);
   update_sum node;
   update_sum right)
let rotate_right rbt node =
  let parent = node.parent in
  let left = node.left in
  (match parent with
   | Some parent -> parent |. (left_or_right_set ~node left)
   | None -> rbt.root <- left);
  node.parent <- left;
  (let left = left |. cast_not_option in
   let left_right = left.right in
   node.left <- left_right;
   (match left_right with
    | Some left_right -> left_right.parent <- (Some node)
    | None -> ());
   left.parent <- parent;
   left.right <- (Some node);
   update_sum node;
   update_sum left)
let rec find_insert rbt node node_to_insert value =
  match node with
  | None -> None
  | Some node ->
      let cmp = ((rbt.compare value node.value)[@bs ]) in
      if cmp == 0
      then Some node
      else
        if cmp < 0
        then
          (if node.left != None
           then rbt |. (find_insert node.left node_to_insert value)
           else
             (node_to_insert.parent <- (Some node);
              node.left <- (Some node_to_insert);
              None))
        else
          if node.right != None
          then rbt |. (find_insert node.right node_to_insert value)
          else
            (node_to_insert.parent <- (Some node);
             node.right <- (Some node_to_insert);
             None)
let rec _addLoop rbt current_node =
  if (Some current_node) == rbt.root
  then current_node.color <- Black
  else
    if (current_node.parent |. cast_not_option).color == Black
    then ()
    else
      if
        (let uncle = uncle_of current_node in
         (uncle != None) && ((uncle |. cast_not_option).color == Red))
      then
        ((current_node.parent |. cast_not_option).color <- Black;
         ((uncle_of current_node) |. cast_not_option).color <- Black;
         ((grand_parent_of current_node) |. cast_not_option).color <- Red;
         _addLoop rbt ((grand_parent_of current_node) |. cast_not_option))
      else
        (let current_node =
           if
             (not (is_left current_node)) &&
               (is_left (current_node.parent |. cast_not_option))
           then
             (rotate_left rbt (current_node.parent |. cast_not_option);
              current_node.left |. cast_not_option)
           else
             if
               (is_left current_node) &&
                 (not (is_left (current_node.parent |. cast_not_option)))
             then
               (rotate_right rbt (current_node.parent |. cast_not_option);
                current_node.right |. cast_not_option)
             else current_node in
         (current_node.parent |. cast_not_option).color <- Black;
         ((grand_parent_of current_node) |. cast_not_option).color <- Red;
         if is_left current_node
         then rotate_right rbt ((grand_parent_of current_node) |. cast_not_option)
         else rotate_left rbt ((grand_parent_of current_node) |. cast_not_option))
let add rbt value ~height  =
  rbt.size <- (rbt.size + 1);
  (let node_to_insert = create_node ~value ~color:Red ~height in
   let inserted =
     if rbt.root == None
     then (rbt.root <- (Some node_to_insert); true)
     else
       (let found_node = find_insert rbt rbt.root node_to_insert value in
        found_node == None) in
   if inserted
   then
     (rbt |. (update_sum_recursive node_to_insert);
      _addLoop rbt node_to_insert;
      Some node_to_insert)
   else None)
let remove_node rbt node =
  let node_to_remove =
    match ((node.left), (node.right)) with
    | (Some _, Some _) ->
        let successor = (peek_min_node node.right) |. cast_not_option in
        (node.value <- (successor.value);
         node.height <- (successor.height);
         successor)
    | _ -> node in
  let successor =
    match node_to_remove.left with | None -> node_to_remove.right | left -> left in
  let (successor, is_leaf) =
    match successor with
    | None ->
        let leaf = create_node ~value:([%raw "0"]) ~color:Black ~height:0. in
        let is_leaf = ((fun x -> x == leaf)[@bs ]) in (leaf, is_leaf)
    | Some successor -> (successor, (((fun _ -> false))[@bs ])) in
  let node_parent = node_to_remove.parent in
  successor.parent <- node_parent;
  (match node_parent with
   | None -> ()
   | Some parent ->
       parent |. (left_or_right_set ~node:node_to_remove (Some successor)));
  rbt |. (update_sum_recursive successor);
  if node_to_remove.color == Black
  then
    (if successor.color == Red
     then
       (successor.color <- Black;
        if successor.parent == None then rbt.root <- (Some successor))
     else
       (let break = ref false in
        let successor_ref = ref successor in
        while not break.contents do
          let successor = successor_ref.contents in
          match successor.parent with
          | None -> (rbt.root <- (Some successor); break.contents <- true)
          | Some successor_parent ->
              let sibling = sibling_of successor in
              (if
                 (sibling != None) &&
                   ((sibling |. cast_not_option).color == Red)
               then
                 (successor_parent.color <- Red;
                  (sibling |. cast_not_option).color <- Black;
                  if is_left successor
                  then rotate_left rbt successor_parent
                  else rotate_right rbt successor_parent);
               (let sibling = sibling_of successor in
                let sibling_n_n = sibling |. cast_not_option in
                if
                  (successor_parent.color == Black) &&
                    ((sibling == None) ||
                       (((sibling_n_n.color == Black) &&
                           ((sibling_n_n.left == None) ||
                              ((sibling_n_n.left |. cast_not_option).color ==
                                 Black)))
                          &&
                          ((sibling_n_n.right == None) ||
                             ((sibling_n_n.right |. cast_not_option).color ==
                                Black))))
                then
                  (if sibling != None then sibling_n_n.color <- Red;
                   successor_ref.contents <- successor_parent)
                else
                  if
                    (successor_parent.color == Red) &&
                      ((sibling == None) ||
                         (((sibling_n_n.color == Black) &&
                             ((sibling_n_n.left == None) ||
                                ((sibling_n_n.left |. cast_not_option).color ==
                                   Black)))
                            &&
                            ((sibling_n_n.right == None) ||
                               ((sibling_n_n.right |. cast_not_option).color ==
                                  Black))))
                  then
                    (if sibling != None then sibling_n_n.color <- Red;
                     successor_parent.color <- Black;
                     break.contents <- true)
                  else
                    if
                      (sibling != None) &&
                        ((sibling |. cast_not_option).color == Black)
                    then
                      (let sibling = sibling |. cast_not_option in
                       if
                         (((is_left successor) &&
                             ((sibling.right == None) ||
                                ((sibling.right |. cast_not_option).color ==
                                   Black)))
                            && (sibling.left != None))
                           && ((sibling.left |. cast_not_option).color == Red)
                       then
                         (sibling.color <- Red;
                          (sibling.left |. cast_not_option).color <- Black;
                          rotate_right rbt sibling)
                       else
                         if
                           (((not (is_left successor)) &&
                               ((sibling.left == None) ||
                                  ((sibling.left |. cast_not_option).color ==
                                     Black)))
                              && (sibling.right != None))
                             &&
                             ((sibling.right |. cast_not_option).color == Red)
                         then
                           (sibling.color <- Red;
                            (sibling.right |. cast_not_option).color <- Black;
                            rotate_left rbt sibling);
                       break.contents <- true)
                    else
                      (let sibling = sibling_of successor in
                       let sibling = sibling |. cast_not_option in
                       sibling.color <- (successor_parent.color);
                       if is_left successor
                       then
                         ((sibling.right |. cast_not_option).color <- Black;
                          rotate_right rbt successor_parent)
                       else
                         ((sibling.left |. cast_not_option).color <- Black;
                          rotate_left rbt successor_parent))))
          done));
  if ((is_leaf successor)[@bs ])
  then
    (if rbt.root == (Some successor) then rbt.root <- None;
     (match successor.parent with
      | None -> ()
      | Some parent -> parent |. (left_or_right_set ~node:successor None)))
let remove rbt value =
  match find_node rbt rbt.root value with
  | Some node -> (rbt |. (remove_node node); rbt.size <- (rbt.size - 1); true)
  | None -> false
let rec find_node_through_callback rbt node cb =
  match node with
  | None -> None
  | Some node ->
      let cmp = ((cb node)[@bs ]) in
      if cmp == 0
      then Some node
      else
        if cmp < 0
        then find_node_through_callback rbt node.left cb
        else find_node_through_callback rbt node.right cb
let remove_through_callback rbt cb =
  match find_node_through_callback rbt rbt.root cb with
  | Some node -> (rbt |. (remove_node node); rbt.size <- (rbt.size - 1); true)
  | None -> false
let make ~compare  = { size = 0; root = None; compare }
let make_with array ~compare  =
  let rbt = make ~compare in
  array |.
    (Js.Array2.for_each
       (fun (value, height) -> (add rbt value ~height) |. ignore));
  rbt
let rec height_of_interval rbt node lhs rhs =
  match node with
  | None -> 0.
  | Some n ->
      if (lhs == None) && (rhs == None)
      then n.sum
      else
        if
          (lhs != None) &&
            (((rbt.compare n.value (lhs |. cast_not_option))[@bs ]) < 0)
        then rbt |. (height_of_interval n.right lhs rhs)
        else
          if
            (rhs != None) &&
              (((rbt.compare n.value (rhs |. cast_not_option))[@bs ]) > 0)
          then rbt |. (height_of_interval n.left lhs rhs)
          else
            (n.height +. (rbt |. (height_of_interval n.left lhs None))) +.
              (rbt |. (height_of_interval n.right None rhs))
let height_of_interval rbt lhs rhs = height_of_interval rbt rbt.root lhs rhs
let rec first_visible_node node top =
  match node with
  | None -> None
  | Some node ->
      if node.sum <= top
      then None
      else
        (let node_height = node.height in
         let sum_left =
           match node.left with | None -> 0.0 | Some left -> left.sum in
         if sum_left > top
         then first_visible_node node.left top
         else
           if (sum_left +. node_height) > top
           then Some node
           else
             (let offset = sum_left +. node_height in
              first_visible_node node.right (top -. offset)))
let last_visible_node node top =
  match first_visible_node node top with
  | None -> node |. peek_max_node
  | first -> first
let first_visible_value rbt ~top  =
  match first_visible_node rbt.root top with
  | None -> None
  | Some node -> Some (node.value)
let rec leftmost node =
  match node.left with | None -> node | Some node -> node |. leftmost
let rec first_right_parent node =
  match node.parent with
  | None -> None
  | Some parent ->
      ((if is_left node then Some parent else parent |. first_right_parent)
      [@res.ternary ])
let next_node node =
  match node.right with
  | None -> node |. first_right_parent
  | Some right -> Some (right |. leftmost)
let rec sum_left_spine node ~from_right_child  =
  let left_spine =
    match node.left with
    | None -> node.height
    | Some left -> ((if from_right_child then node.height +. left.sum else 0.0)
        [@res.ternary ]) in
  match node.parent with
  | None -> left_spine
  | Some parent ->
      left_spine +.
        (parent |.
           (sum_left_spine ~from_right_child:(parent.right == (Some node))))
let get_y node = (node |. (sum_left_spine ~from_right_child:true)) -. node.height
let rec iterate ~inclusive  first_node last_node ~callback  =
  match first_node with
  | None -> ()
  | Some node ->
      (if inclusive then ((callback node)[@bs ]);
       if first_node != last_node
       then
         (if not inclusive then ((callback node)[@bs ]);
          iterate ~inclusive (node |. next_node) last_node ~callback))
let rec iterate_with_y ?y  ~inclusive  first_node last_node ~callback  =
  match first_node with
  | None -> ()
  | Some node ->
      let y = match y with | None -> node |. get_y | Some y -> y in
      (if inclusive then ((callback node y)[@bs ]);
       if first_node != last_node
       then
         (if not inclusive then ((callback node y)[@bs ]);
          iterate_with_y ~y:(y +. node.height) ~inclusive (node |. next_node)
            last_node ~callback))
let rec update_sum node ~delta  =
  match node with
  | None -> ()
  | Some node ->
      (node.sum <- (node.sum +. delta); node.parent |. (update_sum ~delta))
let update_height node ~height  =
  let delta = height -. node.height in
  node.height <- height; (Some node) |. (update_sum ~delta)
type nonrec 'value old_new_visible =
  {
  mutable old: 'value array ;
  mutable new_: 'value array }
let get_anchor_delta rbt ~anchor  =
  match anchor with
  | None -> 0.0
  | Some (value, y) ->
      (match rbt |. (find_node rbt.root value) with
       | Some node -> y -. (node |. get_y)
       | None -> 0.0)
let on_changed_visible ?(anchor= None)  rbt ~old_new_visible  ~top:top_
  ~bottom:bottom_  ~appear  ~remained  ~disappear  =
  let old = old_new_visible.new_ in
  let new_ = old_new_visible.old in
  (new_ |.
     (Js.Array2.remove_count_in_place ~pos:0 ~count:(new_ |. Js.Array2.length)))
    |. ignore;
  old_new_visible.old <- old;
  old_new_visible.new_ <- new_;
  (let anchor_delta = rbt |. (get_anchor_delta ~anchor) in
   let top = top_ -. anchor_delta in
   let top = ((if top < 0.0 then 0.0 else top)[@res.ternary ]) in
   let bottom = bottom_ -. anchor_delta in
   let first = first_visible_node rbt.root top in
   let last = last_visible_node rbt.root bottom in
   let old_len = old |. Js.Array2.length in
   let old_iter = ref 0 in
   iterate_with_y ~inclusive:true first last
     ((fun node ->
         fun y_ ->
           let y = y_ +. anchor_delta in
           if y >= 0.0
           then
             (while
                (old_iter.contents < old_len) &&
                  (((rbt.compare (Js.Array2.unsafe_get old old_iter.contents)
                       node.value)
                     [@bs ]) < 0)
                do
                (((disappear (Js.Array2.unsafe_get old old_iter.contents))
                 [@bs ]);
                 old_iter.contents <- (old_iter.contents + 1))
                done;
              (new_ |. (Js.Array2.push node.value)) |. ignore;
              if old_iter.contents < old_len
              then
                (let cmp =
                   ((rbt.compare (Js.Array2.unsafe_get old old_iter.contents)
                       node.value)
                   [@bs ]) in
                 if cmp = 0
                 then
                   (((remained node y)
                    [@bs ]);
                    old_iter.contents <- (old_iter.contents + 1))
                 else ((appear node y)[@bs ]))
              else ((appear node y)[@bs ])))[@bs ]);
   while old_iter.contents < old_len do
     (((disappear (Js.Array2.unsafe_get old old_iter.contents))
      [@bs ]);
      old_iter.contents <- (old_iter.contents + 1))
     done)
