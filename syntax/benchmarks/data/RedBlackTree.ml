type nonrec nodeColor =
  | Red
  | Black
type 'value node =
  {
  mutable left: 'value node option ;
  mutable right: 'value node option ;
  mutable parent: 'value node option ;
  mutable sum: float ;
  mutable color: nodeColor ;
  mutable height: float ;
  mutable value: 'value }
type nonrec 'value t =
  {
  mutable size: int ;
  mutable root: 'value node option ;
  compare: (('value -> 'value -> int)[@bs ]) }
let createNode ~color  ~value  ~height  =
  { left = None; right = None; parent = None; sum = 0.; height; value; color
  }
external castNotOption : 'a option -> 'a = "%identity"
let updateSum node =
  let leftSum = match node.left with | None -> 0. | Some left -> left.sum in
  let rightSum = match node.right with | None -> 0. | Some right -> right.sum in
  node.sum <- ((leftSum +. rightSum) +. node.height)
let rec updateSumRecursive rbt node =
  updateSum node;
  (match node.parent with
   | None -> ()
   | Some parent -> rbt |. (updateSumRecursive parent))
let grandParentOf node =
  match node.parent with | None -> None | Some ref_ -> ref_.parent
let isLeft node =
  match node.parent with
  | None -> false
  | Some parent -> (Some node) == parent.left
let leftOrRightSet ~node  x value =
  ((if isLeft node then x.left <- value else x.right <- value)[@ns.ternary ])
let siblingOf node =
  if isLeft node
  then (castNotOption node.parent).right
  else (castNotOption node.parent).left
let uncleOf node =
  match grandParentOf node with
  | None -> None
  | Some grandParentOfNode ->
      if isLeft (castNotOption node.parent)
      then grandParentOfNode.right
      else grandParentOfNode.left
let rec findNode rbt node value =
  match node with
  | None -> None
  | Some node ->
      let cmp = ((rbt.compare value node.value)[@bs ]) in
      if cmp == 0
      then Some node
      else
        if cmp < 0
        then findNode rbt node.left value
        else findNode rbt node.right value
let has rbt value = (findNode rbt rbt.root value) != None
let rec peekMinNode node =
  match node with
  | None -> None
  | Some node ->
      ((if node.left == None then Some node else node.left |. peekMinNode)
      [@ns.ternary ])
let rec peekMaxNode node =
  match node with
  | None -> None
  | Some node ->
      ((if node.right == None then Some node else node.right |. peekMaxNode)
      [@ns.ternary ])
let rotateLeft rbt node =
  let parent = node.parent in
  let right = node.right in
  (match parent with
   | Some parent -> parent |. (leftOrRightSet ~node right)
   | None -> rbt.root <- right);
  node.parent <- right;
  (let right = right |. castNotOption in
   let rightLeft = right.left in
   node.right <- rightLeft;
   (match rightLeft with
    | Some rightLeft -> rightLeft.parent <- (Some node)
    | None -> ());
   right.parent <- parent;
   right.left <- (Some node);
   updateSum node;
   updateSum right)
let rotateRight rbt node =
  let parent = node.parent in
  let left = node.left in
  (match parent with
   | Some parent -> parent |. (leftOrRightSet ~node left)
   | None -> rbt.root <- left);
  node.parent <- left;
  (let left = left |. castNotOption in
   let leftRight = left.right in
   node.left <- leftRight;
   (match leftRight with
    | Some leftRight -> leftRight.parent <- (Some node)
    | None -> ());
   left.parent <- parent;
   left.right <- (Some node);
   updateSum node;
   updateSum left)
let rec findInsert rbt node nodeToInsert value =
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
           then rbt |. (findInsert node.left nodeToInsert value)
           else
             (nodeToInsert.parent <- (Some node);
              node.left <- (Some nodeToInsert);
              None))
        else
          if node.right != None
          then rbt |. (findInsert node.right nodeToInsert value)
          else
            (nodeToInsert.parent <- (Some node);
             node.right <- (Some nodeToInsert);
             None)
let rec _addLoop rbt currentNode =
  if (Some currentNode) == rbt.root
  then currentNode.color <- Black
  else
    if (currentNode.parent |. castNotOption).color == Black
    then ()
    else
      if
        (let uncle = uncleOf currentNode in
         (uncle != None) && ((uncle |. castNotOption).color == Red))
      then
        ((currentNode.parent |. castNotOption).color <- Black;
         ((uncleOf currentNode) |. castNotOption).color <- Black;
         ((grandParentOf currentNode) |. castNotOption).color <- Red;
         _addLoop rbt ((grandParentOf currentNode) |. castNotOption))
      else
        (let currentNode =
           if
             (not (isLeft currentNode)) &&
               (isLeft (currentNode.parent |. castNotOption))
           then
             (rotateLeft rbt (currentNode.parent |. castNotOption);
              currentNode.left |. castNotOption)
           else
             if
               (isLeft currentNode) &&
                 (not (isLeft (currentNode.parent |. castNotOption)))
             then
               (rotateRight rbt (currentNode.parent |. castNotOption);
                currentNode.right |. castNotOption)
             else currentNode in
         (currentNode.parent |. castNotOption).color <- Black;
         ((grandParentOf currentNode) |. castNotOption).color <- Red;
         if isLeft currentNode
         then rotateRight rbt ((grandParentOf currentNode) |. castNotOption)
         else rotateLeft rbt ((grandParentOf currentNode) |. castNotOption))
let add rbt value ~height  =
  rbt.size <- (rbt.size + 1);
  (let nodeToInsert = createNode ~value ~color:Red ~height in
   let inserted =
     if rbt.root == None
     then (rbt.root <- (Some nodeToInsert); true)
     else
       (let foundNode = findInsert rbt rbt.root nodeToInsert value in
        foundNode == None) in
   if inserted
   then
     (rbt |. (updateSumRecursive nodeToInsert);
      _addLoop rbt nodeToInsert;
      Some nodeToInsert)
   else None)
let removeNode rbt node =
  let nodeToRemove =
    match ((node.left), (node.right)) with
    | (Some _, Some _) ->
        let successor = (peekMinNode node.right) |. castNotOption in
        (node.value <- (successor.value);
         node.height <- (successor.height);
         successor)
    | _ -> node in
  let successor =
    match nodeToRemove.left with | None -> nodeToRemove.right | left -> left in
  let (successor, isLeaf) =
    match successor with
    | None ->
        let leaf = createNode ~value:([%bs.raw "0"]) ~color:Black ~height:0. in
        let isLeaf = ((fun x -> x == leaf)[@bs ]) in (leaf, isLeaf)
    | Some successor -> (successor, (((fun _ -> false))[@bs ])) in
  let nodeParent = nodeToRemove.parent in
  successor.parent <- nodeParent;
  (match nodeParent with
   | None -> ()
   | Some parent ->
       parent |. (leftOrRightSet ~node:nodeToRemove (Some successor)));
  rbt |. (updateSumRecursive successor);
  if nodeToRemove.color == Black
  then
    (if successor.color == Red
     then
       (successor.color <- Black;
        if successor.parent == None then rbt.root <- (Some successor))
     else
       (let break = ref false in
        let successorRef = ref successor in
        while not break.contents do
          let successor = successorRef.contents in
          match successor.parent with
          | None -> (rbt.root <- (Some successor); break.contents <- true)
          | Some successorParent ->
              let sibling = siblingOf successor in
              (if
                 (sibling != None) &&
                   ((sibling |. castNotOption).color == Red)
               then
                 (successorParent.color <- Red;
                  (sibling |. castNotOption).color <- Black;
                  if isLeft successor
                  then rotateLeft rbt successorParent
                  else rotateRight rbt successorParent);
               (let sibling = siblingOf successor in
                let siblingNN = sibling |. castNotOption in
                if
                  (successorParent.color == Black) &&
                    ((sibling == None) ||
                       (((siblingNN.color == Black) &&
                           ((siblingNN.left == None) ||
                              ((siblingNN.left |. castNotOption).color ==
                                 Black)))
                          &&
                          ((siblingNN.right == None) ||
                             ((siblingNN.right |. castNotOption).color ==
                                Black))))
                then
                  (if sibling != None then siblingNN.color <- Red;
                   successorRef.contents <- successorParent)
                else
                  if
                    (successorParent.color == Red) &&
                      ((sibling == None) ||
                         (((siblingNN.color == Black) &&
                             ((siblingNN.left == None) ||
                                ((siblingNN.left |. castNotOption).color ==
                                   Black)))
                            &&
                            ((siblingNN.right == None) ||
                               ((siblingNN.right |. castNotOption).color ==
                                  Black))))
                  then
                    (if sibling != None then siblingNN.color <- Red;
                     successorParent.color <- Black;
                     break.contents <- true)
                  else
                    if
                      (sibling != None) &&
                        ((sibling |. castNotOption).color == Black)
                    then
                      (let sibling = sibling |. castNotOption in
                       if
                         (((isLeft successor) &&
                             ((sibling.right == None) ||
                                ((sibling.right |. castNotOption).color ==
                                   Black)))
                            && (sibling.left != None))
                           && ((sibling.left |. castNotOption).color == Red)
                       then
                         (sibling.color <- Red;
                          (sibling.left |. castNotOption).color <- Black;
                          rotateRight rbt sibling)
                       else
                         if
                           (((not (isLeft successor)) &&
                               ((sibling.left == None) ||
                                  ((sibling.left |. castNotOption).color ==
                                     Black)))
                              && (sibling.right != None))
                             &&
                             ((sibling.right |. castNotOption).color == Red)
                         then
                           (sibling.color <- Red;
                            (sibling.right |. castNotOption).color <- Black;
                            rotateLeft rbt sibling);
                       break.contents <- true)
                    else
                      (let sibling = siblingOf successor in
                       let sibling = sibling |. castNotOption in
                       sibling.color <- (successorParent.color);
                       if isLeft successor
                       then
                         ((sibling.right |. castNotOption).color <- Black;
                          rotateRight rbt successorParent)
                       else
                         ((sibling.left |. castNotOption).color <- Black;
                          rotateLeft rbt successorParent))))
          done));
  if ((isLeaf successor)[@bs ])
  then
    (if rbt.root == (Some successor) then rbt.root <- None;
     (match successor.parent with
      | None -> ()
      | Some parent -> parent |. (leftOrRightSet ~node:successor None)))
let remove rbt value =
  match findNode rbt rbt.root value with
  | Some node -> (rbt |. (removeNode node); rbt.size <- (rbt.size - 1); true)
  | None -> false
let rec findNodeThroughCallback rbt node cb =
  match node with
  | None -> None
  | Some node ->
      let cmp = ((cb node)[@bs ]) in
      if cmp == 0
      then Some node
      else
        if cmp < 0
        then findNodeThroughCallback rbt node.left cb
        else findNodeThroughCallback rbt node.right cb
let removeThroughCallback rbt cb =
  match findNodeThroughCallback rbt rbt.root cb with
  | Some node -> (rbt |. (removeNode node); rbt.size <- (rbt.size - 1); true)
  | None -> false
let make ~compare  = { size = 0; root = None; compare }
let makeWith array ~compare  =
  let rbt = make ~compare in
  array |.
    (Js.Array2.forEach
       (fun (value, height) -> (add rbt value ~height) |. ignore));
  rbt
let rec heightOfInterval rbt node lhs rhs =
  match node with
  | None -> 0.
  | Some n ->
      if (lhs == None) && (rhs == None)
      then n.sum
      else
        if
          (lhs != None) &&
            (((rbt.compare n.value (lhs |. castNotOption))[@bs ]) < 0)
        then rbt |. (heightOfInterval n.right lhs rhs)
        else
          if
            (rhs != None) &&
              (((rbt.compare n.value (rhs |. castNotOption))[@bs ]) > 0)
          then rbt |. (heightOfInterval n.left lhs rhs)
          else
            (n.height +. (rbt |. (heightOfInterval n.left lhs None))) +.
              (rbt |. (heightOfInterval n.right None rhs))
let heightOfInterval rbt lhs rhs = heightOfInterval rbt rbt.root lhs rhs
let rec firstVisibleNode node top =
  match node with
  | None -> None
  | Some node ->
      if node.sum <= top
      then None
      else
        (let nodeHeight = node.height in
         let sumLeft =
           match node.left with | None -> 0.0 | Some left -> left.sum in
         if sumLeft > top
         then firstVisibleNode node.left top
         else
           if (sumLeft +. nodeHeight) > top
           then Some node
           else
             (let offset = sumLeft +. nodeHeight in
              firstVisibleNode node.right (top -. offset)))
let lastVisibleNode node top =
  match firstVisibleNode node top with
  | None -> node |. peekMaxNode
  | first -> first
let firstVisibleValue rbt ~top  =
  match firstVisibleNode rbt.root top with
  | None -> None
  | Some node -> Some (node.value)
let rec leftmost node =
  match node.left with | None -> node | Some node -> node |. leftmost
let rec firstRightParent node =
  match node.parent with
  | None -> None
  | Some parent ->
      ((if isLeft node then Some parent else parent |. firstRightParent)
      [@ns.ternary ])
let nextNode node =
  match node.right with
  | None -> node |. firstRightParent
  | Some right -> Some (right |. leftmost)
let rec sumLeftSpine node ~fromRightChild  =
  let leftSpine =
    match node.left with
    | None -> node.height
    | Some left -> ((if fromRightChild then node.height +. left.sum else 0.0)
        [@ns.ternary ]) in
  match node.parent with
  | None -> leftSpine
  | Some parent ->
      leftSpine +.
        (parent |.
           (sumLeftSpine ~fromRightChild:(parent.right == (Some node))))
let getY node = (node |. (sumLeftSpine ~fromRightChild:true)) -. node.height
let rec iterate ~inclusive  firstNode lastNode ~callback  =
  match firstNode with
  | None -> ()
  | Some node ->
      (if inclusive then ((callback node)[@bs ]);
       if firstNode != lastNode
       then
         (if not inclusive then ((callback node)[@bs ]);
          iterate ~inclusive (node |. nextNode) lastNode ~callback))
let rec iterateWithY ?y  ~inclusive  firstNode lastNode ~callback  =
  match firstNode with
  | None -> ()
  | Some node ->
      let y = match y with | None -> node |. getY | Some y -> y in
      (if inclusive then ((callback node y)[@bs ]);
       if firstNode != lastNode
       then
         (if not inclusive then ((callback node y)[@bs ]);
          iterateWithY ~y:(y +. node.height) ~inclusive (node |. nextNode)
            lastNode ~callback))
let rec updateSum node ~delta  =
  match node with
  | None -> ()
  | Some node ->
      (node.sum <- (node.sum +. delta); node.parent |. (updateSum ~delta))
let updateHeight node ~height  =
  let delta = height -. node.height in
  node.height <- height; (Some node) |. (updateSum ~delta)
type nonrec 'value oldNewVisible =
  {
  mutable old: 'value array ;
  mutable new_: 'value array }
let getAnchorDelta rbt ~anchor  =
  match anchor with
  | None -> 0.0
  | Some (value, y) ->
      (match rbt |. (findNode rbt.root value) with
       | Some node -> y -. (node |. getY)
       | None -> 0.0)
let onChangedVisible ?(anchor= None)  rbt ~oldNewVisible  ~top:top_
  ~bottom:bottom_  ~appear  ~remained  ~disappear  =
  let old = oldNewVisible.new_ in
  let new_ = oldNewVisible.old in
  (new_ |.
     (Js.Array2.removeCountInPlace ~pos:0 ~count:(new_ |. Js.Array2.length)))
    |. ignore;
  oldNewVisible.old <- old;
  oldNewVisible.new_ <- new_;
  (let anchorDelta = rbt |. (getAnchorDelta ~anchor) in
   let top = top_ -. anchorDelta in
   let top = ((if top < 0.0 then 0.0 else top)[@ns.ternary ]) in
   let bottom = bottom_ -. anchorDelta in
   let first = firstVisibleNode rbt.root top in
   let last = lastVisibleNode rbt.root bottom in
   let oldLen = old |. Js.Array2.length in
   let oldIter = ref 0 in
   iterateWithY ~inclusive:true first last
     ((fun node ->
         fun y_ ->
           let y = y_ +. anchorDelta in
           if y >= 0.0
           then
             (while
                (oldIter.contents < oldLen) &&
                  (((rbt.compare (Js.Array2.unsafe_get old oldIter.contents)
                       node.value)
                     [@bs ]) < 0)
                do
                (((disappear (Js.Array2.unsafe_get old oldIter.contents))
                 [@bs ]);
                 oldIter.contents <- (oldIter.contents + 1))
                done;
              (new_ |. (Js.Array2.push node.value)) |. ignore;
              if oldIter.contents < oldLen
              then
                (let cmp =
                   ((rbt.compare (Js.Array2.unsafe_get old oldIter.contents)
                       node.value)
                   [@bs ]) in
                 if cmp = 0
                 then
                   (((remained node y)
                    [@bs ]);
                    oldIter.contents <- (oldIter.contents + 1))
                 else ((appear node y)[@bs ]))
              else ((appear node y)[@bs ])))[@bs ]);
   while oldIter.contents < oldLen do
     (((disappear (Js.Array2.unsafe_get old oldIter.contents))
      [@bs ]);
      oldIter.contents <- (oldIter.contents + 1))
     done)
