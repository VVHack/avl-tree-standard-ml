use "../utility.sml";

(* Implements an AVL tree in Standard ML and also includes a pretty print  *)

type key = string;
type 'a node = key * 'a
datatype 'a tree = LEAF | TREE of 'a tree * 'a node * 'a tree;
val empty = LEAF;

fun lookup (key, LEAF) = nil
  | lookup (key, TREE(l,(k,v),r)) =
    if key < k then lookup(key, l)
    else if key > k then lookup(key, r)
    else v;

fun depth LEAF = 0
  | depth (TREE(l,(k,v),r)) = 1 + max(depth l)(depth r);

fun left_rotate LEAF = LEAF
  | left_rotate (TREE(l, (k, v), r)) =
    case r of LEAF => raise Fail "Invalid tree for left rotation"
            | TREE(rl, (kr, vr), rr) => TREE(TREE(l, (k,v), rl), (kr, vr), rr);

fun right_rotate LEAF = LEAF
  | right_rotate (TREE(l, (k, v), r)) =
    case l of LEAF => raise Fail "Invalid tree for right rotation"
            | TREE(ll, (kl, vl), lr) => TREE(ll, (kl, vl), TREE(lr, (k, v), r));

fun balance LEAF = LEAF
  | balance (TREE(l,(k,v),r)) =
    let val depthl = depth(l)
        val depthr = depth(r) in
    if depthl - depthr > 1 then
        case l of LEAF => raise Fail "This is not possible"
                | TREE(ll, (kl, vl), lr) =>
                  let val depthll = depth(ll)
                      val depthlr = depth(lr) in
                  if depthll >= depthlr then
                      right_rotate (TREE(l, (k, v), r))
                  else
                      right_rotate (TREE(left_rotate(TREE(ll, (kl, vl), lr)), (k, v), r))
                  end
    else if depthr - depthl > 1 then
        case r of LEAF => raise Fail "This is not possible"
                | TREE(rl, (kr, vr), rr) =>
                  let val depthrl = depth(rl)
                      val depthrr = depth(rr) in
                  if depthrr >= depthrl then
                      left_rotate (TREE(l, (k, v), r))
                  else
                      left_rotate (TREE(l, (k, v), right_rotate(TREE(rl, (kr, vr), rr))))
                  end
    else TREE(l,(k,v),r)
    end;

fun insert (key,value,LEAF) = TREE(LEAF,(key,value),LEAF)
  | insert (key,value,TREE(l,(k,v),r)) =
        if key < k then
            balance(TREE(insert(key,value,l),(k,v),r))
        else if key > k then
            balance(TREE(l,(k,v),insert(key,value,r)))
        else TREE(l,(k,value),r);

fun fuse ([], []) = []
  | fuse (left_levels, []) = left_levels
  | fuse ([], right_levels) = right_levels
  | fuse (this_left_level::[], this_right_level::[]) =
         [this_left_level ^ "" ^ this_right_level]
  | fuse (this_left_level::[], this_right_level::next_right_levels) =
         [this_left_level ^ "" ^ this_right_level] @ next_right_levels
  | fuse (this_left_level::next_left_levels, this_right_level::[]) =
         [this_left_level ^ this_right_level] @ next_left_levels
  | fuse (this_left_level::"\n"::next_left_levels,
         this_right_level::"\n"::next_right_levels) =
         let val next_levels_fused = fuse(next_left_levels, next_right_levels) in
             (this_left_level ^ this_right_level)::"\n"::next_levels_fused
         end
  | fuse (this_left_level::s::next_left_levels,
         this_right_level::t::next_right_levels) = raise Domain;

fun treeToString (prefix, LEAF) = (prefix, [repeat prefix " "])
  | treeToString (prefix, TREE(l, (k, v:int), r)) =
    let val kv_str = "(" ^ k ^ "," ^ Int.toString(v) ^ ")"
        val len_kv_str = String.size("(" ^ k ^ "," ^ Int.toString(v) ^ ")") in
    let val (left_space, left_levels) = treeToString(prefix, l)
        val (right_space, right_levels) = treeToString(len_kv_str, r) in
    let val root_string =
        (repeat (left_space)  " ") ^ kv_str ^ (repeat (right_space-len_kv_str) " ")
        val space = left_space + right_space in
        (space, root_string::"\n"::fuse(left_levels, right_levels))
    end
    end
    end;

fun printtree (tree: int tree) =
    let val (num_leaves, levels) = treeToString (0, tree) in
        print ((stringListCombined levels) ^ "\n")
    end;
