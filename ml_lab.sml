(***************************************************************
*
* CSCI 305 - ML Programming Lab
*
* <Tanner> <Gascon>
* <tanner.a.gascon@gmail.com>
*
***************************************************************)

(* Define your data type and functions here *)
datatype 'element set = Empty | Set of 'element * 'element set;

fun f [] = [] (* a *)
| f (x::xs) = (x+1) :: (f xs) (* b *);

(*Create a function to see if an element is a member of a list by recursion *)
fun isMemberList(e,[]) = false 
	| isMemberList(e, x::xs) = 
		if(e=x) then true
		else isMemberList(e,xs); 

(*Create a function to see if an element is a member of a set by recursion  *)
fun isMember e Empty = false 
	| isMember e (Set(x, xs)) = 
		if(e=x) then true
		else isMember e xs; 

(*Create a function that converts a list to a set by iterating through the list and checking if the element is a repeat or not*)
fun list2Set([]) = Empty 
	|  list2Set(x::xs) = 
		if(isMemberList(x, xs)) then list2Set(xs) else Set (x, list2Set(xs));

(*Create a function that finds the union of two sets by checking if the element a is in set y and recursively adding to a new set*)
fun union a (Empty) = a 
	| union (Empty) a = a
	| union (Set(a,b)) y =
		if(isMember a y) then union b y else Set(a, union b y);

(*Create a function that finds the intersection of two sets and returns the intersected set*)
fun intersect a (Empty) = Empty 
	| intersect (Empty) a = Empty
	| intersect (Set(a,b)) y =
		if(isMember a y) then Set(a, intersect b y) else intersect b y;

(* Simple function to stringify the contents of a Set of characters *)
fun stringifyCharSet Empty = ""
  | stringifyCharSet (Set(y, ys)) = Char.toString(y) ^ " " ^ stringifyCharSet(ys);

(* Simple function to stringify the contents of a Set of ints *)
fun stringifyIntSet Empty = ""
  | stringifyIntSet (Set(w, ws)) = Int.toString(w) ^ " " ^ stringifyIntSet(ws);

(* Simple function to stringify the contents of a Set of strings *)
fun stringifyStringSet Empty = ""
  | stringifyStringSet (Set(z, zs)) = z ^ " " ^ stringifyStringSet(zs);

(* Simple function that prints a set of integers *)
fun print_int x = print ("{ " ^ stringifyIntSet(x) ^ "}\n");

(* Simple function that prints a set of strings *)
fun print_str x = print ("{ " ^ stringifyStringSet(x) ^ "}\n");

(* Simple function that prints a set of characters *)
fun print_chr x = print ("{ " ^ stringifyCharSet(x) ^ "}\n");

list2Set [1, 3, 2];
list2Set [#"a", #"b", #"c"];
list2Set [];
list2Set [6, 2, 2];
list2Set ["x", "y", "z", "x"];

(* Question 1 *)
f [3, 1, 4, 1, 5, 9];

(* Question 5 *)
val quest5 = isMember "one" (list2Set ["1", "2", "3", "4"]);
print ("\nQuestion 5: " ^ Bool.toString(quest5) ^ "\n");

(* Question 7 *)
val quest7 = list2Set ["it", "was", "the", "best", "of", "times,", "it", "was", "the", "worst", "of", "times"];
print "\nQuestion 7: ";
print_str quest7;
print "\n";

(* Question 9 *)
print "\nQuestion 9: ";
print_str (union (list2Set ["green", "eggs", "and"]) (list2Set ["ham"]));

(* Question 10 *)
print "\nQuestion 10: ";
print_str (intersect (list2Set ["stewed", "tomatoes", "and", "macaroni"]) (list2Set ["macaroni", "and", "cheese"]));
