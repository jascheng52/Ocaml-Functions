(*Jason Cheng 113502781 R06 *)
(*Compute integer exponents where x is base and n is power*)
let rec pow x n = 
    if n = 0 then 1
    else x * (pow x (n-1));;
(*Compute float exponents where x is base and n is power*)
let rec float_pow x n = 
    if n = 0 then 1.0
    else x *. (float_pow x (n-1));;

(*Helper for reverse List*)
let rec revH list newList= match list with
| [] -> newList
| h::t -> let addList = h::newList in revH t addList;;
(*Reverses List where list is the input*)
let rev list = revH list []

(*Gets first element from list *)
let rec getFirst list = match list with
  | [] -> []
  | h::t -> [h];;
(*Helper for compress*)
let rec compressHelp list newList = match list with
  | [] -> newList
  | h::t -> if (getFirst newList = [h]) then (compressHelp t newList)
      else (compressHelp t (h::newList));;
(*Remove Duplicates from a list*)
let compress list = rev (compressHelp list []);;



(*Remove elements from list if the element satisfies the predicate
theFunction*)
let rec remove_if list theFunction = match list with
| [] -> []
| h::t -> if(theFunction h) then remove_if t theFunction
            else h::(remove_if t theFunction);;
(*Helper for slice*)
let rec sliceHelper list i j newList index = match list with
| [] -> newList
| h::t -> if index >= i && index < j then (let addList = h::newList in sliceHelper t i j addList (index +1))
    else (sliceHelper t i j newList (index+ 1) );;
(*Slices a list from int i(inclusive) to int j (exclusive)*)
let slice list i j = match list with
| [] -> []
| h::t -> if i > j then [] else let finalList = sliceHelper list i j [] 0 in rev finalList;;



(*Problem 5 *)
(*Creates Equivalent Classes*)
let rec createEquiv element equivFunc list = match list with
| [] -> []
| h::t -> if (equivFunc element h) then h::(createEquiv element equivFunc t)
            else createEquiv element equivFunc t
;; 
 (*Removes all occurences of dupe from the list*)
let rec removeDupe list dupe = match list with
| [] -> []
| h::t -> if (h = dupe) then removeDupe t dupe
            else h::(removeDupe t dupe)
;;
(*Removes intersection of list and removeList *)
let rec traverseRemove list removeList = match removeList with
| [] -> list;
| h::t -> traverseRemove (removeDupe list h) t
;;
(*Returns a list of equivalance classes with 
the cooresponding elements in the class
based off the function equivFunc*)
let rec equivs equivFunc list = match list with
| [] -> []
| h::t ->  let classList = createEquiv h equivFunc list in  
            (classList)::(equivs equivFunc (traverseRemove t classList))


(*Checks if n is a prime from a primelist*)
let rec checkPrime n list = match list with
| [] -> true
| h::t -> if (n mod h = 0) then false
            else checkPrime n t
;;
(*Generates a list of primes up to n count = 2 descending*)
let rec getPrimes n list count = 
if(count <= n) then (
    if (checkPrime count list) then let update = count::list in getPrimes n update (count+1)
    else getPrimes n list (count+1) )
else list
;;
(*Checks if element in list*)
let rec inList element list = match list with
| [] -> false
| h::t -> if (h = element) then true else inList element t
;;
(*Finds pair of numbers*)
let rec findPair primeList n = match primeList with
| [] -> (0,0)
| h::t -> if(inList (n-h) primeList) then ((n-h),h ) else findPair t n
;;

(*Finds a Goldback pair where the sum is n*)
let goldbachpair n = let primeList = getPrimes n [] 2 in findPair primeList n;;



(*Returns a boolean if the function f and g
produce the same results of every element on the list*)
let rec equiv_on f g list = match list with
| [] -> true
| h::t -> if ((f h) = (g h) ) then equiv_on f g t
          else false
;; 

(*Returns a list where cmp ,a function that compares
2 elements and returns one of them, is applied to the list*)
let rec pairwisefilter cmp lst = match lst with
| [] -> []
| h::m::t -> (cmp h m)::(pairwisefilter cmp t)
| h::t -> [h]
;;

(*Gets term from tuple*)
let getTerm tuple = match tuple with
| (a,n) -> fun x -> a*(pow x n)
;;
(*Generates list of terms*)
let rec termList list = match list with
| [] -> []
| h::t -> (getTerm h)::(termList t)
;;
(*Applies x to each term*)
let rec calcList list x sum = match list with
| [] -> sum
| h::t -> (h x) + (calcList t x sum)
;;
(*Takes a list of tuple(a,b) where a represents a coefficient and b an exponent
and returns a polynomial function corresponding to the list*)
let polynomial list = let terms = termList list in   
                     fun x  -> (calcList terms x 0)
;;


(*powerset Helper*)
let rec combin element list listcp = match list with
| [] -> listcp
| h::t -> let newList = (element::h)::listcp in 
            combin element t newList
;;
(*Creates powerset of a list*)
let rec powerset list = match list with
| [] -> [[]]
| h::t -> let powSetT = powerset t in
            combin h powSetT powSetT
;;


