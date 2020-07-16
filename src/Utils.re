/**
 * Applies the function to both elements of the tuple, and returns a tuple of the results
 */
let (<%>) = ((a, b), f) => (f(a), f(b));
let (>>=) = (a, f) => Js.Promise.then_(f, a)
let (>>-) = (a, f) => Js.Promise.then_(a => Js.Promise.resolve( f(a) ), a)
let (>>|) = (a, f) => Js.Promise.catch(a => Js.Promise.resolve( f(a) ), a)

let (->>): ('a => 'b, 'c => 'd) => ('a => 'd) = (f, g) => a => g(f(a))

let int_of_bool = p => if (p) {1} else {0};

// Converts a function of two elements into a function of a tuple
let uncurry: (('a, 'b) => 'c) => ((('a, 'b)) => 'c) = 
  f => ((a, b)) => f(a,b)

let make_dict: (string, string) => array(string) = (a, b) => {
  open Belt_SetString;

  let aux = a =>
    a
    ->Js.String2.split("")
    ->Belt.Array.reduce(empty, add);

  let va = aux(a);
  let vb = aux(b);

  union(va, vb)->toArray;
};

let vectors_of_words: ((string, string)) => (array(int), array(int)) = ((a, b)) => {
  open Belt.Array;
  
  let count = (tx, a) => 
    reduce(tx, 0, (acc, b) => 
      acc + int_of_bool(a == b)
    );
  
  let aux = (dict, a) => 
    dict->map(count(a->Js.String2.split("")))

  let dict = make_dict(a, b);
  (a, b) <%> aux(dict);
};

/**
 * Calculates the cosine similarity of 2 vectors.
 * Returns `None` when the vectors are of different lengths
 */
let cos: ((array(int), array(int))) => option(float) =
  ((a, b)) => {
    open Belt.Array;

    let (m, n) = (a, b) <%> length;

    if (m != n) {
      None;
    } else {

      let (dot_prod, sum_a, sum_b) =
        zip(a, b)
        ->reduce(
            (0.0, 0.0, 0.0),
            ((dp, sa, sb), (ai, bi)) => {
              let (ai, bi) = (ai, bi) <%> float_of_int;

              (
                dp +. ai *. bi, 
                sa +. ai ** 2.0, 
                sb +. bi ** 2.0
              );
            },
          );

      if (sum_a == 0.0 || sum_b == 0.0) {
        Some(0.0);
      } else {
        Some(dot_prod /. sqrt(sum_a *. sum_b));
      };

    };
  };
