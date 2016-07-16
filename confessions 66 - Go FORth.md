![header](https://filebox.tymoon.eu/file/TVRBNU5RPT0=)  
As part of yet another yak-shaving quest I've come to write a library that I only now realise I've been missing for a while: an extensible and generic iteration macro.

Now, you might be familiar with the [Iterate](https://common-lisp.net/project/iterate/) library that is supposed to fill some of the same niches as [For](https://shinmera.github.io/for) does. Despite being available for a very long time, Iterate is not very commonplace as far as I've been able to tell. Most people still use the Common Lisp standard `loop`, `do*` or `map*` variants-- I do too.

I've tried to get into iterate multiple times, but I never really was able to get along with it, especially when it came to figuring out how to extend it for further constructs. Now, naturally this may just be my problem, but nevertheless I'm bold enough to consider that enough justification for me to go ahead and write my own attempt at a solution for the problem. I won't elaborate why I don't like iterate here as I believe there isn't much constructive or useful input to be gained from doing so.

Instead I will try to illustrate what For does, why it does it, and how it does it. A good part of that is already covered [in the documentation](https://shinmera.github.io/for) but I will allow myself to be a bit more prosaic rather than declarative here. So let's dive in and have a look at the most simple of loops-- an infinite one!

```common-lisp
(for ())
```

Absolutely breathtaking.

The main idea behind For in contrast to Loop and Iterate is to mirror the structure of `let`. As such we always have a list of bindings and a body. Unlike `let` however, every variable is followed by a symbol that describes what kind of binding it is-- how it is initialised, stepped, how if at all it terminates the loop, and whether it delivers a return value. So let's take a look at something a bit more sophisticated.

```common-lisp
(for ((i ranging 1 10))
  (print i))
```

Here we see an actual binding in action. We bind a variable `i` using the `ranging` type with the arguments `1` and `10`. As probably expected this will go through the numbers 1 to 10 and print each of them. Bindings can accept any number and kind of argument they want to:

```common-lisp
(for ((i ranging 1 10 :by 2))
  (print i))
```

And now it will step by twos. Stunning.

Similar to Loop, For can of course also iterate over various sequences and other objects. Out of the box iteration bindings for lists, vectors, hash-tables, and packages is provided. Also just like loop we can accumulate values in various ways. Here too we support the same things as Loop does, namely collecting, appending, nconcing, counting, summing, maximising, minimising.

Additionally however, For provides a generic iterator mechanism for the cases where you do not really know or care what type your sequence is. This can also be used to update the sequence in-place if doing so makes sense. Let's see a practical example of converting a generic kind of sequence into a list:

```common-lisp
(for ((item over my-sequence)
      (list collecting item)))
```

This will function without any work required from you for lists, vectors, arrays, streams, wild pathnames, packages, and hash-tables. It can also be extended to be able to iterate over any kind of sequence you might want by writing a new iterator class and the implementation for three simple methods.

Sometimes it's also necessary to terminate the loop according to some condition, and a binding does not seem like the correct place to put this kind of constraint. This is why in addition to bindings we have clauses that can appear in the body of the For.

```common-lisp
(for ((i from 0))
  (while (< i 10))
  (print i))
```

This is nice and can easily be implemented by a macrolet. At least that's what I went for until I started trying to wrap my brain around the problem of return values. Some clauses like `thereis` would like to return a value-- in this case whether the test has succeeded at all.

In the case of bindings where we have full control over the expressions and literals we can easily transform them however we want. This allows bindings to establish forms that wrap around the For body, add return values, and so on. However, in the case of clauses implemented through a macrolet the expansion happens within the body and at a different time. It is thus impossible¹ for the clause macro to communicate that it would like to hook into the mechanism surrounding the body. We could do it at run-time of course, but that would mean run-time consing and unnecessary tests every iteration-- way too costly.

So, unfortunately I had to retract that idea and instead go for a minimal code-walking. It is so minimal that I don't know if it can even really be called that. What For now does is look through each item in its body, test whether it is a cons with a symbol in its car that refers to a clause, and if so call the clause expander function for that. This allows us to give clauses the same amount of power as bindings have and fixes the issues we had before. As you can see however there is a cost associated with it. In order to avoid full-blown code-walking (something understandably frowned upon) we can only recognise literal top-level For body expressions as clauses.

Nevertheless I think this is a small price to pay. The amount of times I would want to use a clause within another form seems very minimal to me at this point in time. Who knows though, I may come to eat my words at a later date.

Another thing worth mentioning I think is the actual extension mechanism of For itself. As per usual for my systems there's varying levels of support to help you, but you can always ignore them and just get full control so you can define exactly what's going on how.

So- the lowest we can go is defining a clause or binding function directly. We can define the above `while` clause like that simply enough. After all, all it needs to do is expand to a test that ends the loop if passed instead of the clause form.

```common-lisp
(define-direct-clause while (form)
  (values NIL `(unless ,form (end-for)))
```

Generally if we think about what an iteration is about, we can distinguish three sections: an initialisation that introduces some values and initialises them, a body section that is executed on every step, and an end section that determines a return value. This is reflected in the three values that a binding or clause function must return-- a form to wrap around the rest of the For block, a form to evaluate every step, and an optional form to evaluate as a return value if we think we have data that would be useful to return. To illustrate the return value we'll also look at the `returning` clause, which is useful if we have a non-standard value we'd like to give back, or if we want to force the primary value to something else.

```common-lisp
(define-direct-clause returning (form)
  (values NIL NIL form))
```

All return values from bindings and clauses are gathered together into a single `values` form at the end of the For. This allows us to have multiple `collect` bindings or combine an accumulation with a clause and things like that. In general it's just convenient to allow the loop to return multiple values.

Most bindings and clauses outside of the most primitive ones will want to establish some kind of helper variables around the loop to keep, say, the head and tail of the list being accumulated. In order to provide this conveniently the `&aux` arguments in the lambda-list of the next definition macros are rewritten such that their value inside the definition body is a gensym and it automatically expands to a `let` that binds the gensym to the specified value. Thus writing our `collecting` binding becomes very simple:

```common-lisp
(define-form-binding collecting (var form &aux (head (cons NIL NIL)) (tail head))
  `(setf ,tail (setf (cdr ,tail) (cons ,form NIL))
         ,var (cdr ,head)))
```

The `head` and `tail` are bound to fresh gensyms in the body, so they insert gensyms into our backquote expression. Simultaneously the definition takes care of the first return value for us by constructing an appropriate `let*` form that binds the gensym contained in `head` and `tail` to `(cons NIL NIL)` and `head` respectively. This form is then wrapped around the rest of the For so that the variables are available within the body.

Finally, often times we also know that all of the arguments passed to the binding need to only be evaluated once before the loop. To make this convenient we have `define-value-binding` which treats the actual binding arguments similar to how the `&aux` arguments work. Using this, defining something like `across` becomes trivial as well:

```common-lisp
(define-value-binding across (var vector &aux (i -1) (length (length vector)))
  `(if (= ,length (incf ,i))
       (end-for)
       (update ,var (aref ,vector ,i))))
```

But just as mentioned before, if you don't trust the system to do this for you or simply don't like it, you can always return to the low-level definition macros and do the plumbing yourself.

Given that For allows you to both expand into body forms, surrounding forms, and return value forms, I think it is safe to say that pretty much every feature you might need to express in an iterator can be done and without much to write either. Take a look at the [definitions of the standard bindings and clauses](https://github.com/Shinmera/for/blob/master/standard.lisp) to get a feel for it.

Finally I'd like to take a look at the previously mentioned iterator system that For bundles with it. As stated, there's very little you need to do to add support for a new data type. Subclass `iterator`, add methods for `has-more`, `next`, and `make-iterator` and you're done. With that in place, you can directly go ahead and use the `over` binding to go through your sequence. If it makes sense you can also add support for `(setf current)`, enabling you to use the `updating` binding which permits setting the current element in the sequence as well.

All in all I hope that I've figured out some good solutions to the problems presented by an extensible looping construct. Since the system is still very young, I don't really have too much experience with it myself yet and can't make any grand claims like this being the "be all end all iteration macro" or whatever. But it doesn't have to be that either. It solves the problem that drove me into this direction, as well as a few other ones on top of that, so I'm fine with it being what it is. If I've managed to convince you to look at For to see if it fits into your toolbelt, then I would already have achieved much more than I initially set out to do.

---

[1]: This is not quite true, as pointed out to me by Mark Cox. It is indeed possible to make macros communicate with a bit of ingenuity. Relevant to this are the [COMPILER-LET-CONFUSION Issue](http://www.lispworks.com/documentation/HyperSpec/Issues/iss066_w.htm) in the CLHS, and [an example](http://plaster.tymoon.eu/view/F1) he was kind enough to write up to illustrate it.
