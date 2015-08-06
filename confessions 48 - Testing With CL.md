![header](https://filebox.tymoon.eu/file/TXpNdw==)
I haven't come across this anywhere yet, but I think it's worth writing a quick entry about, just so that it's referable. So, writing tests is a common enough occurrence in programming and Common Lisp is no exception. The vast amount of [testing frameworks](http://cliki.net/Test%20Framework) is both a sign of the repeated desire to have a comfortable way to write tests and the general 'I can do it better' syndrome prevalent in Common Lisp. However, this blog is not about those things, but about another, much easier aspect: Running tests.

Having an easy way to run your tests, possibly even automated, is great. Most frameworks don't go into that, so the first instinct of any test writer is to just dump all tests into a file and have a function to run them. Hopefully the tests will be segregated into their own package or system. Still, it's far less than stellar to have to know what the test system is called, load it manually and then run some project-specific test function.

Luckily, if you're using [ASDF](https://reader.tymoon.eu/article/267) for your systems there's a way to make this all streamlined and convenient. The first thing you will want to do is define a separate system for your tests that depends on whatever testing framework you use and the system to test, of course. That way the tests won't have to be loaded if the user doesn't need them. Then, in the system definition of your main project you add a new property to connect the two:

```commonlisp
(asdf:defsystem my-system
  ...
  :in-order-to ((asdf:test-op (asdf:test-op :test-system))))
```

What this does is tell ASDF that if you perform the `test-op` on your system, it will delegate that to calling `test-op` on `:test-system`, which should be adapted to whatever you named your test system, naturally. This means that you can now call `(asdf:test-system :my-system)` and have it automatically load and test your proper test system. But, we aren't quite there yet, there's one last thing we need to do, which is to tell ASDF how to execute our test suite.

In order to do this we'll need a method on `asdf:perform`, the function responsible for performing any kind of ASDF operation on a component or system. This method definition should be in the source of your test system and can either call or directly replace your main test function:

```commonlisp
(defmethod asdf:perform ((op asdf:test-op) (sys (eql (asdf:find-system :my-system))))
  (run-tests))
```

Once that's in, you can freely call `asdf:test-system` and it should just work. Doing it this way is beneficial both because it gives users a streamlined interface to perform tests and because it is neatly integrated with the rest of the build system and thus automatable.

Happy testing!
