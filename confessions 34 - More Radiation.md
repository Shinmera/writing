![header](http://shinmera.tymoon.eu/public/17351834_p0.jpg)
I was bored enough to implement the SQLite bridge for Radiance last week, so I'm now ready to share another quick looksies into some more of Radiance's ecosystem with you. This time we'll have a first gander at the interfaces system and database interaction, an important component of many an application.

What we'll do this time is build a very simple Twitter mock application. As [last time](http://blog.tymoon.eu/article/291), we'll start out by creating a new module for our app.

```commonlisp
(ql:quickload :radiance)
(radiance:startup)
(radiance:create-module :twatter)
(in-package #:twatter)
```

In order to store statuses, we'll use a database. What kind is not important to us, so we'll use Radiance's database interface. Additionally we'll use a thin object wrapper that is available as an interface as well, called a `data-model`. In order to use interfaces, we need to have a way to load and depend on them. Luckily for us, Radiance doesn't require its own weird dependency and loading system and instead hooks into ASDF. Open up your `twatter.asd` and change it to look similar to the following.

```commonlisp
(asdf:defsystem #:twatter
  ...
  :depends-on ((:interface :database)
               (:interface :data-model)))
```

Next we want to actually load these systems to be able to work with them. We can do that by just quickloading our system again.

```commonlisp
(ql:quickload :twatter)
```

By default Radiance's configuration sets `i-sqlite` and `r-simple-model` as the implementation for the two, which are included as drivers. Since we have already started up Radiance before, the database is not automatically connected. Instead we'll simulate the startup by triggering the `startup-done` hook.

```commonlisp
(trigger 'startup-done)
```

You should be getting a log message about it connecting to the `DEV` database. You can now query the database by using the functions within the `db` package. All interfaces are specified in [`radiance-web/interfaces.lisp`](https://github.com/Shinmera/radiance-web/blob/master/interfaces.lisp), which is probably an easier way to look at what you can do than jumping straight into the implementation definitions.

First thing we need for our application is a table to store the statuses in of course. In Radiance the usual way to do this is to define a trigger on `db:connected` and then call `db:create`. Open up your `twatter.lisp` and do something akin the following.

```commonlisp
(define-trigger db:connected ()
  (db:create 'twatter-statuses '((user (:varchar 32)) (text (:varchar 140)) (time (:integer 5)))))
```

What kind of types for the fields are available is specified strictly. The Radiance specification for the database interface is pretty much complete. See the current [draft](http://shinmera.tymoon.eu/public/radiance-spec.pdf) on page 19-28. The code above will ensure that our table with the requested structure exists. To make sure that it exists right now, you can either copypaste that `db:create` form into your REPL or trigger the `db:connected` hook.

You can ascertain that it worked by calling `db:structure` on `twatter-statuses`. It should reflect almost exactly the structure you passed in. Depending on the implementation of the database and the types it has available, it might upgrade types to bigger ones, as is written in the spec.

First let's add in an API function to make new statuses.

```commonlisp
(define-api twatter/status/create (user text) ()
  (db:insert 'twatter-statuses
             `((user . ,user)
               (text . ,text)
               (time . ,(get-universal-time))))
  (api-output "Ok"))
```

Call it up! [/api/twatter/status/create?user=tester&text=bla](http://localhost:8080/api/twatter/status/create?user=tester&text=bla) and let's see where we stand.

```commonlisp
(mapcar #'dm::print (dm:get 'twatter-statuses (db:query :all)))
```

This uses a `data-model` extension function the `r-simple-model` provides. We'll use it to make it easy to print what the database contains. You should see a nice list representation of the status/es you made.

At this point it would be useful to explain a bit more about the interfaces mechanism. While it isn't strictly part of Radiance, it is nevertheless something that grew out of it. The library used to provides the functionality is called [modularize-interfaces](http://shinmera.github.io/modularize-interfaces/), an extension to the module system we discussed before. It provides a straightforward macro that allows you to define stubs for functions, macros, and whatnot. It then expands to a module and stub definitions, which implementing modules need to override.

If you take a look at the `r-simple-model` module, you'll see that this too works in a very straight-forward manner, thanks to the way CL functions. In order to set a module as an implementation for an interface, simply add an `(:implements #:interface-foo)` to the `define-module` form. As hinted at in the last entry, the `:implements` option is an extension to the `define-module` form that makes it differ from a standard `defpackage`.

One part that is not included in `modularize-interfaces` and is instead part of `radiance-core`, is what actually maps modules to interfaces and provides the resolving of the `(:interface #:foo)` in the ASDF system. You can set your preferred implementations in the `radiance.uc.lisp` file in Radiance's root folder.

Radiance also defines a mechanism to allow optional features depending on the active implementations. So if you f.e. had a part of your application that provides administration panels, but would not want to strictly require the availability of an administration implementation, you can use the `define-implement-hook` macro, but we'll get to explaining that in-depth some other time.

For now let's worry about building a front-end for our twatter application. We'll, once again, use Clip as our templating system of choice. Add `r-clip` to the ASDF dependencies of the twatter system and load it. Next, we'll make a crude template.

```
&lt;!DOCTYPE html&gt;
&lt;html xmlns="http://www.w3.org/1999/xhtml"&gt;
  &lt;head&gt;
    &lt;meta charset="utf-8"/&gt;
    &lt;title&gt;&lt;c:splice lquery="(text user)" /&gt; - Twatter Profile&lt;/title&gt;
    &lt;link rel="stylesheet" type="text/css" href="/static/twatter/default.css" /&gt;
  &lt;/head&gt;
  &lt;body&gt;
    &lt;h1 lquery="(text user)"&gt;USER&lt;/h1&gt;
    &lt;h3&gt;Create a new Status&lt;/h3&gt;
    &lt;form action="/api/twatter/status/create" method="post"&gt;
      &lt;textarea name="text" maxlength="140" required&gt;&lt;/textarea&gt;
      &lt;input type="hidden" name="user" value="VALUE" lquery="(val user)" /&gt;
      &lt;input type="submit" value="Update!" /&gt;
    &lt;/form&gt;
    &lt;h3&gt;Status Updates&lt;/h3&gt;
    &lt;ul iterate="statuses"&gt;
      &lt;li&gt;
        &lt;article&gt;
          &lt;header&gt;
            &lt;span class="username" lquery="(text user)"&gt;USER&lt;/span&gt;
            &lt;span class="timestamp" lquery="(text (twatter::format-time time))"&gt;1900-01-01 00:00:00&lt;/span&gt;
          &lt;/header&gt;
          &lt;blockquote lquery="(text text)"&gt;
            TEXT
          &lt;/blockquote&gt;
        &lt;/article&gt;
      &lt;/li&gt;
    &lt;/ul&gt;
  &lt;/body&gt;
&lt;/html&gt;
```

This is a bit of a big template for a tutorial, but we'll use this project in further entries and extend it along the way, so we might as well make a proper one. Wiring this up to the data is a short `define-page` away:

```commonlisp
(defun format-time (ut)
  (format NIL "~:@{~4,'0d.~2,'0d.~2,'0d ~2,'0d:~2,'0d:~2,'0d~}"
          (subseq (nreverse (multiple-value-list (decode-universal-time ut))) 3)))

(define-page twatter-user #@"/user/([a-zA-Z]+)" (:uri-groups (username) :lquery (template "user.ctml"))
  (r-clip:process
   T
   :user username
   :statuses (dm:get 'twatter-statuses (db:query :all) :sort '((time :DESC)))))
```

I added an extra function which formats the timestamp prettily. It is used by the template as you might notice upon closer inspection. For now you can hit up [localhost:8080/user/test](http://localhost:8080/user/test) and see the status/es you made. You should also be able to submit new statuses, although it's sub-par right now as it won't redirect you back upon doing so.

Before we get to prettying that up though, let's add some CSS to make this less of a bother to look at. Create a file called `default.css` into the `static/` folder of your twatter module and define some style rules. Maybe something like this.

```
html,body{
    font-family: Arial, sans-serif;
    background: #FAFAFA;
}

body{
    width: 800px;
    margin: 0 auto 0 auto;
    border: 1px solid #DDD;
    background: #FFF;
    border-top: none;
}

h1{margin: 0; padding: 20px;}
h3{margin: 10px;}

form{padding: 0 10px 0 10px;}
form textarea, form input{
    box-sizing: border-box;
    width: 100%;
    border: 1px solid #DDD;
    background: #FAFAFA;
}

ul{list-style: none; padding: 0;}
ul li{padding: 10px; border-top: 1px solid #DDD;}
ul li .timestamp{float:right;}
ul li blockquote{font-size: 18pt; margin: 5px; word-wrap: break-word;}
```

Excuse the compressed layout, but I don't want to pad the blog out with too much CSS. I've already linked the CSS file in the template above, so the effect should be immediate. As you can see, the path `/static/MODULE/` is automatically resolved to the static directory within your module folder. Now that we've taken care of this, we can move on to making the application more robust.

First, one solution to the problem of the gross form submission would be to use an AJAX request in the back. I'm not a fan of JS solutions to essential things and prefer my sites to work without JS and only use it for additional smoothness. Instead, in order to make the API page work nicely for browser clients too, we'll add an extra parameter that'll tell it whether to redirect or to return data.

```commonlisp
(define-api twatter/status/create (user text &optional client) ()
  (unless (<= 1 (length text) 140)
    (error 'api-argument-invalid :argument 'text :message "Text must be between 1 and 140 characters."))
  (unless (<= 1 (length user) 32)
    (error 'api-argument-invalid :argument 'text :message "User must be between 1 and 32 characters."))
  (db:insert 'twatter-statuses
             `((user . ,user)
               (text . ,text)
               (time . ,(get-universal-time))))
  (when client (redirect (referer)))
  (api-output "Ok"))
```

I also took the liberty of adding some validation functions to make sure we have some nice errors if someone tried something nasty. To make this work though we'll need to add a new hidden field to our template:

```
&lt;input type="hidden" name="client" value="true" /&gt;
```

Now submission happens smoothly from the client side as well! Next we'd most likely want to implement some kind of system that allows us to actually login as someone instead of just post as anyone, add some way to browse through statuses, follow other people and combine statuses into a home timeline and so on.

I'll keep that all for another time though.
