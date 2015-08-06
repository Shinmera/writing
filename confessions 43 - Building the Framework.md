![header](http://filebox.tymoon.eu/file/TWprPQ==)
[Last time](http://blog.tymoon.eu/article/293) we took a quick look at how authentication and authorisation work in Radiance. However, in order to avoid a kludge in Radiance I had to resort on a bit of a hacky way to let you authenticate yourself. This time we'll work out our own solution to the problem.

The reason why there's a kludge is that Radiance's modules traditionally occupy their own subdomain each. There is currently no mechanism in place to allow flawless translation from/to a host without subdomains. I wanted to avoid having to instruct you to add another entry to your hosts file for each module, but there's currently no other way.

So, in order to avoid the issue altogether until I've resolved it in the framework and can continue on normally, we're going to write our own authentication module instead that doesn't use subdomains. This is also a good opportunity to introduce you to the framework design aspect of Radiance.

Radiance is a 'full package'. It's not a fixed framework, but allows you to exchange components that make up the interfaces we've been using previously. Writing an interface implementation usually starts out with looking at what the interface is supposed to do. Here we hit another roadblock in Radiance development: The specification is not complete yet.

However, for the authentication module this shouldn't be much of an issue. The interface doesn't have to be capable of much and what it has to do is rather obvious. So, let's make use of one of the inspection tools delivered to see what kind of work we're up against:

```commonlisp
(interfaces:print-interface-stub :auth)
```

This will print you a ready to use series of stub functions that we have to implement. So as I said, not much: A function to retrieve the current user `auth:current`, one to associate a user with a session `auth:associate` and one to redirect the user to the login page `auth:login!`. We also have one automatically generated hook that is called for any interface and a secondary hook that we will have to trigger ourselves whenever a user is associated.

That's all there is to this interface at the moment. You may of course inspect any other interface as well now, to see all it has to offer. However, since the spec isn't done yet there won't be any documentation for the functions, so how much you can gather simply from their name is questionable.

Either way, let's get started on implementing this. Once more we'll create a new module for ourselves. However, we'll want to depend on some things, so

```commonlisp
(radiance:create-module "trivial-auth")
```

Our interface will make use of users and sessions, so add those two interfaces to your system's dependencies and reload the system to make sure they're present.

```commonlisp
(asdf:defsystem #:trivial-auth
  :defsystem-depends-on (:radiance)
  :class "radiance:module"
  :components ((:file "trivial-auth"))
  :depends-on ((:interface :session)
               (:interface :user)))
```

Now we'll want to tell the framework that our module is actually implementing an interface. You do that by adding an `:implements` form to your module definition:

```commonlisp
(define-module #:trivial-auth
  (:use #:cl #:radiance)
  (:implements #:auth))
```

Then simply copy-paste the interface stubs (you can leave out the `define-hook` ones, we won't modify them) from the repl into your new source file.

Taking care of the first function, `auth:current`, is just a matter of deciding where to store the user object and bind it to a session. We could use a hash-table for that, but that's messy, since we'd have to hook into when sessions are deleted and do some bookkeeping with that. Instead we can simply employ the session interface's fields feature:

```commonlisp
(defun auth:current (&optional (session *session*))
  (session:field session 'user))
```

Since the `'user` symbol is in our own package we can be sure that no module will accidentally set or read that field. `auth:associate` should be similar degrees of easy to implement:

```commonlisp
(defun auth:associate (user &optional (session *session*))
  (setf (session:field session 'user)
        (etypecase user
          (string (user:get user))
          (user:user user)))
  (trigger 'auth:associate session))
```

Here we add some convenience to allow the users to pass a string of the username as well as a direct user object. We also make sure to trigger the hook we found in the interface definition earlier. And we're already on to the last function. Now we have to consider how to allow users to actually perform a login and what kind of authentication process we want to have.

To keep things simple we'll use a simple password scheme that is hard-coded into our system. If you want you can write your own version that uses the database for storage.

```commonlisp
(defvar *passwords* (make-hash-table :test 'equalp))
```

To make things easier to handle we'll add an accessor for the password. Since getting and setting passwords is probably a thing that someone from outside the module might find useful too, we can use unexterned interface symbols to provide non-standard functionality:

```commonlisp
(defun auth::password (user)
  (etypecase user
    (string (gethash user *passwords*))
    (user:user (auth::password (user:username user)))))

(defun (setf auth::password) (password user)
  (etypecase user
    (string
     (setf (gethash user *passwords*) password))
    (user:user
     (setf (auth::password (user:username user)) password))))
```

We're going to be storing passwords in plaintext for now and adapt it for hashes later. Next we'll want a way to let the users actually interact with this system from the outside. Usually for this kind of thing I employ the strategy of creating API pages for the actual handling and a simple page with a form that redirects to the API:

```commonlisp
(define-api trivial-auth/login (username password) ()
  (or (let ((user (user:get username :if-does-not-exist NIL)))
        (when (and user (string= password (auth::password user)))
          (auth:associate user)
          (if (string= (post/get "browser") "true")
              (redirect "/login" 303)
              (api-output "Login successful."))))
      (error 'api-error "Invalid username or password.")))
```

After using `(setf (auth::password "radguy") "something")` you should now be able to log in using the API call page. However, to make it just a tiny bit nicer we'll add a primitive static page frontend:

```commonlisp
(define-page login #@"/login" ()
  (format NIL "<html><head><title>Login</title></head>
<body>~:[
<form action=\"/api/trivial-auth/login\" method=\"post\">
<input type=\"hidden\" name=\"browser\" value=\"true\" />
<label>Username</label><input type=\"text\" name=\"username\" /><br />
<label>Password</label><input type=\"password\" name=\"password\" /><br />
<input type=\"submit\" name=\"Login\" /></form>~;
<h1>You are already logged in.</h1>~]</body></html>"
          (auth:current)))
```

Finally now that we have a page for users, we can implement `auth:login!`:

```commonlisp
(defun auth:login! (&optional (landing-page (referer *request*)))
  (let ((session (or *session* (session:start))))
    (setf (session:field session 'landing-page) landing-page)
    (redirect "/login" 303)))
```

We'll also want to adapt one line of the login api page to reflect the landing page:

```commonlisp
(redirect (or (session:field *session* 'landing-page) "/") 303)
```

And that's already pretty much all there is to this interface. There are of course a number of improvements to do: A way for users to log out, a better login page, hashing the passwords and storing them somewhere persistent, providing a way for the user to change the password, etc.

However, I think this will suffice as a quick introduction to writing implementations for Radiance interfaces. As you can see, it's extremely straightforward to do and requires practically no extra knowledge to handle.

There probably won't be a new entry in this series for a while until I've battled the issues I mentioned at the beginning at least to some respectable degree.
