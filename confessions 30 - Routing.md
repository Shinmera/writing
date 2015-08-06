![header](http://shinmera.tymoon.eu/public/45448213.jpg)
No, this blog is not about [another ISP issue](http://blog.tymoon.eu/article/282), but rather about the issue of routing in the context of web frameworks. I've mentioned this problem briefly [before](http://blog.tymoon.eu/article/285). The first question that needs answering in order to solve this problem is this: What does routing even need to be able to do?

Traditionally modules in Radiance occupy one or more subdomains for themselves, with some special paths reserved for the system. However, this would exclude a wide variety of people who can't simply use subdomains to their advantage. Therefore one use case would require routes to be able to modify paths and subdomains. Since Radiance also allows the starting of multiple servers on ports it makes sense to also pull in port manipulation.

What we end up with is that a route needs to be able to analyse and affect every part that makes up an URI: The subdomains, the port and the path. Most routing systems I've found in web frameworks only concern themselves with path routing. Most of them also look like an absolute nightmare to me, either due to a lack of flexibility or because their setup is just completely insane. That just won't do for Radiance.

Now that we already face the predicament that we need complete control over the URI, let's see if we can at least somewhat simplify the process. Generally the subdomain part of a URI can be split by its dots and thus we can make the processing thereof comparable to that of a lambda-list, with integrated string comparison. A similar solution can be applied to the path part, splitting it by its slashes. However, the path needs additional flexibility since often times we would like to identify or structure its parts with a regex.

Taking this into account, specifying the matcher part of a route requires two special lambda-lists for the domains and path and an equality checker for the port. Does that sound good? Well, sure, it sounds acceptable if you're writing lisp code, but it sure sounds awful if you try to fit this complicated scheme into something that is easy to write in a configuration file. So that won't do and we haven't even touched on the problems of how to specify regex groups in the target part or anything about that.

At this point of my prototyping I was already going half insane because I couldn't find a good way to specify this with enough flexibility while at the same time cutting it down to a format that could be easily written in a string format. I don't think it's possible either. There's just too much of a stretch between the required amount of flexibility and the restraints of string formats. So what to do then?

Well, split it up into layers of course. My current idea for the routing system is now as follows. At the lowest level there is a very primitive system that only performs one operation: It calls a single function and passes along a mutable URI object. That is the most basic 'route' you can have from an architectural standpoint. This gives you full control to do whatever you please. On the next level there is a matching-route. This route takes five arguments, one each for the subdomain extended-lambda-list, port and path e-l-l, one argument to name the URI variable and lastly the body that performs arbitrary transformations on the URI object. If your e-l-ls contain symbols, the symbols are bound to the corresponding parts. One higher you also have a similarly short, but limited writing form for the target: three parts, two of which are in list form, that make up the URI changes. And finally a 'configurable' form that takes two strings that are compiled into a matcher and target respectively.

Now that I've covered the base idea let's take a look at some examples of these.

```commonlisp
(define-route noop
  ;; Noop!
  )

(define-matching-route virtual-modules (* * (("_(.*)" module) &rest path)) uri
  (push (first module) (domains uri))
  (setf (path uri) path))

(define-target-route blog-alias (("blog") * *) (("reader") * *))
(define-target-route robots (* * ("robots.txt")) (* * ("static" "robots.txt")))
(define-target-route admin-port (() 10000 *) (("admin") 80 *))

(define-string-route weather "shitty.weather/" "/rain")
(define-string-route reverse-domains "([^\\.]*).reverse/(.*)" "/\\1/\\2")
```

Obviously due to their respective restrictions some routes simply are not possible in some schemes. In that case I suppose one will have to simply deal with it and configure it with a lisp file. I don't want to invest the time to create some kind of monster specification DSL just so I can confuse everyone and push it into the configuration that nobody will use because it's too complex. Anyway, I guess I'll explain how these e-l-ls are built as well as the minimal string spec.

```
e-l-l ::= TOKEN* [&optional TOKEN* [&rest symbol]]
TOKEN ::= symbol | string | REGEX
REGEX ::= (regex-string symbol*)
```

A symbol means that the corresponding part/s are bound to that symbol for the body of the route. If you specify a string, the relating part needs to be STRING-EQUAL in the case of a domain check or STRING= in the case of a path check. Each symbol in the REGEX part corresponds to a regex match group. In the case of string specifiers as optional tokens, the entire match fails if the string comparison fails. For domains comparison, the domains are compared in reverse order to how they're usually written in a URI in order to match up with the precedence order.

The target specification is much simpler since it only requires inserting of symbol values and attaching the individual parts with the fitting separator. The star anywhere simply means that it's neither considered for a match or changed in the target.

Finally for the string routes I've decided to go the easy way of just doing full regexes for the entire URI because anything else would just increase complexity immensely without bringing much of a benefit in either readability or flexibility. The only special treatment applied is that dots outside of regex groups match a dot precisely and not regex' default of a dot-all.

I'm also thinking about turning the target-routes into some format that is at least easily printable and readable so it could be dumped into configuration files if so desired. But I'll see. Even after pondering over this problem for a long while and trying to see at other solutions I just don't know if there even is a way that isn't just a pain in the ass in some way. I hope this is at least some approximation of the ideal solution. Right now, I just want to stop thinking about it for a while and regain my sanity.
